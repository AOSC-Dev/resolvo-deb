use oma_apt::{
    cache::PackageSort,
    new_cache,
    package::{BaseDep, DepType, Dependency, Package},
};
use resolvo::{
    Candidates, Dependencies, DependencyProvider, NameId, Pool, SolvableId, SolverCache, VersionSet,
};
use tracing::info;
use version_compare::Cmp;

use crate::pkgversion::PkgVersion;
use std::{collections::HashMap, fmt::Display, hash::Hash};

#[derive(Debug, Clone)]
pub struct OmaDependency {
    pub name: String,
    pub comp_symbol: Option<String>,
    pub ver: Option<String>,
    pub target_ver: Option<String>,
    pub comp_ver: Option<String>,
}

impl From<&BaseDep<'_>> for OmaDependency {
    fn from(dep: &BaseDep) -> Self {
        Self {
            name: dep.name().to_owned(),
            comp_symbol: dep.comp().map(|x| x.to_string()),
            ver: dep.version().map(|x| x.to_string()),
            target_ver: dep.target_ver().ok().map(|x| x.to_string()),
            comp_ver: dep
                .comp()
                .and_then(|x| Some(format!("{x} {}", dep.version()?))),
        }
    }
}

#[derive(Debug)]
pub struct OmaDependencyGroup(Vec<Vec<OmaDependency>>);

impl OmaDependencyGroup {
    pub fn inner(self) -> Vec<Vec<OmaDependency>> {
        self.0
    }
}

impl Display for OmaDependencyGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, d) in self.0.iter().enumerate() {
            if d.len() == 1 {
                // 如果数组长度为一，则肯定第一个位置有值
                // 因此直接 unwrap
                let dep = d.first().unwrap();
                f.write_str(&dep.name)?;
                if let Some(comp) = &dep.comp_ver {
                    f.write_str(&format!(" ({comp})"))?;
                }
                if i != self.0.len() - 1 {
                    f.write_str(", ")?;
                }
            } else {
                let total = d.len() - 1;
                for (num, base_dep) in d.iter().enumerate() {
                    f.write_str(&base_dep.name)?;
                    if let Some(comp) = &base_dep.comp_ver {
                        f.write_str(&format!(" ({comp})"))?;
                    }
                    if i != self.0.len() - 1 {
                        if num != total {
                            f.write_str(" | ")?;
                        } else {
                            f.write_str(", ")?;
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

impl OmaDependency {
    pub fn map_deps(deps: &[Dependency]) -> OmaDependencyGroup {
        let mut res = vec![];

        for dep in deps {
            if dep.is_or() {
                let mut v = vec![];
                for base_dep in &dep.base_deps {
                    v.push(Self::from(base_dep));
                }
                res.push(v);
            } else {
                let lone_dep = dep.first();
                res.push(vec![Self::from(lone_dep)]);
            }
        }

        OmaDependencyGroup(res)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum OmaDepType {
    Depends,
    PreDepends,
    Suggests,
    Recommends,
    Conflicts,
    Replaces,
    Obsoletes,
    Breaks,
    Enhances,
}

impl Display for OmaDepType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<&DepType> for OmaDepType {
    fn from(v: &oma_apt::package::DepType) -> Self {
        match v {
            oma_apt::package::DepType::Depends => OmaDepType::Depends,
            oma_apt::package::DepType::PreDepends => OmaDepType::PreDepends,
            oma_apt::package::DepType::Suggests => OmaDepType::Suggests,
            oma_apt::package::DepType::Recommends => OmaDepType::Recommends,
            oma_apt::package::DepType::Conflicts => OmaDepType::Conflicts,
            oma_apt::package::DepType::Replaces => OmaDepType::Replaces,
            oma_apt::package::DepType::Obsoletes => OmaDepType::Obsoletes,
            oma_apt::package::DepType::Breaks => OmaDepType::Breaks,
            oma_apt::package::DepType::Enhances => OmaDepType::Enhances,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DebPackageVersion {
    pub name: String,
    pub version: PkgVersion,
    pub requires: Vec<Requirement>,
    pub limits: Vec<Requirement>,
    // pub suggests: Vec<Requirement>,
    // provides: Vec<Requirement>,
}

#[derive(Debug, Clone)]
pub struct Requirement {
    pub name: String,
    pub flags: Option<String>,
    pub version: Option<PkgVersion>,
    pub preinstall: bool,
}

impl PartialEq for Requirement {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.version == other.version && self.flags == other.flags
    }
}

impl Eq for Requirement {}

impl Hash for Requirement {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.version.as_ref().map(|x| x.to_string()).hash(state);
        self.flags.hash(state);
    }
}

impl Display for Requirement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let req = &self;
        write!(
            f,
            "{}-{}",
            req.flags.as_ref().unwrap_or(&"UNDEF".to_string()),
            req.version
                .as_ref()
                .map(|x| x.to_string())
                .unwrap_or("UNDEF".to_string())
        )
    }
}

impl PartialEq for DebPackageVersion {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.version == other.version
    }
}

impl std::cmp::Eq for DebPackageVersion {}

impl PartialOrd for DebPackageVersion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.version.cmp(&other.version))
    }
}

impl Ord for DebPackageVersion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.version.cmp(&other.version)
    }
}

impl Display for DebPackageVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.version)?;
        Ok(())
    }
}

impl Requirement {
    fn to_cmp(&self) -> Option<version_compare::Cmp> {
        match self.flags.as_deref().unwrap_or("") {
            "EQ" => Some(version_compare::Cmp::Eq),
            "GT" => Some(version_compare::Cmp::Gt),
            "GE" => Some(version_compare::Cmp::Ge),
            "LT" => Some(version_compare::Cmp::Lt),
            "LE" => Some(version_compare::Cmp::Le),
            "NE" => Some(version_compare::Cmp::Ne),
            _ => None,
        }
    }
}

impl VersionSet for Requirement {
    type V = DebPackageVersion;

    fn contains(&self, other: &Self::V) -> bool {
        let v_package = &other.version;
        let cmp = self.to_cmp();
        if cmp.is_none() || self.version.is_none() {
            return true;
        }

        let v_test = self.version.as_ref().unwrap();

        info!("Comparing: {} {:?} {}", v_package, cmp.unwrap(), v_test);

        let res = match cmp {
            Some(Cmp::Eq) => v_package == v_test,
            Some(Cmp::Gt) => v_package > v_test,
            Some(Cmp::Ge) => v_package >= v_test,
            Some(Cmp::Lt) => v_package < v_test,
            Some(Cmp::Le) => v_package <= v_test,
            Some(Cmp::Ne) => v_package != v_test,
            None => true,
        };

        res
    }
}

#[derive(Default)]
pub struct DebProvider {
    pub pool: Pool<Requirement>,
    pub solve_packages: HashMap<String, Vec<SolvableId>>,
    // todo: this should disable individual rules / requirements
    pub disable_suggest: bool,
}

impl DebProvider {
    pub fn from_repodata(disable_suggest: bool) -> Self {
        let cache = new_cache!().unwrap();
        let packages = cache.packages(&PackageSort::default()).unwrap();

        let pool = Pool::default();
        let mut solve_packages = HashMap::new();

        for pkg in packages {
            let (requires, limits) = get_requirment(&pkg);

            let pack = DebPackageVersion {
                name: pkg.name().to_string(),
                version: PkgVersion::try_from(pkg.candidate().unwrap().version()).unwrap(),
                requires,
                limits,
            };

            let name_id = pool.intern_package_name(pkg.name());
            let solvable = pool.intern_solvable(name_id, pack.clone());

            let provides = solve_packages
                .entry(pkg.name().to_string())
                .or_insert_with(Vec::new);

            provides.push(solvable);
        }

        Self {
            pool,
            solve_packages,
            disable_suggest,
        }
    }
}

impl DependencyProvider<Requirement> for DebProvider {
    fn pool(&self) -> &Pool<Requirement> {
        &self.pool
    }

    fn sort_candidates(
        &self,
        _solver: &SolverCache<Requirement, String, Self>,
        solvables: &mut [SolvableId],
    ) {
        solvables.sort_by(|a, b| {
            let a = self.pool.resolve_solvable(*a).inner();
            let b = self.pool.resolve_solvable(*b).inner();

            a.version.cmp(&b.version)
        });
    }

    fn get_candidates(&self, name: NameId) -> Option<Candidates> {
        let package_name = self.pool.resolve_package_name(name);
        let _package = self.solve_packages.get(package_name)?;
        let candidates = match self.solve_packages.get(package_name) {
            Some(candidates) => candidates.clone(),
            None => Vec::default(),
        };
        let mut candidates = Candidates {
            candidates,
            ..Candidates::default()
        };

        candidates.hint_dependencies_available = candidates.candidates.clone();

        // let favor = self.favored.get(package_name);
        // let locked = self.locked.get(package_name);
        // let excluded = self.excluded.get(package_name);
        // for pack in package {
        //     let solvable = self.pool.resolve_solvable(*pack);
        //     candidates.candidates.push(solvable);
        //     // if Some(pack) == favor {
        //     //     candidates.favored = Some(solvable);
        //     // }
        //     // if Some(pack) == locked {
        //     //     candidates.locked = Some(solvable);
        //     // }
        //     // if let Some(excluded) = excluded.and_then(|d| d.get(pack)) {
        //     //     candidates
        //     //         .excluded
        //     //         .push((solvable, self.pool.intern_string(excluded)));
        //     // }
        // }

        Some(candidates)
    }

    fn get_dependencies(&self, solvable: SolvableId) -> Dependencies {
        let candidate = self.pool.resolve_solvable(solvable);
        let _package_name = self.pool.resolve_package_name(candidate.name_id());
        let pack = candidate.inner();

        let requirements = &pack.requires;
        let limits = &pack.limits;

        let mut result = Dependencies::default();

        for req in requirements {
            let dep_name = self.pool.intern_package_name(&req.name);
            let dep_spec = self.pool.intern_version_set(dep_name, req.clone());
            result.requirements.push(dep_spec);
        }

        for limit in limits {
            let dep_name = self.pool.intern_package_name(&limit.name);
            let dep_spec = self.pool.intern_version_set(dep_name, limit.clone());
            result.constrains.push(dep_spec);
        }
        // if !self.disable_suggest {
        //     for req in &pack.suggests {
        //         if req.name.starts_with('/') || req.name.contains(" if ") {
        //             continue;
        //         };
        //         let dep_name = self.pool.intern_package_name(&req.name);
        //         let dep_spec = self.pool.intern_version_set(dep_name, req.clone());
        //         result.requirements.push(dep_spec);
        //     }
        // }

        result
    }
}

fn get_requirment(pkg: &Package) -> (Vec<Requirement>, Vec<Requirement>) {
    let mut requires = vec![];
    let mut limit = vec![];
    if let Some(cand) = pkg.candidate() {
        let deps_map = cand.depends_map();
        let deps = deps_map
            .get(&DepType::Depends)
            .map(|x| OmaDependency::map_deps(x).inner());

        let deps_pre = deps_map
            .get(&DepType::PreDepends)
            .map(|x| OmaDependency::map_deps(x).inner());

        let mut all_deps = vec![];

        if let Some(deps) = deps {
            all_deps.extend(deps);
        }

        if let Some(deps) = deps_pre {
            all_deps.extend(deps);
        }

        for dep in all_deps {
            for b in dep {
                requires.push(Requirement {
                    name: b.name,
                    flags: match b.comp_symbol.as_deref() {
                        Some(">=") => Some("GE".to_string()),
                        Some(">") => Some("GT".to_string()),
                        Some("<=") => Some("LE".to_string()),
                        Some("<") => Some("LT".to_string()),
                        Some("=") => Some("EQ".to_string()),
                        Some(">>") => Some("GT".to_string()),
                        Some("<<") => Some("LT".to_string()),
                        _ => None,
                    },
                    version: b.ver.map(|x| PkgVersion::try_from(x.as_str()).unwrap()),
                    preinstall: false,
                })
            }
        }

        // a (replace b <= 1.0)
        // 当 b <= 1.0 时，a 就是 b
        // 因此，c dep b <= 1.0 就是 c dep a
        // let replaces = deps_map
        //     .get(&DepType::Replaces)
        //     .map(|x| OmaDependency::map_deps(x).inner());

        // if let Some(replaces) = replaces {
        //     for dep in replaces {
        //         for b in dep {

        //         }
        //     }
        // }

        let breaks = deps_map
            .get(&DepType::Breaks)
            .map(|x| OmaDependency::map_deps(x).inner());
        let conflicts = deps_map
            .get(&DepType::Conflicts)
            .map(|x| OmaDependency::map_deps(x).inner());

        let mut all_rev_ship_deps = vec![];

        if let Some(breaks) = breaks {
            all_rev_ship_deps.extend(breaks);
        }

        if let Some(conflicts) = conflicts {
            all_rev_ship_deps.extend(conflicts);
        }

        for dep in all_rev_ship_deps {
            for b in dep {
                limit.push(Requirement {
                    name: b.name,
                    flags: match b.comp_symbol.as_deref() {
                        Some(">=") => Some("LT".to_string()),
                        Some(">") => Some("LE".to_string()),
                        Some("<=") => Some("GT".to_string()),
                        Some("<") => Some("GE".to_string()),
                        Some("=") => Some("NE".to_string()),
                        Some(">>") => Some("LE".to_string()),
                        Some("<<") => Some("GE".to_string()),
                        _ => None,
                    },
                    version: b.ver.map(|x| PkgVersion::try_from(x.as_str()).unwrap()),
                    preinstall: true,
                })
            }
        }
    }

    (requires, limit)
}
