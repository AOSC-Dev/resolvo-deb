use oma_apt::{cache::PackageSort, new_cache, package::DepType};
use resolvo::{
    Candidates, Dependencies, DependencyProvider, NameId, Pool, SolvableId, SolverCache, VersionSet,
};
use version_compare::Cmp;

use crate::{pkgversion::PkgVersion, OmaDependency};
use std::{collections::HashMap, fmt::Display, hash::Hash};

#[derive(Debug, Clone)]
pub struct DebPackageVersion {
    pub name: String,
    pub version: PkgVersion,
    pub requires: Vec<Requirement>,
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

        println!("Comparing: {} {:?} {}", v_package, cmp.unwrap(), v_test);

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
            let mut requires = vec![];
            if let Some(cand) = pkg.candidate() {
                let deps = cand.depends_map();
                let deps = deps
                    .get(&DepType::Depends)
                    .map(|x| OmaDependency::map_deps(x).inner());

                if let Some(deps) = deps {
                    for dep in deps {
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
                }
            }

            let pack = DebPackageVersion {
                name: pkg.name().to_string(),
                version: PkgVersion::try_from(pkg.candidate().unwrap().version()).unwrap(),
                requires,
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

        let mut result = Dependencies::default();

        for req in requirements {
            if req.name.starts_with('/') || req.name.contains(" if ") {
                continue;
            };
            let dep_name = self.pool.intern_package_name(&req.name);
            let dep_spec = self.pool.intern_version_set(dep_name, req.clone());
            result.requirements.push(dep_spec);
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
