use oma_debcontrol::Paragraph;
use resolvo::{
    Candidates, Dependencies, DependencyProvider, NameId, Pool, SolvableId, SolverCache, VersionSet,
};
use thiserror::Error;
use tracing::info;
use version_compare::Cmp;

use crate::pkgversion::PkgVersion;
use std::{collections::HashMap, fmt::Display, hash::Hash};

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

        let res = match cmp {
            Some(Cmp::Eq) => v_package == v_test,
            Some(Cmp::Gt) => v_package > v_test,
            Some(Cmp::Ge) => v_package >= v_test,
            Some(Cmp::Lt) => v_package < v_test,
            Some(Cmp::Le) => v_package <= v_test,
            Some(Cmp::Ne) => v_package != v_test,
            None => true,
        };

        info!(
            "Compare: {} {:?} {} {}",
            v_package,
            cmp.unwrap(),
            v_test,
            res
        );

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
    pub fn from_repodata(packages: &str, disable_suggest: bool) -> Self {
        let packages = Packages::new(packages).unwrap();

        let pool = Pool::default();
        let mut solve_packages = HashMap::new();

        for pkg in packages.0 {
            let (requires, limits) = get_requirment(&pkg);

            let pack = DebPackageVersion {
                name: pkg.name.to_string(),
                version: PkgVersion::try_from(pkg.version.unwrap().as_str()).unwrap(),
                requires,
                limits,
            };

            let name_id = pool.intern_package_name(&pkg.name);
            let solvable = pool.intern_solvable(name_id, pack.clone());

            let provides = solve_packages
                .entry(pkg.name.to_string())
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

            b.version.cmp(&a.version)
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

fn get_requirment(pkg: &DebPackage) -> (Vec<Requirement>, Vec<Requirement>) {
    let mut requires = vec![];
    let mut limit = vec![];
    if let Some(cand) = &pkg.version {
        let deps = pkg.depends.clone();
        let deps_pre = pkg.pre_depends.clone();

        let mut all_deps = vec![];
        all_deps.extend(deps);
        all_deps.extend(deps_pre);

        for dep in all_deps {
            requires.push(Requirement {
                name: dep.name,
                flags: match dep.comp {
                    Some(ref comp) => match comp.symbol.as_str() {
                        ">=" => Some("GE".to_string()),
                        ">" => Some("GT".to_string()),
                        "<=" => Some("LE".to_string()),
                        "<" => Some("LT".to_string()),
                        "=" => Some("EQ".to_string()),
                        ">>" => Some("GT".to_string()),
                        "<<" => Some("LT".to_string()),
                        _ => None,
                    },
                    None => None,
                },
                version: match dep.comp {
                    Some(comp) => Some(PkgVersion::try_from(comp.ver.as_str()).unwrap()),
                    None => None,
                },
                preinstall: false,
            })
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

        let mut all_rev_ship_deps = vec![];
        all_rev_ship_deps.extend(pkg.breaks.clone());
        all_rev_ship_deps.extend(pkg.conflicts.clone());

        for dep in all_rev_ship_deps {
            limit.push(Requirement {
                name: dep.name,
                flags: match dep.comp {
                    Some(ref comp) => match comp.symbol.as_str() {
                        ">=" => Some("LT".to_string()),
                        ">" => Some("LE".to_string()),
                        "<=" => Some("GT".to_string()),
                        "<" => Some("GE".to_string()),
                        "=" => Some("NE".to_string()),
                        ">>" => Some("LE".to_string()),
                        "<<" => Some("GE".to_string()),
                        _ => None,
                    },
                    _ => None,
                },
                version: match dep.comp {
                    Some(c) => Some(PkgVersion::try_from(c.ver.as_str()).unwrap()),
                    None => None,
                },
                preinstall: false,
            })
        }
    }

    (requires, limit)
}

#[derive(Debug, Error)]
pub enum ResolvoDebError {
    #[error("{0}")]
    SyntaxError(String),
    #[error("Has no name")]
    MissingName,
}

struct Packages(Vec<DebPackage>);

#[derive(Clone)]
struct DebPackage {
    name: String,
    version: Option<String>,
    arch: Option<String>,
    depends: Vec<Dep>,
    breaks: Vec<Dep>,
    conflicts: Vec<Dep>,
    replaces: Vec<Dep>,
    pre_depends: Vec<Dep>,
}

impl Packages {
    fn new(packages: &str) -> Result<Self, ResolvoDebError> {
        let packages = oma_debcontrol::parse_str(packages)
            .map_err(|e| ResolvoDebError::SyntaxError(e.to_string()))?;

        let mut res = vec![];

        for pkg in packages {
            let name = debcontrol_field(&pkg, "Package");
            let version = debcontrol_field(&pkg, "Version");
            let arch = debcontrol_field(&pkg, "Architecture");
            let depends = debcontrol_field(&pkg, "Depends")
                .map(|x| parse_deps(&x))
                .unwrap_or_default();
            let breaks = debcontrol_field(&pkg, "Breaks")
                .map(|x| parse_deps(&x))
                .unwrap_or_default();
            let conflicts = debcontrol_field(&pkg, "Conflicts")
                .map(|x| parse_deps(&x))
                .unwrap_or_default();
            let replaces = debcontrol_field(&pkg, "Replaces")
                .map(|x| parse_deps(&x))
                .unwrap_or_default();
            let pre_depends = debcontrol_field(&pkg, "PreDepends")
                .map(|x| parse_deps(&x))
                .unwrap_or_default();

            res.push(DebPackage {
                name: name.ok_or_else(|| ResolvoDebError::MissingName)?,
                version,
                arch,
                depends,
                breaks,
                conflicts,
                replaces,
                pre_depends,
            });
        }

        Ok(Self(res))
    }
}

fn debcontrol_field(value: &Paragraph, field: &str) -> Option<String> {
    value
        .fields
        .iter()
        .find(|x| x.name == field)
        .map(|x| x.value.to_string())
}

#[derive(Clone)]
struct Dep {
    name: String,
    comp: Option<Comp>,
}

#[derive(Clone)]
struct Comp {
    symbol: String,
    ver: String,
}

fn parse_deps(s: &str) -> Vec<Dep> {
    let mut res = vec![];
    let v = s.trim().split(',');

    for i in v {
        let name = i.trim().split_once(" ");
        if let Some((name, comp)) = name {
            res.push(Dep {
                name: name.to_string(),
                comp: parse_comp(comp),
            })
        } else {
            res.push(Dep {
                name: i.trim().to_string(),
                comp: None,
            })
        }
    }

    res
}

fn parse_comp(s: &str) -> Option<Comp> {
    let s = s.trim();
    let s = s.strip_prefix('(').and_then(|x| x.strip_suffix(')'))?;
    let (symbol, ver) = s.split_once(' ')?;

    Some(Comp {
        symbol: symbol.to_string(),
        ver: ver.to_string(),
    })
}
