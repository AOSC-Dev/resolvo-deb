use std::{collections::BTreeSet, fmt::Display};

use clap::Parser;
use oma_apt::package::{BaseDep, DepType, Dependency};
use pkgversion::PkgVersion;
use provider::{DebProvider, Requirement};
use resolvo::DefaultSolvableDisplay;


mod provider;
mod pkgversion;

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

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    packages: Vec<String>,
}

fn main() {
    let args = Args::parse();
    let pkgs = args.packages;

    tracing_subscriber::fmt::init();

    let provider = DebProvider::from_repodata(true);
    let mut solver = resolvo::Solver::new(provider);
    let mut specs = Vec::new();
    for pkg in pkgs {
        let spec = Requirement {
            name: pkg.to_string(),
            flags: Some("GT".into()),
            version: Some(PkgVersion::try_from("0").unwrap()),
            preinstall: false,
        };
        
        println!("Resolving for: {}", spec);
        let name_id = solver.pool().intern_package_name(pkg);
        let spec_id = solver.pool().intern_version_set(name_id, spec);

        specs.push(spec_id);
    }

    let solvables = match solver.solve(specs) {
        Ok(solvables) => solvables,
        Err(problem) => {
            println!(
                "Error: {}",
                problem.display_user_friendly(&solver, &DefaultSolvableDisplay)
            );
            return;
        }
    };

    let resolved: BTreeSet<String> = solvables
        .iter()
        .map(|s| s.display(solver.pool()).to_string())
        .collect();

    println!("Resolved:\n");

    for r in resolved {
        println!("- {}", r);
    }
}
