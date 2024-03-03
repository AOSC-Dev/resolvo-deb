#[cfg(feature = "local")]
use oma_apt::raw::cache::raw::Version;
use resolvo::{problem::Problem, SolvableId, Solver};
use solver::DebPkgPool;
#[cfg(feature = "local")]
use solver::ResolvoDebError;
use tracing::info;

use crate::{pkgversion::PkgVersion, solver::Requirement};

mod pkgversion;
mod solver;

pub struct DebSolver(pub Solver<Requirement, String, DebPkgPool>);

impl DebSolver {
    pub fn new(packages_file: &str) -> Self {
        let provider = DebPkgPool::from_repodata(packages_file);
        let solver = resolvo::Solver::new(provider);

        Self(solver)
    }

    pub fn solve(&mut self, pkgs: Vec<Requirement>) -> Result<Vec<SolvableId>, Problem> {
        let mut specs = Vec::new();
        for spec in pkgs {
            info!("Resolving for: {}", &spec);
            let name_id = self.0.pool().intern_package_name(&spec.name);
            let spec_id = self.0.pool().intern_version_set(name_id, spec);

            specs.push(spec_id);
        }

        self.0.solve(specs)
    }

    #[cfg(feature = "local")]
    pub fn get_requirement(
        &mut self,
        pkgs: Vec<Version>,
    ) -> Result<Vec<Requirement>, ResolvoDebError> {
        use oma_apt::{cache::PackageSort, new_cache};

        let mut specs = Vec::new();
        for pkg in pkgs {
            let name = pkg.parent_pkg().name().to_string();
            let spec = Requirement {
                name: name.to_string(),
                flags: Some("EQ".to_owned()),
                version: Some(PkgVersion::try_from(pkg.version()).unwrap()),
                preinstall: false,
            };

            specs.push(spec);
        }

        let cache = new_cache!()?;
        let installed = cache.packages(&PackageSort::default().installed())?;

        for i in installed {
            let name = i.name().to_string();
            let spec = Requirement {
                name: name.to_string(),
                flags: Some("EQ".to_owned()),
                version: Some(PkgVersion::try_from(i.installed().unwrap().version()).unwrap()),
                preinstall: false,
            };

            specs.push(spec);
        }

        Ok(specs)
    }

    #[cfg(feature = "local")]
    pub fn new_local() -> Result<Self, ResolvoDebError> {
        let solver = DebPkgPool::from_local_cache()?;
        let solver = resolvo::Solver::new(solver);

        Ok(Self(solver))
    }
}
