use solver::DebPkgPool;
#[cfg(feature = "local")]
use solver::ResolvoDebError;
use resolvo::{problem::Problem, SolvableId, Solver};
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

    pub fn solve(&mut self, pkgs: Vec<String>) -> Result<Vec<SolvableId>, Problem> {
        let mut specs = Vec::new();
        for pkg in pkgs {
            let spec = Requirement {
                name: pkg.to_string(),
                flags: Some("GE".into()),
                version: Some(PkgVersion::try_from("0").unwrap()),
                preinstall: false,
            };

            info!("Resolving for: {}", spec);
            let name_id = self.0.pool().intern_package_name(pkg);
            let spec_id = self.0.pool().intern_version_set(name_id, spec);

            specs.push(spec_id);
        }

        self.0.solve(specs)
    }

    #[cfg(feature = "local")]
    pub fn new_local() -> Result<Self, ResolvoDebError> {
        let solver = DebPkgPool::from_local_cache()?;
        let solver = resolvo::Solver::new(solver);

        Ok(Self(solver))
    }
}
