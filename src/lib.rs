use provider::DebProvider;
use resolvo::{problem::Problem, SolvableId, Solver};

use crate::{pkgversion::PkgVersion, provider::Requirement};

mod pkgversion;
mod provider;

pub fn create_solver() -> Solver<Requirement, String, DebProvider> {
    let provider = DebProvider::from_repodata(true);
    let solver = resolvo::Solver::new(provider);

    solver
}

pub fn resolve(
    solver: &mut Solver<Requirement, String, DebProvider>,
    pkgs: Vec<String>,
) -> Result<Vec<SolvableId>, Problem> {
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

    solver.solve(specs)
}
