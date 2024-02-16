use std::collections::BTreeSet;

use clap::Parser;
use resolvo::DefaultSolvableDisplay;
use resolvo_deb::DebSolver;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    packages: Vec<String>,
}

fn main() {
    let args = Args::parse();
    let pkgs = args.packages;

    tracing_subscriber::fmt::init();

    let mut solver = DebSolver::new();
    let solvables = match solver.solve(pkgs) {
        Ok(solvables) => solvables,
        Err(problem) => {
            println!(
                "Problem: {}",
                problem.display_user_friendly(&solver.0, &DefaultSolvableDisplay)
            );
            return;
        }
    };

    let resolved: BTreeSet<String> = solvables
        .iter()
        .map(|s| s.display(solver.0.pool()).to_string())
        .collect();

    println!("Resolved:\n");

    for r in resolved {
        println!("- {}", r);
    }
}
