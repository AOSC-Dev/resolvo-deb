use std::collections::BTreeSet;

use clap::Parser;
use resolvo::DefaultSolvableDisplay;
use resolvo_deb::{create_solver, resolve};

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    packages: Vec<String>,
}

fn main() {
    let args = Args::parse();
    let pkgs = args.packages;

    tracing_subscriber::fmt::init();

    let mut solver = create_solver();
    let solvables = match resolve(&mut solver, pkgs) {
        Ok(solvables) => solvables,
        Err(problem) => {
            println!("Problem: {}", problem.display_user_friendly(&solver, &DefaultSolvableDisplay));
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
