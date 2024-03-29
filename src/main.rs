use std::collections::BTreeSet;

use clap::Parser;
use oma_apt::new_cache;
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

    #[cfg(not(feature = "local"))]
    {
        let client = reqwest::blocking::Client::builder()
            .user_agent("resolvo-deb")
            .build()
            .unwrap();

        let arch = get_arch_name().unwrap();

        let mut packages_file = String::new();

        for i in [arch, "all"] {
            let arch_packages = client
                .get(format!(
                    "https://repo.aosc.io/debs/dists/stable/main/binary-{i}/Packages"
                ))
                .send()
                .unwrap()
                .error_for_status()
                .unwrap()
                .text()
                .unwrap();

            packages_file.push_str(&arch_packages);
        }

        let mut solver = DebSolver::new(&packages_file);

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

    #[cfg(feature = "local")]
    {
        let mut solver = DebSolver::new_local().unwrap();
        let cache = new_cache!().unwrap();

        let mut pkgs_res = vec![];
        for i in pkgs {
            let pkg = cache.get(&i).unwrap();
            let pkg = if pkg.versions().collect::<Vec<_>>().is_empty() {
                if let Some(pkg) = pkg.provides().next().map(|x| x.target_pkg()) {
                    pkg
                } else {
                    pkg.unique()
                }
            } else {
                pkg.unique()
            };

            let pkg = oma_apt::package::Package::new(&cache, pkg);
            let cand = pkg.candidate().unwrap().unique();
            pkgs_res.push(cand);
        }

        let solvables = solver.get_requirement(pkgs_res).unwrap();
        let solvables = match solver.solve(solvables){
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
}

/// AOSC OS specific architecture mapping for ppc64
#[cfg(target_arch = "powerpc64")]
#[cfg(not(feature = "local"))]
#[inline]
pub(crate) fn get_arch_name() -> Option<&'static str> {
    let mut endian: libc::c_int = -1;
    let result;
    unsafe {
        result = libc::prctl(libc::PR_GET_ENDIAN, &mut endian as *mut libc::c_int);
    }
    if result < 0 {
        return None;
    }
    match endian {
        libc::PR_ENDIAN_LITTLE | libc::PR_ENDIAN_PPC_LITTLE => Some("ppc64el"),
        libc::PR_ENDIAN_BIG => Some("ppc64"),
        _ => None,
    }
}

/// AOSC OS specific architecture mapping table
#[cfg(not(target_arch = "powerpc64"))]
#[cfg(not(feature = "local"))]
#[inline]
pub(crate) fn get_arch_name() -> Option<&'static str> {
    use std::env::consts::ARCH;
    match ARCH {
        "x86_64" => Some("amd64"),
        "x86" => Some("i486"),
        "powerpc" => Some("powerpc"),
        "aarch64" => Some("arm64"),
        "mips64" => Some("loongson3"),
        "riscv64" => Some("riscv64"),
        "loongarch64" => Some("loongarch64"),
        _ => None,
    }
}
