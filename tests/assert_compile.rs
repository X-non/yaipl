use rayon::prelude::*;
use std::{
    any::Any,
    env::current_dir,
    fs::{self, DirEntry},
    io::{self},
    ops::Range,
    panic::catch_unwind,
    path::{Path, PathBuf},
};

use yaipl::{
    build_program,
    cli::{CLIOptions, DebugFlags},
};

fn try_compile(full_path: PathBuf) -> Result<(), Box<dyn Any + Send>> {
    catch_unwind(|| {
        build_program(&CLIOptions {
            flags: DebugFlags {
                dump_ast: false,
                dump_tokens: false,
            },
            command: yaipl::cli::Command::Check { path: full_path },
        })
    })
}
fn rows_contained(src: &str, range: Range<usize>) -> (&str, Range<usize>) {
    let start = src[..range.start].rfind('\n').unwrap_or(0);
    let end = src[range.end..].find('\n').unwrap_or(src.len());

    (&src[start..end].trim(), start..end)
}

fn test_folder(subfolder: &Path) -> io::Result<impl ParallelIterator<Item = io::Result<DirEntry>>> {
    let mut path = current_dir().unwrap();
    path.push(r"tests\compile_tests\");
    path.push(subfolder);

    fs::read_dir(path).map(|dir| {
        dir.filter(|a| {
            a.as_ref()
                .map_or(false, |a| a.file_type().unwrap().is_file())
        })
        .par_bridge()
    })
}

fn try_compile_all_in(subfolder: &Path) -> Vec<(String, Result<(), Box<dyn Any + Send>>)> {
    test_folder(subfolder)
        .unwrap()
        .map(|dir| {
            let file_path = dir.unwrap().path();
            let name = file_path.to_string_lossy().to_string();
            (name, try_compile(file_path))
        })
        .collect()
}
#[test]
fn assert_compile_success() {
    let compiled = try_compile_all_in(Path::new("compile_success"));
    for (name, result) in compiled {
        eprintln!("Start of {name}");
        assert!(
            result.is_ok(),
            "{:?}",
            result.as_ref().map_err(|err| err.downcast_ref::<String>())
        );

        eprintln!("End of {name}");
    }
}
#[test]
fn assert_compile_faliure() {
    let compiled = try_compile_all_in(Path::new("compile_failure"));
    for (name, result) in compiled {
        eprintln!("Start of {name}");
        assert!(result.is_err(), "{:?}", result);
        eprintln!("End of {name}");
    }
}
