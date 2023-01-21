use assert_cmd::assert::{Assert, AssertResult};
use assert_cmd::Command;
use rayon::prelude::*;
use std::io;

use std::ops::Deref;
use std::{
    env::current_dir,
    fs::{self},
    path::{Path, PathBuf},
};

fn compile_success_folder() -> impl Iterator<Item = PathBuf> {
    test_folder(Path::new("compile_success"))
}
fn compile_failure_folder() -> impl Iterator<Item = PathBuf> {
    test_folder(Path::new("compile_failure"))
}

fn test_folder(subfolder: &Path) -> impl Iterator<Item = PathBuf> {
    let mut path = current_dir().unwrap();
    path.push(r"tests\compile_tests\");
    path.push(subfolder);

    let dir = fs::read_dir(&path)
        .unwrap_or_else(|err| panic!("Can't open {}, Err: {}", path.display(), err));
    let files = dir.filter_map(|file| {
        let file = file.ok()?;
        if file.file_type().ok()?.is_file() {
            return Some(file.path());
        }
        None
    });

    files
}

fn start_build(path: &Path) -> Command {
    let mut command = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
    command.arg("check").arg(path);
    command
}

fn start_all<'a>(files: impl 'a + Iterator<Item = &'a Path>) -> impl 'a + Iterator<Item = Command> {
    files.map(|file| start_build(file))
}
fn print_with_left_line(text: &str) {
    for line in text.lines() {
        println!("       | {}", line);
    }
}
fn assert_forall<Assertion: Sync + Fn(Assert) -> AssertResult>(
    files: impl Iterator<Item = PathBuf>,
    assertion: Assertion,
) {
    let files: Vec<_> = files.collect();
    let commands = start_all(files.iter().map(Deref::deref))
        .zip(files.iter())
        .par_bridge();
    let results = commands
        .map(|(mut command, path)| (command.assert(), path))
        .map(|(assert, path)| (assertion(assert), path));

    let mut results: Vec<_> = results.collect();
    //places the successes first
    results.sort_unstable_by_key(|(result, _)| result.is_ok());
    if results.iter().all(|(result, _)| result.is_ok()) {
        return;
    }

    for (result, name) in results {
        let succsess = match &result {
            Ok(_) => "PASS",
            Err(_) => "FAIL",
        };
        println!("{} {}", succsess, name.display());

        if let Err(err) = result {
            let assert = err.assert();
            let output = assert.get_output();
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);

            println!("     <std error>");
            print_with_left_line(&stderr);
            println!("     </std error>");
            println!();

            println!("     <std output>");
            print_with_left_line(&stdout);
            println!("     </std output>");
        }
    }
    panic!("Test failed for som files");
}
#[test]
fn assert_compile_success() {
    let files = compile_success_folder();

    assert_forall(files, Assert::try_success);
}
#[test]
fn assert_compile_faliure() {
    let files = compile_failure_folder();

    assert_forall(files, Assert::try_failure);
}
