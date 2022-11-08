use rayon::prelude::*;
use std::{
    fs::{self, DirEntry, File},
    io::{self, Read},
    path::{Path, PathBuf},
    str::FromStr,
};

use yaipl::frontend::parser::{ast::Ast, ParseError, Parser};
fn compile(src: &str) -> Result<Ast, ParseError> {
    let mut parser = Parser::new(src);
    let root = parser.parse_root_module()?;
    let (idents, _) = parser.into_interners();
    Ok(Ast::new(root, idents))
}

fn test_folder(subfolder: &Path) -> io::Result<impl ParallelIterator<Item = io::Result<DirEntry>>> {
    let mut path = PathBuf::from_str(r"C:\Users\caspe\repos\yaipl\tests\compile_tests").unwrap();
    path.push(subfolder);

    fs::read_dir(path).map(|dir| {
        dir.filter(|a| {
            a.as_ref()
                .map_or(false, |a| a.file_type().unwrap().is_file())
        })
        .par_bridge()
    })
}

fn compile_all_in(subfolder: &Path) -> Vec<(PathBuf, Result<Ast, ParseError>)> {
    let files = test_folder(subfolder).unwrap().map(|a| {
        let file_path = a.unwrap().path();
        (file_path.clone(), File::open(file_path))
    });
    let compiled = files
        .map(|(path, a)| {
            let mut string = String::new();
            a.expect(&path.to_str().unwrap())
                .read_to_string(&mut string)
                .unwrap();
            (path, string)
        })
        .map(|(name, a)| (name, compile(&a)));

    compiled.collect()
}
#[test]
fn assert_compile_success() {
    let files = compile_all_in(Path::new("compile_success"));

    for (name, result) in files {
        assert!(result.is_ok(), "file: {:?}, value: {:#?}", name, result);
    }
}
#[test]
fn assert_compile_faliure() {
    let files = compile_all_in(Path::new("compile_failure"));

    for (name, result) in files {
        assert!(result.is_err(), "file: {:?}, value: {:#?}", name, result);
    }
}
