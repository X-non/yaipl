use rayon::prelude::*;
use std::{
    fs::{self, DirEntry, File},
    io::{self, Read},
    ops::Range,
    path::{Path, PathBuf},
    str::FromStr,
};

use yaipl::frontend::parser::{ast::Ast, ParseError, Parser};
fn compile(src: &str) -> Result<Ast, ParseError> {
    let mut parser = Parser::new(src);
    let root = parser.parse_root_module()?;
    let (idents, strings) = parser.into_interners();
    Ok(Ast::new(root, idents, strings))
}
fn rows_contained(src: &str, range: Range<usize>) -> (&str, Range<usize>) {
    let start = src[..range.start].rfind('\n').unwrap_or(0);
    let end = src[range.end..].find('\n').unwrap_or(src.len());

    (&src[start..end].trim(), start..end)
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

fn compile_all_in(subfolder: &Path) -> Vec<(PathBuf, Result<Ast, ParseError>, String)> {
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
        .map(|(name, a)| (name, compile(&a), a));

    compiled.collect()
}
#[test]
fn assert_compile_success() {
    let files = compile_all_in(Path::new("compile_success"));

    for (name, result, src) in files {
        match result {
            Err(err) => {
                let extra_info = match &err {
                    ParseError::UnexpectedToken(range) => {
                        Some(rows_contained(&src, range.into_src_range()))
                    }
                    _ => None,
                };
                if let Some((text, range)) = extra_info {
                    eprintln!("{}", range.start);
                    eprintln!("{}", text);
                    eprintln!("{}", range.end)
                }
                panic!("file: {name:?}, err: {err:#?}");
            }
            Ok(_) => {}
        }
    }
}
#[test]
fn assert_compile_faliure() {
    let files = compile_all_in(Path::new("compile_failure"));

    for (name, result, _) in files {
        assert!(result.is_err(), "file: {:?}, value: {:#?}", name, result);
    }
}
