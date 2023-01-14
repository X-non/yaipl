use std::{cell::RefCell, path::Path, rc::Rc};

use crate::frontend::{parser::ParseError, span::Span};

pub struct DiagnosticContext<'src> {
    src: &'src str,
    filepath: Rc<Path>,

    newlines_generated_until: u32,
    newlines: RefCell<Vec<u32>>,
}

impl<'src> DiagnosticContext<'src> {
    pub fn new(file: &Path, src: &'src str) -> Self {
        Self {
            filepath: file.into(),
            src,
            newlines_generated_until: 0,
            newlines: Default::default(),
        }
    }
    pub fn resolve_span(&self, span: Span) -> SrcFileCoordinate {
        //FIXME: mabye use the cache
        resolve_span_from_src(self.src, span)
    }
    pub fn report_parse_error(&self, err: ParseError) -> ! {
        let location = match err.span() {
            Some(span) => format!("{}", self.resolve_span(span)),
            None => "*UNLOCATED*".to_string(),
        };

        panic!(
            "\nParse Error @ {}{} \n {:?} \n",
            self.filepath.to_string_lossy(),
            location,
            err
        );
    }

    pub fn filepath(&self) -> &Path {
        self.filepath.as_ref()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SrcFileCoordinate {
    pub line: u32,
    pub column: u32,
}

impl std::fmt::Display for SrcFileCoordinate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{}:{}", self.line, self.column)
    }
}

pub fn resolve_span(newlines: &[u32], span: Span) -> SrcFileCoordinate {
    let index = span.byte_offset();
    let newlines_before = newlines_before(newlines, index);
    let newline_offset = newlines[newlines_before];

    SrcFileCoordinate {
        line: newlines_before as u32 + 1,
        column: index - newline_offset + 1,
    }
}
pub fn resolve_span_from_src(src: &str, span: Span) -> SrcFileCoordinate {
    let before = &src[..(span.byte_offset() as usize)];
    let line = before.chars().filter(|&c| c == '\n').count() + 1; // line number starts counting at 1
    let fix_first_line = if line == 1 { 1 } else { 0 };
    let col = before.len() - before.rfind('\n').unwrap_or_default() + fix_first_line;

    SrcFileCoordinate {
        line: line as u32,
        column: col as u32,
    }
}

fn newlines_before(newlines: &[u32], index: u32) -> usize {
    let newlines_before = match newlines.binary_search(&index) {
        Ok(num) | Err(num) => num,
    };

    newlines_before
}
