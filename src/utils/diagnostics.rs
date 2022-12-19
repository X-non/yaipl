use std::cell::RefCell;

use crate::frontend::{parser::ParseError, span::Span};

pub struct DiagnosticContext<'src> {
    src: &'src str,

    newlines_generated_until: u32,
    newlines: RefCell<Vec<u32>>,
}

impl<'src> DiagnosticContext<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
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
        match err {
            ParseError::UnexpectedToken(span) => panic!(
                "UnexpectedToken: {:?} @ {:?}",
                &self.src[span.into_src_range()],
                resolve_span_from_src(self.src, span)
            ),
            rest => panic!("{rest:?}"),
        }
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
        line: newlines_before as u32,
        column: index - newline_offset,
    }
}
pub fn resolve_span_from_src(src: &str, span: Span) -> SrcFileCoordinate {
    let before = &src[..(span.byte_offset() as usize)];
    let col = before.len() - before.rfind('\n').unwrap_or_default();
    let line = before.chars().filter(|&c| c == '\n').count();

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
