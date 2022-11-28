use crate::frontend::span::Span;

struct DiagnosticContext<'src> {
    src: &'src str,
    newlines: Vec<u32>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SrcFileCoordinate {
    pub line: u32,
    pub column: u32,
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
    let (before, after) = src.split_at(span.byte_offset() as usize);
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
