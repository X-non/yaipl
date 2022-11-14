use crate::frontend::span::Span;

struct DiagnosticContext<'src> {
    src: &'src str,
    newlines: Vec<u32>,
}
pub fn resolve_span(newlines: &[u32], span: Span) -> (u32, u32) {
    let index = span.byte_offset();
    let newlines_before = newlines_before(newlines, index);
    let newline_offset = newlines[newlines_before];
    (index - newline_offset, newlines_before as u32)
}
pub fn resolve_span_from_src(src: &str, span: Span) -> (u32, u32) {
    let (before, after) = src.split_at(span.byte_offset() as usize);
    let col = before.len() - before.rfind('\n').unwrap_or_default();
    let row = before.chars().filter(|&c| c == '\n').count();

    (col as u32, row as u32)
}

fn newlines_before(newlines: &[u32], index: u32) -> usize {
    let newlines_before = match newlines.binary_search(&index) {
        Ok(num) | Err(num) => num,
    };

    newlines_before
}
