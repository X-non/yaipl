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

fn newlines_before(newlines: &[u32], index: u32) -> usize {
    let newlines_before = match newlines.binary_search(&index) {
        Ok(num) | Err(num) => num,
    };

    newlines_before
}
