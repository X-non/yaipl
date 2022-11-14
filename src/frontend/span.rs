#[derive(Clone, Copy)]
pub struct Span {
    byte_offset: u32,
    len: u16,
}
impl Span {
    pub(crate) fn byte_offset(&self) -> u32 {
        self.byte_offset
    }
}
