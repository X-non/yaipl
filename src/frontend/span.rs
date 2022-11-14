use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    byte_offset: u32,
    len: u16,
}
impl Span {
    pub(crate) fn byte_offset(&self) -> u32 {
        self.byte_offset
    }

    pub fn len(&self) -> u16 {
        self.len
    }
    pub fn into_src_range(self)-> Range<usize>{
        (self.byte_offset()as usize)..(self.byte_offset() as usize + self.len() as usize)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpanConversionError {
    ToLongRange,
    ToLongSource,
}
impl TryFrom<Range<usize>> for Span {
    type Error = SpanConversionError;

    fn try_from(value: Range<usize>) -> Result<Self, Self::Error> {
        let Ok(byte_offset) = value.start.try_into() else { 
            return Err(SpanConversionError::ToLongSource); 
        };
        
        let Ok(len) = value.len().try_into() else {
            return  Err(SpanConversionError::ToLongRange);
        };
        
        Ok(Span {
            byte_offset,
            len,
        })
    }
}
