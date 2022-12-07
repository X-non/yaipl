use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    byte_offset: u32,
    len: u32,
}
impl Span {
    pub fn byte_offset(&self) -> u32 {
        self.byte_offset
    }
    
    pub fn len(&self) -> u32 {
        self.len
    }
    pub fn into_src_range(self)-> Range<usize>{
        (self.byte_offset()as usize)..(self.byte_offset() as usize + self.len() as usize)
    }
    pub fn combine(self, rhs:Self) -> Self {
        let start = Ord::min(self.byte_offset(), rhs.byte_offset());
        let end = Ord::max(self.byte_offset() + self.len(), rhs.byte_offset() + rhs.len());

        let len = end - start;


        Span { byte_offset: start, len }
    }
    pub fn combine_mabye(self, rhs:Option<Self>) -> Self{
        if let Some(rhs) = rhs {
            self.combine(rhs)
        } else {
            self
        }
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
