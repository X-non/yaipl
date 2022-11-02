use super::Interned;

pub type Identifier = Interned<Ident>;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrLiteral;
