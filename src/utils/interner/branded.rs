use super::Interned;

pub type Identifier = Interned<Ident>;
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident;
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrLiteral;
