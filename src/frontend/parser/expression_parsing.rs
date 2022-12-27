use super::{ast::Expr, ParseResult, Parser};

impl<'src> Parser<'src> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_add_or_sub()
    }
}
