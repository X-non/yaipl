use crate::frontend::lexer::{Token, TokenKind};

use super::{
    ast::{Binary, Expr},
    ParseResult, Parser,
};

impl<'src> Parser<'src> {
    fn parse_binary<M, ParseFn>(
        &mut self,
        matcher: M,
        higher_presidence_parser: ParseFn,
    ) -> ParseResult<Expr>
    where
        M: Fn(&Token) -> bool,
        ParseFn: Fn(&mut Parser<'src>) -> ParseResult<Expr>,
    {
        let mut workning_expr = higher_presidence_parser(self)?;
        while let Some(sep) = self.eat_if(&matcher) {
            let op = sep.kind.try_into().expect("matcher is wrong");
            let rhs = higher_presidence_parser(self)?;

            //FIXME: this is always left associative
            let binary = Binary {
                op,
                lhs: Box::new(workning_expr),
                rhs: Box::new(rhs),
            };
            workning_expr = Expr::Binary(binary)
        }
        Ok(workning_expr)
    }
    pub fn parse_term(&mut self) -> ParseResult<Expr> {
        self.parse_binary(
            |token| matches!(token.kind, TokenKind::Plus | TokenKind::Minus),
            Self::parse_factor,
        )
    }
    pub fn parse_factor(&mut self) -> ParseResult<Expr> {
        self.parse_binary(
            |token| matches!(token.kind, TokenKind::Star | TokenKind::Slash),
            Self::parse_unary,
        )
    }
    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if let Some(unary_token) = self.eat_if(|token| token.kind == TokenKind::Minus) {
            return Ok(Expr::UnaryMinus(Box::new(self.parse_unary()?)));
        }

        self.parse_call_expr()
    }
}
