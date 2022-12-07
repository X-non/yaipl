use crate::frontend::lexer::{Token, TokenKind};

use super::{
    ast::{Binary, ExprKind},
    ParseResult, Parser,
};

impl<'src> Parser<'src> {
    fn parse_binary<M, ParseFn>(
        &mut self,
        matcher: M,
        higher_presidence_parser: ParseFn,
    ) -> ParseResult<ExprKind>
    where
        M: Fn(&Token) -> bool,
        ParseFn: Fn(&mut Parser<'src>) -> ParseResult<ExprKind>,
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
            workning_expr = ExprKind::Binary(binary)
        }
        Ok(workning_expr)
    }
    pub fn parse_term(&mut self) -> ParseResult<ExprKind> {
        self.parse_binary(
            |token| matches!(token.kind, TokenKind::Plus | TokenKind::Minus),
            Self::parse_factor,
        )
    }
    pub fn parse_factor(&mut self) -> ParseResult<ExprKind> {
        self.parse_binary(
            |token| matches!(token.kind, TokenKind::Star | TokenKind::Slash),
            Self::parse_unary,
        )
    }
    fn parse_unary(&mut self) -> ParseResult<ExprKind> {
        if let Some(unary_token) = self.eat_if(|token| token.kind == TokenKind::Minus) {
            return Ok(ExprKind::UnaryMinus(Box::new(self.parse_unary()?)));
        }

        self.parse_call_expr()
    }
}
