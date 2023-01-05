use crate::frontend::lexer::{Token, TokenKind};

use super::{
    ast::{Binary, Expr, ExprKind},
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
        while let Some(op_token) = self.eat_if(&matcher) {
            let op = op_token.kind.try_into().expect("matcher is wrong");
            let rhs = higher_presidence_parser(self)?;

            //FIXME: this is always left associative
            let binary = Binary {
                op,
                op_span: op_token.span,
                lhs: Box::new(workning_expr),
                rhs: Box::new(rhs),
            };
            workning_expr = Expr {
                span: binary.span(),
                kind: ExprKind::Binary(binary),
            }
        }
        Ok(workning_expr)
    }

    pub fn parse_add_or_sub(&mut self) -> ParseResult<Expr> {
        self.parse_binary(
            |token| matches!(token.kind, TokenKind::Plus | TokenKind::Minus),
            Self::parse_mul_or_div,
        )
    }
    pub fn parse_mul_or_div(&mut self) -> ParseResult<Expr> {
        self.parse_binary(
            |token| matches!(token.kind, TokenKind::Star | TokenKind::Slash),
            Self::parse_unary,
        )
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if let Some(unary_token) = self.eat_if(|token| token.kind == TokenKind::Minus) {
            let content = self.parse_unary()?;
            let content_span = content.span;
            let kind = ExprKind::UnaryMinus(Box::new(content));
            let span = unary_token.span.combine(content_span);
            return Ok(Expr { span, kind });
        }

        self.parse_call_expr()
    }
}
