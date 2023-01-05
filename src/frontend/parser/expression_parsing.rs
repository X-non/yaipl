use crate::frontend::{
    lexer::{Token, TokenKind},
    span::Span,
};

use super::{
    ast::{Binary, BinaryOp, Expr, ExprKind},
    ParseError, ParseResult, Parser,
};
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BindingPower(u8);

fn bp(n: u8) -> BindingPower {
    BindingPower(n)
}
fn binary_binding_power(op: BinaryOp) -> (BindingPower, BindingPower) {
    match op {
        BinaryOp::Add | BinaryOp::Sub => (bp(1), bp(2)),
        BinaryOp::Mul | BinaryOp::Div => (bp(3), bp(4)),
    }
}

impl<'src> Parser<'src> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_with_binding_power(bp(0))
    }

    fn peek_binary_op(&mut self) -> Option<(BinaryOp, Span)> {
        let token = self.peek();
        let token_kind = &token.kind;
        Some((token_kind.try_into().ok()?, token.span))
    }

    fn consume_unary_op(&mut self) -> Option<Token> {
        self.eat_if(|token| matches!(token.kind, TokenKind::Minus))
    }

    fn consume_literals(&mut self) -> Option<Expr> {
        let token = self.eat_if(Token::is_literal)?;

        let kind = match token.kind {
            TokenKind::Integer(num) => ExprKind::Integer(num),
            TokenKind::Float(num) => ExprKind::Float(num),
            TokenKind::String(string) => ExprKind::String(string),
            TokenKind::Ident(ident) => ExprKind::Variable(ident),
            TokenKind::True => ExprKind::Bool(true),
            TokenKind::False => ExprKind::Bool(false),
            _ => unreachable!("The match doesn't match `Token::is_literal`"),
        };

        Some(Expr {
            span: token.span,
            kind,
        })
    }
    fn parse_with_binding_power(&mut self, min_bp: BindingPower) -> ParseResult<Expr> {
        let mut lhs = if let Some(literal) = self.consume_literals() {
            literal
        } else {
            return Err(ParseError::UnexpectedToken(self.peek().span));
        };

        loop {
            if self.is_at_eof() {
                break;
            }

            if let Some(op) = self.consume_unary_op() {
                todo!()
            } else if let Some((op, op_span)) = self.peek_binary_op() {
                let (lhs_bp, rhs_bp) = binary_binding_power(op);

                if lhs_bp < min_bp {
                    break;
                }
                self.eat();
                let rhs = self.parse_with_binding_power(rhs_bp)?;

                lhs = Expr {
                    span: lhs.span.combine(rhs.span),
                    kind: ExprKind::Binary(Binary {
                        op_span,
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                };

                todo!()
            } else {
                todo!()
            };
        }
        todo!()
    }
}
