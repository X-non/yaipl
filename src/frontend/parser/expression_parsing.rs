use crate::frontend::{
    lexer::{Token, TokenKind},
    span::Span,
};

use super::{
    ast::{Binary, BinaryOp, Expr, ExprKind, FnCall},
    ParseErrorKind, ParseResult, Parser,
};
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BindingPower(u8);
#[inline]
fn bp(n: u8) -> BindingPower {
    debug_assert!(n % 2 == 1, "{n} isin't odd");
    BindingPower(n)
}
enum Associative {
    Left,
    Right,
}
fn infix_bp(n: u8, associativity: Associative) -> (BindingPower, BindingPower) {
    debug_assert!(n % 2 == 1);
    //TODO: add comment explaining why
    match associativity {
        Associative::Left => (BindingPower(n), BindingPower(n + 1)),
        Associative::Right => (BindingPower(n + 1), BindingPower(n)),
    }
}

fn infix_binding_power(op: BinaryOp) -> (BindingPower, BindingPower) {
    match op {
        BinaryOp::Mul | BinaryOp::Div => infix_bp(17, Associative::Left),
        BinaryOp::Add | BinaryOp::Sub => infix_bp(15, Associative::Left),

        BinaryOp::Equals
        | BinaryOp::NotEquals
        | BinaryOp::LessThan
        | BinaryOp::LessThanOrEqual
        | BinaryOp::GreaterThan
        | BinaryOp::GreaterThanOrEqual => infix_bp(11, Associative::Left),

        BinaryOp::And => infix_bp(7, Associative::Left),
        BinaryOp::Xor => infix_bp(5, Associative::Left),
        BinaryOp::Or => infix_bp(3, Associative::Left),
    }
}
fn postfix_binding_power(postfix: &Token) -> Option<BindingPower> {
    let bp = match &postfix.kind {
        TokenKind::OpenParen => bp(7),
        _ => return None,
    };
    Some(bp)
}

impl<'src> Parser<'src> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_with_binding_power(BindingPower(0))
    }

    fn peek_binary_op(&mut self) -> Option<(BinaryOp, Span)> {
        let token = self.peek();
        let token_kind = &token.kind;
        Some((token_kind.try_into().ok()?, token.span))
    }

    fn peek_unary_op(&mut self) -> Option<Token> {
        // self.eat_if(|token| matches!(token.kind, TokenKind::Minus))
        None
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
            return Err(ParseErrorKind::UnexpectedToken.with_span(self.peek().span));
        };

        loop {
            if let Some(op) = self.peek_unary_op() {
                todo!()
            } else if let Some((op, op_span)) = self.peek_binary_op() {
                let (lhs_bp, rhs_bp) = infix_binding_power(op);

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
            } else if let Some(postfix) = self.peek_postfix_op() {
                let bp = postfix_binding_power(postfix).expect("previous call should ensure that");
                if bp < min_bp {
                    break;
                }
                let postfix = self.eat();
                match postfix.kind {
                    TokenKind::OpenParen => {
                        let (argumant_span, arguments) = self.parse_argument_list()?;
                        lhs = Expr {
                            span: lhs.span.combine(argumant_span),
                            kind: ExprKind::FnCall(Box::new(FnCall {
                                callee: lhs,
                                arguments,
                            })),
                        }
                    }
                    _ => unimplemented!(),
                };
            }
            break;
        }
        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {

    use crate::frontend::parser::Parser;

    use super::*;

    #[test]
    fn parse_single_binary_expression() {
        for op in ["+", "-", "*", "/"] {
            let src = format!("a {op} b");
            let mut parser = Parser::new(&src);
            let expr = parser.parse_expr().expect("No parse error should occur");

            match &expr.kind {
                ExprKind::Binary(Binary { lhs, rhs, .. }) => match (&lhs.kind, &rhs.kind) {
                    (ExprKind::Variable(_), ExprKind::Variable(_)) => {}
                    _ => panic!("The lhs and rhs of the expr need to be varibles. {expr:?}"),
                },
                _ => panic!("Should be Binary:{expr:?}"),
            }
        }
    }
}
