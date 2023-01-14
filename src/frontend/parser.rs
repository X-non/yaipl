use std::fmt::Debug;

use crate::utils::smallvec::SmallVec;

use self::ast::{
    Assignment, Ast, Block, BlockWithCondition, Expr, ExprKind, FnArguments, FnCall, FnDecl,
    IfBranchSet, Item, ItemKind, Module, Stmt, StmtKind, VaribleDecl, WhileLoop,
};

use super::{
    lexer::{Lexer, Token, TokenKind},
    span::Span,
};
pub mod ast;
mod binary_parsing;
mod expression_parsing;
pub type ParseResult<T> = Result<T, ParseError>;

pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Option<Span>,
    pub created_at: std::panic::Location<'static>,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseError")
            .field("kind", &self.kind)
            .field("span", &self.span)
            .field("created_at", &self.created_at.to_string())
            .finish()
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedEOF,
    UnexpectedToken,
    Expected(Box<dyn 'static + Debug + Send>),
    WholeProgramNotParsed(Box<Module>),
    FnArgOnlyComma,
}

impl ParseErrorKind {
    #[track_caller]
    pub fn with_span(self, span: Span) -> ParseError {
        ParseError {
            kind: self,
            span: Some(span),
            created_at: *std::panic::Location::caller(),
        }
    }
}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

// impl From<EOF> for ParseError {
//     fn from(_: EOF) -> Self {
//         Self::UnexpectedEOF.
//     }
// }
#[derive(Debug)]
pub struct EOF;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src),
        }
    }
    pub fn trace_tokens(&mut self, trace: &'src mut Vec<Token>) {
        self.lexer.trace_tokens(trace);
    }

    pub fn peek(&mut self) -> &Token {
        self.lexer
            .peek()
            .expect("Should not try peek token after EOF")
    }
    pub fn eat(&mut self) -> Token {
        self.lexer
            .next()
            .expect("Should not try peek token after EOF")
    }
    pub fn eat_if<P: FnOnce(&Token) -> bool>(&mut self, p: P) -> Option<Token> {
        let should_eat = p(self.peek());
        if should_eat {
            Some(self.eat())
        } else {
            None
        }
    }
    /// eats a token if f returns Some()
    pub fn map_eat_if<T, E, F>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&Token) -> Result<T, E>,
    {
        let result = f(self.peek());
        if result.is_ok() {
            self.eat();
        }

        result
    }
    pub fn is_at_eof(&mut self) -> bool {
        self.peek().is_eof()
    }

    pub fn parse_file_ast(&mut self) -> ParseResult<Ast> {
        let root = self.parse_module()?;
        let (strings, identifiers) = self.lexer.clone_interners();
        Ok(Ast::new(root, identifiers, strings))
    }

    pub fn parse_root_module(&mut self) -> ParseResult<Module> {
        let module = self.parse_module()?;
        if !self.is_at_eof() {
            return Err(
                ParseErrorKind::WholeProgramNotParsed(Box::new(module)).with_span(self.peek().span)
            );
        }

        Ok(module)
    }

    fn parse_module(&mut self) -> ParseResult<Module> {
        let mut items = Vec::new();
        while !self.is_at_eof() {
            items.push(self.parse_item()?)
        }

        Ok(Module { items })
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        if let Some(fn_token) = self.eat_if(|token| token.kind == TokenKind::Func) {
            let fn_decl = self.parse_fn_decl()?;
            let item = Item {
                span: fn_token.span.combine(fn_decl.block.span), // FIXME: This is kinda hacky
                kind: ItemKind::FnDecl(fn_decl),
            };
            Ok(item)
        } else {
            Err(ParseErrorKind::UnexpectedToken.with_span(self.peek().span))
        }
    }

    fn parse_if(&mut self) -> ParseResult<IfBranchSet> {
        let if_branch = self.parse_if_branch()?;
        eprintln!("First IfBranch Done");
        let mut else_if_branches = Vec::new();

        let mut else_block = None;
        loop {
            // if no else/else if branch we are done
            if self.eat_if(|token| token.kind == TokenKind::Else).is_none() {
                break;
            }
            //parsing else if
            if let Some(_) = self.eat_if(|token| token.kind == TokenKind::If) {
                let else_if_branch = self.parse_if_branch()?;
                eprintln!("Else if branch done");

                else_if_branches.push(else_if_branch);
            } else {
                else_block = Some(self.parse_block(None)?);
                eprintln!("Else Block Done");
                break;
            }
        }

        Ok(IfBranchSet {
            if_branch,
            else_if_branches,
            else_block,
        })
    }
    /// parses the condition and the block of the branch
    fn parse_if_branch(&mut self) -> ParseResult<BlockWithCondition> {
        let condition = self.parse_expr()?;
        let block = self.parse_block(None)?;
        Ok(BlockWithCondition {
            span: condition.span.combine(block.span), //FIXME: Should probobly include the if or [else, if] tokens spans
            condition,
            block,
        })
    }

    fn parse_call_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;
        while self
            .eat_if(|token| token.kind == TokenKind::OpenParen)
            .is_some()
        {
            let (right_paren, arguments) = self.parse_argument_list()?;

            let expr_span = expr.span;
            let kind = ExprKind::FnCall(Box::new(FnCall {
                arguments,
                callee: expr,
            }));

            expr = Expr {
                span: expr_span.combine(right_paren),
                kind,
            };
        }
        Ok(expr)
    }
    fn parse_primary(&mut self) -> ParseResult<Expr> {
        self.map_eat_if(|token| {
            let kind = match token.kind {
                TokenKind::Integer(num) => ExprKind::Integer(num),
                TokenKind::Float(num) => ExprKind::Float(num),
                TokenKind::String(string) => ExprKind::String(string),
                TokenKind::Ident(ident) => ExprKind::Variable(ident),
                TokenKind::True => ExprKind::Bool(true),
                TokenKind::False => ExprKind::Bool(false),
                _ => return Err(ParseErrorKind::UnexpectedToken.with_span(token.span)),
            };
            Ok(Expr {
                kind,
                span: token.span,
            })
        })
    }
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        let kind;
        let span;
        if let Some(if_token) = self.eat_if(|token| token.kind == TokenKind::If) {
            let if_branch_set = self.parse_if()?;
            span = if_token.span.combine(if_branch_set.span());
            kind = StmtKind::If(if_branch_set);
        } else if let Some(let_token) = self.eat_if(|token| token.kind == TokenKind::Let) {
            let varible_decl = self.parse_varible_decl()?;
            span = let_token.span.combine(varible_decl.intializer.span);
            kind = StmtKind::VaribleDecl(varible_decl);
        } else if let Some(open_brace) = self.eat_if(|token| token.kind == TokenKind::OpenBrace) {
            let block = self.parse_block(Some(open_brace))?;
            span = block.span;
            kind = StmtKind::Block(block);
        } else if let Some(while_token) = self.eat_if(|token| token.kind == TokenKind::While) {
            let condition = self.parse_expr()?;
            let block = self.parse_block(None)?;
            span = while_token.span.combine(block.span);
            kind = StmtKind::WhileLoop(WhileLoop { condition, block });
        } else {
            let expr = self.parse_expr()?;

            if self
                .eat_if(|token| token.kind == TokenKind::Equal)
                .is_some()
            {
                let rhs = self.parse_expr()?;
                span = expr.span.combine(rhs.span);
                kind = StmtKind::Assignment(Box::new(Assignment {
                    assignee: expr,
                    rhs,
                }))
            } else {
                span = expr.span;
                kind = StmtKind::Expr(expr);
            }
        };

        let semi_colon = self.eat_if(|tok| tok.kind == TokenKind::SemiColon);
        let span = match &semi_colon {
            Some(semi_colon) => semi_colon.span,
            None => span,
        };

        if semi_colon.is_none() && kind.needs_semi_colon() {
            return Err(ParseErrorKind::Expected(Box::new(TokenKind::SemiColon))
                .with_span(self.peek().span));
        }

        Ok(Stmt { kind, span })
    }

    fn parse_block(&mut self, consumed_open_brace: Option<Token>) -> ParseResult<Block> {
        let open_brace = match consumed_open_brace {
            Some(open_brace) => open_brace,
            None => self
                .eat_if(|token| token.kind == TokenKind::OpenBrace)
                .ok_or(
                    ParseErrorKind::Expected(Box::new(TokenKind::OpenBrace))
                        .with_span(self.peek().span),
                )?,
        };

        let mut stmts = Vec::new();

        let closed_brace = loop {
            if let Some(closed_brace) = self.eat_if(|token| token.kind == TokenKind::ClosedBrace) {
                break closed_brace;
            }

            stmts.push(self.parse_stmt()?);
        };

        let span = open_brace.span.combine(closed_brace.span);

        Ok(Block { stmts, span })
    }

    fn parse_varible_decl(&mut self) -> ParseResult<VaribleDecl> {
        let (name, name_span) = self.map_eat_if(|tok| match tok.kind {
            TokenKind::Ident(ident) => Ok((ident, tok.span)),
            _ => Err(ParseErrorKind::UnexpectedToken.with_span(tok.span)),
        })?;

        self.eat_if(|token| token.kind == TokenKind::Equal).ok_or(
            ParseErrorKind::Expected(Box::new(TokenKind::Equal)).with_span(self.peek().span),
        )?;

        let intializer = self.parse_expr()?;
        Ok(VaribleDecl {
            name,
            intializer,
            name_span,
        })
    }

    fn parse_fn_decl(&mut self) -> ParseResult<FnDecl> {
        let (name, name_span) = self.map_eat_if(|token| match token.kind {
            TokenKind::Ident(name) => Ok((name, token.span)),
            _ => Err(ParseErrorKind::Expected(Box::new("Identifier")).with_span(token.span)),
        })?;

        let name = name;
        let block = self.parse_block(None)?;

        Ok(FnDecl {
            name,
            block,
            name_span,
        })
    }

    fn parse_argument_list(&mut self) -> ParseResult<(Span, FnArguments)> {
        //exists to disallow <callee>(,);
        let mut last_consumed_comma = None;
        let mut argument_exprs = SmallVec::new();

        let right_paren = loop {
            if let Some(right_paren) = self.eat_if(|token| token.kind == TokenKind::ClosedParen) {
                break right_paren;
            }

            argument_exprs.push(self.parse_expr()?);
            last_consumed_comma = self.eat_if(|token| token.kind == TokenKind::Comma);

            if let Some(right_paren) = self.eat_if(|token| token.kind == TokenKind::ClosedParen) {
                break right_paren;
            }
        };

        if last_consumed_comma.is_some() && argument_exprs.len() == 0 {
            return Err(ParseErrorKind::FnArgOnlyComma.with_span(last_consumed_comma.unwrap().span));
        }
        let fn_arguments = FnArguments {
            span: argument_exprs
                .first()
                .map(|first| first.span.combine(argument_exprs.last().unwrap().span)),
            arguments: argument_exprs,
        };
        Ok((right_paren.span, fn_arguments))
    }

    fn peek_postfix_op(&mut self) -> Option<&Token> {
        let token = self.peek();
        match &token.kind {
            TokenKind::OpenParen => Some(token),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn can_parse_while_loop() {
        let src = "while true {}";
        let stmt = Parser::new(src)
            .parse_stmt()
            .expect("Should be valid while loop");
        assert!(matches!(&stmt.kind, StmtKind::WhileLoop(_)))
    }

    #[test]
    fn can_parse_expr_stmt() {
        let stmt = Parser::new("a + b;")
            .parse_stmt()
            .expect("This should be a valid expr stmt");

        assert!(matches!(
            &stmt.kind,
            StmtKind::Expr(Expr {
                kind: ExprKind::Binary(_),
                ..
            })
        ));
    }
}
