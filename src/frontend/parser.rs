use std::fmt::Debug;

use crate::utils::{
    interner::{
        branded::{Ident, StrLiteral},
        Interner,
    },
    smallvec::SmallVec,
};

use self::ast::{
    Block, BlockWithCondition, Expr, ExprKind, FnArguments, FnCall, FnDecl, IfBranchSet, Item,
    ItemKind, Module, Stmt, StmtKind, VaribleDecl, WhileLoop,
};

use super::{
    lexer::{Lexer, Token, TokenKind},
    span::Span,
};
pub mod ast;
mod binary_parsing;

pub type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken(Span),
    Expected(Box<dyn 'static + Debug + Send>),
    WholeProgramNotParsed(Box<Module>),
    FnArgOnlyComma,
}

impl From<EOF> for ParseError {
    fn from(_: EOF) -> Self {
        Self::UnexpectedEOF
    }
}
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
    pub fn into_interners(self) -> (Interner<Ident>, Interner<StrLiteral>) {
        self.lexer.into_interners()
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

    #[allow(dead_code)]
    pub fn parse_root_module(&mut self) -> ParseResult<Module> {
        let module = self.parse_module()?;
        if !self.is_at_eof() {
            return Err(ParseError::WholeProgramNotParsed(Box::new(module)));
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
            Err(ParseError::UnexpectedToken(self.peek().span))
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
                else_block = Some(self.parse_block(true)?);
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
        let block = self.parse_block(true)?;
        Ok(BlockWithCondition {
            span: condition.span.combine(block.span), //FIXME: Should probobly include the if or [else, if] tokens spans
            condition,
            block,
        })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_term()
    }

    fn parse_call_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;
        while self
            .eat_if(|token| token.kind == TokenKind::LeftParen)
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
                _ => return Err(ParseError::UnexpectedToken(token.span)),
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
            let mut block = self.parse_block(false)?;
            block.span = open_brace.span.combine(block.span);
            span = block.span;
            kind = StmtKind::Block(block);
        } else if let Some(while_token) = self.eat_if(|token| token.kind == TokenKind::While) {
            let condition = self.parse_expr()?;
            let block = self.parse_block(true)?;
            span = while_token.span.combine(block.span);
            kind = StmtKind::WhileLoop(WhileLoop { condition, block });
        } else {
            let expr = self.parse_expr()?;
            span = expr.span;
            kind = StmtKind::Expr(expr);
        };

        let semi_colon = self.eat_if(|tok| tok.kind == TokenKind::SemiColon);
        let span = match &semi_colon {
            Some(semi_colon) => semi_colon.span,
            None => span,
        };

        if semi_colon.is_none() && kind.needs_semi_colon() {
            return Err(ParseError::Expected(Box::new(TokenKind::SemiColon)));
        }

        Ok(Stmt { kind, span })
    }

    // Fixme self.parse_block should probobly take something like Option<Token> and fix its own span
    fn parse_block(&mut self, consume_open_brace: bool) -> ParseResult<Block> {
        if consume_open_brace {
            self.eat_if(|token| token.kind == TokenKind::OpenBrace)
                .ok_or(ParseError::Expected(Box::new(TokenKind::OpenBrace)))?;
        }
        let mut stmts = Vec::new();

        let closed_brace = loop {
            if let Some(closed_brace) = self.eat_if(|token| token.kind == TokenKind::ClosedBrace) {
                break closed_brace;
            }

            stmts.push(self.parse_stmt()?);
        };

        let span = if let Some(stmt) = stmts.first() {
            stmt.span.combine(closed_brace.span)
        } else {
            todo!("actually pass the open brace")
        };

        Ok(Block { stmts, span })
    }

    fn parse_varible_decl(&mut self) -> ParseResult<VaribleDecl> {
        let (name, name_span) = self.map_eat_if(|tok| match tok.kind {
            TokenKind::Ident(ident) => Ok((ident, tok.span)),
            _ => Err(ParseError::UnexpectedToken(tok.span)),
        })?;

        self.eat_if(|token| token.kind == TokenKind::Equal)
            .ok_or(ParseError::Expected(Box::new(TokenKind::Equal)))?;

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
            _ => Err(ParseError::Expected(Box::new("Identifier"))),
        })?;

        let name = name;
        let block = self.parse_block(true)?;

        Ok(FnDecl {
            name,
            block,
            name_span,
        })
    }

    fn parse_argument_list(&mut self) -> ParseResult<(Span, FnArguments)> {
        //exists to disallow <callee>(,);
        let mut consumed_comma = false;
        let mut argument_exprs = SmallVec::new();

        let right_paren = loop {
            if let Some(right_paren) = self.eat_if(|token| token.kind == TokenKind::RightParen) {
                break right_paren;
            }

            argument_exprs.push(self.parse_expr()?);
            consumed_comma = self
                .eat_if(|token| token.kind == TokenKind::Comma)
                .is_some();

            if let Some(right_paren) = self.eat_if(|token| token.kind == TokenKind::RightParen) {
                break right_paren;
            }
        };

        if consumed_comma && argument_exprs.len() == 0 {
            return Err(ParseError::FnArgOnlyComma);
        }
        let fn_arguments = FnArguments {
            span: argument_exprs
                .first()
                .map(|first| first.span.combine(argument_exprs.last().unwrap().span)),
            arguments: argument_exprs,
        };
        Ok((right_paren.span, fn_arguments))
    }
}
