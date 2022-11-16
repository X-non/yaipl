use std::{fmt::Debug, ops::Range};

use crate::utils::interner::{
    branded::{Ident, StrLiteral},
    Interner,
};

use self::ast::{
    Block, Expr, FnArguments, FnCall, FnDecl, IfBranch, IfBranchSet, Item, Module, Stmt,
    VaribleDecl,
};

use super::{
    lexer::{Lexer, Token, TokenKind},
    span::Span,
};
pub mod ast;

pub type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken(Span),
    Expected(Box<dyn 'static + Debug + Send>),
    WholeProgramNotParsed(Box<Module>),
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
    src: &'src str,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src),
            src,
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
        let start_of_item = self.eat();

        let item = match start_of_item.kind {
            TokenKind::Func => {
                let fn_decl = self.parse_fn_decl()?;
                Item::FnDecl(fn_decl)
            }
            _ => return Err(ParseError::UnexpectedToken(start_of_item.span)),
        };
        Ok(item)
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
    fn parse_if_branch(&mut self) -> ParseResult<IfBranch> {
        let condition = self.parse_expr()?;
        let block = self.parse_block(true)?;
        Ok(IfBranch { condition, block })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;
        while self
            .eat_if(|token| token.kind == TokenKind::LeftParen)
            .is_some()
        {
            let arguments = self.parse_argument_list()?;
            expr = Expr::FnCall(Box::new(FnCall {
                arguments,
                callee: expr,
            }));
        }

        Ok(expr)
    }
    fn parse_primary(&mut self) -> ParseResult<Expr> {
        self.map_eat_if(|token| match token.kind {
            TokenKind::Integer(num) => Ok(Expr::Integer(num)),
            TokenKind::Float(num) => Ok(Expr::Float(num)),
            TokenKind::String(string) => Ok(Expr::String(string)),
            TokenKind::Ident(ident) => Ok(Expr::Variable(ident)),
            TokenKind::True => Ok(Expr::Bool(true)),
            TokenKind::False => Ok(Expr::Bool(false)),
            _ => return Err(ParseError::UnexpectedToken(token.span)),
        })
    }
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        let token = self.eat();

        let stmt = match token.kind {
            TokenKind::If => Stmt::If(self.parse_if()?),
            TokenKind::Let => Stmt::VaribleDecl(self.parse_varible_decl()?),
            TokenKind::OpenBrace => Stmt::Block(self.parse_block(false)?),
            _ => return Err(ParseError::UnexpectedToken(token.span)),
        };

        let semi_colon = self.eat_if(|tok| tok.kind == TokenKind::SemiColon);

        if semi_colon.is_none() && stmt.needed_semi_colon() {
            return Err(ParseError::Expected(Box::new(TokenKind::SemiColon)));
        }

        Ok(stmt)
    }
    fn parse_block(&mut self, consume_open_brace: bool) -> ParseResult<Block> {
        if consume_open_brace {
            self.eat_if(|token| token.kind == TokenKind::OpenBrace)
                .ok_or(ParseError::Expected(Box::new(TokenKind::OpenBrace)))?;
        }
        let mut stmts = Vec::new();

        loop {
            let end_of_block = self
                .eat_if(|token| token.kind == TokenKind::ClosedBrace)
                .is_some();
            if end_of_block {
                eprintln!("end of block");
                break;
            }

            stmts.push(self.parse_stmt()?);
        }

        Ok(Block { stmts })
    }

    fn parse_varible_decl(&mut self) -> ParseResult<VaribleDecl> {
        let name = self.map_eat_if(|tok| match tok.kind {
            TokenKind::Ident(ident) => Ok(ident),
            _ => Err(ParseError::UnexpectedToken(tok.span)),
        })?;

        self.eat_if(|token| token.kind == TokenKind::Equal)
            .ok_or(ParseError::Expected(Box::new(TokenKind::Equal)))?;

        let intializer = self.parse_expr()?;
        Ok(VaribleDecl { name, intializer })
    }

    fn parse_fn_decl(&mut self) -> ParseResult<FnDecl> {
        let name = self.map_eat_if(|token| match token.kind {
            TokenKind::Ident(name) => Ok(name),
            _ => Err(ParseError::Expected(Box::new("Identifier"))),
        })?;

        let name = name;
        let block = self.parse_block(true)?;

        Ok(FnDecl { name, block })
    }

    fn parse_argument_list(&mut self) -> ParseResult<FnArguments> {
        loop {
            let token = self.eat();
            match token.kind {
                TokenKind::RightParen => break,
                TokenKind::EOF => return Err(ParseError::UnexpectedEOF),
                _ => continue,
            }
        }

        Ok(FnArguments::empty())
    }
}
