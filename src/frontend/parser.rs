use std::{fmt::Debug, iter::Peekable, ops::Range};

use self::ast::{Block, Expr, IfBranch, IfBranchSet, Item, Module, Stmt, VaribleDecl};

use super::lexer::{Lexer, Token, TokenKind};
mod ast;

pub type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken(Range<usize>),
    Expected(Box<dyn 'static + Debug>),
    WholeProgramNotParsed,
}

impl From<EOF> for ParseError {
    fn from(_: EOF) -> Self {
        Self::UnexpectedEOF
    }
}
#[derive(Debug)]
pub struct EOF;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
        }
    }

    pub fn peek(&mut self) -> Result<&Token, EOF> {
        self.lexer.peek().ok_or(EOF)
    }

    pub fn eat(&mut self) -> Result<Token, EOF> {
        self.lexer.next().ok_or(EOF)
    }
    pub fn eat_if<P: FnOnce(&Token) -> bool>(&mut self, p: P) -> Result<Option<Token>, EOF> {
        let should_eat = self.peek().map(p)?;
        if should_eat {
            let token = self
                .eat()
                .expect("Eat should be OK(...) eat and peek should be in sync");
            Ok(Some(token))
        } else {
            Ok(None)
        }
    }
    /// eats a token if f returns Some()
    pub fn peek_map_eat_if<T, E, F>(&mut self, f: F) -> Result<Result<T, E>, EOF>
    where
        F: FnOnce(&Token) -> Result<T, E>,
    {
        let peek = self.peek()?;
        let result = f(peek);
        if result.is_ok() {
            self.eat().expect("Eat and peek don't match");
        }

        Ok(result)
    }
    pub fn is_at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
    #[allow(dead_code)]
    pub fn parse_program(&mut self) -> ParseResult<Module> {
        let module = self.parse_module()?;
        if !self.is_at_eof() {
            return Err(ParseError::WholeProgramNotParsed);
        }

        Ok(module)
    }

    fn parse_module(&mut self) -> ParseResult<Module> {
        let mut items = Vec::new();
        while let Some(item) = self.try_parse_any_item()? {
            items.push(item)
        }

        Ok(Module { items })
    }

    fn try_parse_any_item(&mut self) -> ParseResult<Option<Item>> {
        let start_of_item = self
            .eat_if(|tok| matches!(&tok.kind, TokenKind::If))
            .ok()
            .flatten();

        let start_of_item = match start_of_item {
            Some(start) => start,
            None => return Ok(None),
        };
        let item = match start_of_item.kind {
            TokenKind::If => {
                let branch_set = dbg!(self.parse_if())?;
                Item::If(branch_set)
            }
            _ => unreachable!(),
        };
        Ok(Some(item))
    }

    fn parse_if(&mut self) -> ParseResult<IfBranchSet> {
        let if_branch = self.parse_if_branch()?;
        let mut else_if_branches = Vec::new();
        let mut else_block = None;
        loop {
            // if no else/else if branch we are done
            if let Ok(None) | Err(_) = self.eat_if(|token| token.kind == TokenKind::Else) {
                break;
            }
            //parsing else if
            if let Some(_) = self.eat_if(|token| token.kind == TokenKind::If)? {
                let else_if_branch = self.parse_if_branch()?;

                else_if_branches.push(else_if_branch);
            } else {
                else_block = Some(self.parse_block(true)?);
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
        self.peek_map_eat_if(|token| match token.kind {
            TokenKind::Integer(num) => Ok(Expr::Integer(num)),
            TokenKind::Float(num) => Ok(Expr::Float(num)),
            TokenKind::String => Ok(Expr::String(token.span.clone())),
            _ => Err(ParseError::UnexpectedToken(token.span.clone())),
        })?
    }
    fn try_parse_stmt(&mut self) -> ParseResult<Option<Stmt>> {
        let token = self.eat_if(|token| match token.kind {
            TokenKind::If | TokenKind::Let => true,
            _ => false,
        })?;
        let stmt = if let Some(token) = token {
            match token.kind {
                TokenKind::If => Stmt::If(self.parse_if()?),
                TokenKind::Else => Stmt::VaribleDecl(self.parse_varible_decl()?),
                TokenKind::LeftBrace => Stmt::Block(self.parse_block(false)?),
                _ => return Err(ParseError::UnexpectedToken(token.span)),
            }
        } else {
            return Ok(None);
        };

        Ok(Some(stmt))
    }
    fn parse_block(&mut self, consume_open_brace: bool) -> ParseResult<Block> {
        if consume_open_brace {
            self.eat_if(|token| token.kind == TokenKind::LeftBrace)?
                .ok_or(ParseError::Expected(Box::new("Open Brace")))?;
        }

        //TODO Actually parse something in here
        while let Some(token) = self.eat_if(|tok| tok.kind != TokenKind::RightBrace)? {
            if token.kind == TokenKind::LeftBrace {
                todo!("Nested Blocks")
            }
        }

        self.eat_if(|token| matches!(token.kind, TokenKind::RightBrace))?
            .ok_or(ParseError::Expected(Box::new("Closed Brace")))?;

        Ok(Block { stmts: vec![] })
    }

    fn parse_varible_decl(&mut self) -> ParseResult<VaribleDecl> {
        let name_span = self.peek_map_eat_if(|tok| match tok.kind {
            TokenKind::Ident => Ok(tok.span.clone()),
            _ => Err(ParseError::UnexpectedToken(tok.span.clone())),
        })??;

        self.eat_if(|token| token.kind == TokenKind::Equal)?
            .ok_or(ParseError::Expected(Box::new(TokenKind::Equal)))?;

        let intializer = self.parse_expr()?;
        Ok(VaribleDecl {
            name: name_span,
            intializer,
        })
    }
}
