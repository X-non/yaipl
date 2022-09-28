use std::{error::Error, iter::Peekable};

use self::ast::{Block, Expr, IfBranch, IfBranchSet, Item, Module};

use super::lexer::{Lexer, Token, TokenKind};
mod ast;

pub type ParseResult<T> = Result<T, ParseError>;
pub enum ParseError {
    UnexpectedEOF,
}

impl From<UnexpectedEOF> for ParseError {
    fn from(_: UnexpectedEOF) -> Self {
        Self::UnexpectedEOF
    }
}
struct UnexpectedEOF;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
        }
    }

    pub fn peek(&mut self) -> Result<Option<&Token>, UnexpectedEOF> {
        match self.lexer.peek() {
            Some(t) => Ok(Some(t)),
            None if self.is_at_eof() => Err(UnexpectedEOF),
            None => Ok(None),
        }
    }

    pub fn eat(&mut self) -> Result<Option<Token>, UnexpectedEOF> {
        match self.lexer.next() {
            Some(t) => Ok(Some(t)),
            None if self.is_at_eof() => Err(UnexpectedEOF),
            None => Ok(None),
        }
    }
    pub fn eat_if<P: FnOnce(&Token) -> bool>(
        &mut self,
        p: P,
    ) -> Result<Option<Token>, UnexpectedEOF> {
        let tok = match self.peek() {
            Ok(Some(val)) => val,
            err => {
                err?;
                return Ok(None);
            }
        };

        if p(tok) {
            self.eat()
        } else {
            Ok(None)
        }
    }

    pub fn is_at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub fn parse_module(&mut self) -> ParseResult<Module> {
        let items = Vec::new();
        while let Some(item) = self.try_parse_any_item()? {}

        Ok(Module { items })
    }

    fn try_parse_any_item(&mut self) -> ParseResult<Option<Item>> {
        let start_of_item = self.eat_if(|tok| matches!(&tok.kind, TokenKind::If))?;
        let start_of_item = match start_of_item {
            Some(start) => start,
            None if self.is_at_eof() => return Err(ParseError::UnexpectedEOF),
            None => return Ok(None),
        };
        match start_of_item.kind {
            TokenKind::If => self.parse_if(),
            _ => unreachable!(),
        }
    }

    fn parse_if(&mut self) -> ParseResult<IfBranchSet> {
        let if_branch = self.parse_if_branch()?;
        while  {
            
        }
    }
    /// parses the condition and the block of the branch
    fn parse_if_branch(&mut self) -> ParseResult<IfBranch> {
        let condition = self.parse_expr()?;
        let block = self.parse_block()?;
        Ok(IfBranch { condition, block })
    }

    fn parse_expr(&self) -> ParseResult<Expr> {
        todo!()
    }

    fn parse_block(&self) -> ParseResult<Block> {
        todo!()
    }
}
