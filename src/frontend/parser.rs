use std::{error::Error, iter::Peekable};

use self::ast::{Block, Expr, IfBranch, IfBranchSet, Item, Module};

use super::lexer::{Lexer, Token, TokenKind};
mod ast;

pub type ParseResult<T> = Result<T, ParseError>;
pub enum ParseError {
    UnexpectedEOF,
}

impl From<EOF> for ParseError {
    fn from(_: EOF) -> Self {
        Self::UnexpectedEOF
    }
}
#[derive(Debug)]
struct EOF;

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

    pub fn is_at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
    #[allow(dead_code)]
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
            TokenKind::If => todo!(),
            _ => unreachable!(),
        }
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
                else_block = Some(self.parse_block()?);
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
