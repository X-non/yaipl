use std::iter::Peekable;

use super::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    pub fn eat(&mut self) -> Option<Token> {
        self.lexer.next()
    }
    pub fn eat_if<P: FnOnce(&Token) -> bool>(&mut self, p: P) -> Option<Token> {
        if p(self.peek()?) {
            self.eat()
        } else {
            None
        }
    }

    pub fn is_at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }
}
