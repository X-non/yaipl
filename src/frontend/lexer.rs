use std::ops::Range;

use logos::Logos;

struct Lexer<'a> {
    logos_lexer: logos::Lexer<'a, TokenKind>,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

#[derive(Logos, Debug, PartialEq)]
pub enum TokenKind {
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[regex(r"[0-9]([0-9]|_)+", |lex| lex.slice().parse())]
    Integer(u64),

    #[regex(r"[0-9]([0-9]|_)+\.([0-9]|_)*", |lex| lex.slice().parse())]
    Float(f64),

    //https://gist.github.com/cellularmitosis/6fd5fc2a65225364f72d3574abd9d5d5
    #[regex(r#""([^"\\]|\\[\s\S])*""#)]
    String,

    #[regex(r"(_|[a-zA-Z])+(_|a-zA-Z0-9)*")]
    Ident,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[error]
    #[regex(r"[ \t\n\f\r]+", |_| logos::Skip)]
    Error,
}

#[cfg(test)]
impl<'a> Lexer<'a> {
    pub(crate) fn new(src: &'a str) -> Self {
        Self {
            logos_lexer: TokenKind::lexer(src),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = if let Some(kind) = self.logos_lexer.next() {
            Token {
                kind,
                span: self.logos_lexer.span(),
            }
        } else {
            return None;
        };

        Some(token)
    }
}

mod tests {
    use crate::frontend::lexer::{Lexer, TokenKind};

    #[test]
    fn lexes_right_token() {
        let mut lexer = Lexer::new("if bla {wee} else {wee}");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::If);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Ident);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::LeftBrace);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Ident);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RightBrace);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Else);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::LeftBrace);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Ident);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RightBrace);
    }
}
