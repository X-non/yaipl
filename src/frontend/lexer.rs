use std::ops::Range;

use logos::Logos;

use crate::utils::interner::{
    branded::{Ident, Identifier, StrLiteral},
    Interned, Interner,
};

pub struct Lexer<'a> {
    logos_lexer: logos::Lexer<'a, TokenKind>,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = (Interner<Ident>, Interner<StrLiteral>))]
pub enum TokenKind {
    #[token("let")]
    Let,
    #[token("=")]
    Equal,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token(";")]
    SemiColon,

    #[token("fn")]
    Func,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex(r"[0-9]([0-9]|_)*", |lex| lex.slice().parse())]
    Integer(u64),

    #[regex(r"[0-9]([0-9]|_)+\.([0-9]|_)*", |lex| lex.slice().parse())]
    Float(f64),

    //https://gist.github.com/cellularmitosis/6fd5fc2a65225364f72d3574abd9d5d5
    #[regex(r#""([^"\\]|\\[\s\S])*""#, |lex| lex.extras.1.intern(lex.slice()))]
    String(Interned<StrLiteral>),

    #[regex(r"(_|[a-zA-Z])+(_|[a-z0-9])*", |lex| lex.extras.0.intern(lex.slice()))]
    Ident(Identifier),

    #[token("{")]
    OpenBrace,

    #[token("}")]
    ClosedBrace,

    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[error]
    #[regex(r"[ \t\n\f\r]+", logos::skip)] //whitespace
    #[regex(r"//.*", logos::skip)]
    Error,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            logos_lexer: TokenKind::lexer_with_extras(src, (Interner::new(), Interner::new())),
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

#[cfg(test)]
mod tests {
    use super::{Lexer, TokenKind};

    #[test]
    fn valid_identifiers() {
        let text = "abc a1b ab3 a2b3 __abc a_b a___";
        let lexer = Lexer::new(text);
        for token in lexer {
            assert!(
                matches!(token.kind, TokenKind::Ident(_)),
                "span: {:?}, lexeme: {}",
                token.span.clone(),
                &text[token.span]
            );
        }
    }
    #[test]
    fn lexes_right_token() {
        use crate::frontend::lexer::{Lexer, TokenKind};
        let mut lexer = Lexer::new("if bla {wee} else {wee}");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::If);
        assert!(matches!(lexer.next().unwrap().kind, TokenKind::Ident(_)));

        assert_eq!(lexer.next().unwrap().kind, TokenKind::OpenBrace);
        assert!(matches!(lexer.next().unwrap().kind, TokenKind::Ident(_)));
        assert_eq!(lexer.next().unwrap().kind, TokenKind::ClosedBrace);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Else);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::OpenBrace);
        assert!(matches!(lexer.next().unwrap().kind, TokenKind::Ident(_)));
        assert_eq!(lexer.next().unwrap().kind, TokenKind::ClosedBrace);
        assert!(lexer.next().is_none());
    }
}
