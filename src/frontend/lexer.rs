use std::ops::Range;

use logos::Logos;

use crate::utils::interner::{
    branded::{Ident, Identifier, StrLiteral},
    Interned, Interner,
};

pub struct Lexer<'a> {
    logos_lexer: logos::Lexer<'a, TokenKind>,
    peek: Option<Token>,
    has_returned_eof: bool,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

impl Token {
    pub fn is_eof(&self) -> bool {
        self.kind.is_eof()
    }
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

    EOF,

    #[error]
    #[regex(r"[ \t\n\f\r]+", logos::skip)] //whitespace
    #[regex(r"//.*", logos::skip)]
    Error,
}

impl TokenKind {
    /// Returns `true` if the token kind is [`EOF`].
    ///
    /// [`EOF`]: TokenKind::EOF
    #[must_use]
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::EOF)
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            has_returned_eof: false,
            peek: None,
            logos_lexer: TokenKind::lexer_with_extras(src, (Interner::new(), Interner::new())),
        }
    }
    pub fn peek(&mut self) -> Option<&Token> {
        if self.peek.is_none() {
            self.peek = self.next();
        }
        self.peek.as_ref()
    }

    pub(crate) fn into_interners(self) -> (Interner<Ident>, Interner<StrLiteral>) {
        self.logos_lexer.extras
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(a) = self.peek.take() {
            return Some(a);
        }

        if let Some(kind) = self.logos_lexer.next() {
            Some(Token {
                kind,
                span: self.logos_lexer.span(),
            })
        } else if !self.has_returned_eof {
            self.has_returned_eof = true;
            Some(Token {
                kind: TokenKind::EOF,
                span: self.logos_lexer.span(),
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, TokenKind};

    #[test]
    fn valid_identifiers() {
        let text = "abc a1b ab3 a2b3 __abc a_b a___";
        let mut lexer = Lexer::new(text);
        for _ in 0..7 {
            let token = lexer.next().unwrap();
            assert!(
                matches!(token.kind, TokenKind::Ident(_)),
                "span: {:?}, lexeme: {}",
                token.span.clone(),
                &text[token.span]
            );
        }
        assert_eq!(lexer.next().unwrap().kind, TokenKind::EOF);
        assert!(lexer.next().is_none());
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
        assert_eq!(lexer.next().unwrap().kind, TokenKind::EOF);

        assert!(lexer.next().is_none());
    }
}
