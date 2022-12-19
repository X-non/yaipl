use std::{borrow::Cow, rc::Rc};

use logos::Logos;

use crate::utils::interner::{
    branded::{Ident, Identifier, StrLiteral},
    Interned, Interner,
};

use super::span::Span;

pub struct Lexer<'a> {
    logos_lexer: logos::Lexer<'a, TokenKind>,
    peek: Option<Token>,
    has_returned_eof: bool,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn is_eof(&self) -> bool {
        self.kind.is_eof()
    }
}
pub struct LexerState {
    pub strings: Rc<Interner<StrLiteral>>,
    pub identifiers: Rc<Interner<Ident>>,
}
struct InvalidEscapeSequence(usize, char);

fn split_first_char(s: &str) -> Option<(char, &str)> {
    let mut chars = s.chars();
    chars.next().map(|c| (c, chars.as_str()))
}
fn handel_str_literal(
    lexer: &mut logos::Lexer<TokenKind>,
) -> Result<Interned<StrLiteral>, InvalidEscapeSequence> {
    let content = lexer
        .slice()
        .strip_prefix('"')
        .expect("string literals start with \"")
        .strip_suffix('"')
        .expect("string literals end with \"");

    let escaped = unescape_special_chars(content)?;

    let interned = lexer.extras.strings.intern(&escaped);

    Ok(interned)
}

fn unescape_special_chars(content: &str) -> Result<Cow<'_, str>, InvalidEscapeSequence> {
    let mut escaped = String::new();
    let mut unchecked = content;

    while let Some(index) = unchecked.find("\\") {
        let (before_escape, with_backslash) = unchecked.split_at(index);
        let with_escape_seq = with_backslash.strip_prefix("\\").unwrap();
        let (first, rest) = split_first_char(with_escape_seq)
            .expect("cant have a \\ alone before string end due to the string lexing");
        let replacement = match first {
            'n' => '\n',
            'r' => '\r',
            invalid => return Err(InvalidEscapeSequence(index, invalid)),
        };

        escaped.push_str(before_escape);
        escaped.push(replacement);
        unchecked = rest;
    }
    //prevents extera alloc if the content contains no escape codes
    if escaped.is_empty() {
        Ok(Cow::Borrowed(content))
    } else {
        escaped.push_str(unchecked);
        Ok(Cow::Owned(escaped))
    }
}
#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexerState)]
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
    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("in")]
    In,

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
    #[regex(r#""([^"\\]|\\[\s\S])*""#, handel_str_literal)]
    String(Interned<StrLiteral>),

    #[regex(r"(_|[a-zA-Z])+(_|[a-z0-9])*", |lex| lex.extras.identifiers.intern(lex.slice()))]
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
            logos_lexer: TokenKind::lexer_with_extras(
                src,
                LexerState {
                    identifiers: Rc::new(Interner::new()),
                    strings: Rc::new(Interner::new()),
                },
            ),
        }
    }
    pub fn peek(&mut self) -> Option<&Token> {
        if self.peek.is_none() {
            self.peek = self.next();
        }
        self.peek.as_ref()
    }

    pub fn clone_interners(&self) -> (Rc<Interner<StrLiteral>>, Rc<Interner<Ident>>) {
        let state = &self.logos_lexer.extras;

        (state.strings.clone(), state.identifiers.clone())
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(a) = self.peek.take() {
            return Some(a);
        }

        let kind = if let Some(kind) = self.logos_lexer.next() {
            kind
        } else if !self.has_returned_eof {
            self.has_returned_eof = true;
            TokenKind::EOF
        } else {
            return None;
        };

        Some(Token {
            kind,
            span: Span::try_from(self.logos_lexer.span()).unwrap(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::diagnostics::resolve_span_from_src;

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
                resolve_span_from_src(text, token.span),
                &text[token.span.into_src_range()]
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
