use std::{fmt::Debug, rc::Rc};

use self::ast::{
    Assignment, Ast, Block, BlockWithCondition, Expr, ExprKind, FnArguments, FnCall, FnDecl,
    FnParameter, IfBranchSet, Item, ItemKind, Module, Stmt, StmtKind, VaribleDecl, WhileLoop,
};

use super::{
    lexer::{Lexer, Token, TokenKind},
    span::Span,
};
pub mod ast;
mod expression_parsing;
pub type ParseResult<T> = Result<T, ParseError>;

pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Option<Span>,
    pub created_at: std::panic::Location<'static>,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseError")
            .field("kind", &self.kind)
            .field("span", &self.span)
            .field("created_at", &self.created_at.to_string())
            .finish()
    }
}

#[derive(Debug)]
pub struct ParsedList<Node> {
    pub start: Token,
    pub end: Token,
    pub elements: Vec<Node>,
}
#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedEOF,
    UnexpectedToken,
    Expected(Box<dyn 'static + Debug + Send>),
    WholeProgramNotParsed(Box<Module>),
    ListOnlyComma,
}

impl ParseErrorKind {
    #[track_caller]
    pub fn with_span(self, span: Span) -> ParseError {
        ParseError {
            kind: self,
            span: Some(span),
            created_at: *std::panic::Location::caller(),
        }
    }
}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

// impl From<EOF> for ParseError {
//     fn from(_: EOF) -> Self {
//         Self::UnexpectedEOF.
//     }
// }
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
    pub fn trace_tokens(&mut self, trace: &'src mut Vec<Token>) {
        self.lexer.trace_tokens(trace);
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

    pub fn parse_file_ast(&mut self) -> ParseResult<Ast> {
        let root = self.parse_module()?;
        let (strings, identifiers) = self.lexer.clone_interners();
        Ok(Ast::new(root, identifiers, strings))
    }

    pub fn parse_root_module(&mut self) -> ParseResult<Module> {
        let module = self.parse_module()?;
        if !self.is_at_eof() {
            return Err(
                ParseErrorKind::WholeProgramNotParsed(Box::new(module)).with_span(self.peek().span)
            );
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
                kind: ItemKind::FnDecl(Rc::new(fn_decl)),
            };
            Ok(item)
        } else {
            Err(ParseErrorKind::UnexpectedToken.with_span(self.peek().span))
        }
    }

    fn parse_if(&mut self) -> ParseResult<IfBranchSet> {
        let if_branch = self.parse_if_branch()?;
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

                else_if_branches.push(else_if_branch);
            } else {
                else_block = Some(self.parse_block(None)?);
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
        let block = self.parse_block(None)?;
        Ok(BlockWithCondition {
            span: condition.span.combine(block.span), //FIXME: Should probobly include the if or [else, if] tokens spans
            condition,
            block,
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
            let block = self.parse_block(Some(open_brace))?;
            span = block.span;
            kind = StmtKind::Block(block);
        } else if let Some(while_token) = self.eat_if(|token| token.kind == TokenKind::While) {
            let condition = self.parse_expr()?;
            let block = self.parse_block(None)?;
            span = while_token.span.combine(block.span);
            kind = StmtKind::WhileLoop(WhileLoop { condition, block });
        } else if let Some(return_token) = self.eat_if(|token| token.kind == TokenKind::Return) {
            let return_value_expr = self.parse_expr()?;
            span = return_token.span.combine(return_value_expr.span);
            kind = StmtKind::Return(return_value_expr);
        } else {
            let expr = self.parse_expr()?;

            if self
                .eat_if(|token| token.kind == TokenKind::Equal)
                .is_some()
            {
                let rhs = self.parse_expr()?;
                span = expr.span.combine(rhs.span);
                kind = StmtKind::Assignment(Box::new(Assignment {
                    assignee: expr,
                    rhs,
                }))
            } else {
                span = expr.span;
                kind = StmtKind::Expr(expr);
            }
        };

        let semi_colon = self.eat_if(|tok| tok.kind == TokenKind::SemiColon);
        let span = match &semi_colon {
            Some(semi_colon) => semi_colon.span,
            None => span,
        };

        if semi_colon.is_none() && kind.needs_semi_colon() {
            return Err(ParseErrorKind::Expected(Box::new(TokenKind::SemiColon))
                .with_span(self.peek().span));
        }

        Ok(Stmt { kind, span })
    }

    fn parse_block(&mut self, consumed_open_brace: Option<Token>) -> ParseResult<Block> {
        let open_brace = match consumed_open_brace {
            Some(open_brace) => open_brace,
            None => self
                .eat_if(|token| token.kind == TokenKind::OpenBrace)
                .ok_or(
                    ParseErrorKind::Expected(Box::new(TokenKind::OpenBrace))
                        .with_span(self.peek().span),
                )?,
        };

        let mut stmts = Vec::new();

        let closed_brace = loop {
            if let Some(closed_brace) = self.eat_if(|token| token.kind == TokenKind::ClosedBrace) {
                break closed_brace;
            }

            stmts.push(self.parse_stmt()?);
        };

        let span = open_brace.span.combine(closed_brace.span);

        Ok(Block { stmts, span })
    }

    fn parse_varible_decl(&mut self) -> ParseResult<VaribleDecl> {
        let (name, name_span) = self.map_eat_if(|tok| match tok.kind {
            TokenKind::Ident(ident) => Ok((ident, tok.span)),
            _ => Err(ParseErrorKind::UnexpectedToken.with_span(tok.span)),
        })?;

        self.eat_if(|token| token.kind == TokenKind::Equal).ok_or(
            ParseErrorKind::Expected(Box::new(TokenKind::Equal)).with_span(self.peek().span),
        )?;

        let intializer = self.parse_expr()?;
        Ok(VaribleDecl {
            name,
            intializer,
            name_span,
        })
    }

    fn parse_fn_parameter(&mut self) -> ParseResult<FnParameter> {
        self.map_eat_if(|token| match token.kind {
            TokenKind::Ident(name) => Ok(FnParameter {
                name,
                span: token.span,
            }),
            _ => Err(ParseErrorKind::Expected(Box::new("Identifier")).with_span(token.span)),
        })
    }
    fn parse_fn_decl(&mut self) -> ParseResult<FnDecl> {
        let (name, name_span) = self.map_eat_if(|token| match token.kind {
            TokenKind::Ident(name) => Ok((name, token.span)),
            _ => Err(ParseErrorKind::Expected(Box::new("Identifier")).with_span(token.span)),
        })?;

        let parameters = self
            .parse_list_like(
                Token::is_open_paren,
                Token::is_closed_paren,
                Parser::parse_fn_parameter,
            )?
            .into();
        let block = self.parse_block(None)?;

        Ok(FnDecl {
            name,
            block,
            name_span,
            parameters,
        })
    }

    fn parse_argument_list(&mut self) -> ParseResult<FnArguments> {
        let ParsedList {
            start,
            end,
            elements,
            ..
        } = self.parse_list_like(
            Token::is_open_paren,
            Token::is_closed_paren,
            Parser::parse_expr,
        )?;

        Ok(FnArguments::new(start.span.combine(end.span), elements))
    }

    fn peek_postfix_op(&mut self) -> Option<&Token> {
        let token = self.peek();
        match &token.kind {
            TokenKind::OpenParen => Some(token),
            _ => None,
        }
    }

    pub fn parse_list_like<PStart, PEnd, ParseFn, Node>(
        &mut self,
        is_start: PStart,
        is_end: PEnd,
        element_parser: ParseFn,
    ) -> ParseResult<ParsedList<Node>>
    where
        PStart: Fn(&Token) -> bool,
        PEnd: Fn(&Token) -> bool,
        ParseFn: Fn(&mut Self) -> ParseResult<Node>,
    {
        let start_token = self.eat_if(&is_start).ok_or(
            ParseErrorKind::Expected(Box::new("The start token")).with_span(self.peek().span),
        )?;

        //exists to disallow <callee>(,);
        let mut last_consumed_comma = None;
        let mut elements = Vec::new();

        let end_token = loop {
            if let Some(end_token) = self.eat_if(&is_end) {
                break end_token;
            }

            elements.push(element_parser(self)?);
            last_consumed_comma = self.eat_if(|token| token.kind == TokenKind::Comma);

            if let Some(end_token) = self.eat_if(&is_end) {
                break end_token;
            }
        };

        if last_consumed_comma.is_some() && elements.len() == 0 {
            return Err(ParseErrorKind::ListOnlyComma.with_span(last_consumed_comma.unwrap().span));
        };
        Ok(ParsedList {
            start: start_token,
            end: end_token,
            elements,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn can_parse_while_loop() {
        let src = "while true {}";
        let stmt = Parser::new(src)
            .parse_stmt()
            .expect("Should be valid while loop");
        assert!(matches!(&stmt.kind, StmtKind::WhileLoop(_)))
    }

    #[test]
    fn can_parse_expr_stmt() {
        let stmt = Parser::new("a + b;")
            .parse_stmt()
            .expect("This should be a valid expr stmt");

        assert!(matches!(
            &stmt.kind,
            StmtKind::Expr(Expr {
                kind: ExprKind::Binary(_),
                ..
            })
        ));
    }
}
