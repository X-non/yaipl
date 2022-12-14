use std::{fmt::Display, rc::Rc, usize};

use crate::frontend::parser::ast::{
    Ast, Block, Expr, ExprKind, FnDecl, IfBranchSet, Item, ItemKind, Module, Stmt, StmtKind,
    VaribleDecl,
};
use crate::frontend::span::Span;
use std::fmt::Write;

use super::{
    diagnostics::DiagnosticContext,
    interner::{
        branded::{Ident, Identifier, StrLiteral},
        Interned, Interner,
    },
};

pub struct AstPrinter {
    diagnostic_context: Rc<DiagnosticContext<'static>>,
    idents: Rc<Interner<Ident>>,
    str_literals: Rc<Interner<StrLiteral>>,
    current_indent: usize,
    rows: String,
}

impl Display for AstPrinter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rows)
    }
}

impl AstPrinter {
    pub fn resolve_str(&self, interned: Interned<StrLiteral>) -> &str {
        self.str_literals.lookup(interned)
    }
    pub fn resovle_ident(&self, interned: Identifier) -> &str {
        self.idents.lookup(interned)
    }
    fn new(
        idents: Rc<Interner<Ident>>,
        str_literals: Rc<Interner<StrLiteral>>,
        diagnostic_context: Rc<DiagnosticContext<'static>>,
    ) -> Self {
        Self {
            rows: Default::default(),
            current_indent: 0,
            idents,
            str_literals,
            diagnostic_context,
        }
    }
    pub fn from_node<Node: TreePrintable>(
        node: &Node,
        idents: Rc<Interner<Ident>>,
        str_literals: Rc<Interner<StrLiteral>>,
        diagnostic_context: Rc<DiagnosticContext<'static>>,
    ) -> Self {
        let mut printer = Self::new(idents, str_literals, diagnostic_context);
        printer.write(node);
        printer
    }
    pub fn write<Node: TreePrintable>(&mut self, node: &Node) {
        node.print(self);
    }
    fn indent(&self) -> String {
        "  ".repeat(self.current_indent)
    }
    pub fn emit_located(&mut self, row: impl AsRef<str>, span: Span) {
        writeln!(
            self.rows,
            "{}{} [@ {}]",
            self.indent(),
            row.as_ref(),
            self.diagnostic_context.resolve_span(span)
        )
        .unwrap();
    }
    pub fn emit(&mut self, row: impl AsRef<str>) {
        writeln!(self.rows, "{}{}", self.indent(), row.as_ref()).unwrap();
    }
    pub fn emit_children<'a>(
        &'a mut self,
        children: impl 'a + IntoIterator<Item = impl 'a + TreePrintable>,
    ) {
        self.emit_indented(|printer| {
            for child in children {
                child.print(printer);
            }
        });
    }

    pub fn emit_indented<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.current_indent += 1;
        f(self);
        self.current_indent -= 1;
    }

    pub fn emit_labled<F: FnOnce(&mut Self)>(&mut self, label: impl AsRef<str>, f: F) {
        self.emit(label);
        self.emit_indented(|printer| f(printer));
    }
}
pub trait TreePrintable {
    fn print(&self, printer: &mut AstPrinter);
}

impl<T: TreePrintable> TreePrintable for &T {
    fn print(&self, printer: &mut AstPrinter) {
        <T as TreePrintable>::print(*self, printer)
    }
}

impl TreePrintable for Expr {
    fn print(&self, printer: &mut AstPrinter) {
        match &self.kind {
            ExprKind::Integer(val) => {
                printer.emit_located(format!("Literal int `{val}`"), self.span)
            }
            ExprKind::Float(val) => {
                printer.emit_located(format!("Literal float `{val}`"), self.span)
            }
            ExprKind::Bool(val) => printer.emit_located(format!("Literal bool `{val}`"), self.span),
            ExprKind::String(val) => printer.emit_located(
                format!("Literal string `{:?}`", printer.resolve_str(*val)),
                self.span,
            ),
            ExprKind::Variable(val) => printer.emit_located(
                format!("Identifier `{}`", printer.resovle_ident(*val)),
                self.span,
            ),
            ExprKind::FnCall(fn_call) => {
                printer.emit_located(format!("FnCall"), self.span);

                printer.emit_labled("Callee", |printer| fn_call.callee.print(printer));

                for (i, expr) in fn_call.arguments.arguments.iter().enumerate() {
                    printer.emit_labled(format!("Arg #{i}"), |printer| expr.print(printer));
                }
            }
            ExprKind::Binary(binary) => {
                printer.emit_located("BinaryExpr ", self.span);
                printer.emit_indented(|printer| {
                    binary.lhs.print(printer);
                    printer.emit_located(format!("{}", binary.op), binary.op_span);
                    binary.rhs.print(printer);
                })
            }
            ExprKind::UnaryMinus(content) => {
                printer.emit_labled("UnaryMinus", |printer| {
                    content.print(printer);
                });
            }
        }
    }
}

impl TreePrintable for Stmt {
    fn print(&self, printer: &mut AstPrinter) {
        match &self.kind {
            StmtKind::If(IfBranchSet {
                if_branch,
                else_if_branches,
                else_block,
            }) => {
                printer.emit_labled("If", |printer| {
                    printer.emit_labled("Conditon", |printer| if_branch.condition.print(printer));
                    if_branch.block.print(printer);
                });

                for (i, else_if_branch) in else_if_branches.iter().enumerate() {
                    printer.emit_labled(format!("If Else #{}", i), |printer| {
                        printer.emit_labled("Conditon", |printer| {
                            else_if_branch.condition.print(printer)
                        });
                        else_if_branch.block.print(printer);
                    })
                }
                if let Some(else_block) = else_block.as_ref() {
                    printer.emit_labled("Else", |printer| else_block.print(printer));
                } else {
                    printer.emit("/* No Else Branch */");
                }
            }

            StmtKind::Block(block) => block.print(printer),
            StmtKind::VaribleDecl(var_decl) => var_decl.print(printer),
            StmtKind::Expr(expr) => printer.emit_labled("ExprStmt", |printer| expr.print(printer)),
            StmtKind::WhileLoop(while_loop) => {
                printer.emit_located("While", self.span);
                printer.emit_indented(|printer| {
                    printer.emit_labled("Condition", |printer| {
                        while_loop.condition.print(printer);
                    });
                    while_loop.block.print(printer);
                })
            }
        }
    }
}
impl TreePrintable for Block {
    fn print(&self, printer: &mut AstPrinter) {
        printer.emit_located("Block", self.span);

        if self.stmts().is_empty() {
            printer.emit_indented(|printer| printer.emit("/* No Block Body */"));
        } else {
            printer.emit_children(self.stmts());
        }
    }
}

impl TreePrintable for VaribleDecl {
    fn print(&self, printer: &mut AstPrinter) {
        printer.emit_located(
            format!("VarDecl of `{}`", printer.resovle_ident(self.name)),
            self.name_span,
        );
        printer.emit_indented(|printer| {
            printer.emit_labled("Initalizer", |printer| self.intializer.print(printer))
        });
    }
}

impl TreePrintable for Module {
    fn print(&self, printer: &mut AstPrinter) {
        printer.emit("Module");
        printer.emit_children(&self.items);
    }
}

impl TreePrintable for Item {
    fn print(&self, printer: &mut AstPrinter) {
        match &self.kind {
            ItemKind::FnDecl(fn_decl) => fn_decl.print(printer),
        }
    }
}
impl TreePrintable for FnDecl {
    fn print(&self, printer: &mut AstPrinter) {
        printer.emit(format!("FnDecl `{}`", printer.resovle_ident(self.name)));
        printer.emit_indented(|printer| self.block.print(printer));
    }
}

impl TreePrintable for Ast {
    fn print(&self, printer: &mut AstPrinter) {
        self.root.print(printer);
    }
}
