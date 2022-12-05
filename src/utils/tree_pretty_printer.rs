use std::{
    cell::Cell,
    fmt::{Debug, Display},
    iter::{once, Once},
    rc::Rc,
    usize,
};

use crate::frontend::parser::ast::{
    Ast, Block, Expr, FnDecl, IfBranch, IfBranchSet, Item, Module, Stmt, VaribleDecl,
};
use std::fmt::Write;

use super::interner::{
    branded::{Ident, Identifier, StrLiteral},
    Interned, Interner,
};

struct TreePrinter {
    idents: Rc<Interner<Ident>>,
    str_literals: Rc<Interner<StrLiteral>>,
    current_indent: usize,
    rows: String,
}

impl Display for TreePrinter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rows)
    }
}

impl TreePrinter {
    pub fn resolve_str(&self, interned: Interned<StrLiteral>) -> &str {
        self.str_literals.lookup(interned)
    }
    pub fn resovle_ident(&self, interned: Identifier) -> &str {
        self.idents.lookup(interned)
    }
    fn new(idents: Rc<Interner<Ident>>, str_literals: Rc<Interner<StrLiteral>>) -> Self {
        Self {
            rows: Default::default(),
            current_indent: 0,
            idents,
            str_literals,
        }
    }
    fn indent(&self) -> String {
        "  ".repeat(self.current_indent)
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
trait TreePrintable {
    fn print(&self, printer: &mut TreePrinter);
}

impl<T: TreePrintable> TreePrintable for &T {
    fn print(&self, printer: &mut TreePrinter) {
        <T as TreePrintable>::print(*self, printer)
    }
}

impl TreePrintable for Expr {
    fn print(&self, printer: &mut TreePrinter) {
        match self {
            Expr::Integer(val) => printer.emit(format!("Literal int `{val}`")),
            Expr::Float(val) => printer.emit(format!("Literal float `{val}`")),
            Expr::Bool(val) => printer.emit(format!("Literal bool `{val}`")),
            Expr::String(val) => {
                printer.emit(format!("Literal string `{:?}`", printer.resolve_str(*val)))
            }
            Expr::Variable(val) => {
                printer.emit(format!("Identifier `{}`", printer.resovle_ident(*val)))
            }
            Expr::FnCall(fn_call) => {
                printer.emit(format!("FnCall"));

                printer.emit_labled("Callee", |printer| fn_call.callee.print(printer));

                for (i, expr) in fn_call.arguments.arguments.iter().enumerate() {
                    printer.emit_labled(format!("Arg #{i}"), |printer| expr.print(printer));
                }
            }
        }
    }
}

impl TreePrintable for Stmt {
    fn print(&self, printer: &mut TreePrinter) {
        match self {
            Stmt::If(IfBranchSet {
                if_branch,
                else_if_branches,
                else_block,
            }) => {
                printer.emit_labled("If", |printer| {
                    printer.emit_labled("Conditon", |printer| if_branch.condition.print(printer));
                    if_branch.block.print(printer);
                });

                for (i, IfBranch { condition, block }) in else_if_branches.iter().enumerate() {
                    printer.emit_labled(format!("If Else #{}", i), |printer| {
                        printer
                            .emit_labled("Conditon", |printer| if_branch.condition.print(printer));
                        if_branch.block.print(printer);
                    })
                }
                if let Some(else_block) = else_block.as_ref() {
                    printer.emit_labled("Else", |printer| else_block.print(printer));
                } else {
                    printer.emit("/* No Else Branch */");
                }
            }

            Stmt::Block(block) => block.print(printer),
            Stmt::VaribleDecl(var_decl) => var_decl.print(printer),
            Stmt::Expr(expr) => printer.emit_labled("ExprStmt", |printer| expr.print(printer)),
        }
    }
}
impl TreePrintable for Block {
    fn print(&self, printer: &mut TreePrinter) {
        printer.emit("Block");

        if self.stmts().is_empty() {
            printer.emit_indented(|printer| printer.emit("/* No Block Body */"));
        } else {
            printer.emit_children(self.stmts());
        }
    }
}

impl TreePrintable for VaribleDecl {
    fn print(&self, printer: &mut TreePrinter) {
        printer.emit(format!("VarDecl of `{}`", printer.resovle_ident(self.name)));
        printer.emit_indented(|printer| {
            printer.emit_labled("Initalizer", |printer| self.intializer.print(printer))
        });
    }
}

impl TreePrintable for Module {
    fn print(&self, printer: &mut TreePrinter) {
        printer.emit("Module");
        printer.emit_children(&self.items);
    }
}

impl TreePrintable for Item {
    fn print(&self, printer: &mut TreePrinter) {
        match self {
            Item::FnDecl(fn_decl) => fn_decl.print(printer),
        }
    }
}
impl TreePrintable for FnDecl {
    fn print(&self, printer: &mut TreePrinter) {
        printer.emit(format!("FnDecl `{}`", printer.resovle_ident(self.name)));
        printer.emit_indented(|printer| self.block.print(printer));
    }
}

impl TreePrintable for Ast {
    fn print(&self, printer: &mut TreePrinter) {
        self.root.print(printer);
    }
}

impl std::fmt::Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut printer = TreePrinter::new(self.identifiers.clone(), self.strings.clone());
        self.print(&mut printer);
        write!(f, "{}", printer)
    }
}
