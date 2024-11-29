use ast_printer::DisplayCode;

use crate::ast::types::*;
use crate::ast::{print_boxed_commasep, CodeResult};
use std::fmt::Write;

use crate::ast::{Code, CodePrinter};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssOp {
    Non,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    BitAnd,
    BitOr,
    BitXor,
}
impl Code for AssOp {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> Result<CodePrinter, core::fmt::Error> {
        write!(
            cp,
            "{}",
            match self {
                Self::Non => "=",
                Self::Mul => "*=",
                Self::Div => "/=",
                Self::Mod => "%=",
                Self::Add => "+=",
                Self::Sub => "-=",
                Self::BitAnd => "&=",
                Self::BitOr => "|=",
                Self::BitXor => "^=",
            }
        )?;
        Ok(cp)
    }
}
impl AssOp {
    pub fn to_binop(&self) -> Option<BinOp> {
        match self {
            Self::Non => None,
            Self::Mul => Some(BinOp::Mul),
            Self::Div => Some(BinOp::Div),
            Self::Mod => Some(BinOp::Mod),
            Self::Add => Some(BinOp::Add),
            Self::Sub => Some(BinOp::Sub),
            Self::BitAnd => Some(BinOp::BitAnd),
            Self::BitOr => Some(BinOp::BitOr),
            Self::BitXor => Some(BinOp::BitXor),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LeftUnaryOp {
    Inc,
    Dec,
    Plus,
    Minus,
    BoolNot,
    BitNot,
    Deref,
    Ref,
}
impl Code for LeftUnaryOp {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        write!(
            cp,
            "{}",
            match self {
                Self::Inc => "++",
                Self::Dec => "--",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::BoolNot => "!",
                Self::BitNot => "~",
                Self::Deref => "*",
                Self::Ref => "&",
            }
        )?;
        Ok(cp)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    ShftL,
    ShftR,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,
}
impl Code for BinOp {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        write!(
            cp,
            "{}",
            match self {
                Self::Mul => "+",
                Self::Div => "-",
                Self::Mod => "%",
                Self::Add => "+",
                Self::Sub => "-",
                Self::ShftL => "<<",
                Self::ShftR => ">>",
                Self::Lt => "<",
                Self::Gt => ">",
                Self::Le => "<=",
                Self::Ge => ">=",
                Self::Eq => "==",
                Self::Ne => "!=",
                Self::BitAnd => "&",
                Self::BitXor => "^",
                Self::BitOr => "|",
                Self::BoolAnd => "&&",
                Self::BoolOr => "||",
            }
        )?;
        Ok(cp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    UnaryIncRight(Box<Expr>),
    UnaryDecRight(Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Index(Box<Expr>, Box<Expr>),

    LUnop(LeftUnaryOp, Box<Expr>),

    Cast(TypeName, Box<Expr>),

    Binop(Box<Expr>, BinOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

    Assign(Box<Expr>, AssOp, Box<Expr>),

    Comma(Box<Expr>, Box<Expr>),

    Ident(String),
    ConstBool(bool),
    Const(usize),
    ConstFloat(String),
    StringLiteral(String),
}

impl Code for Expr {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::UnaryIncRight(inner) => {
                cp = inner.pretty_fmt(cp)?;
                write!(cp, "++")?;
                Ok(cp)
            }
            Self::UnaryDecRight(inner) => {
                cp = inner.pretty_fmt(cp)?;
                write!(cp, "--")?;
                Ok(cp)
            }
            Self::Call(func, params) => {
                cp = func.pretty_fmt(cp)?;
                write!(cp, "(")?;
                cp = print_boxed_commasep(cp, params)?;
                write!(cp, ")")?;
                Ok(cp)
            }
            Self::Index(arr, idx) => {
                cp = arr.pretty_fmt(cp)?;
                write!(cp, "[")?;
                cp = idx.pretty_fmt(cp)?;
                write!(cp, "]")?;
                Ok(cp)
            }
            Self::LUnop(op, inner) => {
                cp = op.pretty_fmt(cp)?;
                cp = inner.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Cast(typnm, inner) => {
                write!(cp, "(")?;
                cp = typnm.pretty_fmt(cp)?;
                write!(cp, ")")?;
                cp = inner.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Binop(lhs, op, rhs) => {
                cp = lhs.pretty_fmt(cp)?;
                write!(cp, " ")?;
                cp = op.pretty_fmt(cp)?;
                write!(cp, " ")?;
                cp = rhs.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Ternary(cond, tb, fb) => {
                cp = cond.pretty_fmt(cp)?;
                write!(cp, " ? ")?;
                cp = tb.pretty_fmt(cp)?;
                write!(cp, " : ")?;
                cp = fb.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Assign(lhs, asop, rhs) => {
                cp = lhs.pretty_fmt(cp)?;
                write!(cp, " ")?;
                cp = asop.pretty_fmt(cp)?;
                write!(cp, " ")?;
                cp = rhs.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Comma(lhs, rhs) => {
                cp = lhs.pretty_fmt(cp)?;
                write!(cp, ", ")?;
                cp = rhs.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Ident(inner) => {
                write!(cp, "{}", inner)?;
                Ok(cp)
            }
            Self::Const(inner) => {
                write!(cp, "{}", inner)?;
                Ok(cp)
            }
            Self::ConstFloat(inner) => {
                write!(cp, "{}", inner)?;
                Ok(cp)
            }
            Self::ConstBool(inner) => {
                write!(cp, "{}", inner)?;
                Ok(cp)
            }
            Self::StringLiteral(lit) => {
                write!(cp, "\"{}\"", lit)?;
                Ok(cp)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundStmt(pub Vec<Declaration>, pub Vec<Box<Stmt>>);
impl Code for CompoundStmt {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        let CompoundStmt(decls, stmts) = self;
        if decls.is_empty() && stmts.is_empty() {
            write!(cp, "{{}}")?;
        } else {
            write!(cp, "{{")?;
            cp = cp.indent();

            for stmt in decls {
                cp = stmt.pretty_fmt(cp)?;
                write!(cp, "\n")?;
            }
            for stmt in stmts {
                cp = stmt.pretty_fmt(cp)?;
                write!(cp, "\n")?;
            }
            cp = cp.unindent().unwrap();

            write!(cp, "}}")?;
        }
        Ok(cp)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq, Eq, DisplayCode)]
pub enum Stmt {
    CompoundStmt(CompoundStmt, Position),
    ExprStmt(Option<Box<Expr>>, Position),
    If(Box<Expr>, Box<Stmt>, Position),
    IfElse(Box<Expr>, Box<Stmt>, Box<Stmt>, Position),
    While(Box<Expr>, Box<Stmt>, Position),
    DoWhile(Box<Stmt>, Box<Expr>, Position),
    Continue(Position),
    Break(Position),
    Return(Option<Box<Expr>>, Position),
    For(
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Box<Stmt>,
        Position,
    ),
}
impl Stmt {
    pub fn get_position(&self) -> &Position {
        match self {
            Self::CompoundStmt(_, pos)
            | Self::ExprStmt(_, pos)
            | Self::If(_, _, pos)
            | Self::IfElse(_, _, _, pos)
            | Self::While(_, _, pos)
            | Self::DoWhile(_, _, pos)
            | Self::Continue(pos)
            | Self::Break(pos)
            | Self::Return(_, pos)
            | Self::For(_, _, _, _, pos) => pos,
        }
    }
}

impl Code for Stmt {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::CompoundStmt(inner, _) => {
                cp = inner.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::ExprStmt(inner, _) => {
                if let Some(i) = inner {
                    cp = i.pretty_fmt(cp)?;
                    write!(cp, ";")?;
                    Ok(cp)
                } else {
                    write!(cp, ";")?;
                    Ok(cp)
                }
            }
            Self::If(cond, b, _) => {
                write!(cp, "if (")?;
                cp = cond.pretty_fmt(cp)?;
                write!(cp, ") ")?;
                cp = b.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::IfElse(cond, tb, fb, _) => {
                write!(cp, "if(")?;
                cp = cond.pretty_fmt(cp)?;
                write!(cp, ") ")?;
                cp = tb.pretty_fmt(cp)?;
                write!(cp, " else ")?;
                cp = fb.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::While(cond, b, _) => {
                write!(cp, "while (")?;
                cp = cond.pretty_fmt(cp)?;
                write!(cp, ") ")?;
                cp = b.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::DoWhile(b, cond, _) => {
                write!(cp, "do ")?;
                cp = b.pretty_fmt(cp)?;
                write!(cp, " while (")?;
                cp = cond.pretty_fmt(cp)?;
                write!(cp, ")")?;
                Ok(cp)
            }
            Self::Continue(_) => {
                write!(cp, "continue;")?;
                Ok(cp)
            }
            Self::Break(_) => {
                write!(cp, "break;")?;
                Ok(cp)
            }
            Self::Return(Some(i), _) => {
                write!(cp, "return ")?;
                cp = i.pretty_fmt(cp)?;
                write!(cp, ";")?;
                Ok(cp)
            }
            Self::Return(None, _) => {
                write!(cp, "return;")?;
                Ok(cp)
            }
            Self::For(i, c, r, b, _) => {
                write!(cp, "for (")?;
                if let Some(inner) = i {
                    cp = inner.pretty_fmt(cp)?;
                }
                write!(cp, ";")?;
                if let Some(inner) = c {
                    cp = inner.pretty_fmt(cp)?;
                }
                write!(cp, ";")?;
                if let Some(inner) = r {
                    cp = inner.pretty_fmt(cp)?;
                }
                write!(cp, ") ")?;
                cp = b.pretty_fmt(cp)?;
                Ok(cp)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, DisplayCode)]
pub struct Function(pub PrimType, pub Declarator, pub CompoundStmt);
impl Code for Function {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        let Self(ptype, decl, cstmt) = self;
        cp = ptype.pretty_fmt(cp)?;
        write!(cp, " ")?;
        cp = decl.pretty_fmt(cp)?;
        write!(cp, " ")?;
        cp = cstmt.pretty_fmt(cp)?;
        Ok(cp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, DisplayCode)]
pub enum TranslationUnit {
    Glob(Declaration),
    Func(Function),
}
impl Code for TranslationUnit {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::Glob(inner) => {
                cp = inner.pretty_fmt(cp)?;
            }
            Self::Func(inner) => {
                cp = inner.pretty_fmt(cp)?;
            }
        }
        Ok(cp)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, DisplayCode)]
pub struct Source(pub Vec<TranslationUnit>);
impl Code for Source {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        for tu in self.0.iter() {
            cp = tu.pretty_fmt(cp)?;
            cp.newline();
        }
        Ok(cp)
    }
}
