use ast_printer::DisplayCode;

use super::expr::*;
use super::{print_boxed_commasep, print_commasep};
use super::{Code, CodePrinter, CodeResult};
use std::fmt::Write;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrimType {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
}
impl Code for PrimType {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        write!(
            cp,
            "{}",
            match self {
                Self::Void => "void",
                Self::Char => "char",
                Self::Short => "short",
                Self::Int => "int",
                Self::Long => "long",
                Self::Float => "float",
                Self::Double => "double",
                Self::Signed => "signed",
                Self::Unsigned => "unsigned",
                Self::Bool => "bool",
            }
        )?;
        Ok(cp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeName(pub PrimType, pub AbstractDeclarator);
impl Code for TypeName {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        let Self(ptyp, adecl) = self;
        cp = ptyp.pretty_fmt(cp)?;
        cp = adecl.pretty_fmt(cp)?;
        Ok(cp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbstractDeclarator(pub usize, pub Option<Box<DirectAbstractDeclarator>>);

impl Code for AbstractDeclarator {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        let Self(plv, dadecl) = self;
        for _ in 0..*plv {
            write!(cp, "*")?;
        }
        if let Some(dadecl) = dadecl {
            cp = dadecl.pretty_fmt(cp)?;
            Ok(cp)
        } else {
            Ok(cp)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectAbstractDeclarator {
    AbstractDeclarator(AbstractDeclarator),
    Array(Option<Box<DirectAbstractDeclarator>>, usize),
    Func(Option<Box<DirectAbstractDeclarator>>, Vec<ParamDeclaration>),
}
impl Code for DirectAbstractDeclarator {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::AbstractDeclarator(inner) => {
                write!(cp, "(")?;
                cp = inner.pretty_fmt(cp)?;
                write!(cp, ")")?;
                Ok(cp)
            }
            Self::Array(opt_inner, sz) => {
                if let Some(inner) = opt_inner {
                    cp = inner.pretty_fmt(cp)?;
                }
                write!(cp, "[{}]", sz)?;
                Ok(cp)
            }
            Self::Func(opt_inner, params) => {
                if let Some(inner) = opt_inner {
                    cp = inner.pretty_fmt(cp)?;
                }
                print_commasep(cp, params)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamDeclaration {
    Named(PrimType, Declarator),
    Abstract(PrimType, AbstractDeclarator),
}
impl Code for ParamDeclaration {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::Named(t, decl) => {
                cp = t.pretty_fmt(cp)?;
                write!(cp, " ")?;
                cp = decl.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Abstract(t, adecl) => {
                cp = t.pretty_fmt(cp)?;
                cp = adecl.pretty_fmt(cp)?;
                Ok(cp)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, DisplayCode)]
pub struct Declaration(pub PrimType, pub Vec<Declarator>, pub Position);
impl Code for Declaration {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        let Declaration(ptype, decls, _) = self;
        cp = ptype.pretty_fmt(cp)?;
        write!(cp, " ")?;
        cp = print_commasep(cp, decls)?;
        write!(cp, ";")?;
        Ok(cp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declarator {
    NoInit(usize, Box<DirectDeclarator>),
    Init(usize, Box<DirectDeclarator>, Box<Initializer>),
}
impl Code for Declarator {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::NoInit(indir, dd) => {
                for _ in 0..*indir {
                    write!(cp, "*")?;
                }
                cp = dd.pretty_fmt(cp)?;
                Ok(cp)
            }
            Self::Init(indir, dd, init) => {
                for _ in 0..*indir {
                    write!(cp, "*")?;
                }
                cp = dd.pretty_fmt(cp)?;
                write!(cp, " = ")?;
                cp = init.pretty_fmt(cp)?;
                Ok(cp)
            }
        }
    }
}

impl Declarator {
    pub fn name<'a>(&'a self) -> &'a String {
        match self {
            Self::NoInit(_, dd) => dd.name(),
            Self::Init(_, dd, _) => dd.name(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Initializer {
    Scala(Box<Expr>),
    Array(Vec<Box<Initializer>>),
}
impl Code for Initializer {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::Scala(inner) => inner.pretty_fmt(cp),
            Self::Array(inners) => {
                write!(cp, "{{")?;
                cp = print_boxed_commasep(cp, inners)?;
                write!(cp, "}}")?;
                Ok(cp)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectDeclarator {
    Declarator(Declarator),
    Scala(String),
    Array(Box<DirectDeclarator>, usize),
    Func(Box<DirectDeclarator>, Vec<ParamDeclaration>),
}
impl Code for DirectDeclarator {
    fn pretty_fmt(&self, mut cp: CodePrinter) -> CodeResult {
        match self {
            Self::Declarator(decl) => {
                write!(cp, "(")?;
                cp = decl.pretty_fmt(cp)?;
                write!(cp, ")")?;
                Ok(cp)
            }
            Self::Scala(nm) => {
                write!(cp, "{}", nm)?;
                Ok(cp)
            }
            Self::Array(inner, sz) => {
                cp = inner.pretty_fmt(cp)?;
                write!(cp, "[{}]", sz)?;
                Ok(cp)
            }
            Self::Func(inner, dcs) => {
                cp = inner.pretty_fmt(cp)?;
                write!(cp, "(")?;
                cp = print_commasep(cp, dcs)?;
                write!(cp, ")")?;
                Ok(cp)
            }
        }
    }
}

pub trait DeclEquality<T> {
    fn equal(&self, other: &T) -> bool;
}
impl<T> DeclEquality<T> for T
where
    T: PartialEq + Eq,
{
    fn equal(&self, other: &T) -> bool {
        self == other
    }
}

impl DirectDeclarator {
    fn name<'a>(&'a self) -> &'a String {
        match self {
            Self::Declarator(inner) => inner.name(),
            Self::Scala(nm) => nm,
            Self::Array(inner, _) => inner.name(),
            Self::Func(inner, _) => inner.name(),
        }
    }

    fn equal_opt(&self, other: &Option<Box<DirectAbstractDeclarator>>) -> bool {
        match (self, other) {
            (DirectDeclarator::Scala(_), None) => true,
            (lhs, Some(nxt)) => lhs.equal(nxt.as_ref()),
            (_, None) => false,
        }
    }
}

impl DeclEquality<DirectAbstractDeclarator> for DirectDeclarator {
    fn equal(&self, other: &DirectAbstractDeclarator) -> bool {
        match (self, other) {
            (
                DirectDeclarator::Declarator(decl),
                DirectAbstractDeclarator::AbstractDeclarator(adecl),
            ) => decl.equal(adecl),
            (
                DirectDeclarator::Array(nxt, size),
                DirectAbstractDeclarator::Array(opt_nxt, asize),
            ) => {
                if size != asize {
                    return false;
                }
                nxt.equal_opt(opt_nxt)
            }
            (
                DirectDeclarator::Func(nxt, params),
                DirectAbstractDeclarator::Func(opt_nxt, aparams),
            ) => {
                if !nxt.equal_opt(opt_nxt) {
                    return false;
                } else if params.len() != aparams.len() {
                    return false;
                } else {
                    params
                        .iter()
                        .zip(aparams.iter())
                        .all(|(pdecl, apdecl)| pdecl.equal(apdecl))
                }
            }
            _ => false,
        }
    }
}

impl DeclEquality<AbstractDeclarator> for Declarator {
    fn equal(&self, other: &AbstractDeclarator) -> bool {
        match (self, other) {
            (Declarator::NoInit(plv, decl), AbstractDeclarator(aplv, opt_adecl)) => {
                if plv != aplv {
                    return false;
                }
                match (decl.as_ref(), opt_adecl) {
                    (DirectDeclarator::Scala(_), None) => true,
                    (lhs, Some(inner)) => lhs.equal(inner.as_ref()),
                    (_, None) => false,
                }
            }
            _ => false,
        }
    }
}

impl ParamDeclaration {
    fn equal(&self, other: &ParamDeclaration) -> bool {
        use ParamDeclaration::*;
        match (self, other) {
            (Named(lptyp, ldecl), Named(rptyp, rdecl)) => lptyp == rptyp && ldecl.equal(rdecl),
            (Abstract(aptyp, adecl), Named(nptyp, ndecl))
            | (Named(nptyp, ndecl), Abstract(aptyp, adecl)) => aptyp == nptyp && ndecl.equal(adecl),
            (Abstract(lptyp, ldecl), Abstract(rptyp, rdecl)) => {
                lptyp == rptyp && ldecl.equal(rdecl)
            }
        }
    }
}
