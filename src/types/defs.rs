use ast_printer::DisplayCode;

use crate::ast::expr::*;
use crate::ast::print_commasep;
use crate::ast::types::*;

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;
use std::ops;

use crate::types::defs::ATyp::*;
use crate::types::defs::Typ::*;

const INT_WIDTH: usize = 4;
const INT_ATYP: ATyp = Int(true, INT_WIDTH);

pub type TypeResult<T> = Result<T, PositionalTypeError>;

#[derive(Debug, Clone)]
pub enum ErrorLocation {
    Stmt(Stmt),
    Decl(Declaration),
}
impl ErrorLocation {
    pub fn get_position(&self) -> Position {
        match self {
            Self::Stmt(stmt) => stmt.get_position().clone(),
            Self::Decl(Declaration(_, _, pos)) => pos.clone(),
        }
    }
}
impl Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stmt(stmt) => write!(f, "{}", stmt),
            Self::Decl(decl) => write!(f, "{}", decl),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PositionalTypeError {
    err: TypeError,
    at: Vec<ErrorLocation>,
}
impl From<TypeError> for PositionalTypeError {
    fn from(value: TypeError) -> Self {
        Self {
            err: value,
            at: Vec::new(),
        }
    }
}
pub trait AddPosition<T> {
    fn at(&mut self, pos: T);
}
impl AddPosition<Stmt> for PositionalTypeError {
    fn at(&mut self, pos: Stmt) {
        self.at.push(ErrorLocation::Stmt(pos));
    }
}
impl AddPosition<Declaration> for PositionalTypeError {
    fn at(&mut self, pos: Declaration) {
        self.at.push(ErrorLocation::Decl(pos));
    }
}

impl Display for PositionalTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(pos) = self.at.get(0) {
            write!(f, "{}\n  Occured at >> {} <<", self.err, pos)
        } else {
            write!(f, "{}", self.err)
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeError {
    InvalidIdentifier(String),
    InvalidDefinition(String),
    Redefinition(String),
    TypeMismatch(String, Typ, Typ),
    TypesMismatch(String, Vec<Typ>, Vec<Typ>),
    TypeIncompatible,
    NotBool(Typ),
    NotCallable(Typ),
}
impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidIdentifier(i) => {
                write!(f, "Invalid identifier: {}", i)
            }
            Self::InvalidDefinition(i) => {
                write!(f, "Invalid type definition: {}", i)
            }
            Self::Redefinition(i) => {
                write!(f, "Invalid redefinition of identifier: {}", i)
            }
            Self::TypeMismatch(i, lt, rt) => {
                write!(f, "Type mismatch: {}: {} vs {}", i, lt, rt)
            }
            Self::TypesMismatch(i, lts, rts) => {
                write!(f, "Types mismatch: {}: [", i)?;
                for lt in lts {
                    write!(f, "{},", lt)?;
                }
                write!(f, "] vs [")?;
                for rt in rts {
                    write!(f, "{}, ", rt)?;
                }
                write!(f, "]")
            }
            Self::TypeIncompatible => {
                write!(f, "Type incompatible")
            }
            Self::NotBool(typ) => {
                write!(f, "Cannot be bool: {}", typ)
            }
            Self::NotCallable(typ) => {
                write!(f, "Cannot be callsed: {}", typ)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Typ {
    Address(Box<Typ>),
    Array(Box<Typ>, usize),
    Void,
    Arith(ATyp),
    FuncDecl(Box<Typ>, Vec<Typ>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ATyp {
    Int(bool, usize),
    Bool,
    Float(usize),
}

impl Into<Typ> for ATyp {
    fn into(self) -> Typ {
        Arith(self)
    }
}

impl Display for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Address(i) => {
                write!(f, "*{}", i)
            }
            Self::Array(typ, sz) => {
                write!(f, "{}[{}]", typ, sz)
            }
            Self::Void => write!(f, "void"),
            Self::Arith(atyp) => write!(f, "{}", atyp),
            Self::FuncDecl(rettyp, ptypes) => {
                write!(f, "{}(", rettyp)?;
                let mut it = ptypes.iter().peekable();
                while let Some(i) = it.next() {
                    if it.peek().is_some() {
                        write!(f, "{}, ", i)?;
                    } else {
                        write!(f, "{}", i)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for ATyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(signed, bytes) => {
                if *signed {
                    write!(f, "i{}", *bytes * 8)
                } else {
                    write!(f, "u{}", *bytes * 8)
                }
            }
            Self::Float(bytes) => write!(f, "f{}", *bytes * 8),
            Self::Bool => write!(f, "bool"),
        }
    }
}
// Implicit conversion between arithmetic types
// https://en.cppreference.com/w/cpp/language/implicit_conversion
// https://en.cppreference.com/w/cpp/language/array#Array-to-pointer_decay
pub trait ImplicitConversion<T> {
    fn becomes(&self, rhs: T) -> bool;
}

impl ImplicitConversion<&Typ> for Typ {
    fn becomes(&self, rhs: &Typ) -> bool {
        let lhs = self;
        if lhs == rhs {
            return true;
        }
        match (lhs, rhs) {
            (Array(srctyp, _), Address(tgttyp)) => {
                // Handle array-to-pointer decay.
                // decay is applied only once for multidimensional array.
                srctyp == tgttyp
            }
            (Arith(at1), Arith(at2)) => at1.becomes(at2),
            (Address(_), Address(should_void)) => {
                // Pointer conversions
                // Implicit conversion into void pointer
                should_void.as_ref() == &Void
            }
            (lhs @ FuncDecl(_, _), Address(btyp)) => {
                // Function-to-pointer conversion
                match btyp.as_ref() {
                    rhs @ FuncDecl(_, _) => lhs == rhs,
                    _ => false,
                }
            }
            (lhs, Arith(Bool)) => {
                // Boolean conversions.
                lhs != &Void
            }
            _ => false,
        }
    }
}

impl ImplicitConversion<&ATyp> for ATyp {
    fn becomes(&self, _rhs: &ATyp) -> bool {
        // Implicit conversion between integers is always possible by
        // finite sequence of integral promotion and conversion

        // Implicit conversion between floats is always possible by
        // finite sequence of floating-point promotion and conversion

        // Floating-integral conversions.

        // Boolean conversions.

        // Implicit conversions between arithmetic types are always possible.
        true
    }
}

impl ImplicitConversion<ATyp> for Typ {
    fn becomes(&self, rhs: ATyp) -> bool {
        self.becomes(&Arith(rhs))
    }
}

impl Typ {
    pub fn sz(&self) -> usize {
        match self {
            Self::Address(_) => size_of::<usize>(),
            Self::Array(i, n) => i.sz() * *n as usize,
            Self::Void => 0,
            Self::Arith(at) => at.sz(),
            Self::FuncDecl(_, _) => size_of::<usize>(),
        }
    }

    pub const fn from_primtype(value: &PrimType) -> Self {
        match value {
            PrimType::Char => Arith(Int(true, 1)),
            PrimType::Double => Arith(Float(8)),
            PrimType::Float => Arith(Float(4)),
            PrimType::Int => Arith(Int(true, INT_WIDTH)),
            PrimType::Long => Arith(Int(true, 8)),
            PrimType::Short => Arith(Int(true, 2)),
            PrimType::Signed => Arith(Int(true, 4)),
            PrimType::Unsigned => Arith(Int(false, 4)),
            PrimType::Void => Void,
            PrimType::Bool => Arith(Bool),
        }
    }

    pub fn if_bool<T>(&self, value: T) -> TypeResult<T> {
        if self.becomes(Bool) {
            Ok(value)
        } else {
            Err(TypeError::NotBool(self.clone()).into())
        }
    }
}

pub trait IntoTyp<I> {
    fn to_typ(&self, inner: I) -> Typ;
}

impl<T> IntoTyp<&PrimType> for T
where
    T: IntoTyp<Typ>,
{
    fn to_typ(&self, inner: &PrimType) -> Typ {
        self.to_typ(Typ::from_primtype(inner))
    }
}

impl IntoTyp<Typ> for DirectDeclarator {
    fn to_typ(&self, inner: Typ) -> Typ {
        match self {
            DirectDeclarator::Declarator(nxt) => nxt.to_typ(inner),
            DirectDeclarator::Array(nxt, sz) => nxt.to_typ(Array(Box::new(inner), *sz)),

            DirectDeclarator::Func(nxt, params) => {
                let cur = FuncDecl(
                    Box::new(inner),
                    params
                        .iter()
                        .map(|pd| match pd {
                            ParamDeclaration::Named(ptype, decl) => decl.to_typ(ptype),
                            ParamDeclaration::Abstract(ptype, adecl) => adecl.to_typ(ptype),
                        })
                        .collect(),
                );
                nxt.to_typ(cur)
            }
            DirectDeclarator::Scala(_) => inner,
        }
    }
}

impl IntoTyp<Typ> for Declarator {
    fn to_typ(&self, inner: Typ) -> Typ {
        let (plv, nxt) = match self {
            Declarator::Init(plv, dd, _) => (plv, dd),
            Declarator::NoInit(plv, dd) => (plv, dd),
        };

        let cur = (0..*plv).fold(inner, |acc, _| Address(Box::new(acc)));
        nxt.to_typ(cur)
    }
}

impl IntoTyp<Typ> for AbstractDeclarator {
    fn to_typ(&self, inner: Typ) -> Typ {
        let Self(plv, dd) = self;

        let cur = (0..*plv).fold(inner, |acc, _| Address(Box::new(acc)));

        if let Some(nxt) = dd {
            nxt.to_typ(cur)
        } else {
            cur
        }
    }
}

impl IntoTyp<Typ> for DirectAbstractDeclarator {
    fn to_typ(&self, inner: Typ) -> Typ {
        match self {
            DirectAbstractDeclarator::AbstractDeclarator(nxt) => nxt.to_typ(inner),
            DirectAbstractDeclarator::Array(nxt, sz) => {
                let cur = Array(Box::new(inner), *sz);
                if let Some(nxt) = nxt {
                    nxt.to_typ(cur)
                } else {
                    cur
                }
            }
            DirectAbstractDeclarator::Func(nxt, params) => {
                let cur = FuncDecl(
                    Box::new(inner),
                    params
                        .iter()
                        .map(|pd| match pd {
                            ParamDeclaration::Named(ptype, decl) => decl.to_typ(ptype),
                            ParamDeclaration::Abstract(ptype, adecl) => adecl.to_typ(ptype),
                        })
                        .collect(),
                );
                if let Some(nxt) = nxt {
                    nxt.to_typ(cur)
                } else {
                    cur
                }
            }
        }
    }
}

impl ATyp {
    pub fn sz(&self) -> usize {
        match self {
            Self::Bool => 1,
            Self::Int(_signed, bytes) => *bytes,
            Self::Float(bytes) => *bytes,
        }
    }
    pub fn promote(&self) -> Self {
        // Arith operators do not accept integral types smaller than int.
        // Upcast to INT if so.
        match self {
            Int(_, width) => {
                if *width < INT_WIDTH {
                    INT_ATYP
                } else {
                    self.clone()
                }
            }
            Bool => INT_ATYP,
            Float(_) => self.clone(),
        }
    }
}

pub trait FromConst<T> {
    fn from_const(cst: T) -> Self;
}

impl FromConst<usize> for ATyp {
    fn from_const(num: usize) -> Self {
        if num <= i32::MAX.try_into().unwrap() {
            Int(true, 4)
        } else if num <= u32::MAX.try_into().unwrap() {
            Int(false, 4)
        } else if num <= u64::MAX.try_into().unwrap() {
            Int(false, 8)
        } else {
            panic!("Integer constant is too big!")
        }
    }
}

impl FromConst<bool> for ATyp {
    fn from_const(_cst: bool) -> Self {
        Bool
    }
}

impl<T> FromConst<T> for Typ
where
    ATyp: FromConst<T>,
{
    fn from_const(cst: T) -> Self {
        Arith(ATyp::from_const(cst))
    }
}

impl Typ {
    pub fn from_constexpr(cst: &Expr) -> Self {
        match cst {
            Expr::ConstFloat(_) => Arith(Float(4)),
            Expr::Const(v) => Typ::from_const(*v),
            Expr::ConstBool(v) => Typ::from_const(*v),
            _ => panic!("Invalid expr!!"),
        }
    }
}

// Perform usual arithmetic conversion.
// https://en.cppreference.com/w/cpp/language/usual_arithmetic_conversions
impl ops::Add<ATyp> for ATyp {
    type Output = ATyp;
    fn add(self, rhs: ATyp) -> Self::Output {
        let lhs = self.promote();
        let rhs = rhs.promote();

        match (lhs, rhs) {
            (it @ (Float(_) | Int(_, _)), ft @ Float(fb)) | (ft @ Float(fb), it @ Int(_, _)) => {
                // If either operand is of floating-point type, ...
                if let Float(ib) = it {
                    if ib > fb {
                        it
                    } else {
                        ft
                    }
                } else {
                    ft
                }
            }
            (lt @ Int(true, lb), rt @ Int(true, rb))
            | (lt @ Int(false, lb), rt @ Int(false, rb)) => {
                // If T1 and T2 are both signed integer types or both unsigned
                // integer types, C is the type of greater integer conversion
                // rank.
                if lb > rb {
                    lt
                } else {
                    rt
                }
            }
            (st @ Int(true, sb), ut @ Int(false, ub))
            | (ut @ Int(false, ub), st @ Int(true, sb)) => {
                // Otherwise, one type between T1 and T2 is an signed integer
                // type S, the other type is an unsigned integer type U.
                // Apply the following rules:

                // If the integer conversion rank of U is greater than or
                // equal to the integer conversion rank of S, C is U.
                if ub >= sb {
                    ut
                }
                // Otherwise, if S can represent all of the values of U, C is S.
                else if sb >= ub * 2 {
                    st
                }
                // Otherwise, C is the unsigned integer type corresponding to S.
                else {
                    Int(false, sb)
                }
            }
            (Bool, _) | (_, Bool) => {
                unreachable!("atyp cannot be bool after arithmetic promotion")
            }
        }
    }
}
