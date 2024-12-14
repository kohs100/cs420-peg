use crate::ast::expr::*;
use crate::ast::types::*;

use std::fmt::Debug;
use std::fmt::Display;

use super::defs::ATyp::*;
use super::defs::Typ::*;

impl ParamDeclaration {
    pub fn typ_and_name(&self) -> (Typ, Option<&str>) {
        match self {
            ParamDeclaration::Abstract(ptype, decl) => (decl.to_typ(ptype), None),
            ParamDeclaration::Named(ptype, decl) => (decl.to_typ(ptype), Some(decl.name())),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ATyp {
    Int(ITyp),
    Bool,
    Float(FTyp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ITyp {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FTyp {
    F32,
    F64,
}

impl FTyp {
    pub fn width_bytes(&self) -> usize {
        match self {
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }
}

impl Into<ATyp> for FTyp {
    fn into(self) -> ATyp {
        ATyp::Float(self)
    }
}

impl Into<Typ> for FTyp {
    fn into(self) -> Typ {
        Typ::Arith(self.into())
    }
}

impl Into<ATyp> for ITyp {
    fn into(self) -> ATyp {
        ATyp::Int(self)
    }
}
impl Into<Typ> for ITyp {
    fn into(self) -> Typ {
        Typ::Arith(self.into())
    }
}

impl ITyp {
    pub fn signed(&self) -> bool {
        use ITyp::*;
        match self {
            I8 | I16 | I32 | I64 => true,
            U8 | U16 | U32 | U64 => false,
        }
    }
    pub fn width_bytes(&self) -> usize {
        use ITyp::*;
        match self {
            I8 | U8 => 1,
            I16 | U16 => 2,
            I32 | U32 => 4,
            I64 | U64 => 8,
        }
    }
    pub fn as_unsigned(&self) -> Self {
        use ITyp::*;
        match self {
            I8 | U8 => U8,
            I16 | U16 => U16,
            I32 | U32 => U32,
            I64 | U64 => U64,
        }
    }
    pub fn max(&self) -> u64 {
        use ITyp::*;
        match self {
            I8 => i8::MAX.try_into().unwrap(),
            U8 => u8::MAX.try_into().unwrap(),
            I16 => i16::MAX.try_into().unwrap(),
            U16 => u16::MAX.try_into().unwrap(),
            I32 => i32::MAX.try_into().unwrap(),
            U32 => u32::MAX.try_into().unwrap(),
            I64 => i64::MAX.try_into().unwrap(),
            U64 => u64::MAX.try_into().unwrap(),
        }
    }
    pub fn min(&self) -> u64 {
        use ITyp::*;
        match self {
            I8 => i8::MIN.try_into().unwrap(),
            U8 => u8::MIN.try_into().unwrap(),
            I16 => i16::MIN.try_into().unwrap(),
            U16 => u16::MIN.try_into().unwrap(),
            I32 => i32::MIN.try_into().unwrap(),
            U32 => u32::MIN.try_into().unwrap(),
            I64 => i64::MIN.try_into().unwrap(),
            U64 => u64::MIN.try_into().unwrap(),
        }
    }
}

impl Display for ITyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ITyp::*;
        match self {
            I8 => write!(f, "i8"),
            U8 => write!(f, "u8"),
            I16 => write!(f, "i16"),
            U16 => write!(f, "u16"),
            I32 => write!(f, "i32"),
            U32 => write!(f, "u32"),
            I64 => write!(f, "i64"),
            U64 => write!(f, "u64"),
        }
    }
}

impl Into<Typ> for ATyp {
    fn into(self) -> Typ {
        Arith(self)
    }
}

impl Display for FTyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
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
            Self::Int(ityp) => write!(f, "{}", ityp),
            Self::Float(ftyp) => write!(f, "{}", ftyp),
            Self::Bool => write!(f, "bool"),
        }
    }
}
impl Typ {
    pub const fn from_primtype(value: &PrimType) -> Self {
        match value {
            PrimType::Char => Arith(Int(ITyp::I8)),
            PrimType::Double => Arith(Float(FTyp::F64)),
            PrimType::Float => Arith(Float(FTyp::F32)),
            PrimType::Int => Arith(Int(ITyp::I32)),
            PrimType::Long => Arith(Int(ITyp::I64)),
            PrimType::Short => Arith(Int(ITyp::I16)),
            PrimType::Signed => Arith(Int(ITyp::I32)),
            PrimType::Unsigned => Arith(Int(ITyp::U32)),
            PrimType::Void => Void,
            PrimType::Bool => Arith(Bool),
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

pub trait FromConst<T> {
    fn from_const(cst: T) -> Self;
}

impl FromConst<usize> for ATyp {
    fn from_const(num: usize) -> Self {
        if num <= i32::MAX.try_into().unwrap() {
            ITyp::I32.into()
        } else if num <= u32::MAX.try_into().unwrap() {
            ITyp::U32.into()
        } else if num <= u64::MAX.try_into().unwrap() {
            ITyp::U64.into()
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
            Expr::ConstFloat(_) => FTyp::F64.into(),
            Expr::Const(v) => Typ::from_const(*v),
            Expr::ConstBool(v) => Typ::from_const(*v),
            _ => panic!("Invalid expr!!"),
        }
    }
}
