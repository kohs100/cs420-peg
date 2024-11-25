use crate::ast::*;
use std::fmt::Debug;
use std::fmt::Display;
use std::ops;

const INT_WIDTH: usize = 4;
const INT_ATYP: ATyp = ATyp::Int(true, INT_WIDTH);
const INT_TYP: Typ = Typ::Arith(INT_ATYP);

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone)]
pub enum TypeError {
    InvalidIdentifier(String),
    InvalidDefinition(String),
    Redefinition(String),
    NoValidMain(String),
    TypeMismatch(Typ, Typ),
    TypeIncompatible,
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
            Self::NoValidMain(i) => {
                write!(f, "Invalid entry point: {}", i)
            }
            Self::TypeMismatch(lt, rt) => {
                write!(f, "Type mismatch: {} vs {}", lt, rt)
            }
            Self::TypeIncompatible => {
                write!(f, "Type incompatible")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Typ {
    Address(Box<Typ>),
    Array(Box<Typ>, u64),
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
        Typ::Arith(self)
    }
}

// Implicit conversion between arithmetic types
// https://en.cppreference.com/w/cpp/language/implicit_conversion
// https://en.cppreference.com/w/cpp/language/array#Array-to-pointer_decay
pub trait ImplicitConversion<T> {
    fn becomes(&self, rhs: T) -> bool;
}

impl Display for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Address(i) => write!(f, "*{}", i),
            Self::Array(typ, sz) => write!(f, "{}[{}]", typ, sz),
            Self::Void => write!(f, "void"),
            Self::Arith(atyp) => write!(f, "{}", atyp),
            Self::FuncDecl(rettyp, ptypes) => {
                write!(f, "{}(", rettyp)?;
                print_commasep(f, ptypes)?;
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

impl ImplicitConversion<&Typ> for Typ {
    fn becomes(&self, rhs: &Typ) -> bool {
        let lhs = self;
        if lhs == rhs {
            return true;
        }
        match (lhs, rhs) {
            (Typ::Array(srctyp, _), Typ::Address(tgttyp)) => {
                // Handle array-to-pointer decay.
                // decay is applied only once for multidimensional array.
                srctyp == tgttyp
            }
            (Typ::Arith(at1), Typ::Arith(at2)) => at1.becomes(at2),
            (Typ::Address(_), Typ::Address(should_void)) => {
                // Pointer conversions
                // Implicit conversion into void pointer
                should_void.as_ref() == &Typ::Void
            }
            (lhs, Typ::Arith(ATyp::Bool)) => {
                // Boolean conversions.
                lhs != &Typ::Void
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
        self.becomes(&Typ::Arith(rhs))
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

    const fn from_primtype(value: &PrimType) -> Self {
        match value {
            PrimType::Char => Typ::Arith(ATyp::Int(true, 1)),
            PrimType::Double => Typ::Arith(ATyp::Float(8)),
            PrimType::Float => Typ::Arith(ATyp::Float(4)),
            PrimType::Int => Typ::Arith(ATyp::Int(true, INT_WIDTH)),
            PrimType::Long => Typ::Arith(ATyp::Int(true, 8)),
            PrimType::Short => Typ::Arith(ATyp::Int(true, 2)),
            PrimType::Signed => Typ::Arith(ATyp::Int(true, 4)),
            PrimType::Unsigned => Typ::Arith(ATyp::Int(false, 4)),
            PrimType::Void => Typ::Void,
            PrimType::Bool => Typ::Arith(ATyp::Bool),
        }
    }

    pub fn from_decl(ptype: &PrimType, decl: &Declarator) -> Typ {
        let (plv, dd) = match decl {
            Declarator::Init(plv, dd, _) => (plv, dd),
            Declarator::NoInit(plv, dd) => (plv, dd),
        };

        let typ = (0..*plv).fold(Typ::from_primtype(ptype), |acc, _| {
            Typ::Address(Box::new(acc))
        });

        match dd {
            DirectDeclarator::Array(_, len) => len
                .iter()
                .rev()
                .fold(typ, |acc, sz| Typ::Array(Box::new(acc), *sz)),
            DirectDeclarator::Func(_, params) => Typ::FuncDecl(
                Box::new(typ),
                params
                    .iter()
                    .map(|pd| {
                        let ParamDeclaration(pt, dcl) = pd;
                        Typ::from_decl(pt, dcl)
                    })
                    .collect(),
            ),
            DirectDeclarator::Scala(_) => typ,
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
            ATyp::Int(_, width) => {
                if *width < INT_WIDTH {
                    INT_ATYP
                } else {
                    self.clone()
                }
            }
            ATyp::Bool => INT_ATYP,
            ATyp::Float(_) => self.clone(),
        }
    }
}

pub trait FromConst<T> {
    fn from_const(cst: T) -> Self;
}

impl FromConst<u64> for ATyp {
    fn from_const(num: u64) -> Self {
        if num <= i32::MAX.try_into().unwrap() {
            ATyp::Int(true, 4)
        } else if num <= u32::MAX.try_into().unwrap() {
            ATyp::Int(false, 4)
        } else if num <= u64::MAX.try_into().unwrap() {
            ATyp::Int(false, 8)
        } else {
            panic!("Integer constant is too big!")
        }
    }
}

impl FromConst<bool> for ATyp {
    fn from_const(_cst: bool) -> Self {
        ATyp::Bool
    }
}

impl<T> FromConst<T> for Typ
where
    ATyp: FromConst<T>,
{
    fn from_const(cst: T) -> Self {
        Typ::Arith(ATyp::from_const(cst))
    }
}

impl Typ {
    pub fn from_constexpr(cst: &Expr) -> Self {
        match cst {
            Expr::ConstFloat(_) => Typ::Arith(ATyp::Float(4)),
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
            (it @ (ATyp::Float(_) | ATyp::Int(_, _)), ft @ ATyp::Float(fb))
            | (ft @ ATyp::Float(fb), it @ ATyp::Int(_, _)) => {
                // If either operand is of floating-point type, ...
                if let ATyp::Float(ib) = it {
                    if ib > fb {
                        it
                    } else {
                        ft
                    }
                } else {
                    ft
                }
            }
            (lt @ ATyp::Int(true, lb), rt @ ATyp::Int(true, rb))
            | (lt @ ATyp::Int(false, lb), rt @ ATyp::Int(false, rb)) => {
                // If T1 and T2 are both signed integer types or both unsigned
                // integer types, C is the type of greater integer conversion
                // rank.
                if lb > rb {
                    lt
                } else {
                    rt
                }
            }
            (st @ ATyp::Int(true, sb), ut @ ATyp::Int(false, ub))
            | (ut @ ATyp::Int(false, ub), st @ ATyp::Int(true, sb)) => {
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
                    ATyp::Int(false, sb)
                }
            }
            (ATyp::Bool, _) | (_, ATyp::Bool) => {
                unreachable!("atyp cannot be bool after arithmetic promotion")
            }
        }
    }
}
