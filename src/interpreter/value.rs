use std::fmt::Display;

use super::defs::{ATyp, FTyp, ITyp, Typ};
use super::{allocator::VirtualMemory, vptr::VirtualPointer};
use crate::interpreter::vptr::AddrType;

use super::IpretError::{Misc, MiscOwned};
use super::IpretResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    OnMemory(MemObj),
    Inter(InterValue),
}

impl Value {
    pub fn unwrap_memobj(&self) -> Option<&MemObj> {
        if let Self::OnMemory(mo) = self {
            Some(mo)
        } else {
            None
        }
    }

    pub fn unwrap_value<T: VirtualMemory>(&self, vm: &T) -> InterValue {
        match self {
            Self::Inter(iv) => iv.clone(),
            Self::OnMemory(mo) => mo.load(vm),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemObj {
    pub typ: Typ,
    pub addr: VirtualPointer,
}

impl MemObj {
    pub fn new(typ: Typ, addr: VirtualPointer) -> Self {
        Self { typ, addr }
    }
    pub fn load<T: VirtualMemory>(&self, vm: &T) -> InterValue {
        match &self.typ {
            Typ::FuncDecl(_, _) => {
                panic!("Funcdecl value is not allowed!");
            }
            Typ::Array(_, _) => {
                let addr: AddrType = self.addr.into();

                InterValue {
                    typ: self.typ.clone(),
                    value: Box::new(addr.to_ne_bytes()),
                }
            }
            _ => InterValue {
                typ: self.typ.clone(),
                value: vm
                    .get_slice::<u8>(self.addr, 1, self.typ.size_bytes())
                    .to_vec()
                    .into(),
            },
        }
    }
    pub fn store<T: VirtualMemory>(&self, vm: &mut T, val: InterValue) {
        let ba = vm.get_slice_mut::<u8>(self.addr, 1, self.typ.size_bytes());
        assert_eq!(
            val.typ, self.typ,
            "Type mismatch: Tried to store {} -> {}",
            val.typ, self.typ
        );
        assert_eq!(ba.len(), val.value.len(), "Cannot happen");
        ba.copy_from_slice(&val.value);
    }

    pub fn refer(&self) -> InterValue {
        InterValue::from_type(Typ::Address(Box::new(self.typ.clone())), self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterValue {
    pub typ: Typ,
    pub value: Box<[u8]>,
}

impl Display for InterValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.typ {
            Typ::Address(_) | Typ::Array(_, _) => {
                let addr = self.get_as::<AddrType>();
                write!(f, "{}", addr)
            }
            Typ::Arith(atyp) => match atyp {
                ATyp::Bool => write!(f, "{}", self.get_as::<bool>()),
                ATyp::Float(ftyp) => match ftyp {
                    FTyp::F32 => write!(f, "{}", self.get_as::<f32>()),
                    FTyp::F64 => write!(f, "{}", self.get_as::<f64>()),
                },
                ATyp::Int(ityp) => match ityp {
                    ITyp::I8 => write!(f, "{:.03}", self.get_as::<i8>()),
                    ITyp::I16 => write!(f, "{:.03}", self.get_as::<i16>()),
                    ITyp::I32 => write!(f, "{:.03}", self.get_as::<i32>()),
                    ITyp::I64 => write!(f, "{:.03}", self.get_as::<i64>()),
                    ITyp::U8 => write!(f, "{:.03}", self.get_as::<u8>()),
                    ITyp::U16 => write!(f, "{:.03}", self.get_as::<u16>()),
                    ITyp::U32 => write!(f, "{:.03}", self.get_as::<u32>()),
                    ITyp::U64 => write!(f, "{:.03}", self.get_as::<u64>()),
                },
            },
            Typ::Void => write!(f, "VOID"),
            Typ::FuncDecl(_, _) => panic!("Tried to display funcdecl type"),
        }
    }
}

macro_rules! ivalue {
    ( $value:expr, $dst_typ:expr, $dst_rtyp:ty ) => {{
        InterValue::from_type::<$dst_rtyp>(($dst_typ).clone().into(), $value as $dst_rtyp)
    }};
}

macro_rules! int_ivalue_from {
    ( $src_ival:expr, $src_rtyp:ty, $dst_ityp:expr ) => {{
        let src_val = ($src_ival).get_as::<$src_rtyp>();
        use ITyp::*;
        match $dst_ityp {
            I8 => ivalue!(src_val, $dst_ityp, i8),
            I16 => ivalue!(src_val, $dst_ityp, i16),
            I32 => ivalue!(src_val, $dst_ityp, i32),
            I64 => ivalue!(src_val, $dst_ityp, i64),
            U8 => ivalue!(src_val, $dst_ityp, i8),
            U16 => ivalue!(src_val, $dst_ityp, u16),
            U32 => ivalue!(src_val, $dst_ityp, u32),
            U64 => ivalue!(src_val, $dst_ityp, u64),
        }
    }};
}

macro_rules! float_ivalue_from {
    ( $src_ival:expr, $src_rtyp:ty, $dst_ftyp:expr ) => {{
        let src_val = ($src_ival).get_as::<$src_rtyp>();
        use FTyp::*;
        match $dst_ftyp {
            F32 => ivalue!(src_val, $dst_ftyp, f32),
            F64 => ivalue!(src_val, $dst_ftyp, f64),
        }
    }};
}

pub fn assert_typ<T>(typ: &Typ) {
    let should_be = match typ {
        Typ::Array(_, _) => size_of::<AddrType>(),
        _ => typ.size_bytes(),
    };
    assert_eq!(should_be, size_of::<T>(), "Type size incompatible: {}", typ)
}

impl InterValue {
    pub fn void() -> Self {
        Self {
            typ: Typ::Void,
            value: Vec::new().into(),
        }
    }

    pub fn promote(self) -> IpretResult<Self> {
        if let Typ::Arith(atyp) = self.typ {
            use ATyp::*;
            use ITyp::*;
            Ok(match atyp {
                Bool => int_ivalue_from!(self, bool, I32),
                Int(ityp) => match ityp {
                    I8 => int_ivalue_from!(self, bool, I32),
                    I16 => int_ivalue_from!(self, bool, I32),
                    U8 => int_ivalue_from!(self, bool, I32),
                    U16 => int_ivalue_from!(self, bool, I32),
                    _ => self,
                },
                Float(_) => self,
            })
        } else {
            Err(MiscOwned(format!(
                "{} cannot be arithmetic promoted.",
                self.typ
            )))
        }
    }

    pub fn complement(self) -> IpretResult<Self> {
        let promoted = self.promote()?;
        if let Typ::Arith(atyp) = promoted.typ {
            match atyp {
                ATyp::Int(ityp) => match ityp {
                    ITyp::I32 | ITyp::U32 => {
                        let v = promoted.get_as::<u32>();
                        Ok(InterValue::from_type::<u32>(ityp.into(), v.wrapping_neg()).into())
                    }
                    ITyp::I64 | ITyp::U64 => {
                        let v = promoted.get_as::<u64>();
                        Ok(InterValue::from_type::<u64>(ityp.into(), v.wrapping_neg()).into())
                    }
                    _ => Err(Misc("Invalid ityp encountered in during negation")),
                },
                ATyp::Float(ftyp) => match ftyp {
                    FTyp::F32 => {
                        let v = promoted.get_as::<f32>();
                        Ok(InterValue::from_type::<f32>(ftyp.into(), -v).into())
                    }
                    FTyp::F64 => {
                        let v = promoted.get_as::<f64>();
                        Ok(InterValue::from_type::<f64>(ftyp.into(), -v).into())
                    }
                },
                _ => Err(Misc("Invalid atyp encountered in during negation")),
            }
        } else {
            Err(MiscOwned(format!(
                "Cannot perform 2's complement to {}",
                promoted.typ
            )))
        }
    }

    pub fn bitneg(self) -> IpretResult<Self> {
        let mut promoted = self.promote()?;
        let bytes = promoted.get_bytes_mut();

        for byte in bytes {
            *byte = !*byte;
        }

        Ok(promoted)
    }
    pub fn get_as<T: Copy>(&self) -> T {
        assert_typ::<T>(&self.typ);

        unsafe {
            let raw_ptr = self.value.as_ptr() as *const T;
            *raw_ptr
        }
    }

    pub fn get_bytes(&self) -> &Box<[u8]> {
        &self.value
    }

    pub fn get_bytes_mut(&mut self) -> &mut Box<[u8]> {
        &mut self.value
    }

    pub fn iabs(&self) -> IpretResult<(bool, u64)> {
        if let Typ::Arith(atyp) = self.typ {
            match atyp {
                ATyp::Bool => Ok((false, self.get_as::<bool>().into())),
                ATyp::Int(ityp) => Ok(match ityp {
                    ITyp::I8 => {
                        let v = self.get_as::<i8>();
                        (v < 0, v.unsigned_abs().into())
                    }
                    ITyp::I16 => {
                        let v = self.get_as::<i16>();
                        (v < 0, v.unsigned_abs().into())
                    }
                    ITyp::I32 => {
                        let v = self.get_as::<i32>();
                        (v < 0, v.unsigned_abs().into())
                    }
                    ITyp::I64 => {
                        let v = self.get_as::<i64>();
                        (v < 0, v.unsigned_abs().into())
                    }
                    ITyp::U8 => {
                        let v = self.get_as::<u8>();
                        (false, v.into())
                    }
                    ITyp::U16 => {
                        let v = self.get_as::<u8>();
                        (false, v.into())
                    }
                    ITyp::U32 => {
                        let v = self.get_as::<u8>();
                        (false, v.into())
                    }
                    ITyp::U64 => {
                        let v = self.get_as::<u8>();
                        (false, v.into())
                    }
                }),
                _ => Err(MiscOwned(format!("Not an integer type: {}", self.typ))),
            }
        } else {
            Err(MiscOwned(format!("Not an arithmetic type: {}", self.typ)))
        }
    }

    pub fn from_type<T: Copy>(typ: Typ, val: T) -> Self {
        assert_typ::<T>(&typ);
        let sz = size_of::<T>();

        let mut buf: Vec<u8> = Vec::new();
        buf.resize(sz, 0);

        unsafe {
            let raw_ptr = buf.as_mut_ptr() as *mut T;
            *raw_ptr = val;
        }

        let value: Box<[u8]> = buf.into();
        assert_eq!(sz, value.len());

        Self { typ, value }
    }

    pub fn from_const_int(val: u64) -> Self {
        if val < ITyp::I32.max() {
            Self::from_type::<i32>(ITyp::I32.into(), val as i32)
        } else if val < ITyp::I64.max() {
            Self::from_type::<i64>(ITyp::I32.into(), val as i64)
        } else {
            Self::from_type::<u64>(ITyp::U64.into(), val)
        }
    }

    pub fn from_float_str(val: &str) -> Self {
        let parsed: f64 = val.parse().unwrap();
        Self::from_type::<f64>(FTyp::F64.into(), parsed)
    }

    pub fn from_const_bool(val: bool) -> Self {
        Self::from_type::<u8>(ATyp::Bool.into(), val as u8)
    }

    pub fn deref(self) -> IpretResult<MemObj> {
        match &self.typ {
            Typ::Address(ityp) | Typ::Array(ityp, _) => {
                let vptr = self.get_as::<AddrType>();
                Ok(MemObj::new(
                    ityp.as_ref().clone(),
                    VirtualPointer::with(vptr),
                ))
            }
            Typ::FuncDecl(_, _) => {
                panic!("Function value is not allowed!")
            }
            _ => Err(Misc("Dereference through non-pointer type is not allowed.")),
        }
    }

    pub fn cast_into(self, typ: &Typ, _explicit: bool) -> IpretResult<Self> {
        if &self.typ == typ {
            return Ok(self);
        }
        // println!("Casting {} into {}...", self.typ, typ);
        use ATyp::*;
        use FTyp::*;
        use ITyp::*;
        let res = match (&self.typ, typ) {
            (Typ::Address(src_inner_typ), Typ::Address(dst_inner_typ)) => {
                // Pointer conversion.
                // Implicit conversion into/from void pointer.
                if dst_inner_typ.as_ref() == &Typ::Void {
                    Ok(Self {
                        typ: Typ::Void,
                        value: self.value,
                    })
                } else if src_inner_typ.as_ref() == &Typ::Void {
                    Ok(Self {
                        typ: Typ::Void,
                        value: self.value,
                    })
                } else {
                    Err(Misc(
                        "Cannot cast arbitrary pointer type to non-void pointer type.",
                    ))
                }
            }
            (Typ::Array(arr_inner, _sz), atyp @ Typ::Address(ptr_inner)) => {
                // Array-to-pointer decay.
                // decay is applied only once.
                if arr_inner == ptr_inner {
                    Ok(Self {
                        typ: atyp.clone(),
                        value: self.value,
                    })
                } else {
                    Err(MiscOwned(format!(
                        "Cannot decay {} into {}",
                        arr_inner, ptr_inner
                    )))
                }
            }
            (src_typ, Typ::Arith(Bool)) => {
                // Boolean conversion.
                match src_typ {
                    Typ::Void => Err(Misc("Cannot cast void to bool.")),
                    _ => {
                        let bytes = self.get_bytes();
                        let is_zero = bytes.iter().all(|x| *x == 0);
                        Ok(InterValue::from_const_bool(!is_zero))
                    }
                }
            }
            (Typ::Arith(src_atyp), Typ::Arith(dst_atyp)) => match (src_atyp, dst_atyp) {
                (Float(src_ftyp), Float(dst_ftyp)) => Ok(match (src_ftyp, dst_ftyp) {
                    (F32, F64) => float_ivalue_from!(self, f32, F64),
                    (F64, F32) => float_ivalue_from!(self, f64, F32),
                    _ => unreachable!("Identical type"),
                }),
                (_, Bool) => unreachable!("Already covered."),
                (Int(src_ityp), Int(dst_ityp)) => Ok(match src_ityp {
                    I8 => int_ivalue_from!(self, i8, dst_ityp),
                    I16 => int_ivalue_from!(self, i16, dst_ityp),
                    I32 => int_ivalue_from!(self, i32, dst_ityp),
                    I64 => int_ivalue_from!(self, i64, dst_ityp),
                    U8 => int_ivalue_from!(self, u8, dst_ityp),
                    U16 => int_ivalue_from!(self, u16, dst_ityp),
                    U32 => int_ivalue_from!(self, u32, dst_ityp),
                    U64 => int_ivalue_from!(self, u64, dst_ityp),
                }),
                (Int(src_ityp), Float(dst_ftyp)) => Ok(match (src_ityp, dst_ftyp) {
                    (I8, F32) => float_ivalue_from!(self, i8, dst_ftyp),
                    (I16, F32) => float_ivalue_from!(self, i16, dst_ftyp),
                    (I32, F32) => float_ivalue_from!(self, i32, dst_ftyp),
                    (I64, F32) => float_ivalue_from!(self, i64, dst_ftyp),

                    (U8, F32) => float_ivalue_from!(self, u8, dst_ftyp),
                    (U16, F32) => float_ivalue_from!(self, u16, dst_ftyp),
                    (U32, F32) => float_ivalue_from!(self, u32, dst_ftyp),
                    (U64, F32) => float_ivalue_from!(self, u64, dst_ftyp),

                    (I8, F64) => float_ivalue_from!(self, i8, dst_ftyp),
                    (I16, F64) => float_ivalue_from!(self, i16, dst_ftyp),
                    (I32, F64) => float_ivalue_from!(self, i32, dst_ftyp),
                    (I64, F64) => float_ivalue_from!(self, i64, dst_ftyp),

                    (U8, F64) => float_ivalue_from!(self, u8, dst_ftyp),
                    (U16, F64) => float_ivalue_from!(self, u16, dst_ftyp),
                    (U32, F64) => float_ivalue_from!(self, u32, dst_ftyp),
                    (U64, F64) => float_ivalue_from!(self, u64, dst_ftyp),
                }),
                (Float(src_ftyp), Int(dst_ityp)) => Ok(match (src_ftyp, dst_ityp) {
                    (F32, I8) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, I16) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, I32) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, I64) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, U8) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, U16) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, U32) => int_ivalue_from!(self, f32, dst_ityp),
                    (F32, U64) => int_ivalue_from!(self, f32, dst_ityp),
                    (F64, I8) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, I16) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, I32) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, I64) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, U8) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, U16) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, U32) => int_ivalue_from!(self, f64, dst_ityp),
                    (F64, U64) => int_ivalue_from!(self, f64, dst_ityp),
                }),
                (Bool, Float(ftyp)) => Ok(match ftyp {
                    F32 => InterValue::from_type::<f32>(
                        F32.into(),
                        if self.get_as::<bool>() { 1.0 } else { 0. },
                    ),
                    F64 => InterValue::from_type::<f64>(
                        F64.into(),
                        if self.get_as::<bool>() { 1.0 } else { 0. },
                    ),
                }),
                (Bool, Int(ityp)) => Ok(match ityp {
                    I8 => int_ivalue_from!(self, bool, ityp),
                    I16 => int_ivalue_from!(self, bool, ityp),
                    I32 => int_ivalue_from!(self, bool, ityp),
                    I64 => int_ivalue_from!(self, bool, ityp),
                    U8 => int_ivalue_from!(self, bool, ityp),
                    U16 => int_ivalue_from!(self, bool, ityp),
                    U32 => int_ivalue_from!(self, bool, ityp),
                    U64 => int_ivalue_from!(self, bool, ityp),
                }),
            },
            _ => Err(MiscOwned(format!(
                "Invalid casting: from {} to {}",
                self.typ, typ
            ))),
        }?;
        assert_eq!(&res.typ, typ);
        Ok(res)
    }
}

impl Into<Value> for MemObj {
    fn into(self) -> Value {
        Value::OnMemory(self)
    }
}
impl Into<Value> for InterValue {
    fn into(self) -> Value {
        Value::Inter(self)
    }
}

fn do_pointer_arithmetic(
    ptr: InterValue,
    idx: InterValue,
    is_sub: bool,
) -> IpretResult<InterValue> {
    let (inner_typ, bound) = match (&ptr.typ, &idx.typ) {
        (Typ::Address(ityp), Typ::Arith(ATyp::Int(_) | ATyp::Bool)) => (ityp, None),
        (Typ::Array(ityp, sz), Typ::Arith(ATyp::Int(_) | ATyp::Bool)) => (ityp, Some(sz)),
        _ => return Err(Misc("Invalid operands for pointer arithmetic")),
    };

    match inner_typ.as_ref() {
        Typ::Void => return Err(Misc("Pointer arithmetic with void pointer is not allowed.")),
        _ => (),
    };

    let addr = ptr.get_as::<AddrType>();
    let addr_unit = inner_typ.size_vptr_delta();

    let (is_neg, addr_delta) = idx.iabs()?;

    if let Some(bound) = bound {
        if is_neg {
            return Err(Misc("Cannot apply negative index to array"));
        }
        if *bound <= addr_delta.try_into().unwrap() {
            return Err(Misc("Index out of bound"));
        }
    }

    let res_addr = if is_sub == is_neg {
        addr + addr_unit * addr_delta
    } else {
        addr - addr_unit * addr_delta
    };

    Ok(InterValue::from_type(ptr.typ.clone(), res_addr))
}

fn do_usual_arithmetic_conversion(
    lhs: InterValue,
    rhs: InterValue,
) -> IpretResult<(InterValue, InterValue)> {
    let lhs = lhs.promote()?;
    let rhs = rhs.promote()?;

    use ATyp::*;

    let ltyp = lhs.typ.clone();
    let rtyp = rhs.typ.clone();

    let ctyp: Typ = match (ltyp, rtyp) {
        (Typ::Arith(latyp), Typ::Arith(ratyp)) => match (latyp, ratyp) {
            (Bool, _) | (_, Bool) => unreachable!("atyp cannot be bool after arithmetic promotion"),
            (Int(_), Float(ftyp)) | (Float(ftyp), Int(_)) => ftyp.into(),
            (Int(lityp), Int(rityp)) => {
                if lityp.signed() == rityp.signed() {
                    if lityp.width_bytes() < rityp.width_bytes() {
                        lityp.into()
                    } else {
                        rityp.into()
                    }
                } else {
                    let (sityp, uityp) = if lityp.signed() {
                        (lityp, rityp)
                    } else {
                        (rityp, lityp)
                    };
                    let sb = sityp.width_bytes();
                    let ub = uityp.width_bytes();

                    if ub >= sb {
                        uityp.into()
                    } else if sb >= ub * 2 {
                        sityp.into()
                    } else {
                        sityp.as_unsigned().into()
                    }
                }
            }
            (Float(lftyp), Float(rftyp)) => {
                if lftyp.width_bytes() < rftyp.width_bytes() {
                    lftyp.into()
                } else {
                    rftyp.into()
                }
            }
        },
        _ => unreachable!(),
    };

    Ok((lhs.cast_into(&ctyp, false)?, rhs.cast_into(&ctyp, false)?))
}

macro_rules! coerce {
    ($lhs:expr, $op:tt, $rhs:expr, $rtyp:ty, $typ:expr) => {{
        let v = ($lhs).get_as::<$rtyp>() $op ($rhs).get_as::<$rtyp>();
        ivalue!(v, $typ, $rtyp)
    }};
}

macro_rules! coerce_bool {
    ($lhs:expr, $op:tt, $rhs:expr, $rtyp:ty) => {{
        let v: bool = ($lhs).get_as::<$rtyp>() $op ($rhs).get_as::<$rtyp>();
        ivalue!(v, ATyp::Bool, bool)
    }};
}

macro_rules! define_cmp {
    ($lhs:expr, $op:tt, $rhs:expr) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        match (&lhs.typ, &rhs.typ) {
            (Typ::Arith(_), Typ::Arith(_)) => {
                let (lhs, rhs) = do_usual_arithmetic_conversion(lhs, rhs)?;
                assert_eq!(&lhs.typ, &rhs.typ);
                use ATyp::*;
                use FTyp::*;
                use ITyp::*;
                if let Typ::Arith(Float(ftyp)) = lhs.typ {
                    Ok(match ftyp {
                        F32 => coerce_bool!(lhs, $op, rhs, f32),
                        F64 => coerce_bool!(lhs, $op, rhs, f64),
                    })
                } else if let Typ::Arith(Int(ityp)) = lhs.typ {
                    Ok(match ityp {
                        I32 => coerce_bool!(lhs, $op, rhs, i32),
                        I64 => coerce_bool!(lhs, $op, rhs, i64),
                        U32 => coerce_bool!(lhs, $op, rhs, u32),
                        U64 => coerce_bool!(lhs, $op, rhs, u64),
                        _ => unreachable!(),
                    })
                } else {
                    unreachable!()
                }
            }
            (Typ::Address(_) | Typ::Array(_, _), Typ::Address(_) | Typ::Array(_, _)) => {
                Ok(coerce_bool!(lhs, $op, rhs, AddrType))
            }
            _ => Err(MiscOwned(format!(
                "Invalid type for {} - {}",
                lhs.typ, rhs.typ
            ))),
        }
    }};
}

impl InterValue {
    pub fn add(self, rhs: InterValue) -> IpretResult<InterValue> {
        match (&self.typ, &rhs.typ) {
            (Typ::Arith(_), Typ::Arith(_)) => {
                let (lhs, rhs) = do_usual_arithmetic_conversion(self, rhs)?;
                assert_eq!(&lhs.typ, &rhs.typ);
                use ATyp::*;
                use FTyp::*;
                use ITyp::*;
                if let Typ::Arith(Float(ftyp)) = lhs.typ {
                    Ok(match ftyp {
                        F32 => coerce!(lhs, +, rhs, f32, ftyp),
                        F64 => coerce!(lhs, +, rhs, f64, ftyp),
                    })
                } else if let Typ::Arith(Int(ityp)) = lhs.typ {
                    Ok(match ityp {
                        I32 => coerce!(lhs, +, rhs, i32, ityp),
                        I64 => coerce!(lhs, +, rhs, i64, ityp),
                        U32 => coerce!(lhs, +, rhs, u32, ityp),
                        U64 => coerce!(lhs, +, rhs, u64, ityp),
                        _ => unreachable!(),
                    })
                } else {
                    unreachable!()
                }
            }
            (Typ::Address(_) | Typ::Array(_, _), Typ::Arith(ATyp::Int(_))) => {
                do_pointer_arithmetic(self, rhs, false)
            }
            (Typ::Arith(ATyp::Int(_)), Typ::Address(_) | Typ::Array(_, _)) => {
                do_pointer_arithmetic(rhs, self, false)
            }
            _ => Err(MiscOwned(format!(
                "Invalid type for {} + {}",
                self.typ, rhs.typ
            ))),
        }
    }

    pub fn sub(self, rhs: InterValue) -> IpretResult<InterValue> {
        match (&self.typ, &rhs.typ) {
            (Typ::Arith(_), Typ::Arith(_)) => {
                let (lhs, rhs) = do_usual_arithmetic_conversion(self, rhs)?;
                assert_eq!(&lhs.typ, &rhs.typ);
                use ATyp::*;
                use FTyp::*;
                use ITyp::*;
                if let Typ::Arith(Float(ftyp)) = lhs.typ {
                    Ok(match ftyp {
                        F32 => coerce!(lhs, -, rhs, f32, ftyp),
                        F64 => coerce!(lhs, -, rhs, f64, ftyp),
                    })
                } else if let Typ::Arith(Int(ityp)) = lhs.typ {
                    Ok(match ityp {
                        I32 => coerce!(lhs, -, rhs, i32, ityp),
                        I64 => coerce!(lhs, -, rhs, i64, ityp),
                        U32 => coerce!(lhs, -, rhs, u32, ityp),
                        U64 => coerce!(lhs, -, rhs, u64, ityp),
                        _ => unreachable!(),
                    })
                } else {
                    unreachable!()
                }
            }
            (Typ::Address(_) | Typ::Array(_, _), Typ::Arith(ATyp::Int(_))) => {
                do_pointer_arithmetic(self, rhs, true)
            }
            _ => Err(MiscOwned(format!(
                "Invalid type for {} - {}",
                self.typ, rhs.typ
            ))),
        }
    }

    pub fn mul(self, rhs: InterValue) -> IpretResult<InterValue> {
        match (&self.typ, &rhs.typ) {
            (Typ::Arith(_), Typ::Arith(_)) => {
                let (lhs, rhs) = do_usual_arithmetic_conversion(self, rhs)?;
                assert_eq!(&lhs.typ, &rhs.typ);
                use ATyp::*;
                use FTyp::*;
                use ITyp::*;
                if let Typ::Arith(Float(ftyp)) = lhs.typ {
                    Ok(match ftyp {
                        F32 => coerce!(lhs, *, rhs, f32, ftyp),
                        F64 => coerce!(lhs, *, rhs, f64, ftyp),
                    })
                } else if let Typ::Arith(Int(ityp)) = lhs.typ {
                    Ok(match ityp {
                        I32 => coerce!(lhs, *, rhs, i32, ityp),
                        I64 => coerce!(lhs, *, rhs, i64, ityp),
                        U32 => coerce!(lhs, *, rhs, u32, ityp),
                        U64 => coerce!(lhs, *, rhs, u64, ityp),
                        _ => unreachable!(),
                    })
                } else {
                    unreachable!()
                }
            }
            _ => Err(MiscOwned(format!(
                "Invalid type for {} - {}",
                self.typ, rhs.typ
            ))),
        }
    }

    pub fn div(self, rhs: InterValue) -> IpretResult<InterValue> {
        match (&self.typ, &rhs.typ) {
            (Typ::Arith(_), Typ::Arith(_)) => {
                let (lhs, rhs) = do_usual_arithmetic_conversion(self, rhs)?;
                assert_eq!(&lhs.typ, &rhs.typ);
                use ATyp::*;
                use FTyp::*;
                use ITyp::*;
                if let Typ::Arith(Float(ftyp)) = lhs.typ {
                    Ok(match ftyp {
                        F32 => coerce!(lhs, /, rhs, f32, ftyp),
                        F64 => coerce!(lhs, /, rhs, f64, ftyp),
                    })
                } else if let Typ::Arith(Int(ityp)) = lhs.typ {
                    Ok(match ityp {
                        I32 => coerce!(lhs, /, rhs, i32, ityp),
                        I64 => coerce!(lhs, /, rhs, i64, ityp),
                        U32 => coerce!(lhs, /, rhs, u32, ityp),
                        U64 => coerce!(lhs, /, rhs, u64, ityp),
                        _ => unreachable!(),
                    })
                } else {
                    unreachable!()
                }
            }
            _ => Err(MiscOwned(format!(
                "Invalid type for {} - {}",
                self.typ, rhs.typ
            ))),
        }
    }

    pub fn lt(self, rhs: InterValue) -> IpretResult<InterValue> {
        define_cmp!(self, <, rhs)
    }

    pub fn le(self, rhs: InterValue) -> IpretResult<InterValue> {
        define_cmp!(self, <=, rhs)
    }

    pub fn gt(self, rhs: InterValue) -> IpretResult<InterValue> {
        define_cmp!(self, >, rhs)
    }

    pub fn ge(self, rhs: InterValue) -> IpretResult<InterValue> {
        define_cmp!(self, >=, rhs)
    }

    pub fn eq(self, rhs: InterValue) -> IpretResult<InterValue> {
        define_cmp!(self, ==, rhs)
    }

    pub fn ne(self, rhs: InterValue) -> IpretResult<InterValue> {
        define_cmp!(self, !=, rhs)
    }
}
