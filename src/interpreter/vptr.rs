use std::ops;

use super::defs::{ATyp, Typ};

// 64-bit address width as byte
pub const ADDR_WIDTH: usize = size_of::<AddrType>(); // Must be 8
pub type AddrType = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtualPointer(AddrType);

impl PartialOrd for VirtualPointer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for VirtualPointer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl Into<AddrType> for VirtualPointer {
    fn into(self) -> AddrType {
        self.0
    }
}

impl TryInto<usize> for VirtualPointer {
    type Error = <AddrType as TryInto<usize>>::Error;
    fn try_into(self) -> Result<usize, Self::Error> {
        self.0.try_into()
    }
}

impl ops::Sub<VirtualPointer> for VirtualPointer {
    type Output = AddrType;
    fn sub(self, rhs: VirtualPointer) -> Self::Output {
        let Self(lptr) = self;
        let Self(rptr) = rhs;
        assert!(lptr >= rptr);
        lptr - rptr
    }
}

impl ops::Sub<AddrType> for VirtualPointer {
    type Output = VirtualPointer;
    fn sub(self, rhs: AddrType) -> Self::Output {
        let Self(lptr) = self;
        assert!(lptr >= rhs);
        VirtualPointer(lptr - rhs)
    }
}

impl ops::Add<AddrType> for VirtualPointer {
    type Output = VirtualPointer;
    fn add(self, rhs: AddrType) -> Self::Output {
        let Self(lptr) = self;
        VirtualPointer(lptr + rhs)
    }
}

impl ops::AddAssign<AddrType> for VirtualPointer {
    fn add_assign(&mut self, rhs: AddrType) {
        *self = *self + rhs;
    }
}

impl VirtualPointer {
    pub const fn with(addr: AddrType) -> Self {
        Self(addr)
    }
}

impl Typ {
    pub fn size_bytes(&self) -> usize {
        use ATyp::*;
        use Typ::*;
        match self {
            Address(_) => ADDR_WIDTH.try_into().unwrap(),
            Arith(Bool) => 1,
            Arith(Float(ftyp)) => ftyp.width_bytes(),
            Arith(Int(ityp)) => ityp.width_bytes(),
            Array(inner, sz) => *sz * inner.size_bytes(),
            Void => 0,
            FuncDecl(_, _) => ADDR_WIDTH,
        }
    }

    pub fn size_vptr_delta(&self) -> AddrType {
        self.size_bytes().try_into().unwrap()
    }
}
