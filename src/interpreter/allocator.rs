use std::collections::HashMap;

use bitvec::vec::BitVec;

use super::vptr::{AddrType, VirtualPointer};

pub trait VirtualMemory {
    fn get_bytes(&self, ofs: usize, sz: usize) -> &[u8];
    fn get_bytes_mut(&mut self, ofs: usize, sz: usize) -> &mut [u8];

    fn get_slice<T>(&self, ofs: VirtualPointer, unit: usize, len: usize) -> &[T] {
        assert_eq!(size_of::<T>(), unit);

        let unit: usize = unit.try_into().unwrap();
        let slice = self.get_bytes(ofs.try_into().unwrap(), unit * len);
        unsafe {
            let ptr = slice as *const [u8] as *const T;
            return std::slice::from_raw_parts(ptr, len);
        }
    }

    fn get_slice_mut<T>(&mut self, ofs: VirtualPointer, unit: usize, len: usize) -> &mut [T] {
        assert_eq!(size_of::<T>(), unit);

        let unit: usize = unit.try_into().unwrap();
        let slice = self.get_bytes_mut(ofs.try_into().unwrap(), unit * len);
        unsafe {
            let ptr = slice as *mut [u8] as *mut T;
            return std::slice::from_raw_parts_mut(ptr, len);
        }
    }
}

pub trait Allocator {
    fn new() -> Self;
    fn alloc_bytes(&mut self, sz: usize) -> VirtualPointer;
    fn free(&mut self, ofs: VirtualPointer);
}

// -----------
type Page = [u8; 4096];

#[derive(Debug, Clone)]
pub struct LinearAllocator {
    freemap: HashMap<VirtualPointer, AddrType>,
    pool: Vec<Page>,
    bitmap: BitVec,
}

impl LinearAllocator {
    fn check_len(&self) -> usize {
        let cur_num_pg = self.pool.len();

        assert_eq!(0, self.bitmap.len() % 4096);
        assert_eq!(cur_num_pg, self.bitmap.len() / 4096);

        return cur_num_pg;
    }
    fn extend(&mut self) -> usize {
        let nxt_num_pg = self.check_len() + 1;

        self.pool.resize(nxt_num_pg, [0; 4096]);
        self.bitmap.resize(nxt_num_pg * 4096, false);

        self.check_len()
    }
}

impl Allocator for LinearAllocator {
    fn new() -> Self {
        Self {
            freemap: HashMap::new(),
            pool: Vec::new(),
            bitmap: BitVec::new(),
        }
    }

    fn alloc_bytes(&mut self, total_sz: usize) -> VirtualPointer {
        let mut cnt = 0;
        let mut found: Option<usize> = None;
        for (ofs, b) in self.bitmap.iter().enumerate() {
            if *b {
                // Filled
                cnt = 0;
                found = None;
            } else {
                // Empty
                if found.is_none() {
                    found = Some(ofs)
                }
                cnt += 1;
                if cnt >= total_sz {
                    break;
                }
            }
        }
        if let Some(ofs) = found {
            let bor = &mut self.bitmap[ofs..ofs + total_sz];
            bor.fill(true);
            let res = VirtualPointer::with(ofs.try_into().unwrap());
            self.freemap.insert(res, total_sz.try_into().unwrap());
            res
        } else {
            let _ = self.extend();
            self.alloc_bytes(total_sz)
        }
    }

    fn free(&mut self, ofs: VirtualPointer) {
        let sz = self
            .freemap
            .remove(&ofs)
            .expect("Free of Pointer not at Start of Buffer");

        let begin: usize = {
            let a: u64 = ofs.into();
            a.try_into().unwrap()
        };
        let end: usize = {
            let a: u64 = (ofs + sz).into();
            a.try_into().unwrap()
        };

        let bor = &mut self.bitmap[begin..end];
        bor.fill(false);
    }
}

impl VirtualMemory for LinearAllocator {
    fn get_bytes(&self, ofs: usize, sz: usize) -> &[u8] {
        unsafe {
            let ptr = self.pool.as_ptr() as *const [u8];
            let pref = &*ptr;
            return &pref[ofs..ofs + sz];
        }
    }
    fn get_bytes_mut(&mut self, ofs: usize, sz: usize) -> &mut [u8] {
        unsafe {
            let ptr = self.pool.as_mut_ptr() as *mut [u8];
            let pref = &mut *ptr;
            return &mut pref[ofs..ofs + sz];
        }
    }
}
