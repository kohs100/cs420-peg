use bitvec::vec::BitVec;

pub struct Buffer<'a> {
    pool: &'a mut dyn Allocator,
    ofs: usize,
    sz: usize,
}

impl<'a> Drop for Buffer<'a> {
    fn drop(&mut self) {
        self.pool.free(self.ofs, self.sz);
    }
}

impl<'b> Buffer<'b> {
    pub fn as_slice<'a, T>(&'a self, len: usize) -> &'a [T] {
        assert_eq!(size_of::<T>() * len, self.sz);

        let slice = self.pool.get_slice(self.ofs, self.sz);
        unsafe {
            let ptr = slice as *const [u8] as *const T;
            return std::slice::from_raw_parts(ptr, len);
        }
    }

    pub fn as_slice_mut<'a, T>(&'a mut self, len: usize) -> &'a mut [T] {
        assert_eq!(size_of::<T>() * len, self.sz);

        let slice = self.pool.get_slice_mut(self.ofs, self.sz);
        unsafe {
            let ptr = slice as *mut [u8] as *mut T;
            return std::slice::from_raw_parts_mut(ptr, len);
        }
    }

    pub fn get_addr(&self) -> usize {
        self.ofs
    }
}

pub trait AllocCreater {
    fn alloc<'a, T>(&'a mut self, sz: usize) -> Buffer<'a>;
    fn alloc_slice<'a, T>(&'a mut self, len: usize) -> Buffer<'a>;
}

pub trait Allocator {
    fn free(&mut self, ofs: usize, sz: usize);

    fn get_slice(&self, ofs: usize, sz: usize) -> &[u8];
    fn get_slice_mut(&mut self, ofs: usize, sz: usize) -> &mut [u8];
}

// -----------
type Page = [u8; 4096];

#[derive(Debug)]
pub struct LinearAllocator {
    pool: Vec<Page>,
    bitmap: BitVec,
}

impl LinearAllocator {
    pub fn new() -> Self {
        Self {
            pool: Vec::new(),
            bitmap: BitVec::new(),
        }
    }

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

impl AllocCreater for LinearAllocator {
    fn alloc_slice<'a, T>(&'a mut self, len: usize) -> Buffer<'a> {
        let sz = size_of::<T>() * len;

        let mut cnt = 0;
        let mut begin: Option<usize> = None;
        for (i, b) in self.bitmap.iter().enumerate() {
            if *b {
                cnt = 0;
                begin = Some(i);
            } else {
                cnt += 1;
                if cnt >= sz {
                    break;
                }
            }
        }
        if let Some(ofs) = begin {
            let bor = &mut self.bitmap[ofs..ofs + sz];
            bor.fill(true);
            return Buffer {
                pool: self,
                ofs,
                sz,
            };
        } else {
            let _ = self.extend();
            return self.alloc::<T>(sz);
        }
    }

    fn alloc<'a, T>(&'a mut self, sz: usize) -> Buffer<'a> {
        let mut cnt = 0;
        let mut begin: Option<usize> = None;
        for (i, b) in self.bitmap.iter().enumerate() {
            if *b {
                cnt = 0;
                begin = Some(i);
            } else {
                cnt += 1;
                if cnt >= sz {
                    break;
                }
            }
        }
        if let Some(ofs) = begin {
            let bor = &mut self.bitmap[ofs..ofs + sz];
            bor.fill(true);
            return Buffer {
                pool: self,
                ofs,
                sz,
            };
        } else {
            let _ = self.extend();
            return self.alloc::<T>(sz);
        }
    }
}

impl Allocator for LinearAllocator {
    fn free(&mut self, ofs: usize, sz: usize) {
        let bor = &mut self.bitmap[ofs..ofs + sz];
        bor.fill(false);
    }
    fn get_slice(&self, ofs: usize, sz: usize) -> &[u8] {
        unsafe {
            let ptr = self.pool.as_ptr() as *const [u8];
            let pref = &*ptr;
            return &pref[ofs..ofs + sz];
        }
    }
    fn get_slice_mut(&mut self, ofs: usize, sz: usize) -> &mut [u8] {
        unsafe {
            let ptr = self.pool.as_mut_ptr() as *mut [u8];
            let pref = &mut *ptr;
            return &mut pref[ofs..ofs + sz];
        }
    }
}
