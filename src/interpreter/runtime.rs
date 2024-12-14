use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use super::allocator::*;
use super::defs::{ITyp, Typ};
use super::vptr::*;
use crate::ast::expr::Function;

use super::value::*;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ReadOnly<T>(T);
impl<T> ReadOnly<T> {
    pub fn new(inner: T) -> Self {
        Self(inner)
    }
}

impl<T: Copy> ReadOnly<T> {
    pub fn get(&self) -> T {
        self.0
    }
}

impl<T> Deref for ReadOnly<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct PreRuntime<T = LinearAllocator>
where
    T: Allocator + VirtualMemory,
{
    glob: T,
    max_ptr: VirtualPointer,
    symtbl: HashMap<String, MemObj>,
    functbl: HashMap<String, Function>,
}

impl<T> PreRuntime<T>
where
    T: Allocator + VirtualMemory,
{
    pub fn new() -> Self {
        PreRuntime {
            glob: T::new(),
            max_ptr: VirtualPointer::with(0),
            symtbl: HashMap::new(),
            functbl: HashMap::new(),
        }
    }

    pub fn alloc_glob(&mut self, name: &str, typ: &Typ) {
        if let Typ::FuncDecl(_, _) = typ {
            // Do nothing
            return;
        }
        let tsz = typ.size_bytes();
        let res = self.glob.alloc_bytes(typ.size_bytes());
        let new_max = res + tsz.try_into().unwrap();
        if self.max_ptr < new_max {
            self.max_ptr = new_max;
        }
        if self
            .symtbl
            .insert(name.to_owned(), MemObj::new(typ.clone(), res))
            .is_some()
        {
            panic!("Redeclaration in global scope");
        }
    }

    pub fn alloc_func(&mut self, name: &str, func: &Function) {
        if self.functbl.insert(name.to_owned(), func.clone()).is_some() {
            panic!("Redeclaration in function namespace");
        }
    }

    pub fn lookup(&self, id: &str) -> Option<MemObj> {
        self.symtbl.get(id).cloned()
    }
}

impl<T> VirtualMemory for PreRuntime<T>
where
    T: Allocator + VirtualMemory,
{
    fn get_bytes(&self, ofs: usize, sz: usize) -> &[u8] {
        self.glob.get_bytes(ofs, sz)
    }
    fn get_bytes_mut(&mut self, ofs: usize, sz: usize) -> &mut [u8] {
        self.glob.get_bytes_mut(ofs, sz)
    }
}

// |<--- Glob --->|<------ Stack ------>|<------ Heap ------>|
// |0             |OFS_STK              |OFS_HEAP            |
// |       SZ_GLOB|       OFS_STK+SZ_STK|    OFS_HEAP+SZ_HEAP|
#[derive(Debug)]
pub struct Runtime<T>
where
    T: Allocator + VirtualMemory,
{
    glob: T,
    heap: T,
    stack: T,
    ofs_stk: ReadOnly<AddrType>,
    ofs_heap: ReadOnly<AddrType>,
    symtbl: HashMap<String, MemObj>,
    functbl: HashMap<String, Function>,
    strtbl: HashMap<String, VirtualPointer>,
}

impl<T> Runtime<T>
where
    T: Allocator + VirtualMemory,
{
    pub fn new(prt: PreRuntime<T>, max_stk: AddrType) -> Runtime<T> {
        let ofs_stk: AddrType = prt.max_ptr.into();
        Runtime {
            glob: prt.glob,
            heap: T::new(),
            stack: T::new(),
            ofs_stk: ReadOnly::new(ofs_stk),
            ofs_heap: ReadOnly::new(ofs_stk + max_stk),
            symtbl: prt.symtbl,
            functbl: prt.functbl,
            strtbl: HashMap::new(),
        }
    }

    fn alloc_stk(&mut self, typ: &Typ) -> VirtualPointer {
        let raw_ptr = self.stack.alloc_bytes(typ.size_bytes());
        let ofs_ptr = raw_ptr + self.ofs_stk.get();
        if self.ofs_heap.get() < ofs_ptr.into() {
            panic!("Stack size exceeded");
        }
        ofs_ptr
    }

    fn free_stk(&mut self, ofs_ptr: VirtualPointer) {
        assert!(
            self.ofs_heap.get() > ofs_ptr.into(),
            "Cannot free heap region with free_stk"
        );
        assert!(
            self.ofs_stk.get() <= ofs_ptr.into(),
            "Cannot free global region"
        );
        self.stack.free(ofs_ptr - self.ofs_stk.get())
    }

    pub fn lookup(&self, name: &str) -> Option<MemObj> {
        self.symtbl.get(name).map(|x| x.clone())
    }

    pub fn lookup_func(&self, name: &str) -> Option<&Function> {
        self.functbl.get(name)
    }

    pub fn alloc_heap(&mut self, bytes: AddrType) -> VirtualPointer {
        self.heap.alloc_bytes(bytes.try_into().unwrap()) + self.ofs_heap.get()
    }

    pub fn free_heap(&mut self, ofs_ptr: VirtualPointer) {
        assert!(
            self.ofs_heap.get() <= ofs_ptr.into(),
            "Cannot free stack/global region with free_heap"
        );
        self.heap.free(ofs_ptr - self.ofs_heap.get())
    }

    pub fn string_literal(&mut self, literal: &str) -> InterValue {
        let addr: AddrType = if let Some(already_exists) = self.strtbl.get(literal) {
            already_exists.clone().into()
        } else {
            let bytes = literal.as_bytes();
            let nt_str_len: usize = bytes.len() + 1; // Null-terminated string

            let vptr = self.alloc_heap(nt_str_len.try_into().unwrap());
            let addr: AddrType = vptr.into();
            let addr_val: usize = addr.try_into().unwrap();

            let memloc = self.get_bytes_mut(addr_val, bytes.len());

            assert_eq!(memloc.len(), nt_str_len);

            memloc.copy_from_slice(bytes);

            if self.strtbl.insert(literal.to_owned(), vptr).is_some() {
                unreachable!()
            }

            addr
        };
        InterValue::from_type(Typ::Address(Box::new(ITyp::I8.into())), addr)
    }
}

impl<T> VirtualMemory for Runtime<T>
where
    T: Allocator + VirtualMemory,
{
    fn get_bytes(&self, ofs: usize, sz: usize) -> &[u8] {
        let ofs_stk: usize = self.ofs_stk.get().try_into().unwrap();
        let ofs_heap: usize = self.ofs_heap.get().try_into().unwrap();

        if ofs < ofs_stk {
            self.glob.get_bytes(ofs, sz)
        } else if ofs < ofs_heap {
            self.stack.get_bytes(ofs - ofs_stk, sz)
        } else {
            self.heap.get_bytes(ofs - ofs_heap, sz)
        }
    }
    fn get_bytes_mut(&mut self, ofs: usize, sz: usize) -> &mut [u8] {
        let ofs_stk: usize = self.ofs_stk.get().try_into().unwrap();
        let ofs_heap: usize = self.ofs_heap.get().try_into().unwrap();

        if ofs < ofs_stk {
            self.glob.get_bytes_mut(ofs, sz)
        } else if ofs < ofs_heap {
            self.stack.get_bytes_mut(ofs - ofs_stk, sz)
        } else {
            self.heap.get_bytes_mut(ofs - ofs_heap, sz)
        }
    }
}

pub struct StackGuard<'a, T>
where
    T: Allocator + VirtualMemory,
{
    prt: Option<PreRuntime<T>>,
    rt: Option<Rc<RefCell<Runtime<T>>>>,
    parent: Option<&'a StackGuard<'a, T>>,
    stack_allocated: Vec<VirtualPointer>,
    symtbl: HashMap<String, MemObj>,
}

impl<'a, T> StackGuard<'a, T>
where
    T: Allocator + VirtualMemory,
{
    pub fn grow_stk(&mut self, name: &str, typ: Typ) {
        // println!("Allocating stack for variable name: ({}) {}", typ, name);
        if self.prt.is_some() {
            panic!("Cannot allocate stack in preruntime phase!")
        }

        let res = self
            .rt
            .as_ref()
            .expect("No runtime exists in stackguard!!")
            .borrow_mut()
            .alloc_stk(&typ);
        // println!("Allocated: {:?}", res);

        self.stack_allocated.push(res);

        if self
            .symtbl
            .insert(name.to_owned(), MemObj::new(typ, res))
            .is_some()
        {
            panic!("Redeclaration in same stack frame");
        }
    }

    pub fn from_preruntime(prt: PreRuntime<T>) -> Self {
        Self {
            prt: Some(prt),
            rt: None,
            parent: None,
            stack_allocated: Vec::new(),
            symtbl: HashMap::new(),
        }
    }

    pub fn from_runtime(rt: Rc<RefCell<Runtime<T>>>) -> Self {
        Self {
            prt: None,
            rt: Some(rt),
            parent: None,
            stack_allocated: Vec::new(),
            symtbl: HashMap::new(),
        }
    }

    pub fn scope_in(&'a self) -> StackGuard<'a, T> {
        if self.prt.is_some() {
            panic!("Cannot allocate stack in preruntime phase!")
        }

        let rt = self
            .rt
            .as_ref()
            .expect("Cannot scope in with preruntime.")
            .clone();
        Self {
            prt: None,
            rt: Some(rt),
            parent: Some(self),
            stack_allocated: Vec::new(),
            symtbl: HashMap::new(),
        }
    }

    pub fn lookup(&self, id: &str) -> Option<MemObj> {
        if let Some(prt) = self.prt.as_ref() {
            if self.rt.is_some() {
                panic!("Preruntime and runtime co-exists in same stackguard!!");
            }
            prt.lookup(id)
        } else if let Some(rt) = self.rt.as_ref() {
            if let Some(got) = self.symtbl.get(id) {
                // in this scope
                Some(got.clone())
            } else if let Some(parent) = self.parent {
                // in outer scope
                parent.lookup(id)
            } else {
                // in global scope
                rt.borrow().lookup(id)
            }
        } else {
            panic!("No runtime in stackguard!!")
        }
    }

    pub fn lookup_func(&self, id: &str) -> Option<Function> {
        if let Some(_) = self.prt.as_ref() {
            panic!("Function lookup in preruntime is not allowed!!");
        } else if let Some(rt) = self.rt.as_ref() {
            rt.borrow().lookup_func(id).cloned()
        } else {
            panic!("No runtime found in scopeguard!!");
        }
    }

    pub fn extract_prt(&mut self) -> PreRuntime<T> {
        if self.rt.is_some() {
            panic!("Cannot extract preruntime from runtime stackguard.");
        }
        self.prt.take().expect("Preruntime could not be extracted.")
    }

    pub fn get_runtime(&self) -> Rc<RefCell<Runtime<T>>> {
        self.rt
            .as_ref()
            .expect("Cannot get runtime with preruntime stackguard")
            .clone()
    }
}

impl<'a, T> Drop for StackGuard<'a, T>
where
    T: Allocator + VirtualMemory,
{
    fn drop(&mut self) {
        if let Some(rt) = &mut self.rt {
            // println!("Dropping sg with {} vars", self.stack_allocated.len());
            for ptr in self.stack_allocated.iter() {
                rt.borrow_mut().free_stk(*ptr)
            }
        }
    }
}

impl<'a, T> VirtualMemory for StackGuard<'a, T>
where
    T: Allocator + VirtualMemory,
{
    fn get_bytes<'b>(&'b self, ofs: usize, sz: usize) -> &'b [u8] {
        let ptr = if let Some(rt) = self.rt.as_ref() {
            rt.borrow().get_bytes(ofs, sz).as_ptr()
        } else if let Some(prt) = self.prt.as_ref() {
            prt.get_bytes(ofs, sz).as_ptr()
        } else {
            panic!("No runtime exists in stackguard!!")
        };

        unsafe { std::slice::from_raw_parts(ptr, sz) }
    }

    fn get_bytes_mut<'b>(&'b mut self, ofs: usize, sz: usize) -> &'b mut [u8] {
        let ptr = if let Some(rt) = self.rt.as_mut() {
            rt.borrow_mut().get_bytes_mut(ofs, sz).as_mut_ptr()
        } else if let Some(prt) = self.prt.as_mut() {
            prt.get_bytes_mut(ofs, sz).as_mut_ptr()
        } else {
            panic!("No runtime exists in stackguard!!")
        };

        unsafe { std::slice::from_raw_parts_mut(ptr, sz) }
    }
}
