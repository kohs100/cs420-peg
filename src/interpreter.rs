use crate::ast::*;
use crate::heap::*;
use crate::typecheck::*;

use std::fmt::Debug;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug)]
pub enum Value<'a> {
    Address(usize),
    Char(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Signed(i32),
    Unsigned(u32),
    Float(f32),
    Double(f64),
    Function(Vec<String>, &'a Stmt),
}

type RunResult<T> = Result<T, RunError>;

#[derive(Debug, Clone)]
pub enum RunError {
    InvalidIdentifier(String),
    NotInitialized(String),
    Redefinition(String),
    NoValidMain(String),
}
impl Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidIdentifier(i) => {
                write!(f, "Invalid identifier: {}", i)
            }
            Self::NotInitialized(i) => {
                write!(f, "Using uninitizlied identifier: {}", i)
            }
            Self::Redefinition(i) => {
                write!(f, "Invalid redefinition of identifier: {}", i)
            }
            Self::NoValidMain(i) => {
                write!(f, "Invalid entry point: {}", i)
            }
        }
    }
}

struct Environment<'a> {
    memory: Box<dyn Allocator>,
    symtbl: Vec<HashMap<String, Option<Value<'a>>>>,
    heap: Vec<Buffer<'a>>,
}
impl<'a> Debug for Environment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Environment: {:?}", self.symtbl)
    }
}

#[derive(Debug)]
struct ScopeGuard<'a> {
    env: Environment<'a>,
    scope_level: usize,
}

impl<'a> Environment<'a> {
    fn new() -> Self {
        Self {
            memory: Box::new(LinearAllocator::new()),
            symtbl: vec![HashMap::new()],
            heap: Vec::new(),
        }
    }

    fn get_root_scope(self) -> ScopeGuard<'a> {
        ScopeGuard {
            env: self,
            scope_level: 0,
        }
    }
}

impl<'a> ScopeGuard<'a> {
    fn get_value(&self, k: &str) -> RunResult<&Value> {
        let max_lv = self.scope_level;
        for i in 0..(max_lv + 1) {
            let cur_lv = max_lv - i;
            if let Some(i) = self.env.symtbl[cur_lv].get(k) {
                return i.or();
            }
        }
        return Err(RunError::InvalidIdentifier(k.to_owned()));
    }

    fn set_value(&mut self, k: &str, v: Value) -> RunResult<()> {
        if let Some(a) = self.env.symtbl[self.scope_level].insert(k.to_owned(), v) {
            return Err(RunError::Redefinition(k.to_owned()));
        }
        Ok(())
    }

    fn get_next_scope(mut self) -> ScopeGuard<'a> {
        self.env.symtbl.push(HashMap::new());
        ScopeGuard {
            env: self.env,
            scope_level: self.scope_level + 1,
        }
    }

    fn get_prev_scope(mut self) -> Result<ScopeGuard<'a>, Environment<'a>> {
        if self.scope_level == 0 {
            return Err(self.env);
        } else {
            let _last_symtbl = self.env.symtbl.pop().unwrap();
            return Ok(ScopeGuard {
                env: self.env,
                scope_level: self.scope_level - 1,
            });
        }
    }
}

pub type Env = HashMap<String, Option<Value>>;

pub type Program = HashMap<String, Value>;

fn interp_stmt<'a>(stmt: &Stmt, env: Environment<'a>) -> RunResult<Environment<'a>> {
    Ok(Environment::new())
}

fn interp_tu<'a>(tu: &TranslationUnit, env: Environment<'a>) -> RunResult<Environment<'a>> {
    let mut sg = env.get_root_scope();
    match tu {
        TranslationUnit::Glob(Declaration(typ, decls)) => {
            for decl in decls {
                let nm = decl.name();
                let typ = Typ::from_decl(typ, decl);
                sg.set_value(nm, None)?;
            }
        }
        TranslationUnit::Func(Function(ptype, decl, cstmt)) => {
            let nm = decl.name();
            let rettyp = Typ::from_decl(ptype, decl);
        }
    };
    Ok(sg.get_prev_scope().expect_err("asdf"))
}

pub fn run(tus: &Vec<TranslationUnit>) -> RunResult<()> {
    let env = tus
        .iter()
        .try_fold(Environment::new(), |env, tu| interp_tu(tu, env))?;

    let main = env
        .get("main")
        .ok_or(RunError::NoValidMain("No main".to_string()))?
        .as_ref()
        .ok_or(RunError::NoValidMain("Uninitialized main".to_string()))?;

    let mut alloc = LinearAllocator::new();

    let buf = alloc.alloc::<usize>(4);
    let a = buf.as_slice(4);

    if let Value::Function(_, _, body) = main {
        let final_env = interp_stmt(&body, env)?;
        Ok(())
    } else {
        Err(RunError::NoValidMain("Ill typed main".to_string()))
    }
}
