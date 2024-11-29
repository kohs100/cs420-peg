use std::{collections::HashMap, fmt::Display};

use crate::types::defs::*;

#[derive(Debug)]
pub struct ScopeGuard<'a> {
    level: usize,
    parent: Option<Box<&'a ScopeGuard<'a>>>,
    rettyp: Option<Typ>,
    symtbl: HashMap<String, Typ>,
    name: String,
    verbose: bool,
}

impl<'a> ScopeGuard<'a> {
    pub fn new(name: &str, verbose: bool) -> Self {
        Self {
            level: 0,
            parent: None,
            rettyp: None,
            symtbl: HashMap::new(),
            verbose,
            name: name.to_owned(),
        }
    }

    pub fn get_type(&self, k: &str) -> Option<Typ> {
        if let Some(found) = self.symtbl.get(k) {
            Some(found.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_type(k)
        } else {
            None
        }
    }

    pub fn declare(&mut self, k: &str, typ: Typ) -> TypeResult<()> {
        if self.symtbl.insert(k.to_owned(), typ).is_some() {
            Err(TypeError::Redefinition(k.to_owned()).into())
        } else {
            Ok(())
        }
    }

    pub fn scope_in<'b>(&'b self, name: &str) -> ScopeGuard<'b> {
        ScopeGuard {
            level: self.level + 1,
            parent: Some(Box::new(self)),
            rettyp: None,
            symtbl: HashMap::new(),
            verbose: self.verbose,
            name: name.to_owned(),
        }
    }

    pub fn set_rettyp(&mut self, rettyp: Typ) {
        assert_eq!(self.level, 1);
        if self.rettyp.replace(rettyp).is_some() {
            panic!("Retval already set!!");
        }
    }

    pub fn get_rettyp(&self) -> &Typ {
        if let Some(ret) = &self.rettyp {
            assert_eq!(self.level, 1);
            ret
        } else if let Some(parent) = &self.parent {
            parent.get_rettyp()
        } else {
            panic!("Not in function context!!")
        }
    }
}

impl<'a> Display for ScopeGuard<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{\n", self.name)?;
        write!(f, "{:#?} }}", self.symtbl)
    }
}

impl<'a> Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        if self.verbose {
            println!("Dropping scope {}", self)
        }
    }
}
