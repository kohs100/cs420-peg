use core::fmt;
use std::fmt::{Display, Write};

pub mod expr;
pub mod types;

pub fn print_commasep<T: Code>(mut cp: CodePrinter, arr: &Vec<T>) -> CodeResult {
    let mut it = arr.iter().peekable();
    while let Some(i) = it.next() {
        cp = i.pretty_fmt(cp)?;
        if it.peek().is_some() {
            write!(cp, ", ")?;
        }
    }
    Ok(cp)
}

pub fn print_boxed_commasep<T: Code>(mut cp: CodePrinter, arr: &Vec<Box<T>>) -> CodeResult {
    let mut it = arr.iter().peekable();
    while let Some(i) = it.next() {
        cp = i.pretty_fmt(cp)?;
        if it.peek().is_some() {
            write!(cp, ", ")?;
        }
    }
    Ok(cp)
}

pub struct CodePrinter {
    indent: usize,
    parent: Option<Box<CodePrinter>>,
    buffer: String,
}

pub type CodeResult = Result<CodePrinter, fmt::Error>;

pub trait Code {
    fn pretty_fmt(&self, cp: CodePrinter) -> CodeResult;
}

impl CodePrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            parent: None,
            buffer: String::new(),
        }
    }
    pub fn indent(mut self) -> CodePrinter {
        self.newline();
        CodePrinter {
            indent: self.indent + 1,
            parent: Some(Box::new(self)),
            buffer: String::new(),
        }
    }
    pub fn unindent(self) -> Option<CodePrinter> {
        self.parent.map(|mut parent| {
            parent.buffer += &self.buffer;
            *parent
        })
    }
    pub fn newline_force(&mut self) {
        self.buffer.push('\n');
    }
    pub fn newline(&mut self) {
        if !self.is_newline() {
            self.newline_force()
        }
    }
    pub fn is_newline(&self) -> bool {
        if let Some(last) = self.buffer.chars().last() {
            last == '\n'
        } else {
            true
        }
    }
}

impl Write for CodePrinter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if self.is_newline() {
            for _ in 0..self.indent {
                self.buffer.push_str("  ");
            }
            self.buffer.push_str(s);
        } else {
            self.buffer.push_str(s);
        }

        Ok(())
    }
}
impl Display for CodePrinter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.buffer)
    }
}
