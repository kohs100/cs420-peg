use std::fmt::Display;

fn print_opt_expr(f: &mut std::fmt::Formatter<'_>, stmt: &Option<Box<Expr>>) -> std::fmt::Result {
    if let Some(i) = stmt {
        write!(f, "{};\n", i)
    } else {
        write!(f, ";")
    }
}

pub fn print_commasep<T: Display>(
    f: &mut std::fmt::Formatter<'_>,
    arr: &Vec<T>,
) -> std::fmt::Result {
    let mut it = arr.iter().peekable();
    while let Some(i) = it.next() {
        if it.peek().is_none() {
            write!(f, "{}", i)?;
        } else {
            write!(f, "{}, ", i)?;
        }
    }
    write!(f, "")
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssTyp {
    Non,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    BitAnd,
    BitOr,
    BitXor,
}
impl Display for AssTyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Non => "=",
                Self::Mul => "*=",
                Self::Div => "/=",
                Self::Mod => "%=",
                Self::Add => "+=",
                Self::Sub => "-=",
                Self::BitAnd => "&=",
                Self::BitOr => "|=",
                Self::BitXor => "^=",
            }
        )
    }
}
impl AssTyp {
    pub fn to_binop(&self) -> Option<BinOp> {
        match self {
            Self::Non => None,
            Self::Mul => Some(BinOp::Mul),
            Self::Div => Some(BinOp::Div),
            Self::Mod => Some(BinOp::Mod),
            Self::Add => Some(BinOp::Add),
            Self::Sub => Some(BinOp::Sub),
            Self::BitAnd => Some(BinOp::BitAnd),
            Self::BitOr => Some(BinOp::BitOr),
            Self::BitXor => Some(BinOp::BitXor),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LeftUnaryOp {
    Inc,
    Dec,
    Plus,
    Minus,
    BoolNot,
    BitNot,
    Deref,
    Ref,
}
impl Display for LeftUnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Inc => "++",
                Self::Dec => "--",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::BoolNot => "!",
                Self::BitNot => "~",
                Self::Deref => "*",
                Self::Ref => "&",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    ShftL,
    ShftR,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,
}
impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Mul => "+",
                Self::Div => "-",
                Self::Mod => "%",
                Self::Add => "+",
                Self::Sub => "-",
                Self::ShftL => "<<",
                Self::ShftR => ">>",
                Self::Lt => "<",
                Self::Gt => ">",
                Self::Le => "<=",
                Self::Ge => ">=",
                Self::Eq => "==",
                Self::Ne => "!=",
                Self::BitAnd => "&",
                Self::BitXor => "^",
                Self::BitOr => "|",
                Self::BoolAnd => "&&",
                Self::BoolOr => "||",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    UnaryIncRight(Box<Expr>),
    UnaryDecRight(Box<Expr>),
    Call(Box<Expr>, Vec<Box<Expr>>),
    Index(Box<Expr>, Box<Expr>),

    LUnop(LeftUnaryOp, Box<Expr>),
    Binop(Box<Expr>, BinOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

    Assign(Box<Expr>, AssTyp, Box<Expr>),

    Comma(Box<Expr>, Box<Expr>),

    Ident(String),
    ConstBool(bool),
    Const(u64),
    ConstFloat(String),
    StringLiteral(String),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnaryIncRight(inner) => write!(f, "{}++", inner),
            Self::UnaryDecRight(inner) => write!(f, "{}--", inner),
            Self::Call(func, params) => {
                write!(f, "{}(", func)?;
                print_commasep(f, params)?;
                write!(f, ")")
            }
            Self::Index(arr, idx) => {
                write!(f, "{}[{}]", arr, idx)
            }
            Self::LUnop(op, inner) => {
                write!(f, "{}{}", op, inner)
            }
            Self::Binop(lhs, op, rhs) => {
                write!(f, "{} {} {}", lhs, op, rhs)
            }
            Self::Ternary(cond, tb, fb) => {
                write!(f, "{} ? {} : {}", cond, tb, fb)
            }
            Self::Assign(lhs, asop, rhs) => {
                write!(f, "{} {} {}", lhs, asop, rhs)
            }
            Self::Comma(lhs, rhs) => {
                write!(f, "{}, {}", lhs, rhs)
            }
            Self::Ident(inner) => {
                write!(f, "{}", inner)
            }
            Self::Const(inner) => {
                write!(f, "{}", inner)
            }
            Self::ConstFloat(inner) => {
                write!(f, "{}", inner)
            }
            Self::ConstBool(inner) => {
                write!(f, "{}", inner)
            }
            Self::StringLiteral(lit) => {
                write!(f, "\"{}\"", lit)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompoundStmt(pub Vec<Declaration>, pub Vec<Box<Stmt>>);

impl Display for CompoundStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CompoundStmt(decls, stmts) = self;
        if decls.is_empty() && stmts.is_empty() {
            write!(f, "{{}}")
        } else {
            write!(f, "{{\n")?;

            for stmt in decls {
                write!(f, "{}", stmt)?;
            }
            for stmt in stmts {
                write!(f, "{}", stmt)?;
            }

            write!(f, "}}")
        }
    }
}

impl From<CompoundStmt> for Stmt {
    fn from(value: CompoundStmt) -> Self {
        Self::CompoundStmt(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    CompoundStmt(CompoundStmt),
    ExprStmt(Option<Box<Expr>>),
    If(Box<Expr>, Box<Stmt>),
    IfElse(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    Continue,
    Break,
    Return(Option<Box<Expr>>),
    For(
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Box<Stmt>,
    ),
}
impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CompoundStmt(inner) => write!(f, "{}", inner),
            Self::ExprStmt(inner) => print_opt_expr(f, inner),
            Self::If(cond, b) => {
                write!(f, "if({}){}", cond, b)
            }
            Self::IfElse(cond, tb, fb) => {
                write!(f, "if({}) {} else {}", cond, tb, fb)
            }
            Self::While(cond, b) => {
                write!(f, "while({}) {}", cond, b)
            }
            Self::DoWhile(b, cond) => {
                write!(f, "do {} while({})", b, cond)
            }
            Self::Continue => write!(f, "continue;\n"),
            Self::Break => write!(f, "break;\n"),
            Self::Return(Some(i)) => write!(f, "return {};\n", i),
            Self::Return(None) => write!(f, "return;\n"),
            Self::For(i, c, r, b) => {
                write!(f, "for(")?;
                if let Some(inner) = i {
                    write!(f, "{}", inner)?;
                }
                write!(f, ";")?;
                if let Some(inner) = c {
                    write!(f, "{}", inner)?;
                }
                write!(f, ";")?;
                if let Some(inner) = r {
                    write!(f, "{}", inner)?;
                }
                write!(f, ") {}", b)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrimType {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
}
impl Display for PrimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Void => "void",
                Self::Char => "char",
                Self::Short => "short",
                Self::Int => "int",
                Self::Long => "long",
                Self::Float => "float",
                Self::Double => "double",
                Self::Signed => "signed",
                Self::Unsigned => "unsigned",
                Self::Bool => "bool",
            }
        )
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamDeclaration(pub PrimType, pub Declarator);
impl Display for ParamDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declaration(pub PrimType, pub Vec<Declarator>);
impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.0)?;
        print_commasep(f, &self.1)?;
        write!(f, ";")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declarator {
    NoInit(u64, DirectDeclarator),
    Init(u64, DirectDeclarator, Box<Initializer>),
}
impl Display for Declarator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoInit(indir, dd) => {
                for _ in 0..*indir {
                    write!(f, "*")?;
                }
                write!(f, "{}", dd)
            }
            Self::Init(indir, dd, init) => {
                for _ in 0..*indir {
                    write!(f, "*")?;
                }
                write!(f, "{} = {}", dd, init)
            }
        }
    }
}
impl Declarator {
    pub fn name<'a>(&'a self) -> &'a String {
        match self {
            Self::NoInit(_, dd) => dd.name(),
            Self::Init(_, dd, _) => dd.name(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Initializer {
    Scala(Box<Expr>),
    Array(Vec<Box<Initializer>>),
}
impl Display for Initializer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scala(inner) => {
                write!(f, "{}", inner)
            }
            Self::Array(inners) => {
                write!(f, "{{")?;
                print_commasep(f, inners)?;
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectDeclarator {
    Scala(String),
    Array(String, Vec<u64>),
    Func(String, Vec<ParamDeclaration>),
}
impl Display for DirectDeclarator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scala(nm) => {
                write!(f, "{}", nm)
            }
            Self::Array(nm, szs) => {
                write!(f, "{}", nm)?;
                for sz in szs {
                    write!(f, "[{}]", sz)?;
                }
                write!(f, "")
            }
            Self::Func(nm, dcs) => {
                write!(f, "{}(", nm)?;
                print_commasep(f, dcs)?;
                write!(f, ")")
            }
        }
    }
}
impl DirectDeclarator {
    fn name<'a>(&'a self) -> &'a String {
        match self {
            Self::Scala(nm) => nm,
            Self::Array(nm, _) => nm,
            Self::Func(nm, _) => nm,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function(pub PrimType, pub Declarator, pub CompoundStmt);
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.0, self.1, self.2)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TranslationUnit {
    Glob(Declaration),
    Func(Function),
}
impl Display for TranslationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Glob(inner) => {
                write!(f, "{}", inner)
            }
            Self::Func(inner) => {
                write!(f, "{}\n", inner)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source(pub Vec<TranslationUnit>);

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for tu in self.0.iter() {
            write!(f, "{}\n", tu)?;
        }
        Ok(())
    }
}
