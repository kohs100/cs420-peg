extern crate peg;

use crate::ast::expr::*;
use crate::ast::types::*;

peg::parser!(pub grammar clang() for str {
    rule number() -> usize = quiet!{
        n:$(['0'..='9']+){ n.parse().unwrap() }
    }

    rule floatdot() -> String = quiet!{
        exp:$(['0'..='9']+) "." frac:$(['0'..='9']*) {
            format!("{}.{}", exp, frac)
        }
    }
    rule dotfloat() -> String = quiet!{
        exp:$(['0'..='9']*) "." frac:$(['0'..='9']+) {
            format!("{}.{}", exp, frac)
        }
    }
    rule bool() -> bool = quiet!{
          "true" { true }
        / "false" { false }
    }

    rule primtype() -> PrimType = quiet!{
          "void" { PrimType::Void }
        / "char"{ PrimType::Char }
        / "short" { PrimType::Short }
        / "int" { PrimType::Int }
        / "long" { PrimType::Long }
        / "float" { PrimType::Float }
        / "double" { PrimType::Double }
        / "signed"{ PrimType::Signed }
        / "unsigned" { PrimType::Unsigned }
        / "bool" { PrimType::Bool}
    }
    rule reserved() = quiet!{
          "if"
        / "else"
        / "do"
        / "while"
        / "for"
        / "return"
        / "continue"
        / "break"
        / "true"
        / "false"
        / primtype()
    }
    rule ident() -> String = quiet!{
        !reserved()
        s:$(['a'..='z' | 'A'..='Z']['0'..='9' | 'a'..='z' | 'A'..='Z']*) {
            s.to_owned()
        }
    }

    rule stringliteral() -> Box<Expr> = quiet!{
        "\"" s:$([^'\"']*) "\"" {
            Box::new(Expr::StringLiteral(s.to_owned()))
        }
    }

    rule _ = quiet!{[' ' | '\r' | '\n' | '\t']*}
    rule ws() = quiet!{[' ' | '\r' | '\n' | '\t']+}

    rule pad<T>(x: rule<T>) -> T = _ v:x() _ {v}

    rule expr_prim() -> Box<Expr> =
        "(" _ v:expr() _ ")" { v }
        / fstr:floatdot() { Box::new(Expr::ConstFloat(fstr)) }
        / fstr:dotfloat() { Box::new(Expr::ConstFloat(fstr)) }
        / n:number() { Box::new(Expr::Const(n)) }
        / b:bool() { Box::new(Expr::ConstBool(b)) }
        / s:ident() { Box::new(Expr::Ident(s)) }
        / s:stringliteral() { s }

    rule expr_postfix() -> Box<Expr> = precedence!{
        v:@ "++" { Box::new(Expr::UnaryIncRight(v)) }
        v:@ "--" { Box::new(Expr::UnaryDecRight(v)) }
        f:@ "(" _ args:pad(<expr_ass()>)**"," _ ")" { Box::new(Expr::Call(f, args)) }
        arr:@ "[" _ idx:expr() _ "]" { Box::new(Expr::Index(arr, idx)) }
        --
        passthrough:expr_prim() { passthrough }
    }

    rule expr_unary() -> Box<Expr> = precedence!{
        "++" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Inc, v)) }
        "--" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Dec, v)) }
        "+" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Plus, v)) }
        "-" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Minus, v)) }
        "!" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::BoolNot, v)) }
        "~" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::BitNot, v)) }
        "*" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Deref, v)) }
        "&" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Ref, v)) }
        --
        passthrough:expr_postfix() { passthrough }
    }

    rule expr_cast() -> Box<Expr> = precedence!{
        "(" _ typ:type_name() _ ")" _ v:@ { Box::new(Expr::Cast(typ, v)) }
        --
        passthrough:expr_unary() { passthrough }
    }

    rule expr_binop() -> Box<Expr> = precedence!{
        x:(@) _ "*" _ y:@ { Box::new(Expr::Binop(x, BinOp::Mul, y)) }
        x:(@) _ "/" _ y:@ { Box::new(Expr::Binop(x, BinOp::Div, y)) }
        x:(@) _ "%" _ y:@ { Box::new(Expr::Binop(x, BinOp::Mod, y)) }
        --
        x:(@) _ "+" _ y:@ { Box::new(Expr::Binop(x, BinOp::Add, y)) }
        x:(@) _ "-" _ y:@ { Box::new(Expr::Binop(x, BinOp::Sub, y)) }
        --
        x:(@) _ "<<" _ y:@ { Box::new(Expr::Binop(x, BinOp::ShftL, y)) }
        x:(@) _ ">>" _ y:@ { Box::new(Expr::Binop(x, BinOp::ShftR, y)) }
        --
        x:(@) _ "<" _ y:@ { Box::new(Expr::Binop(x, BinOp::Lt, y)) }
        x:(@) _ ">" _ y:@ { Box::new(Expr::Binop(x, BinOp::Gt, y)) }
        x:(@) _ "<=" _ y:@ { Box::new(Expr::Binop(x, BinOp::Le, y)) }
        x:(@) _ ">=" _ y:@ { Box::new(Expr::Binop(x, BinOp::Ge, y)) }
        --
        x:(@) _ "==" _ y:@ { Box::new(Expr::Binop(x, BinOp::Eq, y)) }
        x:(@) _ "!=" _ y:@ { Box::new(Expr::Binop(x, BinOp::Ne, y)) }
        --
        x:(@) _ "&" _ y:@ { Box::new(Expr::Binop(x, BinOp::BitAnd, y)) }
        --
        x:(@) _ "^" _ y:@ { Box::new(Expr::Binop(x, BinOp::BitXor, y)) }
        --
        x:(@) _ "|" _ y:@ { Box::new(Expr::Binop(x, BinOp::BitOr, y)) }
        --
        x:(@) _ "&&" _ y:@ { Box::new(Expr::Binop(x, BinOp::BoolAnd, y)) }
        --
        x:(@) _ "||" _ y:@ { Box::new(Expr::Binop(x, BinOp::BoolOr, y)) }
        --
        passthrough:expr_cast() { passthrough }
    }

    rule expr_cond() -> Box<Expr> = precedence!{
        c: expr_binop() _ "?" _ t:expr() _ ":" _ f:@ {
            Box::new(Expr::Ternary(c, t, f))
        }
        --
        passthrough:expr_binop() { passthrough }
    }

    rule expr_ass() -> Box<Expr> = precedence!{
        x:@ _ "=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::Non, y)) }
        x:@ _ "*=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::Mul, y)) }
        x:@ _ "/=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::Div, y)) }
        x:@ _ "%=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::Mod, y)) }
        x:@ _ "+=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::Add, y)) }
        x:@ _ "-=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::Sub, y)) }
        x:@ _ "&=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::BitAnd, y)) }
        x:@ _ "^=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::BitXor, y)) }
        x:@ _ "|=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::BitOr, y)) }
        x:@ _ "<<=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::ShftL, y)) }
        x:@ _ ">>=" _ y:(@) { Box::new(Expr::Assign(x, AssOp::ShftR, y)) }
        --
        passthrough:expr_cond() { passthrough }
    }

    pub rule expr() -> Box<Expr> = precedence!{
        x:@ _ "," _ y:(@) { Box::new(Expr::Comma(x, y)) }
        --
        passthrough:expr_ass() {passthrough}
    }

    rule initializer() -> Box<Initializer>
        = v:expr_ass() {
            Box::new(Initializer::Scala(v))
        }
        / "{" _ vs:pad(<initializer()>)**"," _ "}" {
            Box::new(Initializer::Array(vs))
        }

    rule direct_declarator() -> Box<DirectDeclarator>
        = precedence!{
            "(" _ i:declarator() _ ")" {
                Box::new(DirectDeclarator::Declarator(i) )
            }
            nm:ident() {
                Box::new(DirectDeclarator::Scala(nm))
            }
            v:@ _ "(" ss:pad(<param_decl()>)**"," _ ")" {
                Box::new(DirectDeclarator::Func(v, ss))
            }
            v:@ _ "[" _ sz:number() _ "]" {
                Box::new(DirectDeclarator::Array(v, sz))
            }
        }

    rule direct_abstract_declarator() -> Box<DirectAbstractDeclarator>
        = precedence!{
            "(" _ abs:abstract_declarator() _ ")" {
                Box::new(DirectAbstractDeclarator::AbstractDeclarator(abs))
            }
            v:@ _ "[" _ n:number() _ "]" {
                Box::new(DirectAbstractDeclarator::Array(Some(v), n))
            }
            v:@ _ "(" _ n:pad(<param_decl()>)**"," _ ")" {
                Box::new(DirectAbstractDeclarator::Func(Some(v), n))
            }
            "[" _ n:number() _ "]" {
                Box::new(DirectAbstractDeclarator::Array(None, n))
            }
            "(" _ n:pad(<param_decl()>)**"," _ ")" {
                Box::new(DirectAbstractDeclarator::Func(None, n))
            }
        }

    rule declarator() -> Declarator
        = ps:pad(<"*">)* _ s:direct_declarator() _ "=" _ i:initializer() {
            Declarator::Init(ps.len(), s, i)
        }
        / ps:pad(<"*">)* _ s:direct_declarator() {
            Declarator::NoInit(ps.len(), s)
        }
    rule abstract_declarator() -> AbstractDeclarator
        = ps:pad(<"*">)* _ s:direct_abstract_declarator()? {
            AbstractDeclarator(ps.len(), s)
        }

    rule declaration() -> Declaration
        = begin:position!() t:primtype() ws() s:pad(<declarator()>)++"," _ ";" end:position!() {
            Declaration(t, s, Position(begin, end))
        }
    rule type_name() -> TypeName
        = t:primtype() _ s:abstract_declarator() {
            TypeName(t, s)
        }

    rule param_decl() -> ParamDeclaration
        = t:primtype() ws() s:declarator() {
            ParamDeclaration::Named(t, s)
        } /
        t:primtype() _ s:abstract_declarator() {
            ParamDeclaration::Abstract(t, s)
        }

    rule compound_stmt() -> CompoundStmt
        = "{" _ d:pad(<declaration()>)* _ s:pad(<stmt()>)* _ "}" {
            CompoundStmt(d, s)
        }

    pub rule stmt() -> Box<Stmt>
        = begin:position!() "if" _ "(" _ c:expr() _ ")" _ s1:stmt() _ "else" _ s2:stmt() end:position!(){
            Box::new(Stmt::IfElse(c, s1, s2, Position(begin, end)))
        }
        / begin:position!() "if" _ "(" _ c:expr() _ ")" _ s:stmt() end:position!() {
            Box::new(Stmt::If(c, s, Position(begin, end)))
        }
        / begin:position!() "while" _ "(" _ c:expr() _ ")" _ s:stmt() end:position!() {
            Box::new(Stmt::While(c, s, Position(begin, end)))
        }
        / begin:position!() "do" ws() s:stmt() _ "(" _ c:expr() _ ")" end:position!() {
            Box::new(Stmt::DoWhile(s, c, Position(begin, end)))
        }
        / begin:position!() "for" _ "(" _ i:expr()? _ ";" _ c:expr()? _ ";" _ r:expr()?  _ ")" _ s:stmt() end:position!() {
            Box::new(Stmt::For(i, c, r, s, Position(begin, end)))
        }
        / begin:position!() "return" _ c:expr()? _ ";" end:position!(){
            Box::new(Stmt::Return(c, Position(begin, end)))
        }
        / begin:position!() "continue" _ ";" end:position!() {
            Box::new(Stmt::Continue(Position(begin, end)))
        }
        / begin:position!() "break" _ ";" end:position!() {
            Box::new(Stmt::Break(Position(begin, end)))
        }
        / begin:position!() c:compound_stmt() end:position!() {
            Box::new(Stmt::CompoundStmt(c, Position(begin, end)))
        }
        / begin:position!() s:expr()? _ ";" end:position!(){
            Box::new(Stmt::ExprStmt(s, Position(begin, end)))
        }

    pub rule function() -> Function
        = t:primtype() _ n:declarator() _ b:compound_stmt() {
            Function(t, n, b)
        }

    pub rule translation_unit() -> TranslationUnit
        = f:function() { TranslationUnit::Func(f) }
        / d:declaration() { TranslationUnit::Glob(d)}

    pub rule source() -> Source
        = src:pad(<translation_unit()>)+ { Source(src) }
});

#[cfg(test)]
mod test {
    use crate::ast::expr::*;
    use crate::ast::types::*;

    use crate::clang;

    #[test]
    fn manual() {
        assert_eq!(
            *clang::stmt("{ char a; a+= 4;}").unwrap(),
            Stmt::CompoundStmt(
                CompoundStmt(
                    vec![Declaration(
                        PrimType::Char,
                        vec![Declarator::NoInit(
                            0,
                            Box::new(DirectDeclarator::Scala("a".to_owned()))
                        )],
                        Position(2, 9)
                    ),],
                    vec![Box::new(Stmt::ExprStmt(
                        Some(Box::new(Expr::Assign(
                            Box::new(Expr::Ident("a".to_owned())),
                            AssOp::Add,
                            Box::new(Expr::Const(4))
                        ))),
                        Position(10, 16)
                    ))]
                ),
                Position(0, 17)
            )
        )
    }

    #[test]
    fn floating() {
        assert_eq!(
            *clang::expr("4.3").unwrap(),
            Expr::ConstFloat("4.3".to_owned())
        )
    }

    fn check_recompile_stmt(code: &str) {
        let parsed = clang::stmt(code).unwrap();
        let source = parsed.to_string();
        let reparsed = clang::stmt(&source).unwrap();
        let source = reparsed.to_string();
        let rereparsed = clang::stmt(&source).unwrap();
        assert_eq!(reparsed, rereparsed);
    }

    fn check_recompile_func(code: &str) {
        let parsed = clang::function(code).unwrap();
        let source = parsed.to_string();
        let reparsed = clang::function(&source).unwrap();
        let source = reparsed.to_string();
        let rereparsed = clang::function(&source).unwrap();
        assert_eq!(reparsed, rereparsed);
    }

    fn check_recompile_source(code: &str) {
        let parsed = clang::source(code).unwrap();
        let source = parsed.to_string();
        let reparsed = clang::source(&source).unwrap();
        let source = reparsed.to_string();
        let rereparsed = clang::source(&source).unwrap();
        assert_eq!(reparsed, rereparsed);
    }

    #[test]
    fn recompile() {
        check_recompile_stmt("{ char a = 4;}");
        check_recompile_stmt("{ char a = {2, {2, 5}, {2}}; }");
        check_recompile_func("int a() { char a = {2, {2, 5}, {2}}; }");
        check_recompile_func("int a() { }");
        check_recompile_func("int a(char s) { }");
        check_recompile_func("int a(int s) { }");
        check_recompile_func("int a(int a=3, char b) { }");
        check_recompile_source(
            "
        int a = 3;
        int b;
        int c=2, d;

        int main(char a3) {
            b += a;
            if (b >= 4) {
                c = d + 2;
            } else if (c < 3) {
                b = 12;
            } else {
                asdf = 3;
            }
            for(;;) {
                e = 3;
                break;
            }
            ;;;
        }
        ",
        );
    }

    #[test]
    fn reserved() {
        assert!(clang::stmt("{ char int = 4;}").is_err());
        assert!(clang::stmt("{ char bool = 4;}").is_err());
        assert!(clang::stmt("{ char true = 4;}").is_err());
        assert!(clang::stmt("{ char while = 4;}").is_err());
    }

    #[test]
    fn prototype() {
        check_recompile_source(
            "
                float sum(int a, float);
                int main() { float b = sum(2, 4); }
                float sum(int a, float b) { return a + b; }
            ",
        )
    }
}
