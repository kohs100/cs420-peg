extern crate peg;

use crate::ast::*;

peg::parser!(pub grammar clang() for str {
  rule number() -> u64
      = n:$(['0'..='9']+) { n.parse().unwrap() }
  rule floatdot() -> String
      = exp:$(['0'..='9']+) "." frac:$(['0'..='9']*) { format!("{}.{}", exp, frac) }
  rule dotfloat() -> String
      = exp:$(['0'..='9']*) "." frac:$(['0'..='9']+) { format!("{}.{}", exp, frac) }
  rule bool() -> bool
      = "true" { true }
      / "false" { false }

  rule primtype() -> PrimType
    = "void" { PrimType::Void }
    / "char"{ PrimType::Char }
    / "short" { PrimType::Short }
    / "int" { PrimType::Int }
    / "long" { PrimType::Long }
    / "float" { PrimType::Float }
    / "double" { PrimType::Double }
    / "signed"{ PrimType::Signed }
    / "unsigned" { PrimType::Unsigned }
    / "bool" { PrimType::Bool}

  rule ident() -> String
    = !"if"
      !"else"
      !"do"
      !"while"
      !"for"
      !"return"
      !"continue"
      !"break"
      !"true"
      !"false"
      !primtype()
      s:$(['a'..='z' | 'A'..='Z']['0'..='9' | 'a'..='z' | 'A'..='Z']*) {
          s.to_owned()
      }

  rule stringliteral() -> Box<Expr> = "\"" s:$([^'\"']*) "\"" {
      Box::new(Expr::StringLiteral(s.to_owned()))
  }

  rule _ = [' ' | '\r' | '\n' | '\t']*
  rule ws() = [' ' | '\r' | '\n' | '\t']+

  rule pad<T>(x: rule<T>) -> T = _ v:x() _ {v}

  rule expr_prim() -> Box<Expr> = precedence!{
      v:@ "++" { Box::new(Expr::UnaryIncRight(v)) }
      v:@ "--" { Box::new(Expr::UnaryDecRight(v)) }
      f:@ "(" _ args:pad(<expr()>)**"," _ ")" { Box::new(Expr::Call(f, args)) }
      arr:@ "[" _ idx:expr() _ "]" { Box::new(Expr::Index(arr, idx)) }
      --
      "++" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Inc, v)) }
      "--" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Dec, v)) }
      "+" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Plus, v)) }
      "-" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Minus, v)) }
      "!" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::BoolNot, v)) }
      "~" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::BitNot, v)) }
      "*" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Deref, v)) }
      "&" v:@ { Box::new(Expr::LUnop(LeftUnaryOp::Ref, v)) }
      --
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
      "(" _ v:expr() _ ")" { v }
      fstr:floatdot() { Box::new(Expr::ConstFloat(fstr)) }
      fstr:dotfloat() { Box::new(Expr::ConstFloat(fstr)) }
      n:number() { Box::new(Expr::Const(n)) }
      b:bool() { Box::new(Expr::ConstBool(b)) }
      s:ident() { Box::new(Expr::Ident(s)) }
      s:stringliteral() { s }
  }

  rule expr_cond() -> Box<Expr>
    = c:expr_prim() _ "?" _ t:expr() _ ":" _ f:expr_cond() {
      Box::new(Expr::Ternary(c, t, f))
    }
    / expr_prim()

  rule expr_ass() -> Box<Expr> = precedence!{
      x:@ _ "=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::Non, y)) }
      x:@ _ "*=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::Mul, y)) }
      x:@ _ "/=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::Div, y)) }
      x:@ _ "%=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::Mod, y)) }
      x:@ _ "+=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::Add, y)) }
      x:@ _ "-=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::Sub, y)) }
      x:@ _ "&=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::BitAnd, y)) }
      x:@ _ "^=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::BitXor, y)) }
      x:@ _ "|=" _ y:(@) { Box::new(Expr::Assign(x, AssTyp::BitOr, y)) }
      --
      v: expr_cond() { v }
  }

  pub rule expr() -> Box<Expr> = precedence!{
      x:@ _ "," _ y:(@) { Box::new(Expr::Comma(x, y)) }
      --
      v: expr_ass() {v}
  }

  rule initializer() -> Box<Initializer>
    = v:expr_ass() { Box::new(Initializer::Scala(v)) }
    / "{" _ vs:pad(<initializer()>)**"," _ "}" { Box::new(Initializer::Array(vs)) }

  rule arrsize() -> u64 = _ "[" _ s:number() _ "]" _ { s }
  rule direct_declarator() -> DirectDeclarator
    = v:ident() _ ss:arrsize()+ { DirectDeclarator::Array(v, ss)}
    / v:ident() _ "(" _ ps:pad(<atom_decl()>)**"," _ ")" {
      DirectDeclarator::Func(v, ps)
    }
    / v:ident() { DirectDeclarator::Scala(v) }

  rule declarator() -> Declarator
    = ps:pad(<"*">)* _ s:direct_declarator() _ "=" _ i:initializer() {
      Declarator::Init(ps.len() as u64, s, i)
    }
    / ps:pad(<"*">)* _ s:direct_declarator() {
      Declarator::NoInit(ps.len() as u64, s)
    }

  rule decl() -> Declaration
    = t:primtype() ws() s:pad(<declarator()>)++"," _ ";" { Declaration(t, s) }

  rule atom_decl() -> ParamDeclaration
    = t:primtype() ws() s:declarator() { ParamDeclaration(t, s) }

  rule compound_stmt() -> CompoundStmt
    = "{" _ d:pad(<decl()>)* _ s:pad(<stmt()>)* _ "}" {
        CompoundStmt(d, s)
    }
  pub rule stmt() -> Box<Stmt>
    = "if" _ "(" _ c:expr() _ ")" _ s1:stmt() _ "else" _ s2:stmt() {
          Box::new(Stmt::IfElse(c, s1, s2))
      }
    / "if" _ "(" _ c:expr() _ ")" _ s:stmt() {
          Box::new(Stmt::If(c, s))
      }
    / "while" _ "(" _ c:expr() _ ")" _ s:stmt() {
          Box::new(Stmt::While(c, s))
      }
    / "do" ws() s:stmt() _ "(" _ c:expr() _ ")" {
          Box::new(Stmt::DoWhile(s, c))
      }
    / "for" _ "(" _ i:expr()? _ ";" _ c:expr()? _ ";" r:expr()?  _ ")" _ s:stmt() {
          Box::new(Stmt::For(i, c, r, s))
    }
    / "return" _ c:expr()? _ ";" { Box::new(Stmt::Return(c)) }
    / "continue" _ ";" { Box::new(Stmt::Continue) }
    / "break" _ ";" { Box::new(Stmt::Break) }
    / c:compound_stmt() {
          Box::new(Stmt::CompoundStmt(c))
      }
    / s:expr()? _ ";" {
          Box::new(Stmt::ExprStmt(s))
      }

  pub rule function() -> Function
   = t:primtype() _ n:declarator() _ b:compound_stmt() {
      Function(t, n, b)
   }

  pub rule translation_unit() -> TranslationUnit
   = f:function() { TranslationUnit::Func(f) }
   / d:decl() { TranslationUnit::Glob(d)}

  pub rule source() -> Source
   = src:pad(<translation_unit()>)+ { Source(src) }


});

#[cfg(test)]
mod test {
    use crate::ast::*;
    use crate::clang;

    #[test]
    fn manual() {
        assert_eq!(
            *clang::stmt("{ char a; a+= 4;}").unwrap(),
            Stmt::CompoundStmt(CompoundStmt(
                vec![Declaration(
                    PrimType::Char,
                    vec![Declarator::NoInit(
                        0,
                        DirectDeclarator::Scala("a".to_owned())
                    )]
                )],
                vec![Box::new(Stmt::ExprStmt(Some(Box::new(Expr::Assign(
                    Box::new(Expr::Ident("a".to_owned())),
                    AssTyp::Add,
                    Box::new(Expr::Const(4))
                )))))]
            ))
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
        assert_eq!(parsed, reparsed);
    }

    fn check_recompile_func(code: &str) {
        let parsed = clang::function(code).unwrap();
        let source = parsed.to_string();
        let reparsed = clang::function(&source).unwrap();
        assert_eq!(parsed, reparsed);
    }

    fn check_recompile_source(code: &str) {
        let parsed = clang::source(code).unwrap();
        let source = parsed.to_string();
        let reparsed = clang::source(&source).unwrap();
        assert_eq!(parsed, reparsed);
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
                float sum(int a, float b) { return a + b; }
                int main() { float b = sum(2, 4); }

            ",
        )
    }
}
