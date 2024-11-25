// mod ast;
extern crate peg;
mod ast;
mod heap;
// mod interpreter;
mod parser;
mod types;

use parser::clang;
use peg::{error::ParseError, str::LineCol};
use types::check::check;

fn check_stmt(s: &str) {
    let stmt = clang::stmt(s);
    println!("------------check_stmt------------");
    println!("{:?}", stmt);
    if let Ok(res) = stmt {
        println!("{}", res)
    }
}

fn check_func(s: &str) {
    let stmt = clang::function(s);
    println!("------------check_func------------");
    println!("{:?}", stmt);
    if let Ok(res) = stmt {
        println!("{}", res)
    }
}

fn check_source(s: &str) -> Result<ast::Source, ParseError<LineCol>> {
    let stmt = clang::source(s);
    println!("------------check_source----------");
    println!("{:?}", &stmt);
    println!("   ------------source----------");
    if let Ok(ast::Source(tus)) = &stmt {
        for tu in tus {
            println!("{}", tu)
        }
    }
    println!("   ------------source----------");

    return stmt;
}

pub fn main() {
    // assert_eq!(
    //     list_parser::list("[1,1,2,3,5,8]"),
    //     Ok(vec![1, 1, 2, 3, 5, 8])
    // );
    // assert_eq!(arithmetic::expression("1+1"), Ok(2));
    // assert_eq!(arithmetic::expression("5*5"), Ok(25));
    // assert_eq!(arithmetic::expression("222+3333"), Ok(3555));
    // assert_eq!(arithmetic::expression("2+3*4"), Ok(14));
    // assert_eq!(arithmetic::expression("(2+2)*3"), Ok(12));
    // assert!(arithmetic::expression("(22+)+1").is_err());
    // assert!(arithmetic::expression("1++1").is_err());
    // assert!(arithmetic::expression("3)+1").is_err());

    check_stmt("{ char a; a+= 4;}");
    check_stmt("{ char a = 4;}");
    check_stmt("{ char int = 4;}");
    check_stmt("{ char a = {2, {2, 5}, {2}}; }");

    check_func("int a() { char a = {2, {2, 5}, {2}}; }");
    check_func("int a() { }");
    check_func("int a(char s) { }");
    check_func("int a(int s) { }");
    check_func("int a(int a=3, char b) { }");

    let res = check_source(
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
    if let Ok(ast::Source(tus)) = res {
        let _res = check(&tus);
    }

    let res = check_source(
        "
            float sum(int a, float b) { return a + b; }
            int main() { float b = sum(2, 4.1); }
        ",
    );
    if let Ok(ast::Source(tus)) = res {
        let _res = check(&tus);
    }
}
