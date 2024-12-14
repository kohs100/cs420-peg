// mod ast;
extern crate peg;
mod ast;
mod interpreter;
mod parser;

use parser::clang;
use std::fs::File;
use std::io::Read;

fn main() -> Result<(), std::io::Error> {
    let mut input: String = String::new();
    File::open("./term_ex.c")
        .unwrap()
        .read_to_string(&mut input)?;

    let res = clang::source(&input).unwrap();
    interpreter::interpret::interp_src(&res).unwrap();

    Ok(())
}
