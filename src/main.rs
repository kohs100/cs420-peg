// mod ast;
extern crate peg;
mod ast;
mod interpreter;
mod parser;

use parser::clang;
use std::fs::File;
use std::io::Read;

use std::env::args;

fn main() -> Result<(), std::io::Error> {
    let mut args = args();
    let _exec = args.next().unwrap();
    let path = args.next().expect("No path specified.");

    let mut input: String = String::new();
    File::open(path).unwrap().read_to_string(&mut input)?;

    let res = clang::source(&input).unwrap();
    interpreter::interpret::interp_src(&res).unwrap();

    Ok(())
}
