# CS420-PEG
CS420 Compiler Design Term Project - Sub-C Interpreter

## PEG
This project utilizes [rust-peg](https://github.com/kevinmehall/rust-peg) crate to generate parser with PEG(Parser Expression Grammar).

## Generating test coverage
Install [grcov](https://github.com/mozilla/grcov)
```
$ cargo install grcov
$ rustup component add llvm-tools-preview
```

Generate coverage report
```
$ ./coverage.sh
```