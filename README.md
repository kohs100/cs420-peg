# CS420-PEG

CS420 Compiler Design Term Project - Sub-C Interpreter

## PEG

This project utilizes [rust-peg](https://github.com/kevinmehall/rust-peg) crate to generate parser with PEG(Parser Expression Grammar).

## How to run

### Install rust

```
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Run with C source file

```
$ git clone https://github.com/kohs100/cs420-peg
$ cd cs420-peg
$ cargo run -- test/fibo.c
```

## Implementation

- Declaration with initialization is supported.
  - Array initialization is not supported.
- Main function should have no parameters and must return int type.
- No complex type specifier (unsigned long, unsigned int, ...).
- Stack size is limited to 4MB (configurable in code)
- Function pointer is not supported.
- struct / union / typedef not supported.
- include is not supported.
  - printf / malloc / free is provided as built-in functions.
-
