# CS420-PEG

CS420 Compiler Design Term Project - Sub-C Interpreter

## PEG

This project utilizes [rust-peg](https://github.com/kevinmehall/rust-peg) crate to generate parser with PEG(Parser Expression Grammar).

## How to run

Please refer to [CS420-compiler](https://github.com/kohs100/cs420-compiler)

## Details

### Implementation

- All stack variable declarations must be present at very first of compound statement.
  - Automatic hoisting is not supported.
- Declaration with initialization is supported.
  - Array initialization is not supported.
- Main function should have no parameters and must return int type.
- No complex type specifier (unsigned long, unsigned int, ...).
- Stack size is limited to 4MB (configurable in code)
- Function pointer is not supported.
  - Function names are managed by isolated global namespace.
- struct / union / typedef not supported.
- include is not supported.
  - printf / malloc / free is provided as built-in functions.
  - build-in functions do not print call footprints.
- Tried to respect C-style implicit type conversions.
  - Please refer `src/interpreter/value.rs`
  - Contextual conversion
  - Integer / Floating-point type promotion/conversions
    - Arithmetic types can be implicitly casted each others
  - Usual arithmetic conversions
    - Automatic promotion of small integral types to signed int type (32bit in this implementation)
  - Pointer arithmetic rules
  - Array-to-pointer decay
  - Implicit pointer conversion from/into void pointer

### Memory layout

from `src/interpreter/runtime.rs`:

```
 |<--- Glob --->|<--------- Stack --------->|------- Heap ------>
 |0             |OFS_STK                    |OFS_HEAP
 |       SZ_GLOB|        OFS_STK+SZ_STK(4MB)|
```

- `SZ_GLOB` is pre-allocated in pre-runtime phase, thus stack base offset(`OFS_STK`) will also determined before runtime phase.
- `SZ_STK` is 4MB by default.
- Stack growth direction is upside, not like typical x86 arch.
- String literals are allocated at heap in runtime.
  - Since there are string literal hashtable, it will be allocated maximum only once, not each time its expression be evaluated.
- Memory allocators internally manages memory spaces as unit of page (4KB).
