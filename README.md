# Snack

The language that eats the stack.  Heavily inspired by [porth](https://gitlab.com/tsoding/porth/)
which is inspired off of [forth](https://en.wikipedia.org/wiki/Forth_(programming_language))


# Install

To use Snack you will need [Rust](https://rustup.rs/) and [fasm](https://flatassembler.net/download.php)
After installing Rust just run `cargo install --git https://github.com/cowboy8625/snack`
and you all set.

# Snackc Commands

**Build**
`snackc <filename>`

**Build & Run**
`snackc run <filename>`


# Syntax

**Hello World**
```
"Hello World!\n" 1 1 syscall3
```

**Looping and Conditions**
```
0 while copy 10 <= do
copy 0 == if
100 .
copy 1 == elif
200 .
copy 2 == elif
300 .
copy 3 == elif
400 .
else
copy .
end
1 +
end
```

## Stack Commands

name|Description
----|-----------
copy|Copy's top of stack
swap|Swap's top two items on stack with each other.
drop|Drop's top item from stack.
.|Print's number from top of stack (Works with numbers only
    over|Copy's second from top item on stack.
    rot|Rotate's top three items on stack.
    max|Take top two items on stack and place max value back on top.

## Operators

    name|Description
    ----|-----------
    ==|Check for Equality of two items and places bool on top of stack
    !=|Check for Not Equal of two items and places bool on top of stack
    \>|Check which is Greater of two items and places bool on top of stack
    \<|Check which is Less of two items and places bool on top of stack
    \>=|Check which is Greater or Equal of two items and places bool on top of stack
    \<=|Check which is Less or Equalof two items and places bool on top of stack
    \+|Addes top two items on stack and places Result back on stack
    \-|Subtracts top two items on stack and places Result back on stack
    \*|Multiply top two items on stack and places Result back on stack
    /|Divide top two items on stack and places Result back on stack
    %|Mod top two items on stack and places Result back on stack


## Memory

name|Description
----|-----------
memory|Push's memory address on stack
!|Stores a single byte from a address on the stack.
@|Loads a single byte from address on stack.


## Syscall's

name|arg1|arg2|arg3
----|----|----|----
syscall3|message size|message location|File Descriptor


