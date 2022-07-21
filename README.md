# Snack

The language that eats the stack.  Heavily inspired by [porth](https://gitlab.com/tsoding/porth/)
which is inspired off of [forth](https://en.wikipedia.org/wiki/Forth_(programming_language))

Having the language self hosted is the ultimate goal without the use of
[fasm](https://flatassembler.net/download.php). The knowledge I get from
this project I will put in use for another language project I am working on.


# Install

To use Snack you will need
[Rust](https://rustup.rs/) and
[fasm](https://flatassembler.net/download.php) after installing run
```
git clone https://github.com/cowboy8625/snack
cd snack/snackc
cargo run --release -- run <file.snack>
```
or to install
```
cargo install --git https://github.com/cowboy8625/snack
```


# Snackc Args

Args can be stacked.

```
run           Compiles and runs file.
--debug       Gives debug output to assembly file.
```

and you all set.

# Snackc Commands

**Build**
`snackc <filename>`

**Build & Run**
`snackc run <filename>`


# Syntax

**Hello World**
```
use std

word main -> null in
"Hello World!" println
end
```
**Looping and Conditions**
```
use std

word main -> null in
0 while copy 10 <= do
true == if
"If" println
copy 1 == elif
"ElIf" println
else
"Else" println
end
1 +
end drop
end
```
**Functions**
```
use std

word sub x: u64 y: u64 -> u64 in
x y - return
end

word main -> null in
10 20 sub .
end
```

## Stack Commands

Name|Description
----|-----------
copy|Copy's top of stack
swap|Swap's top two items on stack with each other.
drop|Drop's top item from stack.
.|Print's number from top of stack (Works with numbers only)
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
or|Takes two bools and returns true if one is true
and|Takes two bools and returns true if both are true
\+|Addes top two items on stack and places Result back on stack
\-|Subtracts top two items on stack and places Result back on stack
\*|Multiply top two items on stack and places Result back on stack
/|Divide top two items on stack and places Result back on stack
%|Mod top two items on stack and places Result back on stack


## Memory

Name|Description|Usage
:----|:-----------|:------
memory|Push's memory address on stack
!|Stores a single byte from a address on to the stack.| `memory 72 !`
@|Loads a single byte from address and puts it on the stack.| `"H" @ drop .`


## Syscall's

syscall[1-6] use the one matching required args.
For writing you would do `"Hello" 1 1 syscall3`

## Keywords
name|usage
:---|:---
`use`|import lib snack file
`word`|use to declare a "function"
`in`|open Word Def block
`return`|returns last pushed item to stack
