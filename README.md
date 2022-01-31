# Snack

The language that eats the stack.  Heavily inspired by [porth](https://gitlab.com/tsoding/porth/)
which is inspired off of [forth](https://en.wikipedia.org/wiki/Forth_(programming_language))


# Syntax

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
.|Print's top item with no new line and removes it from stack.
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
