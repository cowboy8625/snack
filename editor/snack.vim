" Vim syntax file
" Language: snack
" Maintainer: Cowboy8625
" Latest Revision: Fri 14 Jan 2022

if exists("b:current_syntax")
  finish
endif

syn keyword snackKeyword if elif else do true false let copy swap drop over rot
syn keyword snackKeyword while end or not and memory const word in use

syn keyword snackFunction println print max
syn keyword snackFunction syscall1 syscall2 syscall3 syscall4 syscall5 syscall6

hi link snackKeyword Keyword
hi link snackFunction Function

syn keyword snackTodo contained TODO FIXME XXX NOTE
syn match snackComment "//.*$" contains=snackTodo

"----------------------------------------------------------------
" Celestia Star Catalog Numbers
"----------------------------------------------------------------

" Regular int like number with - + or nothing in front
syn match snackNumber '\d\+' contained display
syn match snackNumber '[-+]\d\+' contained display

" Floating point number with decimal no E or e (+,-)
syn match snackNumber '\d\+\.\d*' contained display
syn match snackNumber '[-+]\d\+\.\d*' contained display

" Floating point like number with E and no decimal point (+,-)
syn match snackNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+' contained display
syn match snackNumber '\d[[:digit:]]*[eE][\-+]\=\d\+' contained display

" Floating point like number with E and decimal point (+,-)
syn match snackNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' contained display
syn match snackNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+' contained display

syn region snackString start='"' end='"' contained
syn region snackDesc start='"' end='"'

syn match snackHip '\d\{1,6}' nextgroup=snackString
syn region snackDescBlock start="{" end="}" fold transparent contains=ALLBUT,snackHip,crashString

syn keyword snackBlockCmd RA Dec Distance AbsMag nextgroup=snackNumber
syn keyword snackBlockCmd SpectralType nextgroup=snackDesc

syn match snackCharacter /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/
syn match snackCharacter /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/

syn match snackIdentifier contains=snackIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained


hi def link snackIdentifierPrime   snackIdentifier
hi def link snackIdentifier        Identifier
hi def link snackTodo              Todo
hi def link snackComment           Comment
hi def link snackBlockCmd          Statement
hi def link snackHip               Type
hi def link snackString            Constant
hi def link snackDesc              PreProc
hi def link snackNumber            Constant
hi def link snackCharacter         Character

let b:current_syntax = "snack"
