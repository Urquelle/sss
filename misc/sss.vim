if exists("b:current_syntax")
  finish
endif

syntax keyword sssUsing using
syntax keyword sssNew new
syntax keyword sssRemove remove
syntax keyword sssCast cast
syntax keyword sssSizeOf sizeof
syntax keyword sssTypeOf typeof
syntax keyword sssTypeinfo typeinfo
syntax keyword sssTypedef typedef
syntax keyword sssAlignOf alignof
syntax keyword sssOffsetOf offsetof
syntax keyword sssSwitch match
syntax keyword sssOn on
syntax keyword sssDefault default

syntax keyword sssStruct struct
syntax keyword sssUnion union
syntax keyword sssEnum enum
syntax keyword sssProc proc

syntax keyword sssIf if
syntax keyword sssThen then
syntax keyword sssElse else
syntax keyword sssFor for
syntax keyword sssWhile while
syntax keyword sssContinue continue
syntax keyword sssBreak break

syntax keyword sssDataType void string char int float f32 f64 u8 u16 u32 u64 s8 s16 s32 s64 bool typeid
syntax keyword sssBool true false
syntax keyword sssNull nil

syntax keyword sssReturn return
syntax keyword sssDefer defer
syntax keyword sssFree free
syntax keyword sssAssert assert

syntax keyword sssInline inline
syntax keyword sssNoInline no_inline

syntax keyword sssIt it
syntax keyword sssItIndex it_index

syntax region sssChar start=/'/ skip=/\v\\./ end=/'/
syntax region sssString start=/\v"/ skip=/\v\\./ end=/\v"/

syntax keyword sssAutoCast xx

syntax match sssFunction "\v<\w*>(\s*::\s*proc)@="
syntax match sssDynamicFunction "\v<\w*(\s*:\=\s*\(.*\))@="

syntax match sssTagNote "#note \<\w\+\>" display

syntax match sssClass "\v<[A-Z]\w+>" display
syntax match sssConstant "\v<[A-Z0-9,_]+>" display
syntax match sssRange "\.\." display
syntax match sssEllipsis "\.\.\." display
syntax match sssAssignType "\:\:\?" display
syntax match sssDeclare ":=" display
syntax match sssPointer "\*" display
syntax match sssDeref "@" display

syntax match sssReturnOp "->" display
syntax match sssNoInit "---" display

syntax match sssInteger "\-\?\<_*\d\+\>" display
syntax match sssFloat "\-\?\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=" display
syntax match sssFloatHex "\-\?\<0[h][0-9A-Fa-f]\+\>" display
syntax match sssHex "\-\?\<16b[_0-9A-Fa-f]\+\>" display
syntax match sssBin "\-\?\<2b[_0-1]\+\>" display
syntax match sssOct "\-\?\<8b[_0-7]\+\>" display

syntax match sssMacro "#\<\w\+\>" display
syntax match sssTemplate "$\<\w\+\>"
syntax match sssCommentNote "@\<\w\+\>" contained display

syntax match sssLineComment "##.*\n" contains=sssCommentNote
syntax region sssBlockComment start=/##(/ end=/##)/ contains=sssBlockComment, sssCommentNote

highlight link sssIt Keyword
highlight link sssFree Keyword
highlight link sssItIndex Keyword
highlight link sssUsing Keyword
highlight link sssNew Keyword
highlight link sssCast Keyword
highlight link sssSwitch Keyword
highlight link sssOn Keyword
highlight link sssDefault Keyword
highlight link sssSizeOf Keyword
highlight link sssTypeOf Keyword
highlight link sssTypeinfo Keyword
highlight link sssTypedef Keyword
highlight link sssAlignOf Keyword
highlight link sssOffsetOf Keyword
highlight link sssAutoCast Keyword
highlight link sssReturn Keyword
highlight link sssRemove Keyword
highlight link sssContinue Keyword
highlight link sssBreak Keyword

highlight link sssPointer Operator
highlight link sssDeref Operator
highlight link sssDefer Operator
highlight link sssRemove Operator
highlight link sssRange Operator
highlight link sssEllipsis Operator
highlight link sssDeclare Operator
highlight link sssAssignType Operator
highlight link sssReturnOp Operator
highlight link sssNoInit Operator

highlight link sssInline Keyword
highlight link sssNoInline Keyword

highlight link sssString String
highlight link sssChar String

highlight link sssStruct Structure
highlight link sssUnion Structure
highlight link sssEnum Structure
highlight link sssProc Structure

highlight link sssFunction Function
highlight link sssDynamicFunction Function
highlight link sssAssert Function

highlight link sssMacro Macro
highlight link sssIf Conditional
highlight link sssThen Conditional
highlight link sssElse Conditional
highlight link sssFor Repeat
highlight link sssWhile Repeat

highlight link sssLineComment Comment
highlight link sssBlockComment Comment
highlight link sssCommentNote Todo

highlight link sssClass Type

highlight link sssTemplate Constant

highlight link sssTagNote WorkNote
highlight link sssDataType Type
highlight link sssBool Boolean
highlight link sssConstant Constant
highlight link sssNull Type
highlight link sssInteger Number
highlight link sssFloat Float
highlight link sssFloatHex Float
highlight link sssHex Number
highlight link sssBin Number
highlight link sssOct Number

let b:current_syntax = "sss"
