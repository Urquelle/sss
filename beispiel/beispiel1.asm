section .data
    mov   eax            , $2               ; addr: 0
    push  eax                               ; addr: 1
    mov   eax            , $1               ; addr: 2
    pop   edi                               ; addr: 3
    add   eax            , edi              ; addr: 4

section .text
master:
    enter $44                               ; addr: 5
    mov   eax            , $5               ; addr: 6
    mov   [ebp-4]        , eax              ; addr: 7
    mov   eax            , $10              ; addr: 8
    push  eax                               ; addr: 9
    mov   eax            , [ebp-4]          ; addr: 10
    pop   edi                               ; addr: 11
    cmp   al             , dil              ; addr: 12
    setl  al                                ; addr: 13
    cmp   al             , $1               ; addr: 14
if.0:
    jne   [+23]                             ; addr: 15
    lea   rax            , [rbp-44]         ; addr: 16
    add   eax            , $4               ; addr: 17
    push  rax                               ; addr: 18
    pop   rcx                               ; addr: 19
    lea   rax            , test             ; addr: 20
    call  rax                               ; addr: 21
    jmp   [+29]                          ; addr: 22
    mov   al             , $1               ; addr: 23
    cmp   al             , $1               ; addr: 24
if.1:
    jne   [+29]                             ; addr: 25
    mov   eax            , $33              ; addr: 26
    jmp   master.end                        ; res addr: 27
    jmp   [+29]                          ; addr: 28
    lea   eax            , [ebp-8]          ; addr: 29
    push  eax                               ; addr: 30
    lea   rax            , index            ; addr: 31
    call  rax                               ; addr: 32
    pop   edi                               ; addr: 33
    mov   [edi+0]        , eax              ; addr: 34
    lea   rax            , [rbp-32]         ; addr: 35
    push  eax                               ; addr: 36
    mov   eax            , [ebp-8]          ; addr: 37
    mul   eax            , $8               ; addr: 38
    pop   esi                               ; addr: 39
    add   eax            , esi              ; addr: 40
    push  eax                               ; addr: 41
    mov   eax            , [ebp-8]          ; addr: 42
    mul   eax            , $4               ; addr: 43
    pop   esi                               ; addr: 44
    add   eax            , esi              ; addr: 45
    push  eax                               ; addr: 46
    mov   eax            , $15              ; addr: 47
    pop   edi                               ; addr: 48
    mov   [edi+0]        , eax              ; addr: 49
    lea   rax            , [rbp-32]         ; addr: 50
    push  eax                               ; addr: 51
    mov   eax            , [ebp-8]          ; addr: 52
    mul   eax            , $8               ; addr: 53
    pop   esi                               ; addr: 54
    add   eax            , esi              ; addr: 55
    push  eax                               ; addr: 56
    mov   eax            , [ebp-8]          ; addr: 57
    mul   eax            , $4               ; addr: 58
    pop   esi                               ; addr: 59
    add   eax            , esi              ; addr: 60
    mov   eax            , [eax+0]          ; addr: 61
    jmp   master.end                        ; res addr: 62
master.end:
    leave                                   ; addr: 63
    ret                                     ; addr: 64
test:
    enter $8                                ; addr: 65
    mov   [rbp-8]        , rcx              ; addr: 66
    mov   rax            , [rbp-8]          ; addr: 67
    push  eax                               ; addr: 68
    mov   eax            , $25              ; addr: 69
    pop   edi                               ; addr: 70
    mov   [edi+0]        , eax              ; addr: 71
test.end:
    leave                                   ; addr: 72
    ret                                     ; addr: 73
index:
    enter $0                                ; addr: 74
    mov   eax            , $1               ; addr: 75
    jmp   index.end                         ; res addr: 76
index.end:
    leave                                   ; addr: 77
    ret                                     ; addr: 78
