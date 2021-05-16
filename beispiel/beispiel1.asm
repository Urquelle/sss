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
    mov   eax            , $1               ; addr: 31
    push  eax                               ; addr: 32
    pop   ecx                               ; addr: 33
    lea   rax            , index            ; addr: 34
    call  rax                               ; addr: 35
    pop   edi                               ; addr: 36
    mov   [edi+0]        , eax              ; addr: 37
    lea   rax            , [rbp-32]         ; addr: 38
    push  eax                               ; addr: 39
    mov   eax            , $2               ; addr: 40
    push  eax                               ; addr: 41
    pop   ecx                               ; addr: 42
    lea   rax            , index            ; addr: 43
    call  rax                               ; addr: 44
    mul   eax            , $8               ; addr: 45
    pop   esi                               ; addr: 46
    add   eax            , esi              ; addr: 47
    push  eax                               ; addr: 48
    mov   eax            , [ebp-8]          ; addr: 49
    mul   eax            , $4               ; addr: 50
    pop   esi                               ; addr: 51
    add   eax            , esi              ; addr: 52
    push  eax                               ; addr: 53
    mov   eax            , $15              ; addr: 54
    pop   edi                               ; addr: 55
    mov   [edi+0]        , eax              ; addr: 56
    lea   rax            , [rbp-32]         ; addr: 57
    push  eax                               ; addr: 58
    mov   eax            , $2               ; addr: 59
    mul   eax            , $8               ; addr: 60
    pop   esi                               ; addr: 61
    add   eax            , esi              ; addr: 62
    push  eax                               ; addr: 63
    mov   eax            , [ebp-8]          ; addr: 64
    mul   eax            , $4               ; addr: 65
    pop   esi                               ; addr: 66
    add   eax            , esi              ; addr: 67
    mov   eax            , [eax+0]          ; addr: 68
    jmp   master.end                        ; res addr: 69
master.end:
    leave                                   ; addr: 70
    ret                                     ; addr: 71
test:
    enter $8                                ; addr: 72
    mov   [rbp-8]        , rcx              ; addr: 73
    mov   rax            , [rbp-8]          ; addr: 74
    push  eax                               ; addr: 75
    mov   eax            , $25              ; addr: 76
    pop   edi                               ; addr: 77
    mov   [edi+0]        , eax              ; addr: 78
test.end:
    leave                                   ; addr: 79
    ret                                     ; addr: 80
index:
    enter $4                                ; addr: 81
    mov   [ebp-4]        , ecx              ; addr: 82
    mov   eax            , $1               ; addr: 83
    push  eax                               ; addr: 84
    mov   eax            , [ebp-4]          ; addr: 85
    pop   edi                               ; addr: 86
    cmp   al             , dil              ; addr: 87
    sete  al                                ; addr: 88
    cmp   al             , $1               ; addr: 89
if.2:
    jne   [+94]                             ; addr: 90
    mov   eax            , $1               ; addr: 91
    jmp   index.end                         ; res addr: 92
    jmp   [+100]                         ; addr: 93
    mov   al             , $1               ; addr: 94
    cmp   al             , $1               ; addr: 95
if.3:
    jne   [+100]                            ; addr: 96
    mov   eax            , $2               ; addr: 97
    jmp   index.end                         ; res addr: 98
    jmp   [+100]                         ; addr: 99
index.end:
    leave                                   ; addr: 100
    ret                                     ; addr: 101
