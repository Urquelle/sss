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
    jmp   [+30]                          ; addr: 22
    mov   al             , $1               ; addr: 23
    cmp   al             , $1               ; addr: 24
if.1:
    jne   [+30]                             ; addr: 25
    mov   eax            , $33              ; addr: 26
    leave                                   ; addr: 27
    ret                                     ; addr: 28
    jmp   [+30]                          ; addr: 29
    lea   eax            , [ebp-8]          ; addr: 30
    push  eax                               ; addr: 31
    mov   eax            , $1               ; addr: 32
    push  eax                               ; addr: 33
    pop   ecx                               ; addr: 34
    lea   rax            , index            ; addr: 35
    call  rax                               ; addr: 36
    pop   edi                               ; addr: 37
    mov   [edi+0]        , eax              ; addr: 38
    lea   rax            , [rbp-32]         ; addr: 39
    push  eax                               ; addr: 40
    mov   eax            , $2               ; addr: 41
    push  eax                               ; addr: 42
    pop   ecx                               ; addr: 43
    lea   rax            , index            ; addr: 44
    call  rax                               ; addr: 45
    mul   eax            , $8               ; addr: 46
    pop   esi                               ; addr: 47
    add   eax            , esi              ; addr: 48
    push  eax                               ; addr: 49
    mov   eax            , [ebp-8]          ; addr: 50
    mul   eax            , $4               ; addr: 51
    pop   esi                               ; addr: 52
    add   eax            , esi              ; addr: 53
    push  eax                               ; addr: 54
    mov   eax            , $15              ; addr: 55
    pop   edi                               ; addr: 56
    mov   [edi+0]        , eax              ; addr: 57
    lea   rax            , [rbp-32]         ; addr: 58
    push  eax                               ; addr: 59
    mov   eax            , $2               ; addr: 60
    mul   eax            , $8               ; addr: 61
    pop   esi                               ; addr: 62
    add   eax            , esi              ; addr: 63
    push  eax                               ; addr: 64
    mov   eax            , [ebp-8]          ; addr: 65
    mul   eax            , $4               ; addr: 66
    pop   esi                               ; addr: 67
    add   eax            , esi              ; addr: 68
    mov   eax            , [eax+0]          ; addr: 69
    leave                                   ; addr: 70
    ret                                     ; addr: 71
master.end:
    leave                                   ; addr: 72
    ret                                     ; addr: 73
test:
    enter $8                                ; addr: 74
    mov   [rbp-8]        , rcx              ; addr: 75
    mov   rax            , [rbp-8]          ; addr: 76
    push  eax                               ; addr: 77
    mov   eax            , $25              ; addr: 78
    pop   edi                               ; addr: 79
    mov   [edi+0]        , eax              ; addr: 80
test.end:
    leave                                   ; addr: 81
    ret                                     ; addr: 82
index:
    enter $4                                ; addr: 83
    mov   [ebp-4]        , ecx              ; addr: 84
    mov   eax            , $1               ; addr: 85
    push  eax                               ; addr: 86
    mov   eax            , [ebp-4]          ; addr: 87
    pop   edi                               ; addr: 88
    cmp   al             , dil              ; addr: 89
    sete  al                                ; addr: 90
    cmp   al             , $1               ; addr: 91
if.2:
    jne   [+97]                             ; addr: 92
    mov   eax            , $1               ; addr: 93
    leave                                   ; addr: 94
    ret                                     ; addr: 95
    jmp   [+104]                         ; addr: 96
    mov   al             , $1               ; addr: 97
    cmp   al             , $1               ; addr: 98
if.3:
    jne   [+104]                            ; addr: 99
    mov   eax            , $2               ; addr: 100
    leave                                   ; addr: 101
    ret                                     ; addr: 102
    jmp   [+104]                         ; addr: 103
index.end:
    leave                                   ; addr: 104
    ret                                     ; addr: 105
