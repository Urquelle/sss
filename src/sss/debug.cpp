using namespace Urq::Sss::Vm2;

char *regs64[] = {
    "rax",
    "rbx",
    "rcx",
    "rdx",
    "rbp",
    "rsi",
    "rdi",
    "rsp",
    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15",
    "rip",
    "rflags",
};

char *regs32[] = {
    "eax",
    "ebx",
    "ecx",
    "edx",
};

char *regs16[] = {
    "ax",
    "bx",
    "cx",
    "dx",
};

char *regs8l[] = {
    "al",
    "bl",
    "cl",
    "dl",
};

char *regs8h[] = {
    "ah",
    "bh",
    "ch",
    "dh",
};

char *
to_str(Urq::Sss::Vm2::Operand op) {
    char *result = NULL;

    switch ( op.kind ) {
        case OPERAND_IMM: {
            result = buf_printf(result, "%%%%%d", op.val.u64);
        } break;

        case OPERAND_REG64: {
            result = buf_printf(result, "%s", regs64[op.reg64]);
        } break;

        case OPERAND_REG32: {
            result = buf_printf(result, "%s", regs32[op.reg32]);
        } break;

        case OPERAND_REG16: {
            result = buf_printf(result, "%s", regs16[op.reg16]);
        } break;

        case OPERAND_REG8L: {
            result = buf_printf(result, "%s", regs8l[op.reg8l]);
        } break;

        case OPERAND_REG8H: {
            result = buf_printf(result, "%s", regs8h[op.reg8h]);
        } break;

        default: {
            assert(0);
        } break;
    }

    return result;
}

char *
to_str_binop(char *op, Instr *instr) {
    char *result = NULL;

    result = buf_printf(result, "%s %s, %s", op, to_str(instr->operand1), to_str(instr->operand2));

    return result;
}

char *
to_str(Instr *instr) {
    char *output = NULL;

    if ( instr->label ) {
        output = buf_printf(output, "%s:\n    ", instr->label);
    } else {
        output = buf_printf(output, "    ");
    }

    switch ( instr->op ) {
        case OP_CDQ: {
            output = buf_printf(output, "cdq");
        } break;

        case OP_MOV: {
            output = buf_printf(output, to_str_binop("mov", instr));
        } break;

        case OP_MOVZX: {
            output = buf_printf(output, to_str_binop("movzx", instr));
        } break;

        case OP_PUSH: {
            output = buf_printf(output, "push %s", to_str(instr->operand1));
        } break;

        case OP_POP: {
            output = buf_printf(output, "pop %s", to_str(instr->operand1));
        } break;

        case OP_ADD: {
            output = buf_printf(output, to_str_binop("add", instr));
        } break;

        case OP_SUB: {
            output = buf_printf(output, to_str_binop("sub", instr));
        } break;

        case OP_IMUL: {
            output = buf_printf(output, to_str_binop("imul", instr));
        } break;

        case OP_IDIV: {
            output = buf_printf(output, "idiv %s", to_str(instr->dst));
        } break;

        case OP_CMP: {
            output = buf_printf(output, to_str_binop("cmp", instr));
        } break;

        case OP_SETL: {
            output = buf_printf(output, "setl %s", to_str(instr->dst));
        } break;

        case OP_SETLE: {
            output = buf_printf(output, "setle %s", to_str(instr->dst));
        } break;

        case OP_SETE: {
            output = buf_printf(output, "sete %s", to_str(instr->dst));
        } break;

        case OP_SETGE: {
            output = buf_printf(output, "setge %s", to_str(instr->dst));
        } break;

        case OP_SETG: {
            output = buf_printf(output, "setg %s", to_str(instr->dst));
        } break;

        case OP_SETNE: {
            output = buf_printf(output, "setne %s", to_str(instr->dst));
        } break;

        default: {
            assert(0);
        } break;
    }

    return output;
}

void
vm_debug(Instrs instrs, char *file_name) {
    char *output = NULL;

    output = buf_printf(output, "    .text\n");
    output = buf_printf(output, "    .global main\n");
    output = buf_printf(output, "main:\n");
    for ( int32_t i = 0; i < buf_len(instrs); ++i ) {
        Instr *instr = instrs[i];
        output = buf_printf(output, "%s\n", to_str(instr));
    }

    Urq::Os::os_file_write(file_name, output, utf8_str_size(output));
}

