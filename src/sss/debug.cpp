using namespace Urq::Sss::Vm2;

char *regs64_dbg[] = {
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

char *regs32_dbg[] = {
    "eax",
    "ebx",
    "ecx",
    "edx",
    "ebp",
    "esi",
    "edi",
};

char *regs16_dbg[] = {
    "ax",
    "bx",
    "cx",
    "dx",
    "bp",
    "si",
    "di",
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
to_str(Urq::Sss::Vm2::Value val) {
    char *result = NULL;

    switch (val.kind) {
        case Urq::Sss::Vm2::VAL_BOOL: {
            result = buf_printf(result, "%d", val.b ? 1 : 0);
        } break;

        case Urq::Sss::Vm2::VAL_CHAR: {
            result = buf_printf(result, "%c", val.c);
        } break;

        case Urq::Sss::Vm2::VAL_U64: {
            result = buf_printf(result, "%u", val.u64);
        } break;

        case Urq::Sss::Vm2::VAL_S64: {
            result = buf_printf(result, "%d", val.s64);
        } break;

        case Urq::Sss::Vm2::VAL_F32: {
            result = buf_printf(result, "%f", val.f32);
        } break;

        case Urq::Sss::Vm2::VAL_STR: {
            result = buf_printf(result, "%s", val.str);
        } break;
    }

    return result;
}

char *
to_str(Urq::Sss::Vm2::Operand op) {
    char *result = NULL;

    switch ( op.kind ) {
        case OPERAND_IMM: {
            result = buf_printf(result, "%%%%%d", op.val.u64);
        } break;

        case OPERAND_REG64: {
            if ( op.with_displacement ) {
                result = buf_printf(result, "%%%%%d(%s)", op.displacement, regs64_dbg[op.reg64]);
            } else {
                result = buf_printf(result, "%s", regs64_dbg[op.reg64]);
            }
        } break;

        case OPERAND_REG32: {
            if ( op.with_displacement ) {
                result = buf_printf(result, "%%%%%d(%s)", op.displacement, regs32_dbg[op.reg32]);
            } else {
                result = buf_printf(result, "%s", regs32_dbg[op.reg32]);
            }
        } break;

        case OPERAND_REG16: {
            if ( op.with_displacement ) {
                result = buf_printf(result, "%%%%%d(%s)", op.displacement, regs16_dbg[op.reg16]);
            } else {
                result = buf_printf(result, "%s", regs16_dbg[op.reg16]);
            }
        } break;

        case OPERAND_REG8L: {
            if ( op.with_displacement ) {
                result = buf_printf(result, "%%%%%d(%s)", op.displacement, regs8l[op.reg8l]);
            } else {
                result = buf_printf(result, "%s", regs8l[op.reg8l]);
            }
        } break;

        case OPERAND_REG8H: {
            if ( op.with_displacement ) {
                result = buf_printf(result, "%%%%%d(%s)", op.displacement, regs8h[op.reg8h]);
            } else {
                result = buf_printf(result, "%s", regs8h[op.reg8h]);
            }
        } break;

        case OPERAND_NAME: {
            result = buf_printf(result, "%s", to_str(op.val));
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
        case OP_DATA: {
            output = buf_printf(output, "%s", to_str(instr->operand1));
            if ( instr->operand2.kind != OPERAND_NONE ) {
                output = buf_printf(output, " %s", to_str(instr->operand2));
            }
        } break;

        case OP_LEA: {
            output = buf_printf(output, to_str_binop("lea", instr));
        } break;

        case OP_MOV: {
            output = buf_printf(output, to_str_binop("mov", instr));
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

        case OP_RET: {
            output = buf_printf(output, "ret");
        } break;

        case OP_CALL: {
            output = buf_printf(output, "call %s", to_str(instr->operand1));
        } break;

        default: {
            assert(0);
        } break;
    }

    if ( instr->comment ) {
        output = buf_printf(output, "    ; %s", instr->comment);
    }

    return output;
}

void
debug(Instrs instrs, char *file_name) {
    char *output = NULL;

    for ( int32_t i = 0; i < buf_len(instrs); ++i ) {
        Instr *instr = instrs[i];
        output = buf_printf(output, "%s%s%d\n", to_str(instr), instr->comment ? " addr: " : " ; addr: ", instr->addr);
    }

    Urq::Os::os_file_write(file_name, output, utf8_str_size(output));
}

