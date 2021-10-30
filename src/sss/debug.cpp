using namespace Urq::Sss::Vm;

char *regs64_dbg[] = {
    "null",
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
    "null",
    "eax",
    "ebx",
    "ecx",
    "edx",
    "ebp",
    "esi",
    "edi",
    "esp",
    "r8d",
    "r9d",
};

char *regs16_dbg[] = {
    "null",
    "ax",
    "bx",
    "cx",
    "dx",
    "bp",
    "si",
    "di",
    "sp",
    "r8w",
    "r9w",
};

char *regs8_dbg[] = {
    "null",
    "al",
    "bl",
    "cl",
    "dl",
    "bp",
    "",
    "dil",
    "spl",
    "r8b",
    "r9b",
};

char *
to_str(Urq::Sss::Vm::Value val) {
    char *result = NULL;

    switch (val.kind) {
        case Urq::Sss::Vm::VAL_BOOL: {
            result = buf_printf(result, "%d", val.b ? 1 : 0);
        } break;

        case Urq::Sss::Vm::VAL_CHAR: {
            result = buf_printf(result, "%c", val.c);
        } break;

        case Urq::Sss::Vm::VAL_U64: {
            result = buf_printf(result, "%u", val.u64);
        } break;

        case Urq::Sss::Vm::VAL_S64: {
            result = buf_printf(result, "%d", val.s64);
        } break;

        case Urq::Sss::Vm::VAL_S32: {
            result = buf_printf(result, "%d", val.s32);
        } break;

        case Urq::Sss::Vm::VAL_F32: {
            result = buf_printf(result, "%f", val.f32);
        } break;

        case Urq::Sss::Vm::VAL_STR: {
            result = buf_printf(result, "%s", val.str);
        } break;

        default: {
            assert(0);
        } break;
    }

    return result;
}

char *
to_str(Reg_Kind reg, int32_t scale, uint32_t size) {
    char *result = NULL;

    if ( reg == REG_NONE ) {
        return "";
    }

    if ( size == 8 ) {
        result = buf_printf(result, "+(%s*%d)", regs64_dbg[reg], scale);
    } else if ( size == 4 ) {
        result = buf_printf(result, "+(%s*%d)", regs32_dbg[reg], scale);
    } else if ( size == 2 ) {
        result = buf_printf(result, "+(%s*%d)", regs16_dbg[reg], scale);
    } else if ( size == 1 ) {
        result = buf_printf(result, "+(%s*%d)", regs8_dbg[reg], scale);
    }

    return result;
}

char *
to_str(Reg_Kind reg, uint32_t size) {
    char *result = NULL;

    if ( reg == REG_NONE ) {
        return "";
    }

    if ( size == 8 ) {
        result = buf_printf(result, "%s", regs64_dbg[reg]);
    } else if ( size == 4 ) {
        result = buf_printf(result, "%s", regs32_dbg[reg]);
    } else if ( size == 2 ) {
        result = buf_printf(result, "%s", regs16_dbg[reg]);
    } else if ( size == 1 ) {
        result = buf_printf(result, "%s", regs8_dbg[reg]);
    }

    return result;
}

char *
to_str(Urq::Sss::Vm::Operand *op) {
    char *result = NULL;

    switch ( op->kind ) {
        case OPERAND_ADDR: {
            char *displacement = NULL;
            displacement = buf_printf(displacement, "%d", op->addr.displacement);

            result = buf_printf(result, "[%s%s%s%s]", to_str(op->addr.base, op->size),
                    to_str(op->addr.index, op->addr.scale, op->size), (op->addr.displacement >= 0) ? "+" : "",
                    displacement);
        } break;

        case OPERAND_IMM: {
            result = buf_printf(result, "%d", op->val.u64);
        } break;

        case OPERAND_REG: {
            result = buf_printf(result, "%s", to_str(op->reg.kind, op->size));
        } break;

        case OPERAND_LABEL: {
            result = buf_printf(result, "%s", op->label);
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

    result = buf_printf(result, "%-6s%-15s, %-16s", op, to_str(instr->operand1), to_str(instr->operand2));

    return result;
}

char *
to_str(Instr *instr) {
    char *output = NULL;

    if ( instr->label ) {
        output = buf_printf(output, "%s:\n    ", instr->label);

        for ( int i = 0; i < buf_len(vm_labels); ++i ) {
            Vm_Label label = vm_labels[i];

            if ( label.addr == instr->addr && label.label != instr->label ) {
                output = buf_printf(output, "%s:\n    ", label.label);
            }
        }
    } else {
        output = buf_printf(output, "    ");
    }

    switch ( instr->op ) {
        case OP_ADD: {
            output = buf_printf(output, to_str_binop("add", instr));
        } break;

        case OP_CALL: {
            output = buf_printf(output, "call  %-33s", to_str(instr->operand1));
        } break;

        case OP_CMP: {
            output = buf_printf(output, to_str_binop("cmp", instr));
        } break;

        case OP_DIV: {
            output = buf_printf(output, "div   %-33s", to_str(instr->dst));
        } break;

        case OP_ENTER: {
            output = buf_printf(output, "enter %-33s", to_str(instr->operand1));
        } break;

        case OP_IDIV: {
            output = buf_printf(output, "idiv  %-33s", to_str(instr->dst));
        } break;

        case OP_IMUL: {
            output = buf_printf(output, to_str_binop("imul", instr));
        } break;

        case OP_JE: {
            output = buf_printf(output, "je    %-33s", to_str(instr->operand1));
        } break;

        case OP_JMP: {
            output = buf_printf(output, "jmp   %-30s", to_str(instr->operand1));
        } break;

        case OP_JNE: {
            output = buf_printf(output, "jne   %-33s", to_str(instr->operand1));
        } break;

        case OP_JNZ: {
            output = buf_printf(output, "jnz   %-33s", to_str(instr->operand1));
        } break;

        case OP_JZ: {
            output = buf_printf(output, "jz    %-33s", to_str(instr->operand1));
        } break;

        case OP_LEA: {
            output = buf_printf(output, to_str_binop("lea", instr));
        } break;

        case OP_LEAVE: {
            output = buf_printf(output, "%-39s", "leave");
        } break;

        case OP_MOV: {
            output = buf_printf(output, to_str_binop("mov", instr));
        } break;

        case OP_MUL: {
            output = buf_printf(output, to_str_binop("mul", instr));
        } break;

        case OP_NEG: {
            output = buf_printf(output, "neg   %-33s", to_str(instr->operand1));
        } break;

        case OP_NOP: {
            output = buf_printf(output, "%-36s", "nop");
        } break;

        case OP_NOT: {
            output = buf_printf(output, "not %s", to_str(instr->operand1));
        } break;

        case OP_PUSH: {
            output = buf_printf(output, "push  %-33s", to_str(instr->operand1));
        } break;

        case OP_POP: {
            output = buf_printf(output, "pop   %-33s", to_str(instr->operand1));
        } break;

        case OP_SUB: {
            output = buf_printf(output, to_str_binop("sub", instr));
        } break;

        case OP_SETL: {
            output = buf_printf(output, "setl  %-33s", to_str(instr->dst));
        } break;

        case OP_SETLE: {
            output = buf_printf(output, "setle %-33s", to_str(instr->dst));
        } break;

        case OP_SETE: {
            output = buf_printf(output, "sete  %-33s", to_str(instr->dst));
        } break;

        case OP_SETGE: {
            output = buf_printf(output, "setge %-33s", to_str(instr->dst));
        } break;

        case OP_SETG: {
            output = buf_printf(output, "setg  %-33s", to_str(instr->dst));
        } break;

        case OP_SETNE: {
            output = buf_printf(output, "setne %-33s", to_str(instr->dst));
        } break;

        case OP_RET: {
            output = buf_printf(output, "%-39s", "ret");
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
debug(uint8_t *obj, char *file_name) {
    char *output = NULL;

    uint64_t num_instrs = obj_text_num_entries(obj);
    Instrs instrs = (Instrs)obj_text_section(obj);

    for ( uint32_t i = 0; i < num_instrs; ++i ) {
        Instr *instr = instrs[i];

        output = buf_printf(output, "%s%s%d\n", to_str(instr), instr->comment ? " addr: " : " ; addr: ", instr->addr);
    }

    Urq::Os::os_file_write(file_name, output, utf8_str_size(output));
}

