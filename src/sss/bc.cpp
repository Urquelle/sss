struct Bytecode_Instr;

uint32_t val_int(int64_t val);

struct Bytecode {
    Bytecode_Instr * code;
    uint32_t         size;
    uint32_t         cap;
};

enum Value_Kind {
    VAL_NONE,
    VAL_INT,
    VAL_STR,
};

struct Value {
    Value_Kind kind;
    int32_t idx;

    union {
        char     * str_val;
        int64_t    int_val;
    };
};

Value *bytecode_values;

enum Bytecode_Opcode {
    BYTECODEOP_HLT,
    BYTECODEOP_PUSH_CONST,
    BYTECODEOP_PUSH_SYM,
    BYTECODEOP_LOAD_SYM,
};

struct Bytecode_Instr {
    uint32_t   opcode;
    uint32_t   val_idx;
    char     * name;
};

struct Env {
    char   * name;
    Map      syms;
    Value ** sym_list;
    Env    * parent;
};

enum { MAX_STACK_SIZE = 1024 };
int32_t stack[MAX_STACK_SIZE];
int32_t stack_pointer;

Bytecode *
bytecode_new() {
    Bytecode *result = urq_allocs(Bytecode);

    result->code = NULL;
    result->size = 0;
    result->cap  = 0;

    return result;
}

void
stack_push(int32_t val) {
    assert( stack_pointer < MAX_STACK_SIZE );
    stack[stack_pointer++] = val;
}

int32_t
stack_pop() {
    assert(stack_pointer > 0);
    int32_t result = stack[--stack_pointer];

    return result;
}

Env *
env_new(char *name) {
    Env *result = urq_allocs(Env);

    result->name     = name;
    result->syms     = {};
    result->sym_list = NULL;
    result->parent   = NULL;

    return result;
}

Value *
env_fetch(Env *env, char *name) {
    Value *result = (Value *)map_get(&env->syms, name);

    return result;
}

void
env_push(Env *env, char *name, Value *val) {
    map_put(&env->syms, name, val);
    buf_push(env->sym_list, val);
}

void
bytecode_push(Bytecode *bc, Bytecode_Instr instr) {
    if ( bc->size + 1 > bc->cap ) {
        uint32_t cap = ( bc->cap < 256 ) ? 256 : bc->cap*2;
        Bytecode_Instr *code = (Bytecode_Instr *)urq_alloc(cap*sizeof(Bytecode_Instr));
        memcpy((uint8_t *)code, (uint8_t *)bc->code, bc->size*sizeof(Bytecode_Instr));

        bc->code = code;
        bc->cap  = cap;
    }

    bc->code[bc->size] = instr;
    bc->size += 1;
}

Bytecode_Instr
bytecode_instr(uint32_t opcode, uint32_t val_idx) {
    Bytecode_Instr result = {};

    result.opcode  = opcode;
    result.val_idx = val_idx;

    return result;
}

Bytecode_Instr
bytecode_instr(uint32_t opcode, char *name) {
    Bytecode_Instr result = {};

    result.opcode = opcode;
    result.name   = name;

    return result;
}

uint32_t
val_none() {
    Value result = {};
    uint32_t index = (uint32_t)buf_len(bytecode_values);

    result.kind = VAL_NONE;
    result.idx = index;
    result.int_val = 0;
    buf_push(bytecode_values, result);

    return index;
}

uint32_t
val_int(int64_t val) {
    Value result = {};
    uint32_t index = (uint32_t)buf_len(bytecode_values);

    result.kind = VAL_INT;
    result.idx = index;
    result.int_val = val;
    buf_push(bytecode_values, result);

    return index;
}

uint32_t
val_str(char * val) {
    Value result = {};
    uint32_t index = (uint32_t)buf_len(bytecode_values);

    result.kind = VAL_STR;
    result.idx = index;
    result.str_val = val;
    buf_push(bytecode_values, result);

    return index;
}

int32_t
bytecode_expr(Bytecode *bc, Expr *expr) {
    int32_t result = -1;

    switch ( expr->kind ) {
        case EXPR_STR: {
            Expr_Str *e = (Expr_Str *)expr;
            result = val_str(e->val);
            bytecode_push(bc, bytecode_instr(BYTECODEOP_PUSH_CONST, result));
        } break;

        case EXPR_INT: {
            Expr_Int *e = (Expr_Int *)expr;
            result = val_int(e->val);
            bytecode_push(bc, bytecode_instr(BYTECODEOP_PUSH_CONST, result));
        } break;

        case EXPR_IDENT: {
            Expr_Ident *e = (Expr_Ident *)expr;
            bytecode_push(bc, bytecode_instr(BYTECODEOP_LOAD_SYM, e->val));
        } break;

        case EXPR_BIN: {
        } break;

        default: {
            assert(!"unbekannter ausdruck");
        } break;
    }

    return result;
}

void bytecode_stmt(Bytecode *bc, Stmt *stmt) {
    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
            Stmt_Assign *stmt_assign = (Stmt_Assign *)stmt;

            uint32_t lhs_index = bytecode_expr(bc, stmt_assign->lhs);
            uint32_t rhs_index = bytecode_expr(bc, stmt_assign->rhs);

            assert(bytecode_values[lhs_index].kind == bytecode_values[rhs_index].kind);

            switch ( bytecode_values[lhs_index].kind ) {
                case VAL_INT: {
                    bytecode_values[lhs_index].int_val = bytecode_values[rhs_index].int_val;
                } break;

                case VAL_STR: {
                    bytecode_values[lhs_index].str_val = bytecode_values[rhs_index].str_val;
                } break;
            }
        } break;

        case STMT_DECL: {
            Stmt_Decl *stmt_decl = (Stmt_Decl *)stmt;
            Decl *decl = stmt_decl->decl;

            switch ( decl->kind ) {
                case DECL_VAR: {
                    Decl_Var *d = (Decl_Var *)decl;

                    if ( d->expr ) {
                        bytecode_expr(bc, d->expr);
                    }

                    bytecode_push(bc, bytecode_instr(BYTECODEOP_PUSH_SYM, d->name));
                } break;

                case DECL_CONST: {
                    Decl_Const *d = (Decl_Const *)decl;

                    if ( d->expr ) {
                        bytecode_expr(bc, d->expr);
                    }

                    bytecode_push(bc, bytecode_instr(BYTECODEOP_PUSH_SYM, d->name));
                } break;

                default: {
                    assert(0);
                } break;
            }
        } break;

        default: {
            assert(!"unbekannte anweisung");
        } break;
    }
}

Bytecode *
build(Parsed_File *parsed_file) {
    Bytecode *bc = bytecode_new();

    for ( int i = 0; i < buf_len(parsed_file->stmts); ++i ) {
        Stmt *stmt = parsed_file->stmts[i];

        bytecode_stmt(bc, stmt);
    }

    return bc;
}

void eval(Bytecode *bc, Env *env) {
    uint32_t pc = 0;

    while ( pc < bc->size ) {
        Bytecode_Instr instr = bc->code[pc];

        switch ( instr.opcode ) {
            case BYTECODEOP_PUSH_CONST: {
                stack_push(instr.val_idx);
            } break;

            case BYTECODEOP_PUSH_SYM: {
                int32_t val_idx = stack_pop();
                env_push(env, instr.name, &bytecode_values[val_idx]);
            } break;

            case BYTECODEOP_LOAD_SYM: {
                Value *val = env_fetch(env, instr.name);
                stack_push(val->idx);
            } break;

            default: {
                assert(0);
            } break;
        }

        pc++;
    }
}
