Types types;

enum Sym_Kind {
    SYM_NONE,
    SYM_TYPE,
    SYM_VAR,
    SYM_CONST,
    SYM_PROC,
    SYM_NAMESPACE,
};
enum Sym_State {
    SYMSTATE_NONE,
    SYMSTATE_RESOLVING,
    SYMSTATE_RESOLVED,
};
struct Sym : Loc {
    Sym_Kind kind;
    Sym_State state;
    char *name;
    Decl *decl;
    Type *type;
};

struct Scope {
    char *name;
    Map syms;
    Sym **sym_list;
    Scope *parent;

    Module_Syms export_syms;
    size_t      num_export_syms;
};

Scope *sys_scope;
Scope *module_scope;
Scope *global_scope;
Scope *curr_scope;

enum Value_Kind {
    VAL_NONE,
    VAL_CHAR,
    VAL_INT,
    VAL_STR,
    VAL_FLOAT,
    VAL_BOOL,
};
struct Value {
    Value_Kind kind;

    union {
        char    chr;
        int32_t i32;
        float   f32;
        char *  str;
        bool    b;
    };
};

struct Operand {
    Type * type;
    Sym  * sym;
    bool   is_const;
    Value  val;
};

enum Type_Kind {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,

    TYPE_VOID,
    TYPE_CHAR,

    TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,

    TYPE_S8,
    TYPE_S16,
    TYPE_S32,
    TYPE_S64,

    TYPE_F32,
    TYPE_F64,

    TYPE_STRING,
    TYPE_BOOL,
    TYPE_PTR,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_PROC,
    TYPE_ARRAY,
    TYPE_NAMESPACE,
    TYPE_COMPOUND,
    TYPE_VARIADIC,
};

struct Type {
    Type_Kind kind;

    uint32_t size;
    uint32_t align;
    uint32_t id;

    Sym   * sym;
    char  * name;
    Scope * scope;
};

struct Type_Ptr : Type {
    Type *base;
};

struct Type_Array : Type {
    Type   * base;
    size_t   num_elems;
};

struct Type_Compound : Type {
    Compound_Elems elems;
    uint32_t       num_elems;
};

struct Type_Struct : Type {
    Struct_Fields fields;
    size_t        num_fields;
};

struct Type_String : Type {
};

struct Type_Enum : Type {
    Enum_Fields fields;
    size_t num_fields;
};

struct Type_Proc : Type {
    Proc_Params params;
    uint32_t    num_params;
    Proc_Params rets;
    uint32_t    num_rets;
};

struct Type_Variadic : Type {
    Types    types;
    uint32_t num_types;
};

enum { PTR_SIZE = 8 };
uint32_t type_id   = 1;

Type *type_void;
Type *type_char;
Type *type_u8;
Type *type_u16;
Type *type_u32;
Type *type_u64;
Type *type_s8;
Type *type_s16;
Type *type_s32;
Type *type_s64;
Type *type_f32;
Type *type_f64;
Type *type_bool;
Type *type_typeid;
Type *type_string;
Type *type_variadic;

char *
to_str(Expr *expr) {
    switch ( expr->kind ) {
        case EXPR_INT: {
            return "1";
        } break;

        case EXPR_IDENT: {
            return EIDENT(expr)->val;
        } break;

        default: {
            assert(!"unbekannt");
            return "unbekannt";
        } break;
    }
}

char *
to_str(Type *type) {
    char *result = NULL;

    switch ( type->kind ) {
        case TYPE_ARRAY: {
            buf_printf(result, "[] %s", to_str(TARRAY(type)->base));
        } break;

        case TYPE_STRING: {
            buf_printf(result, "string");
        } break;

        default: {
            buf_printf(result, "%s", type->name);
        } break;
    }

    return result;
}

Value
val_char(char val) {
    Value result = {};

    result.kind = VAL_CHAR;
    result.chr  = val;

    return result;
}

Value
val_int(int32_t val) {
    Value result = {};

    result.kind = VAL_INT;
    result.i32  = val;

    return result;
}

Value
val_str(char * val) {
    Value result = {};

    result.kind = VAL_STR;
    result.str  = val;

    return result;
}

Value
val_float(float val) {
    Value result = {};

    result.kind = VAL_FLOAT;
    result.f32  = val;

    return result;
}

Value
val_bool(bool val) {
    Value result = {};

    result.kind = VAL_BOOL;
    result.b    = val;

    return result;
}

Type *
type_new( uint32_t size, Type_Kind kind ) {
    Type *result = urq_allocs(Type);

    result->kind  = kind;
    result->sym   = NULL;
    result->size  = size;
    result->align = 0;
    result->id    = type_id++;
    result->scope = scope_new("type");

    buf_push(types, result);

    return result;
}

Type_String *
type_string_new() {
    Type_String *result = urq_allocs(Type_String);

    result->kind  = TYPE_STRING;
    result->size  = sizeof(uint64_t) + PTR_SIZE;
    result->id    = type_id++;
    result->scope = scope_new("string");

    sym_push_scope(&loc_none, result->scope, "größe", type_u64);

    buf_push(types, result);

    return result;
}

Type_Struct *
type_incomplete_struct(Sym *sym) {
    Type_Struct *result = urq_allocs(Type_Struct);

    result->kind  = TYPE_INCOMPLETE;
    result->sym   = sym;
    result->size  = 0;
    result->align = 0;
    result->id    = type_id++;
    result->scope = scope_new(sym->name);

    buf_push(types, result);

    return result;
}

Type_Enum *
type_incomplete_enum(Sym *sym) {
    Type_Enum *result = urq_allocs(Type_Enum);

    result->kind  = TYPE_INCOMPLETE;
    result->sym   = sym;
    result->size  = 0;
    result->align = 0;
    result->id    = type_id++;
    result->scope = scope_new(sym->name);

    buf_push(types, result);

    return result;
}

Type *
type_namespace(char *name) {
    Type *result = type_new(0, TYPE_NAMESPACE);

    result->name = name;

    buf_push(types, result);

    return result;
}

Type_Compound *
type_compound(Compound_Elems elems, uint32_t num_elems) {
    Type_Compound *result = urq_allocs(Type_Compound);

    result->kind      = TYPE_COMPOUND;
    result->sym       = NULL;
    result->size      = 0;
    result->align     = 0;
    result->id        = type_id++;
    result->scope     = NULL;
    result->elems     = elems;
    result->num_elems = num_elems;

    buf_push(types, result);

    return result;
}

bool
type_isnum(Type *type) {
    bool result = type->kind >= TYPE_U8 && type->kind <= TYPE_F64;

    return result;
}

bool
type_isint(Type *type) {
    bool result = type->kind >= TYPE_U8 && type->kind <= TYPE_S64;

    return result;
}

bool
type_isptr(Type *type) {
    bool result = type->kind == TYPE_PTR;

    return result;
}

bool
type_issigned(Type *type) {
    bool result = type->kind >= TYPE_S8 && type->kind <= TYPE_S64;

    return result;
}

bool
type_isindexable(Type *type) {
    bool result = type->kind == TYPE_ARRAY || type->kind == TYPE_STRING;

    return result;
}

bool
type_iscastable(Type *left, Type *right) {
    if ( left == right ) {
        return true;
    }

    if ( type_isptr(left) && type_isptr(right) ) {
        return type_iscastable(((Type_Ptr *)left)->base, ((Type_Ptr *)right)->base);
    }

    if ( type_isint(left) && type_isint(right) ) {
        if ( type_issigned(left) && type_issigned(right) ) {
            if ( left->size >= right->size ) {
                return true;
            }
        } else if ( !type_issigned(left) && !type_issigned(right) ) {
            if ( left->size >= right->size ) {
                return true;
            }
        }

        return false;
    }

    if ( left->kind == TYPE_STRUCT && right->kind == TYPE_COMPOUND ) {
        if ( TSTRUCT(left)->num_fields > TCMPND(right)->num_elems ) {
            return false;
        }

        for ( int i = 0; i < TSTRUCT(left)->num_fields; ++i ) {
            Struct_Field *field = TSTRUCT(left)->fields[i];
            Compound_Elem *elem = TCMPND(right)->elems[i];

            if ( !type_iscastable(field->type, elem->type) ) {
                return false;
            }
        }

        return true;
    }

    return false;
}

bool
type_are_compatible(Type *left, Type *right) {
    if ( left == right ) {
        return true;
    }

    if ( left->kind == TYPE_ARRAY && right->kind == TYPE_ARRAY ) {
        return type_iscastable(TARRAY(left)->base, TARRAY(right)->base);
    }

    return false;
}

Operand *
operand(Type *type) {
    Operand *result = urq_allocs(Operand);

    result->type     = type;
    result->is_const = false;

    return result;
}

Operand *
operand_const(Type *type) {
    Operand *result = operand(type);

    result->is_const = true;

    return result;
}

Operand *
operand_const(Type *type, Value val) {
    Operand *result = operand(type);

    result->is_const = true;
    result->val      = val;

    return result;
}

Type *
type_cast(Type *left, Type *right) {
    if ( type_iscastable(left, right) ) {
        return left;
    }

    return right;
}

void
operand_cast(Type *type, Operand *op) {
    if ( op->is_const ) {
        op->type = type;
        return;
    }

    op->type = type_cast(type, op->type);
}

Type_Ptr *
type_ptr(Type *base) {
    Type_Ptr *result = urq_allocs(Type_Ptr);

    result->kind = TYPE_PTR;
    result->base = base;

    return result;
}

Type_Array *
type_array(Type *base, size_t num_elems) {
    Type_Array *result = urq_allocs(Type_Array);

    result->kind      = TYPE_ARRAY;
    result->base      = base;
    result->num_elems = num_elems;
    result->scope     = scope_new("array");

    sym_push_scope(&loc_none, result->scope, "größe", type_u64);

    return result;
}

Type_Proc *
type_proc(Proc_Params params, uint32_t num_params, Proc_Params rets, uint32_t num_rets) {
    Type_Proc *result = urq_allocs(Type_Proc);

    result->kind       = TYPE_PROC;
    result->params     = (Proc_Params)MEMDUP(params);
    result->num_params = num_params;
    result->rets       = (Proc_Params)MEMDUP(rets);
    result->num_rets   = num_rets;

    return result;
}

void
type_resolve(Type *type) {
    if ( type->kind == TYPE_COMPLETING ) {
        assert(!"zirkuläre abhängigkeit festgestellt");
    }

    type->kind = TYPE_COMPLETING;
}

Scope *
scope_new(char *name, Scope *parent) {
    Scope *result = urq_allocs(Scope);

    result->name = name;
    result->syms = {};
    result->export_syms = NULL;
    result->num_export_syms = 0;
    result->parent = parent;

    return result;
}

Scope *
scope_set(Scope *scope) {
    Scope *result = curr_scope;
    curr_scope = scope;

    return result;
}

Scope *
scope_enter(char *name = NULL) {
    Scope *result = scope_new(name, curr_scope);

    curr_scope = result;

    return result;
}

void
scope_leave() {
    if ( curr_scope->parent ) {
        curr_scope = curr_scope->parent;
    }
}

void
scope_push(Scope *scope, Sym *sym) {
    Sym *s = (Sym *)map_get(&scope->syms, sym->name);

    if ( s ) {
        report_error(sym, "%s kann nicht nochmal in deklaration verwendet werden", sym->name);
    }

    map_put(&scope->syms, sym->name, sym);
    buf_push(scope->sym_list, sym);
}

void
scope_push_global(Sym *sym) {
    scope_push(global_scope, sym);
}

Sym *
sym_new(Loc *loc, Sym_Kind kind, char *name) {
    Sym *result = urq_allocs(Sym);

    loc_copy(loc, result);

    result->kind = kind;
    result->name = intern_str(name);
    result->decl = NULL;
    result->type = NULL;

    return result;
}

Sym *
sym_new(Loc *loc, char *name) {
    Sym *result = sym_new(loc, SYM_NONE, name);

    return result;
}

Sym *
sym_push_scope(Loc *loc, Scope *scope, char *name, Type *type) {
    Sym *result = sym_new(loc, name);

    result->type = type;
    scope_push(scope, result);

    return result;
}

void
sym_push(Sym *sym) {
    scope_push(curr_scope, sym);
}

Sym *
sym_push(Loc *loc, char *name, Decl *decl) {
    Sym *result = sym_new(loc, name);

    result->decl = decl;
    scope_push(curr_scope, result);

    return result;
}

Sym *
sym_push_namespace(char *name, Type *type) {
    Sym *result = sym_push(&loc_none, name, NULL);

    result->kind  = SYM_NAMESPACE;
    result->state = SYMSTATE_RESOLVED;
    result->type  = type;

    return result;
}

Sym *
sym_push_type(Loc *loc, char *name, Decl *decl) {
    Sym *result = sym_push(&loc_none, name, decl);

    result->kind = SYM_TYPE;

    return result;
}

Sym *
sym_push_var(Loc *loc, char *name, Decl *decl) {
    Sym *result = sym_push(loc, name, NULL);

    result->kind  = SYM_VAR;
    result->state = SYMSTATE_RESOLVED;

    return result;
}

Sym *
sym_push_var(Loc *loc, char *name, Type *type) {
    Sym *result = sym_push(loc, name, NULL);

    result->kind  = SYM_VAR;
    result->state = SYMSTATE_RESOLVED;
    result->type  = type;

    return result;
}

Sym *
sym_push_sys(char *name, Type *type) {
    Sym *result = sym_new(&loc_none, intern_str(name));

    type->name = result->name;

    result->kind  = SYM_TYPE;
    result->state = SYMSTATE_RESOLVED;
    result->type  = type;
    scope_push(sys_scope, result);

    return result;
}

Sym *
sym_get(Scope *scope, char *name) {
    Scope *i = scope;

    while ( i ) {
        Sym *sym = (Sym *)map_get(&i->syms, name);

        if ( sym ) {
            return sym;
        }

        i = i->parent;
    }

    return NULL;
}

Sym *
sym_get(char *name) {
    Sym *result = sym_get(curr_scope, name);

    return result;
}

void
sym_resolve(Sym *sym) {
    if ( sym->state == SYMSTATE_RESOLVED ) {
        return;
    }

    if ( sym->state == SYMSTATE_RESOLVING ) {
        assert(!"zirkuläre abhängigkeit festgestellt");
    }

    sym->state = SYMSTATE_RESOLVING;

    switch ( sym->kind ) {
        case SYM_TYPE: {
            sym->type = resolve_decl_type(sym->decl);
        } break;

        case SYM_VAR: {
            sym->type = resolve_decl_var(sym->decl);
        } break;

        case SYM_CONST: {
            sym->type = resolve_decl_const(sym->decl);
        } break;

        case SYM_PROC: {
            sym->type = resolve_decl_proc(sym->decl);
        } break;

        default: {
            assert(!"nicht unterstütztes symbol");
        } break;
    }

    sym->state = SYMSTATE_RESOLVED;
}

Sym *
resolve_name(char *name) {
    Sym *sym = sym_get(name);

    sym_resolve(sym);

    return sym;
}

Operand *
resolve_expr(Expr *expr, Type *given_type = NULL) {
    Operand *result = NULL;

    if ( !expr ) {
        return result;
    }

    switch ( expr->kind ) {
        case EXPR_CHAR: {
            result = operand_const(type_char, val_char(ECHR(expr)->val));
        } break;

        case EXPR_STR: {
            result = operand_const(type_string, val_str(ESTR(expr)->val));
        } break;

        case EXPR_INT: {
            result = operand_const(type_u32, val_int((int32_t)EINT(expr)->val));
        } break;

        case EXPR_FLOAT: {
            result = operand_const(type_f32, val_float(EFLOAT(expr)->val));
        } break;

        case EXPR_BOOL: {
            result = operand_const(type_bool, val_bool(EBOOL(expr)->val));
        } break;

        case EXPR_IDENT: {
            Sym *sym = resolve_name(EIDENT(expr)->val);

            if ( !sym ) {
                report_error(expr, "unbekanntes symbol: %s", EIDENT(expr)->val);
            }

            EIDENT(expr)->sym = sym;
            result = operand(sym->type);
            result->sym = sym;

            result->is_const = sym->kind == SYM_CONST;
        } break;

        case EXPR_NEW: {
            Operand *op = resolve_expr(ENEW(expr)->expr);

            if ( !op->type ) {
                report_error(expr, "fehlender datentyp für new anweisung");
            }

            assert(op->sym);
            if ( op->sym->kind != SYM_TYPE ) {
                report_error(ENEW(expr)->expr, "ausdruck muß ein datentyp sein");
            }

            result = operand(type_ptr(op->type));
        } break;

        case EXPR_CAST: {
            Type *type_to_cast_to = resolve_typespec(ECAST(expr)->typespec);
            Operand *type_to_cast = resolve_expr(ECAST(expr)->expr);

            /* @AUFGABE: überprüfen ob der datentyp umgewandelt werden darf/kann */
            result = operand(type_to_cast_to);
        } break;

        case EXPR_SIZEOF: {
            Type *type = resolve_typespec(ESIZEOF(expr)->typespec);

            result = operand(type_u32);
        } break;

        case EXPR_TYPEINFO: {
            Operand *op = resolve_expr(ETYPEINFO(expr)->expr);

            if ( !type_isint(op->type) ) {
                report_error(expr, "eine typid erwartet, aber %s bekommen", to_str(op->type));
            }

            Sym *sym = sym_get(intern_str("Datentyp"));

            if ( !sym ) {
                report_error(expr, "Interner Compilerfehler: symbol \"Datentyp\" konnte nicht ermittelt werden");
            }

            result = operand(sym->type);
            result->sym = sym;
        } break;

        case EXPR_TYPEOF: {
            Operand *op = resolve_expr(ETYPEOF(expr)->expr);

            result = operand(type_u32);
        } break;

        case EXPR_KEYWORD: {
            assert(!"schlüsselwort");
        } break;

        case EXPR_UNARY: {
            Operand *op = resolve_expr(EUNARY(expr)->expr);

            if ( !type_isnum(op->type) ) {
                report_error(expr, "numerischen ausdruck erwartet");
            }

            result = operand(type_s32);
        } break;

        case EXPR_BIN: {
            Operand *left = resolve_expr(EBIN(expr)->left);
            Operand *right = resolve_expr(EBIN(expr)->right);

            if ( EBIN(expr)->op >= OP_CMP_START && EBIN(expr)->op <= OP_CMP_END ) {
                result = operand(type_bool);
            } else if ( EBIN(expr)->op >= OP_MATH_START && EBIN(expr)->op <= OP_MATH_END ) {
                if ( left->is_const && right->is_const ) {
                    result = operand_const(left->type);
                } else {
                    result = operand(left->type);
                }
            }

            result->is_const = left->is_const && right->is_const;
        } break;

        case EXPR_AT: {
            Operand *op = resolve_expr(EAT(expr)->expr);

            result = operand(type_ptr(op->type));
        } break;

        case EXPR_FIELD: {
            Operand *base = resolve_expr(EFIELD(expr)->base);
            assert(base->type);
            assert( base->type->scope );

            Sym *sym = sym_get(base->type->scope, EFIELD(expr)->field);
            if ( !sym ) {
                report_error(expr, "symbol %s konnte in %s nicht ermittelt werden", EFIELD(expr)->field, base->type->sym->name);
            }

            result = operand(sym->type);
        } break;

        case EXPR_INDEX: {
            Operand *base = resolve_expr(EINDEX(expr)->base);
            assert(base->type && type_isindexable(base->type));
            Operand *index = resolve_expr(EINDEX(expr)->index);

            if ( !type_isint(index->type) ) {
                report_error(EINDEX(expr)->index, "index muß vom datentyp int sein, bekommen %s", to_str(index->type));
            }

            if ( IS_TARRAY(base->type) ) {
                result = operand(TARRAY(base->type)->base);
            } else {
                assert(IS_TSTR(base->type));
                result = operand(type_char);
            }
        } break;

        case EXPR_PAREN: {
            result = resolve_expr(EPAREN(expr)->expr);
        } break;

        case EXPR_CALL: {
            Operand *op = resolve_expr(ECALL(expr)->base);
            type_complete(op->type);
            assert(op->type && op->type->kind == TYPE_PROC);

            Type_Proc *op_type = TPROC(op->type);

            if ( op_type->num_params != ECALL(expr)->num_args ) {
                report_error(expr, "argumente übergeben %d, erwartet wurden %d", ECALL(expr)->num_args, op_type->num_params);
            }

            for ( uint32_t i = 0; i < TPROC(op->type)->num_params; ++i ) {
                Proc_Param *param = TPROC(op->type)->params[i];

                if ( param->type == type_variadic ) {
                    break;
                }

                Operand *arg = resolve_expr(ECALL(expr)->args[i], param->type);

                operand_cast(param->type, arg);
                /* @AUFGABE: array und zeiger datentypen auf kompatibilität überprüfen */
                if ( !type_are_compatible(param->type, arg->type) ) {
                    report_error(expr, "datentyp erwartet %s, bekommen %s", to_str(param->type), to_str(arg->type));
                }
            }

            if ( TPROC(op->type)->num_rets ) {
                result = operand(TPROC(op->type)->rets[0]->type);
            } else {
                result = operand(type_void);
            }
        } break;

        case EXPR_RANGE: {
            Operand *left  = resolve_expr(ERNG(expr)->left);
            Operand *right = resolve_expr(ERNG(expr)->right);

            result = operand(left->type);
        } break;

        case EXPR_TUPLE: {
            assert(!"in arbeit");
        } break;

        case EXPR_COMPOUND: {
            assert(given_type);

            if ( given_type->kind == TYPE_ARRAY ) {
                if ( TARRAY(given_type)->num_elems ) {
                    if ( ECMPND(expr)->num_elems != TARRAY(given_type)->num_elems ) {
                        report_error(expr, "erwartete anzahl der ausdrücke %d, bekommen %d", TARRAY(given_type)->num_elems, ECMPND(expr)->num_elems);
                    }
                } else {
                    TARRAY(given_type)->num_elems = ECMPND(expr)->num_elems;
                }

                for ( int i = 0; i < ECMPND(expr)->num_elems; ++i ) {
                    Compound_Elem *arg = ECMPND(expr)->elems[i];
                    Operand *op = resolve_expr(arg->value);

                    operand_cast(TARRAY(given_type)->base, op);
                    if ( TARRAY(given_type)->base != op->type ) {
                        report_error(arg, "datentyp erwartet %s, bekommen %s", TARRAY(given_type)->base, op->type);
                    }
                }

                result = operand(given_type);
            } else {
                assert(given_type->kind == TYPE_STRUCT);

                if ( TSTRUCT(given_type)->num_fields != ECMPND(expr)->num_elems ) {
                    report_error(expr, "anzahl der argumente erwartet %d, bekommen %d", TSTRUCT(given_type)->num_fields, ECMPND(expr)->num_elems);
                }

                if ( ECMPND(expr)->is_named ) {
                    assert(!"benamtes compound");
                } else {
                    Compound_Elems args = NULL;
                    for ( int i = 0; i < TSTRUCT(given_type)->num_fields; ++i ) {
                        Struct_Field  * struct_field = TSTRUCT(given_type)->fields[i];
                        Compound_Elem * arg          = ECMPND(expr)->elems[i];

                        if ( arg->name ) {
                            assert(!"unbehandelter fall: benamtes argument");
                        } else {
                            Operand *operand = resolve_expr(arg->value);
                            operand_cast(struct_field->type, operand);

                            if ( struct_field->type != operand->type ) {
                                report_error(arg, "datentyp erwartet %s, bekommen %s", struct_field->type->name, operand->type->name);
                            }

                            arg->type = struct_field->type;
                            buf_push(args, arg);
                        }
                    }
                }

                result = operand(given_type);
            }
        } break;

        default: {
            report_error(expr, "unbekannter ausdruck");
        } break;
    }

    expr->type = result->type;

    return result;
}

Type *
resolve_typespec(Typespec *typespec) {
    Type *result = NULL;

    if ( !typespec ) {
        return result;
    }

    switch ( typespec->kind ) {
        case TYPESPEC_PTR: {
            result = type_ptr(resolve_typespec(TSPTR(typespec)->base));
        } break;

        case TYPESPEC_NAME: {
            Sym *sym = sym_get(TSNAME(typespec)->name);

            if ( !sym ) {
                report_error(typespec, "keinen datentyp %s gefunden", TSNAME(typespec)->name);
            }

            if ( sym->kind != SYM_TYPE ) {
                report_error(typespec, "symbol %s muß ein datentyp sein", sym->name);
            }

            result = sym->type;
        } break;

        case TYPESPEC_ARRAY: {
            Type *type = resolve_typespec(TSARRAY(typespec)->base);

            if ( TSARRAY(typespec)->num_elems ) {
                Operand *op = resolve_expr(TSARRAY(typespec)->num_elems);

                operand_cast(type_u32, op);
                if ( op->type != type_u32 ) {
                    report_error(TSARRAY(typespec)->num_elems, "numerischen datentyp erwartet");
                }

                if ( op->is_const ) {
                    assert(op->val.kind != VAL_NONE);
                    result = type_array(type, op->val.i32);
                } else {
                    result = type_array(type, 0);
                }
            } else {
                result = type_array(type, 0);
            }
        } break;

        case TYPESPEC_VARARG: {
            result = type_variadic;
        } break;

        case TYPESPEC_PROC: {
        } break;

        default: {
            report_error(typespec, "unbekannter typespec");
        } break;
    }

    if ( result ) {
        typespec->type = result;
    }

    return result;
}

Type_Proc *
resolve_decl_proc(Decl *decl) {
    assert(decl->kind == DECL_PROC);

    Proc_Sign *sign = DPROC(decl)->sign;
    for ( size_t i = 0; i < sign->num_params; ++i ) {
        sign->params[i]->type = resolve_typespec(sign->params[i]->typespec);
    }

    for ( size_t i = 0; i < DPROC(decl)->sign->num_rets; ++i ) {
        sign->rets[i]->type = resolve_typespec(sign->rets[i]->typespec);
    }

    return type_proc(sign->params, sign->num_params, sign->rets, sign->num_rets);
}

Type *
resolve_decl_const(Decl *decl) {
    assert(decl->kind == DECL_CONST);

    Type *type = resolve_typespec(DVAR(decl)->typespec);
    Operand *op = resolve_expr(DVAR(decl)->expr);

    if ( !op->is_const ) {
        report_error(decl, "konstanten wert erwartet");
    }

    if ( !type ) {
        type = op->type;
    }

    /* @AUFGABE: prüfen ob type aus dem typespec und dem op passen */
    operand_cast(type, op);
    if ( type != op->type ) {
        report_error(decl, "datentyp erwartet %s, bekommen %s", type->name, op->type->name);
    }

    type_complete(type);

    return type;
}

Type *
resolve_decl_type(Decl *decl) {
    Type *result = resolve_typespec(DTYPE(decl)->typespec);

    return result;
}

Type *
resolve_decl_var(Decl *decl) {
    assert(decl->kind == DECL_VAR);

    Type *type = resolve_typespec(DVAR(decl)->typespec);
    Operand *op = resolve_expr(DVAR(decl)->expr, type);

    if ( !type && !op ) {
        report_error(decl, "datentyp der variable %s konnte nicht ermittelt werden", DVAR(decl)->name);
    }

    if ( !type ) {
        type = op->type;
    }

    if ( op ) {
        operand_cast(type, op);
        if ( type != op->type ) {
            report_error(decl, "datentyp erwartet %s, bekommen %s", type->name, op->type->name);
        }
    }

    type_complete(type);

    return type;
}

Type *
resolve_decl(Decl *decl) {
    Type *result = NULL;

    switch ( decl->kind ) {
        case DECL_CONST: {
            Type *type = resolve_typespec(DCONST(decl)->typespec);
            Operand *op = resolve_expr(DCONST(decl)->expr);

            if ( !type && op->type ) {
                type = op->type;
            }

            /* @AUFGABE: prüfen ob type aus dem typespec und dem op passen */

            result = type;
        } break;

        case DECL_API: {
        } break;

        case DECL_IMPL: {
        } break;

        default: {
            report_error(decl, "unbekannte deklaration");
        } break;
    }

    return result;
}

bool
resolve_stmt(Stmt *stmt, Types rets, uint32_t num_rets) {
    bool result = false;

    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
            Operand *lhs = resolve_expr(SASSIGN(stmt)->lhs);
            Operand *rhs = resolve_expr(SASSIGN(stmt)->rhs);

            if ( lhs->is_const ) {
                report_error(stmt, "versuch der konstanten %s einen wert zuzuweisen", to_str(SASSIGN(stmt)->lhs));
            }

            operand_cast(lhs->type, rhs);
            if ( lhs->type != rhs->type ) {
                report_error(stmt, "datentyp erwartet %s, bekommen %s", lhs->type->name, rhs->type->name);
            }
        } break;

        case STMT_BLOCK: {
            for ( int i = 0; i < SBLOCK(stmt)->num_stmts; ++i ) {
                result = result || resolve_stmt(SBLOCK(stmt)->stmts[i], rets, num_rets);
            }
        } break;

        case STMT_DECL: {
            Decl *decl = SDECL(stmt)->decl;

            switch ( decl->kind ) {
                case DECL_VAR: {
                    Type *type = resolve_decl_var(decl);
                    type_complete(type);
                    Sym *sym = sym_push_var(decl, decl->name, type);
                } break;

                default: {
                    report_error(decl, "unbekannte deklaration");
                } break;
            }
        } break;

        case STMT_EXPR: {
            Operand *operand = resolve_expr(SEXPR(stmt)->expr);
        } break;

        case STMT_FOR: {
            char *it = NULL;

            if ( SFOR(stmt)->it ) {
                assert(SFOR(stmt)->it->kind == EXPR_IDENT);
                it = EIDENT(SFOR(stmt)->it)->val;
            } else {
                it = intern_str("it");
            }

            Operand *cond = resolve_expr(SFOR(stmt)->cond);
            Type *type = cond->type;

            if ( type->kind == TYPE_ARRAY ) {
                type = TARRAY(type)->base;
            }

            scope_enter("for-loop");
            Sym *sym = sym_push_var(stmt, it, type);
            resolve_stmt(SFOR(stmt)->block);
            scope_leave();

            if ( SFOR(stmt)->stmt_else ) {
                scope_enter("for-else");
                resolve_stmt(SFOR(stmt)->stmt_else);
                scope_leave();
            }
        } break;

        case STMT_IF: {
            Operand *op = resolve_expr(SIF(stmt)->cond);

            if ( op->type != type_bool ) {
                report_error(stmt, "boolischen ausdruck erwartet, bekommen %s", op->type->name);
            }

            scope_enter();
            resolve_stmt(SIF(stmt)->stmt);
            scope_leave();

            if ( SIF(stmt)->stmt_else ) {
                resolve_stmt(SIF(stmt)->stmt_else);
            }
        } break;

        case STMT_WHILE: {
            Operand *cond = resolve_expr(SWHILE(stmt)->cond);
            assert(cond->type->kind == TYPE_BOOL);
            resolve_stmt(SWHILE(stmt)->block);
        } break;

        case STMT_MATCH: {
            /* @AUFGABE: match kann result setzen */
            Operand *op = resolve_expr(SMATCH(stmt)->expr);

            for ( int i = 0; i < SMATCH(stmt)->num_lines; ++i ) {
                Match_Line *line = SMATCH(stmt)->lines[i];

                Operand *res_op = resolve_expr(line->resolution);

                operand_cast(op->type, res_op);
                if ( op->type != res_op->type ) {
                    report_error(line->resolution, "datentyp erwartet %s, bekommen %s", res_op->type->name, op->type->name);
                }

                resolve_stmt(line->stmt);
            }
        } break;

        case STMT_RET: {
            if ( curr_scope == global_scope ) {
                report_error(stmt, "return an dieser stelle nicht erlaubt.");
            }

            if ( SRET(stmt)->num_exprs < num_rets ) {
                report_error(stmt, "rückgabewerte erwartet %d, übergeben bekommen %d",
                        num_rets, SRET(stmt)->num_exprs);
            }

            if ( SRET(stmt)->num_exprs > 0 ) {
                for ( uint32_t i = 0; i < num_rets; ++i ) {
                    Operand *operand = resolve_expr(SRET(stmt)->exprs[i]);

                    operand_cast(rets[i], operand);
                    if ( rets[i] != operand->type ) {
                        report_error(SRET(stmt)->exprs[i], "rückgabewert vom datentyp erwartet %s, bekommen %s", rets[i]->name, operand->type->name);
                    }
                }
            }

            result = true;
        } break;

        case STMT_USING: {
            Operand *op = resolve_expr(SUSING(stmt)->expr);
            type_complete(op->type);

            for ( int i = 0; i < buf_len(op->type->scope->sym_list); ++i ) {
                Sym *sym = op->type->scope->sym_list[i];
                sym_push(sym);
            }
        } break;

        default: {
            report_error(stmt, "unbekannte anweisung");
        } break;
    }

    return result;
}

void
resolve_directive(Directive *dir) {
    switch ( dir->kind ) {
        case DIRECTIVE_IMPORT: {
            Scope *scope = scope_new("import", module_scope);
            Scope *prev_scope = scope_set(scope);
            resolve_file(DIRIMPORT(dir)->parsed_file);
            scope_set(prev_scope);

            Scope *push_scope = curr_scope;
            if ( DIRIMPORT(dir)->scope_name ) {
                Type *type = type_namespace(DIRIMPORT(dir)->scope_name);
                Sym *sym   = sym_push_namespace(DIRIMPORT(dir)->scope_name, type);

                type->sym = sym;
                type->scope->name = DIRIMPORT(dir)->scope_name;
                type->scope->parent = curr_scope;

                curr_scope = type->scope;
                push_scope = type->scope;
            }

            /* @INFO: überprüfen ob export_syms im scope gesetzt wurden */
            if ( scope->num_export_syms ) {
                for ( int i = 0; i < scope->num_export_syms; ++i ) {
                    Module_Sym *module_sym = scope->export_syms[i];

                    Sym *export_sym = sym_get(scope, module_sym->name);

                    if ( export_sym ) {
                        bool actually_import = false;
                        if ( DIRIMPORT(dir)->wildcard ) {
                            actually_import = true;
                        } else {
                            for ( int j = 0; j < DIRIMPORT(dir)->num_syms; ++j ) {
                                Module_Sym *dir_sym = DIRIMPORT(dir)->syms[j];

                                if ( dir_sym->name == export_sym->name ) {
                                    actually_import = true;
                                    break;
                                }
                            }
                        }

                        if ( actually_import ) {
                            char *export_name = (module_sym->alias) ? module_sym->alias : module_sym->name;
                            Sym *push_sym = sym_new(module_sym, export_sym->kind, export_name);

                            push_sym->state = export_sym->state;
                            push_sym->decl  = export_sym->decl;
                            push_sym->type  = export_sym->type;

                            scope_push(push_scope, push_sym);
                        }
                    }
                }

            /* @INFO: ansonsten alle symbole exportieren */
            } else {
                for ( int i = 0; i < buf_len(scope->sym_list); ++i ) {
                    Sym *sym = scope->sym_list[i];

                    for ( int j = 0; j < DIRIMPORT(dir)->num_syms; ++j ) {
                        Module_Sym *mod_sym = DIRIMPORT(dir)->syms[j];

                        if ( mod_sym->name == sym->name || DIRIMPORT(dir)->wildcard ) {
                            Sym *push_sym = sym;

                            if ( mod_sym->alias ) {
                                push_sym = sym_new(mod_sym, mod_sym->alias);
                                push_sym->decl = sym->decl;
                                push_sym->type = sym->type;
                            }

                            scope_push(push_scope, push_sym);
                        }
                    }
                }
            }

            if ( DIRIMPORT(dir)->scope_name ) {
                scope_leave();
            }
        } break;

        case DIRECTIVE_EXPORT: {
            curr_scope->export_syms     = DIREXPORT(dir)->syms;
            curr_scope->num_export_syms = DIREXPORT(dir)->num_syms;
        } break;

        case DIRECTIVE_LOAD: {
            resolve_file(DIRLOAD(dir)->parsed_file);
        } break;

        default: {
            report_error(dir, "unbekannte direktive");
        } break;
    }
}

void
resolve_directives(Directive **directives) {
    for ( int i = 0; i < buf_len(directives); ++i ) {
        Directive *dir = directives[i];
        resolve_directive(dir);
    }
}

void
type_complete_struct(Type_Struct *type) {
    Decl *decl = type->sym->decl;

    assert(decl->kind == DECL_STRUCT);
    type->scope = scope_enter(decl->name);

    if ( !DSTRUCT(decl)->num_fields ) {
        report_error(decl, "datenstruktur %s muss mindestens ein feld enthalten", decl->name);
    }

    type->kind = TYPE_STRUCT;

    for ( size_t i = 0; i < DSTRUCT(decl)->num_fields; i++ ) {
        Struct_Field *field = DSTRUCT(decl)->fields[i];

        Type *field_type = 0;
        if ( field->typespec ) {
            field_type = resolve_typespec(field->typespec);
        }

        Operand *operand = NULL;
        if ( field->default_value ) {
            operand = resolve_expr(field->default_value, field_type);
        }

        if ( !field_type ) {
            assert(operand && operand->type);
            field_type = operand->type;
        }

        if ( !field_type ) {
            report_error(field, "datentyp des feldes %s konnte nicht ermittelt werden", field->name);
        }

        type_complete(field_type);

        field->type = field_type;
        field->operand = operand;

        sym_push_var(field, field->name, field_type);
    }

    scope_leave();

    uint32_t offset = 0;
    uint32_t align = 0;

    for ( uint32_t i = 0; i < DSTRUCT(decl)->num_fields; ++i ) {
        type->size += DSTRUCT(decl)->fields[i]->type->size;
        type->align = MAX(type->size, DSTRUCT(decl)->fields[i]->type->align);
        DSTRUCT(decl)->fields[i]->offset = offset;
        offset += DSTRUCT(decl)->fields[i]->type->size;
    }

    ((Type_Struct *)type)->fields     = DSTRUCT(decl)->fields;
    ((Type_Struct *)type)->num_fields = DSTRUCT(decl)->num_fields;
}

void
type_complete_enum(Type_Enum *type) {
    Decl *decl = type->sym->decl;

    assert(decl->kind == DECL_ENUM);
    type->scope = scope_enter(decl->name);

    if ( !DENUM(decl)->num_fields ) {
        report_error(decl, "datenstruktur %s muss mindestens ein feld enthalten", decl->name);
    }

    for ( size_t i = 0; i < DENUM(decl)->num_fields; i++ ) {
        Enum_Field *field = DENUM(decl)->fields[i];

        Operand *op = NULL;
        if ( field->value ) {
            op = resolve_expr(field->value, type_s32);
        }

        if ( op ) {
            if ( !type_isnum(op->type) ) {
                report_error(field->value, "datentyp eines enumeration-feldes muß numerisch sein, stattdessen ist es %s", to_str(op->type));
            }
        } else {
            op = operand(type_s32);
        }

        field->type = type_s32;
        field->operand = op;

        sym_push_var(field, field->name, type_s32);
    }

    scope_leave();
    type->kind = TYPE_ENUM;

    uint32_t align = 0;

    for ( uint32_t i = 0; i < DENUM(decl)->num_fields; ++i ) {
        type->size += DENUM(decl)->fields[i]->type->size;
        type->align = MAX(type->size, DENUM(decl)->fields[i]->type->align);
    }

    ((Type_Enum *)type)->fields     = DENUM(decl)->fields;
    ((Type_Enum *)type)->num_fields = DENUM(decl)->num_fields;
}

void
type_complete(Type *type) {
    if ( type->kind == TYPE_COMPLETING ) {
        assert(!"zirkuläre abhängigkeit festgestellt!");
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

    type->kind = TYPE_COMPLETING;
    Decl *decl = type->sym->decl;

    if ( decl->kind == DECL_STRUCT ) {
        type_complete_struct((Type_Struct *)type);
    } else {
        type_complete_enum((Type_Enum *)type);
    }
}

void
resolve_proc(Sym *sym) {
    Decl_Proc *decl = DPROC(sym->decl);
    assert(sym->state == SYMSTATE_RESOLVED);
    Proc_Sign *sign = decl->sign;

    scope_enter(decl->name);
    for ( uint32_t i = 0; i < sign->num_params; ++i ) {
        Proc_Param *param = sign->params[i];
        sym_push_var(param, param->name, resolve_typespec(param->typespec));
    }

    Types ret_types = NULL;
    for ( uint32_t i = 0; i < sign->num_rets; ++i ) {
        sign->rets[i]->type = resolve_typespec(sign->rets[i]->typespec);
        buf_push(ret_types, sign->rets[i]->type);
    }

    bool returns = false;
    if ( !sign->sys_call ) {
        returns = resolve_stmt(decl->block, ret_types, buf_len(ret_types));
    }

    scope_leave();

    if ( sign->sys_call || sign->num_rets == 0 ) {
    } else if ( !returns ) {
        report_error(decl, "nicht alle zweige liefern einen wert zurueck!");
    }
}

void
register_global_syms(Stmts stmts) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        if (stmts[i]->kind != STMT_DECL) {
            continue;
        }

        Stmt_Decl *stmt = (Stmt_Decl *)stmts[i];

        if ( stmt->decl->kind == DECL_IMPL ) {
            continue;
        }

        Decl *decl = stmt->decl;
        Sym *sym = sym_push(decl, decl->name, decl);
        decl->sym = sym;

        switch ( decl->kind ) {
            case DECL_TYPE:
            case DECL_ENUM:
            case DECL_STRUCT: {
                sym->kind = SYM_TYPE;
            } break;

            case DECL_VAR: {
                sym->kind = SYM_VAR;
            } break;

            case DECL_CONST: {
                sym->kind = SYM_CONST;
            } break;

            case DECL_PROC: {
                sym->kind = SYM_PROC;
            } break;
        }

        if ( decl->kind == DECL_STRUCT || decl->kind == DECL_ENUM) {
            sym->state = SYMSTATE_RESOLVED;

            if ( decl->kind == DECL_STRUCT ) {
                sym->type = type_incomplete_struct(sym);
            } else {
                assert(decl->kind == DECL_ENUM);
                sym->type = type_incomplete_enum(sym);
            }
        }
    }
}

void
sym_finalize(Sym *sym) {
    sym_resolve(sym);

    if ( sym->kind == SYM_TYPE ) {
        type_complete(sym->type);
    } else if ( sym->kind == SYM_PROC ) {
        resolve_proc(sym);
    }
}

void
resolve_file(Parsed_File *parsed_file) {
    resolve_directives(parsed_file->directives);
    register_global_syms(parsed_file->stmts);

    for ( int i = 0; i < buf_len(parsed_file->stmts); ++i ) {
        Stmt *stmt = parsed_file->stmts[i];

        if ( stmt->kind == STMT_DECL ) {
            Sym *sym = sym_get(SDECL(stmt)->decl->name);
            assert(sym);

            if ( sym->decl ) {
                sym_finalize(sym);
            }
        } else {
            resolve_stmt(stmt);
        }
    }
}

void
resolve(Parsed_File *parsed_file, bool check_entry_point) {
    resolve_file(parsed_file);

    if ( check_entry_point ) {
        Sym *entry_point_sym = sym_get(global_scope, intern_str(entry_point));
        if ( !entry_point_sym ) {
            report_error(&loc_none, "einstiegspunkt \"%s\" wurde nicht gefunden", entry_point);
        }

        Type *type = entry_point_sym->type;
        if ( type->kind != TYPE_PROC ) {
            report_error(entry_point_sym->decl, "\"%s\" muss eine prozedur sein", entry_point);
        }

        if ( TPROC(type)->num_params != 1 ) {
            report_error(entry_point_sym->decl, "\"%s\" muss einen parameter entgegennehmen", entry_point);
        }

        Proc_Param *param = TPROC(type)->params[0];
        if ( param->type->kind != TYPE_ARRAY || TARRAY(param->type)->base->kind != TYPE_STRING ) {
            report_error(&loc_none, "%s muss ein [] string sein", param->name);
        }
    }
}

void
resolver_load_sys_modules() {
    char *ext = ".sss";
    char *sss_dir = Urq::Os::os_env("SSS_DIR");
    if ( !sss_dir ) {
        report_error(&loc_none, "SSS_DIR umgebungsvariable setzen");
    }

    char *content = "";
    Urq::Os::os_file_read(path_concat(sss_dir, "typeinfo", ext), &content);

    auto tokens   = tokenize("typeinfo", content);
    auto parsed   = parse(&tokens);

    Scope *curr_scope_prev = curr_scope;
    curr_scope = module_scope;
    resolve(parsed, false);
    curr_scope = curr_scope_prev;
}

void
resolver_init() {
    sys_scope     = scope_new("sys");
    module_scope  = scope_new("module", sys_scope);
    global_scope  = scope_new("global", module_scope);
    curr_scope    = global_scope;

    type_void     = type_new(0, TYPE_VOID);
    type_char     = type_new(1, TYPE_CHAR);
    type_u8       = type_new(1, TYPE_U8);
    type_u16      = type_new(2, TYPE_U16);
    type_u32      = type_new(4, TYPE_U32);
    type_u64      = type_new(8, TYPE_U64);
    type_s8       = type_new(1, TYPE_S8);
    type_s16      = type_new(2, TYPE_S16);
    type_s32      = type_new(4, TYPE_S32);
    type_s64      = type_new(8, TYPE_S64);
    type_f32      = type_new(4, TYPE_F32);
    type_f64      = type_new(8, TYPE_F64);
    type_bool     = type_new(1, TYPE_BOOL);
    type_string   = type_string_new();

    type_variadic = type_new(0, TYPE_VARIADIC);

    sym_push_sys("void",   type_void);
    sym_push_sys("char",   type_char);
    sym_push_sys("u8",     type_u8);
    sym_push_sys("u16",    type_u16);
    sym_push_sys("u32",    type_u32);
    sym_push_sys("u64",    type_u64);
    sym_push_sys("s8",     type_s8);
    sym_push_sys("s16",    type_s16);
    sym_push_sys("s32",    type_s32);
    sym_push_sys("s64",    type_s64);
    sym_push_sys("f32",    type_f32);
    sym_push_sys("f64",    type_f64);
    sym_push_sys("bool",   type_bool);
    sym_push_sys("string", type_string);

    resolver_load_sys_modules();
}
