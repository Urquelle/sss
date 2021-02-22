struct Operand;
struct Proc_Sign;
struct Resolved_Stmt;
struct Scope;
struct Type_Field;
struct Type_Proc;
struct Type_Struct;

typedef Operand       ** Operands;
typedef Resolved_Stmt ** Resolved_Stmts;
typedef Sym           ** Syms;
typedef Type          ** Types;
typedef Type_Field    ** Type_Fields;

Resolved_Stmts   resolve(Parsed_File *parsed_file);
Type           * resolve_decl(Decl *d);
Type           * resolve_decl_const(Decl *decl);
Type_Proc      * resolve_decl_proc(Decl *decl);
Type           * resolve_decl_type(Decl *decl);
Type           * resolve_decl_var(Decl *decl);
Resolved_Stmts   resolve_stmt(Stmt *stmt);
Scope          * scope_new(char *name, Scope *parent = NULL);
void             type_complete(Type *type);

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
struct Sym {
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
Scope *global_scope;
Scope *curr_scope;

struct Operand {
    Type *type;
    bool is_const;
};

enum Type_Kind {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,

    TYPE_VOID,

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

    TYPE_TYPEID,
    TYPE_STRING,
    TYPE_BOOL,
    TYPE_PTR,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_PROC,
    TYPE_ARRAY,
    TYPE_NAMESPACE,
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

struct Type_Field {
    char    * name;
    Type    * type;
    Operand * operand;
};
struct Type_Struct : Type {
    Struct_Fields fields;
    size_t        num_fields;
};

struct Type_Enum : Type {
    Type_Fields fields;
    size_t num_fields;
};

struct Type_Proc : Type {
    Proc_Params params;
    uint32_t    num_params;
    Proc_Params rets;
    uint32_t    num_rets;
};

struct Resolved_Stmt {
    Stmt    * stmt;
    Sym     * sym;
    Type    * type;
    Operand * operand;
};

enum { PTR_SIZE = 8 };
uint32_t type_id   = 1;

Type *type_void;
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

Resolved_Stmt *
resolved_stmt(Stmt *stmt, Sym *sym, Type *type, Operand *operand) {
    Resolved_Stmt *result = urq_allocs(Resolved_Stmt);

    result->stmt    = stmt;
    result->sym     = sym;
    result->type    = type;
    result->operand = operand;

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

    return result;
}

Type *
type_namespace() {
    Type *result = type_new(0, TYPE_NAMESPACE);

    return result;
}

bool
type_isint(Type *type) {
    bool result = type->kind >= TYPE_U8 && type->kind <= TYPE_S64;

    return result;
}

bool
type_issigned(Type *type) {
    bool result = type->kind >= TYPE_S8 && type->kind <= TYPE_S64;

    return result;
}

bool
type_iscastable(Type *left, Type *right) {
    if ( left == right ) {
        return true;
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

void
operand_cast(Type *type, Operand *op) {
    if ( type_iscastable(type, op->type) ) {
        op->type = type;
    } else {
        if ( op->is_const ) {
            op->type = type;
        }
    }
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

    return result;
}

#if 0
Type_Struct *
type_struct(Struct_Fields fields, size_t num_fields) {
    Type_Struct *result = urq_allocs(Type_Struct);

    result->kind       = TYPE_STRUCT;
    result->fields     = (Type_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}
#endif

Type_Enum *
type_enum(Type_Fields fields, size_t num_fields) {
    Type_Enum *result = urq_allocs(Type_Enum);

    result->kind  = TYPE_ENUM;
    result->fields = (Type_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Type_Field *
type_field(char *name, Type *type, Operand *operand = NULL) {
    Type_Field *result = urq_allocs(Type_Field);

    result->name    = name;
    result->type    = type;
    result->operand = operand;

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
        assert(!"symbol bereits registriert");
    }

    map_put(&scope->syms, sym->name, sym);
    buf_push(scope->sym_list, sym);
}

void
scope_push_global(Sym *sym) {
    scope_push(global_scope, sym);
}

Sym *
sym_new(char *name) {
    Sym *result = urq_allocs(Sym);

    result->kind = SYM_NONE;
    result->name = name;
    result->decl = NULL;
    result->type = NULL;

    return result;
}

Sym *
sym_push(char *name, Decl *decl) {
    Sym *result = sym_new(name);

    result->decl = decl;
    scope_push(curr_scope, result);

    return result;
}

Sym *
sym_push_namespace(char *name, Type *type) {
    Sym *result = sym_push(name, NULL);

    result->kind  = SYM_NAMESPACE;
    result->state = SYMSTATE_RESOLVED;
    result->type  = type;

    return result;
}

Sym *
sym_push_type(char *name, Decl *decl) {
    Sym *result = sym_push(name, decl);

    result->kind = SYM_TYPE;

    return result;
}

Sym *
sym_push_var(char *name, Decl *decl) {
    Sym *result = sym_push(name, decl);

    result->kind = SYM_VAR;

    return result;
}

Sym *
sym_push_var(char *name, Type *type) {
    Sym *result = sym_push(name, NULL);

    result->kind = SYM_VAR;
    result->type = type;

    return result;
}

Sym *
sym_push_sys(char *name, Type *type) {
    Sym *result = sym_new(intern_str(name));

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

    return sym;
}

Operand *
resolve_expr(Expr *expr) {
    Operand *result = NULL;

    if ( !expr ) {
        return result;
    }

    switch ( expr->kind ) {
        case EXPR_STR: {
            result = operand_const(type_string);
        } break;

        case EXPR_INT: {
            result = operand_const(type_u32);
        } break;

        case EXPR_FLOAT: {
            result = operand_const(type_f32);
        } break;

        case EXPR_BOOL: {
            result = operand_const(type_bool);
        } break;

        case EXPR_IDENT: {
            Sym *sym = resolve_name(AS_IDENT(expr)->val);

            if ( !sym ) {
                assert(!"unbekanntes symbol");
            }

            AS_IDENT(expr)->sym = sym;
            result = operand(sym->type);
        } break;

        case EXPR_KEYWORD: {
        } break;

        case EXPR_BIN: {
            Operand *left = resolve_expr(AS_BIN(expr)->left);
            Operand *right = resolve_expr(AS_BIN(expr)->right);

            if ( AS_BIN(expr)->op >= OP_CMP_START && AS_BIN(expr)->op <= OP_CMP_END ) {
                result = operand(type_bool);
            } else if ( AS_BIN(expr)->op >= OP_MATH_START && AS_BIN(expr)->op <= OP_MATH_END ) {
                if ( left->is_const && right->is_const ) {
                    result = operand_const(left->type);
                } else {
                    result = operand(left->type);
                }
            }
        } break;

        case EXPR_FIELD: {
            Operand *base = resolve_expr(AS_FIELD(expr)->base);
            assert(base->type);
            assert( base->type->kind == TYPE_NAMESPACE || base->type->kind == TYPE_STRUCT);

            Sym *sym = sym_get(base->type->scope, AS_FIELD(expr)->field);
            assert(sym);

            result = operand(sym->type);
        } break;

        case EXPR_INDEX: {
            Operand *base = resolve_expr(AS_INDEX(expr)->base);
            assert(base->type && base->type->kind == TYPE_ARRAY);
            result = operand(((Type_Array *)base->type)->base);
        } break;

        case EXPR_PAREN: {
            result = resolve_expr(AS_PAREN(expr)->expr);
        } break;

        case EXPR_CALL: {
            Operand *op = resolve_expr(AS_CALL(expr)->base);
            assert(op->type && op->type->kind == TYPE_PROC);

            for ( uint32_t i = 0; i < ((Type_Proc *)op->type)->num_params; ++i ) {
                Operand    *arg   = resolve_expr(AS_CALL(expr)->args[i]);
                Proc_Param *param = ((Type_Proc *)op->type)->params[i];

                operand_cast(param->type, arg);
                if ( param->type != arg->type ) {
                    assert(!"datentyp des übergebenen arguments passt nicht");
                }
            }

            result = op;
        } break;

        case EXPR_RANGE: {
            Operand *left  = resolve_expr(AS_RANGE(expr)->left);
            Operand *right = resolve_expr(AS_RANGE(expr)->right);

            result = operand(left->type);
        } break;

        case EXPR_TUPLE: {
            assert(!"in arbeit");
        } break;

        case EXPR_COMPOUND: {
            assert(!"in arbeit");
        } break;

        default: {
            assert(!"unbekannter ausdruck");
        } break;
    }

    return result;
}

Type *
resolve_typespec(Typespec *t) {
    Type *result = NULL;

    if ( !t ) {
        return result;
    }

    switch ( t->kind ) {
        case TYPESPEC_PTR: {
            Typespec_Ptr *typespec = (Typespec_Ptr *)t;
            result = type_ptr(resolve_typespec(typespec->base));
        } break;

        case TYPESPEC_NAME: {
            Typespec_Name *typespec = (Typespec_Name *)t;
            Sym *sym = sym_get(typespec->name);

            if ( !sym ) {
                assert(!"keinen datentyp gefunden");
            }

            if ( sym->kind != SYM_TYPE ) {
                assert(!"symbol muß ein datentyp sein");
            }

            return sym->type;
        } break;

        case TYPESPEC_ARRAY: {
            Typespec_Array *typespec = (Typespec_Array *)t;
            Operand *op = resolve_expr(typespec->num_elems);
            /* @AUFGABE: den wert 1 mit dem wert aus op ersetzen */
            result = type_array(resolve_typespec(typespec->base), 1);
        } break;

        case TYPESPEC_PROC: {
        } break;

        default: {
            assert(!"unbekannter typespec");
        } break;
    }

    return result;
}

Type_Proc *
resolve_decl_proc(Decl *decl) {
    assert(decl->kind == DECL_PROC);

    Proc_Sign *sign = AS_PROC(decl)->sign;
    for ( size_t i = 0; i < sign->num_params; ++i ) {
        sign->params[i]->type = resolve_typespec(sign->params[i]->typespec);
    }

    for ( size_t i = 0; i < AS_PROC(decl)->sign->num_rets; ++i ) {
        sign->rets[i]->type = resolve_typespec(sign->rets[i]->typespec);
    }

    return type_proc(sign->params, sign->num_params, sign->rets, sign->num_rets);
}

Type *
resolve_decl_const(Decl *decl) {
    assert(decl->kind == DECL_CONST);

    Type *type = resolve_typespec(AS_VAR(decl)->typespec);
    Operand *op = resolve_expr(AS_VAR(decl)->expr);

    if ( !op->is_const ) {
        assert(!"konstanten wert erwartet");
    }

    if ( !type ) {
        type = op->type;
    }

    /* @AUFGABE: prüfen ob type aus dem typespec und dem op passen */
    operand_cast(type, op);
    if ( type != op->type ) {
        assert(!"typespec stimmt mit dem zugewiesenem datentyp nicht überein");
    }

    type_complete(type);

    return type;
}

Type *
resolve_decl_type(Decl *decl) {
    Type *result = resolve_typespec(AS_TYPE(decl)->typespec);

    return result;
}

Type *
resolve_decl_var(Decl *decl) {
    assert(decl->kind == DECL_VAR);

    Type *type = resolve_typespec(AS_VAR(decl)->typespec);
    Operand *op = resolve_expr(AS_VAR(decl)->expr);

    if ( !type ) {
        type = op->type;
    }

    /* @AUFGABE: prüfen ob type aus dem typespec und dem op passen */
    operand_cast(type, op);
    if ( type != op->type ) {
        assert(!"typespec stimmt mit dem zugewiesenem datentyp nicht überein");
    }

    type_complete(type);

    return type;
}

Type *
resolve_decl(Decl *decl) {
    Type *result = NULL;

    switch ( decl->kind ) {
        case DECL_CONST: {
            Type *type = resolve_typespec(AS_CONST(decl)->typespec);
            Operand *op = resolve_expr(AS_CONST(decl)->expr);

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
            assert(!"unbekannte deklaration");
        } break;
    }

    return result;
}

Resolved_Stmts
resolve_stmt(Stmt *stmt) {
    Resolved_Stmts result = NULL;

    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
            Operand *lhs = resolve_expr(AS_ASSIGN(stmt)->lhs);
            Operand *rhs = resolve_expr(AS_ASSIGN(stmt)->rhs);

            operand_cast(lhs->type, rhs);
            if ( lhs->type != rhs->type ) {
                assert(!"zuweisung eines falschen datentyps");
            }

            buf_push(result, resolved_stmt(stmt, NULL, rhs->type, rhs));
        } break;

        case STMT_BLOCK: {
            for ( int i = 0; i < AS_BLOCK(stmt)->num_stmts; ++i ) {
                result = resolve_stmt(AS_BLOCK(stmt)->stmts[i]);
            }
        } break;

        case STMT_DECL: {
            Decl *decl = AS_DECL(stmt)->decl;

            switch ( decl->kind ) {
                case DECL_VAR: {
                    Type *type = resolve_decl_var(decl);
                    type_complete(type);
                    Sym *sym = sym_push_var(decl->name, type);

                    buf_push(result, resolved_stmt(stmt, sym, type, NULL));
                } break;

                default: {
                    assert(!"unbekannte deklaration");
                } break;
            }
        } break;

        case STMT_EXPR: {
            Operand *operand = resolve_expr(AS_EXPR(stmt)->expr);

            buf_push(result, resolved_stmt(stmt, NULL, operand->type, operand));
        } break;

        case STMT_FOR: {
            char *it = NULL;
            if ( AS_FOR(stmt)->it ) {
                assert(AS_FOR(stmt)->it->kind == EXPR_IDENT);
                it = AS_IDENT(AS_FOR(stmt)->it)->val;
            } else {
                it = intern_str("it");
            }

            Operand *cond = resolve_expr(AS_FOR(stmt)->cond);

            scope_enter("for-loop");
            Sym *sym = sym_push_var(it, cond->type);
            resolve_stmt(AS_FOR(stmt)->stmt);
            scope_leave();

            if ( AS_FOR(stmt)->stmt_else ) {
                scope_enter("for-else");
                resolve_stmt(AS_FOR(stmt)->stmt_else);
                scope_leave();
            }

            buf_push(result, resolved_stmt(stmt, NULL, NULL, NULL));
        } break;

        case STMT_IF: {
            Stmt_If *s = (Stmt_If *)stmt;

            Operand *op = resolve_expr(s->cond);
            if ( op->type != type_bool ) {
                assert(!"boolischen ausdruck erwartet");
            }

            scope_enter();
            resolve_stmt(s->stmt);
            scope_leave();

            buf_push(result, resolved_stmt(stmt, NULL, NULL, NULL));
        } break;

        case STMT_MATCH: {
            /* @AUFGABE: match kann result setzen */
            assert(!"in arbeit");
        } break;

        case STMT_RET: {
            if ( curr_scope == global_scope ) {
                assert(!"return an dieser stelle nicht erlaubt.");
            }

            if ( AS_RET(stmt)->num_exprs < AS_RET(stmt)->sign->num_rets ) {
                assert(!"zu wenig rückgabewerte");
            }

            if ( AS_RET(stmt)->num_exprs > 0 ) {
                for ( uint32_t i = 0; i < AS_RET(stmt)->sign->num_rets; ++i ) {
                    Operand *operand = resolve_expr(AS_RET(stmt)->exprs[i]);

                    operand_cast(AS_RET(stmt)->sign->rets[i]->type, operand);
                    if ( AS_RET(stmt)->sign->rets[i]->type != operand->type ) {
                        assert(!"falscher rückgabetyp");
                    }

                    buf_push(result, resolved_stmt(stmt, NULL, operand->type, operand));
                }
            } else {
                buf_push(result, resolved_stmt(stmt, NULL, type_void, operand(type_void)));
            }
        } break;

        default: {
            assert(!"unbekannte anweisung");
        } break;
    }

    return result;
}

Resolved_Stmts
resolve_directive(Directive *dir) {
    Resolved_Stmts result = NULL;

    switch ( dir->kind ) {
        case DIRECTIVE_IMPORT: {
            Scope *scope = scope_new("import", sys_scope);
            Scope *prev_scope = scope_set(scope);
            Resolved_Stmts stmts = resolve(AS_IMPORT(dir)->parsed_file);
            scope_set(prev_scope);

            for ( int i = 0; i < buf_len(stmts); ++i ) {
                buf_push(result, stmts[i]);
            }

            Scope *push_scope = curr_scope;
            if ( AS_IMPORT(dir)->scope_name ) {
                Type *type = type_namespace();
                Sym *sym   = sym_push_namespace(AS_IMPORT(dir)->scope_name, type);

                type->sym = sym;
                type->scope->name = AS_IMPORT(dir)->scope_name;
                type->scope->parent = curr_scope;

                curr_scope = type->scope;
                push_scope = type->scope;
            }

            /* @INFO: überprüfen ob export_syms im scope gesetzt wurden */
            if ( scope->export_syms ) {
                for ( int i = 0; i < scope->num_export_syms; ++i ) {
                    Module_Sym *module_sym = scope->export_syms[i];

                    Sym *export_sym = sym_get(scope, module_sym->name);

                    if ( export_sym ) {
                        char *export_name = (module_sym->alias) ? module_sym->alias : module_sym->name;
                        Sym *push_sym = sym_new(export_name);

                        push_sym->decl = export_sym->decl;
                        push_sym->type = export_sym->type;

                        scope_push(push_scope, push_sym);
                    }
                }

            /* @INFO: ansonsten alle symbole exportieren */
            } else {
                for ( int i = 0; i < buf_len(scope->sym_list); ++i ) {
                    Sym *sym = scope->sym_list[i];

                    for ( int j = 0; j < AS_IMPORT(dir)->num_syms; ++j ) {
                        Module_Sym *mod_sym = AS_IMPORT(dir)->syms[j];

                        if ( mod_sym->name == sym->name || AS_IMPORT(dir)->wildcard ) {
                            Sym *push_sym = sym;

                            if ( mod_sym->alias ) {
                                push_sym = sym_new(mod_sym->alias);
                                push_sym->decl = sym->decl;
                                push_sym->type = sym->type;
                            }

                            scope_push(push_scope, push_sym);
                        }
                    }
                }
            }

            if ( AS_IMPORT(dir)->scope_name ) {
                scope_leave();
            }
        } break;

        case DIRECTIVE_EXPORT: {
            curr_scope->export_syms     = AS_EXPORT(dir)->syms;
            curr_scope->num_export_syms = AS_EXPORT(dir)->num_syms;
        } break;

        case DIRECTIVE_LOAD: {
            result = resolve(AS_LOAD(dir)->parsed_file);
        } break;

        default: {
            assert(!"unbekannte direktive");
        } break;
    }

    return result;
}

Resolved_Stmts
resolve_directives(Directive **directives) {
    Resolved_Stmts result = NULL;

    for ( int i = 0; i < buf_len(directives); ++i ) {
        Directive *dir = directives[i];
        Resolved_Stmts ret = resolve_directive(dir);

        for ( int j = 0; j < buf_len(ret); ++j ) {
            buf_push(result, ret[j]);
        }
    }

    return result;
}

void
type_complete_struct(Type_Struct *type) {
    Decl *decl = type->sym->decl;

    assert(decl->kind == DECL_STRUCT);
    type->scope = scope_enter(decl->name);

    if ( !AS_STRUCT(decl)->num_fields ) {
        assert(!"datenstruktur muss mindestens ein feld enthalten!");
    }

    for ( size_t i = 0; i < AS_STRUCT(decl)->num_fields; i++ ) {
        Struct_Field *field = AS_STRUCT(decl)->fields[i];

        Type *field_type = 0;
        if ( field->typespec ) {
            field_type = resolve_typespec(field->typespec);
        }

        Operand *operand = NULL;
        if ( field->default_value ) {
            operand = resolve_expr(field->default_value);
            field_type = operand->type;
        }

        if ( !field_type ) {
            assert(!"datentyp des feldes konnte nicht ermittelt werden");
        }

        type_complete(field_type);

        field->type = field_type;
        field->operand = operand;

        sym_push_var(field->name, field_type);
    }

    scope_leave();
    type->kind = TYPE_STRUCT;

    uint32_t offset = 0;
    uint32_t align = 0;

    for ( uint32_t i = 0; i < AS_STRUCT(decl)->num_fields; ++i ) {
        type->size += AS_STRUCT(decl)->fields[i]->type->size;
        type->align = MAX(type->size, AS_STRUCT(decl)->fields[i]->type->align);
        AS_STRUCT(decl)->fields[i]->offset = offset;
        offset += AS_STRUCT(decl)->fields[i]->type->size;
    }

    ((Type_Struct *)type)->fields     = AS_STRUCT(decl)->fields;
    ((Type_Struct *)type)->num_fields = AS_STRUCT(decl)->num_fields;
}

void
type_complete_enum(Type_Enum *type) {
    assert(!"implementieren");
    type->kind = TYPE_ENUM;
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
    Decl_Proc *decl = AS_PROC(sym->decl);
    assert(sym->state == SYMSTATE_RESOLVED);
    Proc_Sign *sign = decl->sign;

    scope_enter(decl->name);
    for ( uint32_t i = 0; i < sign->num_params; ++i ) {
        Proc_Param *param = sign->params[i];
        sym_push_var(param->name, resolve_typespec(param->typespec));
    }

    for ( uint32_t i = 0; i < sign->num_rets; ++i ) {
        sign->rets[i]->type = resolve_typespec(sign->rets[i]->typespec);
    }

    bool returns = false;
    if ( !sign->sys_call ) {
        returns = resolve_stmt(decl->block);
    }

    scope_leave();

    if ( sign->sys_call || sign->num_rets == 1 && sign->rets[0]->type == type_void ) {
    } else if ( !returns ) {
        assert(!"nicht alle zweige liefern einen wert zurueck!");
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
        Sym *sym = sym_push(decl->name, decl);

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

        if ( decl->kind == DECL_STRUCT ) {
            sym->state = SYMSTATE_RESOLVED;
            sym->type  = type_incomplete_struct(sym);
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

Resolved_Stmts
resolve(Parsed_File *parsed_file) {
    Resolved_Stmts result = resolve_directives(parsed_file->directives);
    register_global_syms(parsed_file->stmts);

    for ( int i = 0; i < buf_len(parsed_file->stmts); ++i ) {
        Stmt *stmt = parsed_file->stmts[i];

        if ( stmt->kind == STMT_DECL ) {
            Sym *sym = sym_get(AS_DECL(stmt)->decl->name);
            assert(sym);

            if ( sym->decl ) {
                sym_finalize(sym);
            }

            buf_push(result, resolved_stmt(stmt, sym, sym->type, NULL));
        } else {
            Resolved_Stmts stmts = resolve_stmt(stmt);

            for ( int j = 0; j < buf_len(stmts); ++j ) {
                buf_push(result, stmts[j]);
            }
        }
    }

    return result;
}

void
resolver_init() {
    sys_scope    = scope_new("sys");
    global_scope = scope_new("global", sys_scope);
    curr_scope   = global_scope;

    type_void   = type_new(0, TYPE_VOID);
    type_u8     = type_new(1, TYPE_U8);
    type_u16    = type_new(2, TYPE_U16);
    type_u32    = type_new(4, TYPE_U32);
    type_u64    = type_new(8, TYPE_U64);
    type_s8     = type_new(1, TYPE_S8);
    type_s16    = type_new(2, TYPE_S16);
    type_s32    = type_new(4, TYPE_S32);
    type_s64    = type_new(8, TYPE_S64);
    type_f32    = type_new(4, TYPE_F32);
    type_f64    = type_new(8, TYPE_F64);
    type_bool   = type_new(1, TYPE_BOOL);
    type_typeid = type_new(4, TYPE_TYPEID);
    type_string = type_new(type_u32->size + PTR_SIZE, TYPE_STRING); // bytegröße (int) + zeiger zu daten

    sym_push_sys("void",   type_void);
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
    sym_push_sys("typeid", type_typeid);
}
