struct Operand;
struct Scope;
struct Type_Field;

typedef Sym**        Syms;
typedef Type_Field** Type_Fields;

Syms      resolve(Parsed_File *parsed_file);
Type    * resolve_decl(Decl *d);
Operand * resolve_stmt(Stmt *stmt);
Scope   * scope_new(char *name, Scope *parent = NULL);

enum Sym_Kind {
    SYM_NONE,
    SYM_TYPE,
    SYM_VAR,
    SYM_PROC,
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
    TYPE_NAME,
    TYPE_PTR,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_PROC,
};
enum Type_State {
    TYPESTATE_INCOMPLETE,
    TYPESTATE_COMPLETING,
    TYPESTATE_COMPLETE,
};
struct Type {
    Type_Kind kind;
    Type_State state;

    size_t size;
    uint32_t id;

    Scope *scope;
};

struct Type_Ptr : Type {
    Type *base;
};

struct Type_Array : Type {
    Type *base;
    size_t num_elems;
};

struct Type_Field {
    char *name;
    Type *type;
};
struct Type_Struct : Type {
    Type_Fields fields;
    size_t num_fields;
};

struct Type_Enum : Type {
    Type_Fields fields;
    size_t num_fields;
};

struct Type_Proc : Type {
    Type_Fields params;
    size_t num_params;
    Type *ret;
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
Type *type_usize;
Type *type_ssize;
Type *type_typeid;
Type *type_string;

Type *
type_new( size_t size ) {
    Type *result = urq_allocs(Type);

    result->kind  = TYPE_NAME;
    result->size  = size;
    result->id    = type_id++;
    result->scope = scope_new("type");

    return result;
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

    result->base = base;
    result->num_elems = num_elems;

    return result;
}

Type_Struct *
type_struct(Type_Fields fields, size_t num_fields) {
    Type_Struct *result = urq_allocs(Type_Struct);

    result->kind       = TYPE_STRUCT;
    result->fields     = (Type_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Type_Enum *
type_enum(Type_Fields fields, size_t num_fields) {
    Type_Enum *result = urq_allocs(Type_Enum);

    result->kind  = TYPE_ENUM;
    result->fields = (Type_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Type_Field *
type_field(char *name, Type *type) {
    Type_Field *result = urq_allocs(Type_Field);

    result->name = name;
    result->type = type;

    return result;
}

Type_Proc *
type_proc(Type_Fields params, size_t num_params, Type *ret) {
    Type_Proc *result = urq_allocs(Type_Proc);

    result->kind       = TYPE_PROC;
    result->params     = (Type_Fields)MEMDUP(params);
    result->num_params = num_params;

    return result;
}

void
type_resolve(Type *type) {
    if ( type->state == TYPESTATE_COMPLETING ) {
        assert(!"zirkuläre abhängigkeit festgestellt");
    }

    type->state = TYPESTATE_COMPLETING;
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
    if ( map_get(&scope->syms, sym->name) ) {
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
    resolve_decl(sym->decl);
    sym->state = SYMSTATE_RESOLVED;
}

Sym *
resolve_name(char *name) {
    Sym *sym = sym_get(name);

    return sym;
}

Operand *
resolve_expr(Expr *e) {
    Operand *result = NULL;

    if ( !e ) {
        return result;
    }

    switch ( e->kind ) {
        case EXPR_STR: {
            result = operand_const(type_string);
        } break;

        case EXPR_INT: {
            result = operand_const(type_u32);
        } break;

        case EXPR_FLOAT: {
            result = operand_const(type_f32);
        } break;

        case EXPR_IDENT: {
            Expr_Ident *expr = (Expr_Ident *)e;
            Sym *sym = resolve_name(expr->val);

            if ( !sym ) {
                assert(!"unbekanntes symbol");
            }

            expr->sym = sym;
            result = operand(sym->type);
        } break;

        case EXPR_KEYWORD: {
        } break;

        case EXPR_BIN: {
            Expr_Bin *expr = (Expr_Bin *)e;

            Operand *left = resolve_expr(expr->left);
            Operand *right = resolve_expr(expr->right);

            if ( expr->op >= OP_CMP_START || expr->op <= OP_CMP_END ) {
                result = operand(type_bool);
            } else if ( expr->op >= OP_MATH_START || expr->op <= OP_MATH_END ) {
                result = operand(left->type);
            }
        } break;

        case EXPR_FIELD: {
        } break;

        case EXPR_INDEX: {
        } break;

        case EXPR_PAREN: {
        } break;

        case EXPR_CALL: {
        } break;

        case EXPR_RANGE: {
        } break;

        case EXPR_TUPLE: {
        } break;

        case EXPR_COMPOUND: {
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

Type *
resolve_decl(Decl *d) {
    Type *result = NULL;

    switch ( d->kind ) {
        case DECL_VAR: {
            Decl_Var *decl = (Decl_Var *)d;
            Type *type = resolve_typespec(decl->typespec);
            Operand *op = resolve_expr(decl->expr);

            /* @AUFGABE: prüfen ob type aus dem typespec und dem op passen */

            result = type;
        } break;

        case DECL_CONST: {
            Decl_Const *decl = (Decl_Const *)d;
            Type *type = resolve_typespec(decl->typespec);
            Operand *op = resolve_expr(decl->expr);

            if ( !type && op->type ) {
                type = op->type;
            }

            /* @AUFGABE: prüfen ob type aus dem typespec und dem op passen */

            result = type;
        } break;

        case DECL_TYPE: {
            Decl_Type *decl = (Decl_Type *)d;
            Operand *op = resolve_expr(decl->expr);

            assert(op->type);
            result = op->type;
        } break;

        case DECL_ENUM: {
            Decl_Enum *decl = (Decl_Enum *)d;

            Type_Fields fields = NULL;
            for ( int i = 0; i < decl->num_fields; ++i ) {
                Enum_Field *field = decl->fields[i];
                buf_push(fields, type_field(field->name, type_u32));
            }

            result = type_enum(fields, buf_len(fields));
        } break;

        case DECL_STRUCT: {
            Decl_Struct *decl = (Decl_Struct *)d;

            Type_Fields fields = NULL;
            for ( int i = 0; i < decl->num_fields; ++i ) {
                Struct_Field *field = decl->fields[i];
                Type *type = resolve_typespec(field->typespec);
                buf_push(fields, type_field(field->name, type));
            }

            result = type_struct(fields, buf_len(fields));
        } break;

        case DECL_PROC: {
            Decl_Proc *decl = (Decl_Proc *)d;

            Type_Fields params = NULL;
            for ( int i = 0; i < decl->sign->num_params; ++i ) {
                Proc_Param *param = decl->sign->params[i];
                Type *param_type = resolve_typespec(param->typespec);
                Operand *param_expr = resolve_expr(param->default_value);
                buf_push(params, type_field(param->name, param_type /*, param_expr->val */));
            }

            Type *ret = resolve_typespec(decl->sign->ret->typespec);
            if ( !ret ) {
                ret = type_void;
            }

            Scope *scope = scope_enter();
            for ( int i = 0; i < buf_len(params); ++i ) {
                Type_Field *field = params[i];
                sym_push_var(field->name, field->type);
            }

            resolve_stmt(decl->block);
            scope_leave();

            result = type_proc(params, buf_len(params), ret);
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

Operand *
resolve_stmt(Stmt *stmt) {
    Operand *result = NULL;

    switch ( stmt->kind ) {
        case STMT_ASSIGN: {
        } break;

        case STMT_BLOCK: {
            Stmt_Block *s = (Stmt_Block *)stmt;
            for ( int i = 0; i < s->num_stmts; ++i ) {
                result = resolve_stmt(s->stmts[i]);
            }
        } break;

        case STMT_DECL: {
            Stmt_Decl *s = (Stmt_Decl *)stmt;
            Type *type = resolve_decl(s->decl);
        } break;

        case STMT_EXPR: {
            Stmt_Expr *s = (Stmt_Expr *)stmt;
            result = resolve_expr(s->expr);
        } break;

        case STMT_FOR: {
        } break;

        case STMT_IF: {
            Stmt_If *s = (Stmt_If *)stmt;

            Operand *op = resolve_expr(s->cond);
            if ( op->type != type_bool ) {
                assert(!"boolischen ausdruck erwartet");
            }

            result = resolve_stmt(s->stmt);
        } break;

        case STMT_MATCH: {
            /* @AUFGABE: match kann result setzen */
        } break;

        case STMT_RET: {
            Stmt_Ret *s = (Stmt_Ret *)stmt;

            result = resolve_expr(s->expr);
        } break;

        default: {
            assert(!"unbekannte anweisung");
        } break;
    }

    return result;
}

void
resolve_directive(Directive *d) {
    switch ( d->kind ) {
        case DIRECTIVE_IMPORT: {
            Directive_Import *dir = (Directive_Import *)d;

            Scope *scope = scope_new("import", sys_scope);
            Scope *prev_scope = scope_set(scope);
            resolve(dir->parsed_file);
            scope_set(prev_scope);

            Scope *push_scope = curr_scope;
            if ( dir->scope_name ) {
                push_scope = scope_enter(dir->scope_name);
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

                    for ( int j = 0; j < dir->num_syms; ++j ) {
                        Module_Sym *mod_sym = dir->syms[j];

                        if ( mod_sym->name == sym->name || dir->wildcard ) {
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

            if ( dir->scope_name ) {
                scope_leave();
            }
        } break;

        case DIRECTIVE_EXPORT: {
            Directive_Export *dir = (Directive_Export *)d;
            curr_scope->export_syms     = dir->syms;
            curr_scope->num_export_syms = dir->num_syms;
        } break;

        case DIRECTIVE_LOAD: {
            Directive_Load *dir = (Directive_Load *)d;
            resolve(dir->parsed_file);
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
register_global_syms(Stmts stmts) {
    for ( int i = 0; i < buf_len(stmts); ++i ) {
        if (stmts[i]->kind != STMT_DECL) {
            continue;
        }

        Stmt_Decl *stmt = (Stmt_Decl *)stmts[i];

        if ( stmt->decl->kind == DECL_IMPL ) {
            continue;
        }

        Sym *sym = sym_push(stmt->decl->name, stmt->decl);

        switch ( stmt->decl->kind ) {
            case DECL_TYPE:
            case DECL_ENUM:
            case DECL_STRUCT: {
                sym->kind = SYM_TYPE;
            } break;

            case DECL_VAR:
            case DECL_CONST: {
                sym->kind = SYM_VAR;
            } break;

            case DECL_PROC: {
                sym->kind = SYM_PROC;
            } break;
        }
    }
}

Syms
resolve(Parsed_File *parsed_file) {
    resolve_directives(parsed_file->directives);
    register_global_syms(parsed_file->stmts);

    for ( int i = 0; i < buf_len(parsed_file->stmts); ++i ) {
        Stmt *stmt = parsed_file->stmts[i];

        if ( stmt->kind == STMT_DECL ) {
            Stmt_Decl *s = (Stmt_Decl *)stmt;
            Sym *sym = sym_get(s->decl->name);
            assert(sym);
            sym_resolve(sym);
        } else {
            resolve_stmt(stmt);
        }
    }

    return NULL;
}

void
resolver_init() {
    sys_scope    = scope_new("sys");
    global_scope = scope_new("global", sys_scope);
    curr_scope   = global_scope;

    type_void   = type_new(0);
    type_u8     = type_new(1);
    type_u16    = type_new(2);
    type_u32    = type_new(4);
    type_u64    = type_new(8);
    type_s8     = type_new(1);
    type_s16    = type_new(2);
    type_s32    = type_new(4);
    type_s64    = type_new(8);
    type_f32    = type_new(4);
    type_f64    = type_new(8);
    type_bool   = type_new(1);
    type_usize  = type_new(8);
    type_ssize  = type_new(8);
    type_typeid = type_new(4);
    type_string = type_new(type_usize->size + PTR_SIZE); // bytegröße (int) + zeiger zu daten

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
    sym_push_sys("usize",  type_usize);
    sym_push_sys("ssize",  type_ssize);
    sym_push_sys("typeid", type_typeid);
}
