char **keywords;
char *keyword_api;
char *keyword_as;
char *keyword_break;
char *keyword_cast;
char *keyword_const;
char *keyword_defer;
char *keyword_free;
char *keyword_else;
char *keyword_enum;
char *keyword_export;
char *keyword_false;
char *keyword_for;
char *keyword_from;
char *keyword_if;
char *keyword_impl;
char *keyword_import;
char *keyword_load;
char *keyword_match;
char *keyword_new;
char *keyword_note;
char *keyword_proc;
char *keyword_return;
char *keyword_run;
char *keyword_sizeof;
char *keyword_struct;
char *keyword_true;
char *keyword_type;
char *keyword_typeof;
char *keyword_typeinfo;
char *keyword_using;
char *keyword_union;
char *keyword_while;

#define STRUCT(Struct) \
    Struct *result = urq_allocs(Struct); \
    loc_copy(loc, result)

#define STRUCTK(Struct, Kind) \
    Struct *result = urq_allocs(Struct); \
    result->kind = Kind; \
    loc_copy(loc, result)

void *
memdup(void *src, size_t size) {
    if (size == 0) {
        return NULL;
    }

    void *ptr = urq_alloc(size);
    memcpy(ptr, src, size);

    return ptr;
}

#define MEMDUP(x) memdup(x, num_##x * sizeof(*x))

struct Note : Ast_Elem {
    Exprs  exprs;
    size_t num_exprs;
};

enum Expr_Kind {
    EXPR_NONE,

    EXPR_AT,
    EXPR_STR,
    EXPR_CHAR,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_BOOL,
    EXPR_IDENT,
    EXPR_NEW,
    EXPR_RUN,
    EXPR_KEYWORD,
    EXPR_UNARY,
    EXPR_CAST,
    EXPR_BIN,
    EXPR_FIELD,
    EXPR_INDEX,
    EXPR_PAREN,
    EXPR_CALL,
    EXPR_RANGE,
    EXPR_SIZEOF,
    EXPR_TUPLE,
    EXPR_TYPEINFO,
    EXPR_TYPEOF,
    EXPR_COMPOUND,
    EXPR_STMT,
};
struct Expr : Ast_Elem {
    Expr_Kind   kind;
    Type      * type;
};

struct Expr_At : Expr {
    Expr *expr;
};

struct Expr_Int : Expr {
    int64_t val;
};

struct Expr_Char : Expr {
    char val;
};

struct Expr_Float : Expr {
    float val;
};

struct Expr_Bool : Expr {
    bool val;
};

struct Expr_Str : Expr {
    char     * val;
};

struct Expr_Ident : Expr {
    Sym      * sym;
    char     * val;
};

struct Expr_New : Expr {
    Expr *expr;
};

struct Expr_Run : Expr {
    Expr *expr;
};

struct Expr_Keyword : Expr {
    Sym  * sym;
    char * val;
};

struct Expr_Cast : Expr {
    Typespec * typespec;
    Expr     * expr;
};

struct Expr_Sizeof : Expr {
    Typespec * typespec;
};

struct Expr_Typeinfo : Expr {
    Expr * expr;
};

struct Expr_Typeof : Expr {
    Expr * expr;
};

enum Op_Kind {
    OP_NONE,

    OP_ADD,
    OP_MATH_START = OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MATH_END = OP_DIV,

    OP_LT,
    OP_CMP_START = OP_LT,
    OP_LTE,
    OP_EQ,
    OP_GTE,
    OP_GT,
    OP_CMP_END = OP_GT,

    OP_AND,
    OP_LOGIC_START = OP_AND,
    OP_OR,
    OP_LOGIC_END = OP_OR,
};
struct Expr_Bin : Expr {
    Op_Kind op;
    Expr * left;
    Expr * right;
};

struct Expr_Unary : Expr {
    Op_Kind op;
    Expr *expr;
};

struct Expr_Call : Expr {
    Expr  *base;
    Exprs  args;
    size_t num_args;
};

struct Expr_Field : Expr {
    Expr     * base;
    char     * field;
};

struct Expr_Index : Expr {
    Expr *base;
    Expr *index;
};

struct Expr_Paren : Expr {
    Expr *expr;
};

struct Expr_Range : Expr {
    Expr *left;
    Expr *right;
};

struct Expr_Tuple : Expr {
    Exprs exprs;
    size_t num_exprs;
};

struct Expr_Compound : Expr {
    Compound_Elems elems;
    size_t         num_elems;
    bool           is_named;
};

struct Expr_Stmt : Expr {
    Stmt *stmt;
};

enum Stmt_Kind {
    STMT_NONE,
    STMT_ASSIGN,
    STMT_BLOCK,
    STMT_DECL,
    STMT_DEFER,
    STMT_EXPR,
    STMT_FOR,
    STMT_IF,
    STMT_MATCH,
    STMT_RET,
    STMT_WHILE,
    STMT_USING,
};
struct Stmt : Ast_Elem {
    Stmt_Kind kind;
    Notes notes;
};

struct Stmt_Expr : Stmt {
    Expr *expr;
};

struct Stmt_Decl : Stmt {
    Decl *decl;
};

struct Stmt_Assign : Stmt {
    Token *op;
    Expr  *lhs;
    Expr  *rhs;
};

struct Stmt_Block : Stmt {
    Stmts stmts;
    size_t num_stmts;
};

struct Stmt_If : Stmt {
    Expr *cond;
    Stmt *stmt;
    Stmt_If *stmt_else;
};

struct Stmt_Defer : Stmt {
    Stmt *stmt;
};

struct Stmt_For : Stmt {
    Expr *it;
    Expr *cond;
    Stmt *block;
    Stmt *stmt_else;
};

struct Module_Sym : Ast_Elem {
    char * name;
    char * alias;
};

enum Directive_Kind {
    DIRECTIVE_NONE,
    DIRECTIVE_LOAD,
    DIRECTIVE_IMPORT,
    DIRECTIVE_EXPORT,
};
struct Directive : Ast_Elem {
    Directive_Kind kind;
};

struct Directive_Import : Directive {
    char        * scope_name;
    bool          wildcard;
    Module_Syms   syms;
    size_t        num_syms;
    Parsed_File * parsed_file;
};

struct Directive_Export : Directive {
    Module_Syms syms;
    size_t      num_syms;
};

struct Directive_Load : Directive {
    Parsed_File *parsed_file;
};

struct Match_Line : Ast_Elem {
    Expr *resolution;
    Stmt *stmt;
};

struct Stmt_Match : Stmt {
    Expr *expr;
    Match_Lines lines;
    size_t num_lines;
};

struct Aggr_Field : Ast_Elem {
    char     * name;
    Sym      * sym;
    Type     * type;
    Typespec * typespec;
    Expr     * value;
    Operand  * operand;
    int32_t    offset;
};

struct Stmt_Ret : Stmt {
    Exprs       exprs;
    uint32_t    num_exprs;
};

struct Stmt_While : Stmt {
    Expr * cond;
    Stmt * block;
};

struct Stmt_Using : Stmt {
    Expr *expr;
};

enum Decl_Kind {
    DECL_NONE,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_PROC,
    DECL_API,
    DECL_IMPL,
};
struct Decl : Ast_Elem {
    Decl_Kind   kind;
    char      * name;
    Sym       * sym;
};

struct Decl_Var : Decl {
    Typespec *typespec;
    Expr     *expr;
};

struct Decl_Type : Decl {
    Typespec *typespec;
};

struct Decl_Const : Decl {
    Typespec *typespec;
    Expr *expr;
};

struct Decl_Enum : Decl {
    Aggr_Fields fields;
    size_t      num_fields;
};

struct Decl_Struct : Decl {
    Aggr_Fields fields;
    size_t num_fields;
};

struct Decl_Union : Decl {
    Aggr_Fields fields;
    size_t num_fields;
};

struct Decl_Proc : Decl {
    Typespec *typespec;
    Proc_Sign *sign;
    Stmt *block;
};

struct Decl_Api : Decl {
    Decls   decls;
    size_t  num_decls;
};

struct Decl_Impl : Decl {
    Exprs  exprs;
    size_t num_exprs;
    Stmt * block;
};

enum Typespec_Kind {
    TYPESPEC_NONE,
    TYPESPEC_PTR,
    TYPESPEC_ARRAY,
    TYPESPEC_NAME,
    TYPESPEC_PROC,
    TYPESPEC_VARARG,
    TYPESPEC_UNION,
};
struct Typespec : Ast_Elem {
    Typespec_Kind   kind;
    Type          * type;
};

struct Typespec_Name : Typespec {
    char     * name;
    Sym      * sym;
};

struct Typespec_Ptr : Typespec {
    Typespec *base;
};

struct Typespec_Array : Typespec {
    Typespec * base;
    Expr     * num_elems;
};

struct Typespec_Proc : Typespec {
    Proc_Params  params;
    uint32_t     num_params;
    Proc_Params  rets;
    uint32_t     num_rets;
};

struct Typespec_Union : Typespec {
    Aggr_Fields fields;
    uint32_t    num_fields;
};

struct Proc_Param : Loc {
    char     * name;
    Sym      * sym;
    Typespec * typespec;
    Type     * type;
    Expr     * default_value;
};

struct Proc_Sign : Ast_Elem {
    Proc_Params   params;
    uint32_t      num_params;
    Proc_Params   rets;
    uint32_t      num_rets;
    bool          sys_call;
    char        * sys_lib;
};

struct Compound_Elem : Ast_Elem {
    char     * name;
    Sym      * sym;
    Typespec * typespec;
    Type     * type;
    Expr     * value;
};

struct Parsed_File {
    Stmts      stmts;
    Directives directives;
};

Token empty_token_str = {};
Token * empty_token = &empty_token_str;

Token *
token_get(Token_List *tokens) {
    if ( tokens->curr >= buf_len(tokens->list) ) {
        return empty_token;
    }

    Token *result = tokens->list[tokens->curr];

    return result;
}

Token *
token_peek(Token_List *tokens, size_t i = 1) {
    if ( (tokens->curr + i) >= buf_len(tokens->list) ) {
        return empty_token;
    }

    return tokens->list[tokens->curr + i];
}

void
token_eat(Token_List *tokens, size_t i = 1) {
    if ( (tokens->curr + i) < buf_len(tokens->list) ) {
        tokens->curr += i;
    }
}

bool
token_end(Token_List *tokens) {
    bool result = tokens->curr == ( buf_len(tokens->list) - 1 );

    return result;
}

bool
token_is(Token_List *tokens, Token_Kind expected) {
    auto curr = token_get(tokens);
    bool result = curr->kind == expected;

    return result;
}

bool
token_is_cmp(Token_List *tokens) {
    Token *curr = token_get(tokens);

    bool result = curr->kind >= T_FIRST_CMP && curr->kind <= T_LAST_CMP;

    return result;
}

bool
token_is_keyword(Token_List *tokens) {
    Token *curr = token_get(tokens);

    for ( int i = 0; i < buf_len(keywords); ++i ) {
        if ( curr->val_str == keywords[i] ) {
            return true;
        }
    }

    return false;
}

bool
token_is_assign(Token_List *tokens) {
    Token *curr = token_get(tokens);

    bool result = curr->kind >= T_FIRST_ASSIGN && curr->kind <= T_LAST_ASSIGN;

    return result;
}

bool
token_match(Token_List *tokens, Token_Kind expected) {
    if ( token_is(tokens, expected) ) {
        token_eat(tokens);
        return true;
    }

    return false;
}

void
token_expect(Token_List *tokens, Token_Kind expected) {
    if ( !token_is(tokens, expected) ) {
        tokens->has_error = true;
        tokens->error_msg = "nicht das erwartete token";

        report_error(token_get(tokens), "nicht das erwartete token");
    }

    token_eat(tokens);
}

Token *
token_read(Token_List *tokens) {
    Token *result = token_get(tokens);
    token_eat(tokens);

    return result;
}

char *
token_read_str(Token_List *tokens) {
    Token *result = token_get(tokens);
    token_eat(tokens);

    return result->val_str;
}

Op_Kind
token_op(Token *t) {
    switch ( t->kind ) {
        case T_LT:         return OP_LT;
        case T_LTE:        return OP_LTE;
        case T_EQ:         return OP_EQ;
        case T_GTE:        return OP_GTE;
        case T_GT:         return OP_GT;
        case T_PLUS:       return OP_ADD;
        case T_MINUS:      return OP_SUB;
        case T_ASTERISK:   return OP_MUL;
        case T_SLASH:      return OP_DIV;
        case T_AND:        return OP_AND;
        case T_OR:         return OP_OR;

        default:           {
            report_error(t, "unbekanntes token");
            return OP_NONE;
        }
    }
}

Module_Sym *
module_sym(Ast_Elem *loc, char *name, char *alias) {
    STRUCT(Module_Sym);

    result->name  = name;
    result->alias = alias;

    return result;
}

bool
keyword_matches(Token_List *tokens, char *keyword) {
    Token *curr = token_get(tokens);

    if ( curr->val_str == keyword ) {
        token_eat(tokens);
        return true;
    }

    return false;
}

void
keyword_expect(Token_List *tokens, char *keyword) {
    if ( !keyword_matches(tokens, keyword) ) {
        report_error(token_get(tokens), "keyword %s erwartet", keyword);
        return;
    }
}

Compound_Elem *
compound_elem(Ast_Elem *loc, char *name, Typespec *typespec, Expr *value) {
    Compound_Elem *result = urq_allocs(Compound_Elem);

    loc_copy(loc, result);

    result->name     = name;
    result->typespec = typespec;
    result->type     = NULL;
    result->value    = value;

    return result;
}

Note *
note_create(Ast_Elem *loc, Exprs exprs, size_t num_exprs) {
    STRUCT(Note);

    result->exprs     = (Exprs)MEMDUP(exprs);
    result->num_exprs = num_exprs;

    return result;
}
/* expr {{{ */
void
expr_print(Expr *expr) {
    switch ( expr->kind ) {
        case EXPR_INT: {
            printf("%lld", EINT(expr)->val);
        } break;

        default: {
            report_error(expr, "unbekannter ausdruck");
        } break;
    }
}

Expr_At *
expr_at(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Expr_At, EXPR_AT);

    result->expr = expr;

    return result;
}

Expr_Int *
expr_int(Ast_Elem *loc, int64_t val) {
    STRUCTK(Expr_Int, EXPR_INT);

    result->val  = val;

    return result;
}

Expr_Char *
expr_char(Loc *loc, char val) {
    STRUCTK(Expr_Char, EXPR_CHAR);

    result->val = val;

    return result;
}

Expr_Float *
expr_float(Ast_Elem *loc, float val) {
    STRUCTK(Expr_Float, EXPR_FLOAT);

    result->val  = val;

    return result;
}

Expr_Bool *
expr_bool(Ast_Elem *loc, bool val) {
    STRUCTK(Expr_Bool, EXPR_BOOL);

    result->val = val;

    return result;
}

Expr_Str *
expr_str(Ast_Elem *loc, char *val) {
    STRUCTK(Expr_Str, EXPR_STR);

    result->val = val;

    return result;
}

Expr_Ident *
expr_ident(Ast_Elem *loc, char *val) {
    STRUCTK(Expr_Ident, EXPR_IDENT);

    result->sym = NULL;
    result->val = val;

    return result;
}

Expr_New *
expr_new(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Expr_New, EXPR_NEW);

    result->expr = expr;

    return result;
}

Expr_Run *
expr_run(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Expr_Run, EXPR_RUN);

    result->expr = expr;

    return result;
}

Expr_Keyword *
expr_keyword(Ast_Elem *loc, char *val) {
    STRUCTK(Expr_Keyword, EXPR_KEYWORD);

    result->val = val;

    return result;
}

Expr_Call *
expr_call(Ast_Elem *loc, Expr *base, Exprs args, size_t num_args) {
    STRUCTK(Expr_Call, EXPR_CALL);

    result->base     = base;
    result->args     = (Exprs)MEMDUP(args);
    result->num_args = num_args;

    return result;
}

Expr_Unary *
expr_unary(Ast_Elem *loc, Op_Kind op, Expr *expr) {
    STRUCTK(Expr_Unary, EXPR_UNARY);

    result->op   = op;
    result->expr = expr;

    return result;
}

Expr_Bin *
expr_bin(Ast_Elem *loc, Op_Kind op, Expr *left, Expr *right) {
    STRUCTK(Expr_Bin, EXPR_BIN);

    result->op    = op;
    result->left  = left;
    result->right = right;

    return result;
}

Expr_Field *
expr_field(Ast_Elem *loc, Expr *base, char *field) {
    STRUCTK(Expr_Field, EXPR_FIELD);

    result->base  = base;
    result->field = field;

    return result;
}

Expr_Index *
expr_index(Ast_Elem *loc, Expr *base, Expr *index) {
    STRUCTK(Expr_Index, EXPR_INDEX);

    result->base  = base;
    result->index = index;

    return result;
}

Expr_Paren *
expr_paren(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Expr_Paren, EXPR_PAREN);

    result->expr = expr;

    return result;
}

Expr_Range *
expr_range(Ast_Elem *loc, Expr *left, Expr *right) {
    STRUCTK(Expr_Range, EXPR_RANGE);

    result->left  = left;
    result->right = right;

    return result;
}

Expr_Compound *
expr_compound(Ast_Elem *loc, Compound_Elems elems, size_t num_elems, bool is_named) {
    STRUCTK(Expr_Compound, EXPR_COMPOUND);

    result->elems     = (Compound_Elems)MEMDUP(elems);
    result->num_elems = num_elems;
    result->is_named  = is_named;

    return result;
}

Expr_Tuple *
expr_tuple(Ast_Elem *loc, Exprs exprs, size_t num_exprs) {
    STRUCTK(Expr_Tuple, EXPR_TUPLE);

    result->exprs     = (Exprs)MEMDUP(exprs);
    result->num_exprs = num_exprs;

    return result;
}

Expr_Stmt *
expr_stmt(Ast_Elem *loc, Stmt *stmt) {
    STRUCTK(Expr_Stmt, EXPR_STMT);

    result->stmt = stmt;

    return result;
}

Expr_Cast *
expr_cast(Ast_Elem *loc, Typespec *typespec, Expr *expr) {
    STRUCTK(Expr_Cast, EXPR_CAST);

    result->typespec = typespec;
    result->expr     = expr;

    return result;
}

Expr_Sizeof *
expr_sizeof(Ast_Elem *loc, Typespec *typespec) {
    STRUCTK(Expr_Sizeof, EXPR_SIZEOF);

    result->typespec = typespec;

    return result;
}

Expr_Typeinfo *
expr_typeinfo(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Expr_Typeinfo, EXPR_TYPEINFO);

    result->expr = expr;

    return result;
}

Expr_Typeof *
expr_typeof(Loc *loc, Expr *expr) {
    STRUCTK(Expr_Typeof, EXPR_TYPEOF);

    result->expr = expr;

    return result;
}

Expr *
parse_expr_tuple(Token_List *tokens) {
    Token *curr = token_get(tokens);

    token_expect(tokens, T_LPAREN);
    Exprs exprs = NULL;
    if ( !token_is(tokens, T_RPAREN) ) {
        do {
            buf_push(exprs, parse_expr(tokens));
        } while( !token_is(tokens, T_RPAREN) );
    }
    token_expect(tokens, T_RPAREN);

    return expr_tuple(curr, exprs, buf_len(exprs));
}

char *
parse_expr_ident(Token_List *tokens) {
    Expr *expr = parse_expr(tokens);

    assert(expr->kind == EXPR_IDENT);

    return EIDENT(expr)->val;
}

char *
parse_expr_string(Token_List *tokens) {
    Expr *expr = parse_expr(tokens);

    assert(expr->kind == EXPR_STR);

    return ESTR(expr)->val;
}

Expr_Cast *
parse_expr_cast(Token_List *tokens) {
    Token *loc = token_get(tokens);

    token_expect(tokens, T_LPAREN);
    Typespec *typespec = parse_typespec(tokens);
    token_expect(tokens, T_RPAREN);
    Expr *expr = parse_expr(tokens);

    return expr_cast(loc, typespec, expr);
}

Expr_Sizeof *
parse_expr_sizeof(Token_List *tokens) {
    Token *loc = token_get(tokens);

    token_expect(tokens, T_LPAREN);
    Typespec *typespec = parse_typespec(tokens);
    token_expect(tokens, T_RPAREN);

    return expr_sizeof(loc, typespec);
}

Expr_Typeinfo *
parse_expr_typeinfo(Token_List *tokens) {
    Token *loc = token_get(tokens);

    token_expect(tokens, T_LPAREN);
    Expr *expr = parse_expr(tokens);
    token_expect(tokens, T_RPAREN);

    return expr_typeinfo(loc, expr);
}

Expr_Typeof *
parse_expr_typeof(Token_List *tokens) {
    Token *loc = token_get(tokens);

    token_expect(tokens, T_LPAREN);
    Expr *expr = parse_expr(tokens);
    token_expect(tokens, T_RPAREN);

    return expr_typeof(loc, expr);
}

Expr *
parse_expr_base(Token_List *tokens) {
    Expr *result = NULL;
    Token *curr = token_get(tokens);

    if ( token_is(tokens, T_INT) ) {
        Token *t = token_read(tokens);
        result = expr_int(curr, t->val_int);
    } else if ( token_is(tokens, T_CHAR) ) {
        Token *t = token_read(tokens);
        result = expr_char(curr, t->val_str[0]);
    } else if ( token_is(tokens, T_STR) ) {
        Token *t = token_read(tokens);
        result = expr_str(curr, t->val_str);
    } else if ( token_is(tokens, T_FLOAT) ) {
        Token *t = token_read(tokens);
        result = expr_float(curr, t->val_float);
    } else if ( token_is(tokens, T_MINUS) ) {
        while ( token_match(tokens, T_MINUS) ) {
            /* - */
        }

        result = expr_unary(curr, OP_SUB, parse_expr(tokens));
    } else if ( token_match(tokens, T_AT) ) {
        result = expr_at(curr, parse_expr(tokens));
    } else if ( token_is(tokens, T_IDENT) ) {
        if ( token_is_keyword(tokens) ) {
            if ( keyword_matches(tokens, keyword_false) ) {
                result = expr_bool(curr, false);
            } else if ( keyword_matches(tokens, keyword_true) ) {
                result = expr_bool(curr, true);
            } else if ( keyword_matches(tokens, keyword_new) ) {
                result = expr_new(tokens, parse_expr(tokens));
            } else if ( keyword_matches(tokens, keyword_cast) ) {
                result = parse_expr_cast(tokens);
            } else if ( keyword_matches(tokens, keyword_sizeof) ) {
                result = parse_expr_sizeof(tokens);
            } else if ( keyword_matches(tokens, keyword_typeinfo) ) {
                result = parse_expr_typeinfo(tokens);
            } else if ( keyword_matches(tokens, keyword_typeof) ) {
                result = parse_expr_typeof(tokens);
            } else {
                Token *t = token_read(tokens);

                result = expr_keyword(curr, t->val_str);
            }
        } else {
            Token *t = token_read(tokens);
            result = expr_ident(curr, t->val_str);
        }
    } else if ( token_match(tokens, T_LPAREN) ) {
        result = expr_paren(curr, parse_expr(tokens));
    } else if ( token_match(tokens, T_HASH) ) {
        if ( keyword_matches(tokens, keyword_run) ) {
            result = expr_run(curr, parse_expr(tokens));
        }
    } else if ( token_match(tokens, T_LBRACE) ) {
        Compound_Elems elems = NULL;
        bool is_named = false;

        if ( !token_is(tokens, T_RBRACE) ) {
            do {
                curr                = token_get(tokens);
                char     * name     = NULL;
                Typespec * typespec = NULL;
                Expr     * value    = parse_expr(tokens);

                if ( token_match(tokens, T_COLON) ) {
                    assert(value->kind == EXPR_IDENT);
                    name     = EIDENT(value)->val;
                    is_named = true;
                    value    = NULL;
                    typespec = parse_typespec(tokens);
                }

                if ( token_match(tokens, T_EQL_ASSIGN) ) {
                    assert(value->kind == EXPR_IDENT);
                    name  = EIDENT(value)->val;
                    value = parse_expr(tokens);
                }

                token_match(tokens, T_COMMA);

                buf_push(elems, compound_elem(curr, name, typespec, value));
            } while ( !token_is(tokens, T_RBRACE) );
        }

        token_expect(tokens, T_RBRACE);
        result = expr_compound(curr, elems, buf_len(elems), is_named);
    }

    return result;
}

Expr *
parse_expr_field(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_base(tokens);

    while ( token_match(tokens, T_DOT) ) {
        Token *field = token_read(tokens);
        left = expr_field(curr, left, field->val_str);
    }

    return left;
}

Expr *
parse_expr_index(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_field(tokens);

    while ( token_is(tokens, T_LBRACKET) || token_is(tokens, T_LPAREN) ) {
        if ( token_match(tokens, T_LBRACKET) ) {
            Expr *index = parse_expr(tokens);
            left = expr_index(curr, left, index);
            token_expect(tokens, T_RBRACKET);
        } else if ( token_match(tokens, T_LPAREN) ) {
            Exprs args = NULL;
            while ( !token_is(tokens, T_RPAREN) ) {
                buf_push(args, parse_expr(tokens));
                token_match(tokens, T_COMMA);
            }

            token_expect(tokens, T_RPAREN);
            left = expr_call(curr, left, args, buf_len(args));
        }
    }

    return left;
}

Expr *
parse_expr_mul(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_index(tokens);

    while ( token_is(tokens, T_ASTERISK) || token_is(tokens, T_SLASH) ) {
        Token *op = token_read(tokens);
        left = expr_bin(curr, token_op(op), left, parse_expr(tokens));
    }

    return left;
}

Expr *
parse_expr_plus(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_mul(tokens);

    while ( token_is(tokens, T_PLUS) || token_is(tokens, T_MINUS) ) {
        Token *op = token_read(tokens);
        left = expr_bin(curr, token_op(op), left, parse_expr(tokens));
    }

    return left;
}

Expr *
parse_expr_cmp(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_plus(tokens);

    while ( token_is_cmp(tokens) ) {
        Token *op = token_read(tokens);
        left = expr_bin(curr, token_op(op), left, parse_expr_plus(tokens));
    }

    return left;
}

Expr *
parse_expr_log(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_cmp(tokens);

    while ( token_is(tokens, T_AND) || token_is(tokens, T_OR) ) {
        Token *op = token_read(tokens);
        left = expr_bin(curr, token_op(op), left, parse_expr(tokens));
    }

    return left;
}

Expr *
parse_expr_range(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *left = parse_expr_log(tokens);

    if ( token_match(tokens, T_RANGE) ) {
        left = expr_range(curr, left, parse_expr(tokens));
    }

    return left;
}

Expr *
parse_expr_stmt(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *result = NULL;

    if ( keyword_matches(tokens, keyword_if) ) {
        result = expr_stmt(curr, parse_stmt_if(tokens));
    } else if ( keyword_matches(tokens, keyword_match) ) {
        result = expr_stmt(curr, parse_stmt_match(tokens));
    } else {
        result = parse_expr_range(tokens);
    }

    return result;
}

Expr *
parse_expr(Token_List *tokens, bool with_stmt) {
    Expr *result = NULL;
    if ( with_stmt ) {
        result = parse_expr_stmt(tokens);
    } else {
        result = parse_expr_range(tokens);
    }

    return result;
}
/* }}} */

Proc_Sign *
proc_sign(Ast_Elem *loc, Proc_Params params, uint32_t num_params, Proc_Params rets,
        uint32_t num_rets)
{
    STRUCT(Proc_Sign);

    result->params     = params;
    result->num_params = num_params;
    result->rets       = rets;
    result->num_rets   = num_rets;
    result->sys_call   = false;
    result->sys_lib    = NULL;

    return result;
}

Match_Line *
match_line(Ast_Elem *loc, Expr *resolution, Stmt *stmt) {
    STRUCT(Match_Line);

    result->resolution = resolution;
    result->stmt       = stmt;

    return result;
}

Aggr_Field *
aggr_field(Loc *loc, char *name, Typespec *typespec, Expr *value) {
    STRUCT(Aggr_Field);

    result->name      = name;
    result->typespec  = typespec;
    result->value     = value;
    result->type      = NULL;
    result->operand   = NULL;
    result->offset    = 0;

    return result;
}

Decl_Type *
decl_type(Ast_Elem *loc, char *name, Typespec *typespec) {
    STRUCTK(Decl_Type, DECL_TYPE);

    result->name     = name;
    result->typespec = typespec;

    return result;
}

Decl_Var *
decl_var(Ast_Elem *loc, char *name, Typespec *typespec, Expr *expr) {
    STRUCTK(Decl_Var, DECL_VAR);

    result->name     = name;
    result->typespec = typespec;
    result->expr     = expr;

    return result;
}

Decl_Const *
decl_const(Ast_Elem *loc, char *name, Typespec *typespec, Expr *expr) {
    STRUCTK(Decl_Const, DECL_CONST);

    result->name     = name;
    result->typespec = typespec;
    result->expr     = expr;

    return result;
}

Decl_Enum *
decl_enum(Ast_Elem *loc, char *name, Aggr_Fields fields, size_t num_fields) {
    STRUCTK(Decl_Enum, DECL_ENUM);

    result->name       = name;
    result->fields     = (Aggr_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Decl_Struct *
decl_struct(Loc *loc, char *name, Aggr_Fields fields, size_t num_fields) {
    STRUCTK(Decl_Struct, DECL_STRUCT);

    result->name       = name;
    result->fields     = (Aggr_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Decl_Proc *
decl_proc(Ast_Elem *loc, char *name, Typespec *typespec, Proc_Sign *sign, Stmt *block) {
    STRUCTK(Decl_Proc, DECL_PROC);

    result->name     = name;
    result->typespec = typespec;
    result->sign     = sign;
    result->block    = block;

    return result;
}

Decl_Api *
decl_api(Ast_Elem *loc, char *name, Decls decls, size_t num_decls) {
    STRUCTK(Decl_Api, DECL_API);

    result->name      = name;
    result->decls     = decls;
    result->num_decls = num_decls;

    return result;
}

Decl_Impl *
decl_impl(Ast_Elem *loc, char *name, Exprs exprs, size_t num_exprs, Stmt *block) {
    STRUCTK(Decl_Impl, DECL_IMPL);

    result->name      = name;
    result->exprs     = (Exprs)MEMDUP(exprs);
    result->num_exprs = num_exprs;
    result->block     = block;

    return result;
}

bool
ast_valid(Ast_Elem *elem) {
    bool result = ( elem && !elem->has_error );

    return result;
}

Stmt_Expr *
stmt_expr(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Stmt_Expr, STMT_EXPR);

    result->expr = expr;

    return result;
}

Stmt_Decl *
stmt_decl(Ast_Elem *loc, Decl *decl) {
    STRUCTK(Stmt_Decl, STMT_DECL);

    result->decl = decl;

    return result;
}

Stmt_Defer *
stmt_defer(Ast_Elem *loc, Stmt *stmt) {
    STRUCTK(Stmt_Defer, STMT_DEFER);

    result->stmt = stmt;

    return result;
}

Stmt_Assign *
stmt_assign(Ast_Elem *loc, Expr *lhs, Token *op, Expr *rhs) {
    STRUCTK(Stmt_Assign, STMT_ASSIGN);

    result->op  = op;
    result->lhs = lhs;
    result->rhs = rhs;

    return result;
}

Stmt_If *
stmt_if(Ast_Elem *loc, Expr *cond, Stmt *stmt, Stmt_If *stmt_else) {
    STRUCTK(Stmt_If, STMT_IF);

    result->cond = cond;
    result->stmt = stmt;
    result->stmt_else = stmt_else;

    return result;
}

Stmt_For *
stmt_for(Ast_Elem *loc, Expr *it, Expr *cond, Stmt *block, Stmt *stmt_else) {
    STRUCTK(Stmt_For, STMT_FOR);

    result->it        = it;
    result->cond      = cond;
    result->block     = block;
    result->stmt_else = stmt_else;

    return result;
}

Stmt_Block *
stmt_block(Ast_Elem *loc, Stmts stmts, size_t num_stmts) {
    STRUCTK(Stmt_Block, STMT_BLOCK);

    result->stmts     = (Stmts)MEMDUP(stmts);
    result->num_stmts = num_stmts;

    return result;
}

Stmt_Ret *
stmt_ret(Ast_Elem *loc, Exprs exprs, uint32_t num_exprs) {
    STRUCTK(Stmt_Ret, STMT_RET);

    result->exprs     = exprs;
    result->num_exprs = num_exprs;

    return result;
}

Stmt_Match *
stmt_match(Ast_Elem *loc, Expr *expr, Match_Lines lines, size_t num_lines) {
    STRUCTK(Stmt_Match, STMT_MATCH);

    result->expr      = expr;
    result->lines     = (Match_Lines)MEMDUP(lines);
    result->num_lines = num_lines;

    return result;
}

Stmt_While *
stmt_while(Ast_Elem *loc, Expr *cond, Stmt *block) {
    STRUCTK(Stmt_While, STMT_WHILE);

    result->cond  = cond;
    result->block = block;

    return result;
}

Stmt_Using *
stmt_using(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Stmt_Using, STMT_USING);

    result->expr = expr;

    return result;
}

Directive_Import *
directive_import(Ast_Elem *loc, char *scope_name, bool wildcard, Module_Syms syms,
        size_t num_syms, Parsed_File *parsed_file)
{
    STRUCTK(Directive_Import, DIRECTIVE_IMPORT);

    result->scope_name  = scope_name;
    result->wildcard    = wildcard;
    result->syms        = (Module_Syms)MEMDUP(syms);
    result->num_syms    = num_syms;
    result->parsed_file = parsed_file;

    return result;
}

Directive_Export *
directive_export(Ast_Elem *loc, Module_Syms syms, size_t num_syms) {
    STRUCTK(Directive_Export, DIRECTIVE_EXPORT);

    result->syms       = (Module_Syms)MEMDUP(syms);
    result->num_syms   = num_syms;

    return result;
}

Directive_Load *
directive_load(Ast_Elem *loc, Parsed_File *parsed_file) {
    STRUCTK(Directive_Load, DIRECTIVE_LOAD);

    result->parsed_file = parsed_file;

    return result;
}

Proc_Param *
proc_param(Loc *loc, char *name, Typespec *typespec, Expr *default_value = NULL) {
    Proc_Param *result = urq_allocs(Proc_Param);

    loc_copy(loc, result);

    result->name     = name;
    result->typespec = typespec;
    result->type     = NULL;

    return result;
}

Proc_Param *
parse_proc_param(Token_List *tokens) {
    Token *loc = token_get(tokens);

    char *name = NULL;
    Typespec *typespec = NULL;
    Expr *default_value = NULL;

    if ( token_is(tokens, T_IDENT) ) {
        Token *first = token_get(tokens);
        Token *next  = token_peek(tokens, 1);

        if ( next->kind == T_COLON ) {
            token_eat(tokens, 2);
            name = first->val_str;
            typespec = parse_typespec(tokens);
        } else {
            typespec = parse_typespec(tokens);
        }
    } else {
        typespec = parse_typespec(tokens);
    }

    if ( token_match(tokens, T_EQL_ASSIGN) ) {
        default_value = parse_expr(tokens);
    }

    return proc_param(loc, name, typespec, default_value);
}

Typespec_Name *
typespec_name(Ast_Elem *loc, char *name) {
    STRUCTK(Typespec_Name, TYPESPEC_NAME);

    result->name = name;

    return result;
}

Typespec_Ptr *
typespec_ptr(Ast_Elem *loc, Typespec *base) {
    STRUCTK(Typespec_Ptr, TYPESPEC_PTR);

    result->base = base;

    return result;
}

Typespec *
typespec_vararg(Ast_Elem *loc) {
    STRUCTK(Typespec, TYPESPEC_VARARG);

    return result;
}

Typespec_Array *
typespec_array(Ast_Elem *loc, Typespec *base, Expr *num_elems) {
    STRUCTK(Typespec_Array, TYPESPEC_ARRAY);

    result->base = base;
    result->num_elems = num_elems;

    return result;
}

Typespec_Proc *
typespec_proc(Ast_Elem *loc, Proc_Params params, uint32_t num_params, Proc_Params rets, uint32_t num_rets) {
    STRUCTK(Typespec_Proc, TYPESPEC_PROC);

    result->params     = (Proc_Params)MEMDUP(params);
    result->num_params = num_params;
    result->rets       = (Proc_Params)rets;
    result->num_rets   = num_rets;

    return result;
}

Typespec_Union *
typespec_union(Loc *loc, Aggr_Fields fields, uint32_t num_fields) {
    STRUCTK(Typespec_Union, TYPESPEC_UNION);

    result->fields     = (Aggr_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Typespec *
parse_typespec(Token_List *tokens) {
    Typespec *result = NULL;
    Token *curr = token_get(tokens);

    if ( token_is(tokens, T_COLON) ) {
        return result;
    }

    if ( keyword_matches(tokens, keyword_union) ) {
        Aggr_Fields fields = parse_aggr_block(tokens);

        result = typespec_union(curr, fields, buf_len(fields));
    } else {
        if ( curr->kind == T_ASTERISK ) {
            token_eat(tokens);
            result = typespec_ptr(curr, parse_typespec(tokens));
        } else if ( token_match(tokens, T_ELLIPSIS) ) {
            result = typespec_vararg(curr);
        } else if ( curr->kind == T_LBRACKET ) {
            token_eat(tokens);
            Expr *num_elems = NULL;

            if ( !token_is(tokens, T_RBRACKET) ) {
                num_elems = parse_expr(tokens);
            }
            token_expect(tokens, T_RBRACKET);

            result = typespec_array(curr, parse_typespec(tokens), num_elems);
        } else if ( keyword_matches(tokens, keyword_proc) ) {
            token_expect(tokens, T_LPAREN);
            Proc_Params params = NULL;

            if ( !token_is(tokens, T_RPAREN) ) {
                do {
                    buf_push(params, parse_proc_param(tokens));
                } while ( token_match(tokens, T_COMMA) );
            }

            token_expect(tokens, T_RPAREN);

            Proc_Params rets = NULL;
            if ( token_match(tokens, T_ARROW) ) {
                do {
                    buf_push(rets, parse_proc_param(tokens));
                    token_match(tokens, T_COMMA);
                } while ( !token_is(tokens, T_SEMICOLON) && !token_is(tokens, T_COLON) );
            }

            result = typespec_proc(curr, params, (uint32_t)buf_len(params), rets, (uint32_t)buf_len(rets));
        } else if ( curr->kind == T_IDENT ) {
            Token *ident = token_read(tokens);
            result = typespec_name(curr, ident->val_str);
        }
    }

    return result;
}

Decl_Type *
parse_decl_type(Token_List *tokens, char *name) {
    Typespec *typespec = parse_typespec(tokens);

    return decl_type(typespec, name, typespec);
}

Decl_Const *
parse_decl_const(Token_List *tokens, char *name, Typespec *typespec) {
    Expr *expr = parse_expr(tokens, true);

    return decl_const(expr, name, typespec, expr);
}

Aggr_Fields
parse_aggr_block(Token_List *tokens) {
    token_expect(tokens, T_LBRACE);

    Aggr_Fields fields = NULL;
    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            Token **field_names = NULL;
            while ( !token_is(tokens, T_COLON) ) {
                do {
                    Token *field_name = token_read(tokens);
                    assert(field_name->kind == T_IDENT);
                    buf_push(field_names, field_name);
                } while ( token_match(tokens, T_COMMA) );
            }

            token_expect(tokens, T_COLON);
            Typespec *typespec = parse_typespec(tokens);

            Expr *value = NULL;
            if ( token_match(tokens, T_EQL_ASSIGN) ) {
                value = parse_expr(tokens);
            }

            for ( int field_name_index = 0; field_name_index < buf_len(field_names); ++field_name_index ) {
                Token *field_name = field_names[field_name_index];
                buf_push(fields, aggr_field(field_name, field_name->val_str, typespec, value));
            }

            token_expect(tokens, T_SEMICOLON);
        } while ( !token_is(tokens, T_RBRACE) );
    }

    token_expect(tokens, T_RBRACE);

    return fields;
}

Decl_Enum *
parse_decl_enum(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);
    token_expect(tokens, T_LBRACE);

    Aggr_Fields fields = NULL;
    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            Token *field_name = token_get(tokens);
            token_expect(tokens, T_IDENT);

            Expr *value = NULL;
            if ( token_match(tokens, T_COLON) ) {
                token_expect(tokens, T_COLON);
                value = parse_expr(tokens);
            }

            buf_push(fields, aggr_field(field_name, field_name->val_str, typespec_name(field_name, intern_str("u32")), value));

            token_expect(tokens, T_SEMICOLON);
        } while ( !token_is(tokens, T_RBRACE) );
    }

    token_expect(tokens, T_RBRACE);

    return decl_enum(curr, name, fields, buf_len(fields));
}

Decl_Struct *
parse_decl_struct(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);

    Aggr_Fields fields = parse_aggr_block(tokens);

    return decl_struct(curr, name, fields, buf_len(fields));
}

Proc_Sign *
parse_proc_sign(Token_List *tokens) {
    Token *curr = token_get(tokens);

    token_expect(tokens, T_LPAREN);
    Proc_Params params = NULL;
    if ( !token_is(tokens, T_RPAREN) ) {
        do {
            buf_push(params, parse_proc_param(tokens));
            token_match(tokens, T_COMMA);
        } while ( !token_is(tokens, T_RPAREN) );
    }
    token_expect(tokens, T_RPAREN);

    Proc_Params rets = NULL;
    if ( token_match(tokens, T_ARROW) ) {
        buf_push(rets, parse_proc_param(tokens));
    }

    return proc_sign(curr, params, (uint32_t)buf_len(params), rets, (uint32_t)buf_len(rets));
}

Decl_Proc *
parse_decl_proc(Token_List *tokens, char *name, Typespec *typespec) {
    Token *curr = token_get(tokens);

    Proc_Sign *sign = parse_proc_sign(tokens);

    if ( token_match(tokens, T_HASH) ) {
        char *dir = parse_expr_ident(tokens);

        if ( dir == intern_str("sys_call") ) {
            sign->sys_call = true;
            sign->sys_lib = parse_expr_string(tokens);
        } else {
            report_error(curr, "unbekannte direktive");
        }
    }

    Stmt *block = NULL;
    if ( !token_match(tokens, T_SEMICOLON) ) {
        block = parse_stmt_block(tokens, sign);
    }

    return decl_proc(curr, name, typespec, sign, block);
}

Decl_Api *
parse_decl_api(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);

    token_expect(tokens, T_LBRACE);
    Decls decls = NULL;
    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            Token *proc_name = token_read(tokens);
            token_expect(tokens, T_COLON);
            Typespec *typespec = parse_typespec(tokens);
            buf_push(decls, decl_proc(proc_name, proc_name->val_str, typespec, NULL, NULL));

            token_match(tokens, T_COMMA);
        } while ( !token_is(tokens, T_RBRACE) );
    }
    token_expect(tokens, T_RBRACE);

    return decl_api(curr, name, decls, buf_len(decls));
}

Decl_Impl *
parse_decl_impl(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);

    Exprs exprs = NULL;
    if ( token_match(tokens, T_LPAREN) ) {
        if ( !token_is(tokens, T_RPAREN) ) {
            do {
                buf_push(exprs, parse_expr(tokens));
                token_match(tokens, T_COMMA);
            } while( !token_is(tokens, T_RPAREN) );
        }
        token_expect(tokens, T_RPAREN);
    }

    Stmt *block = parse_stmt_block(tokens);

    return decl_impl(curr, name, exprs, buf_len(exprs), block);
}

Stmt_Block *
parse_stmt_block(Token_List *tokens, Proc_Sign *sign) {
    Token *curr = token_get(tokens);

    Stmts stmts = NULL;
    token_expect(tokens, T_LBRACE);
    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            buf_push(stmts, parse_stmt(tokens));
        } while ( !token_is(tokens, T_RBRACE) );
    }
    token_expect(tokens, T_RBRACE);

    return stmt_block(curr, stmts, buf_len(stmts));
}

Stmt_Decl *
parse_stmt_decl(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);
    token_expect(tokens, T_COLON);
    Typespec *typespec = parse_typespec(tokens);

    Decl *decl = NULL;
    if ( token_match(tokens, T_COLON) ) {
        if ( keyword_matches(tokens, keyword_type) ) {
            decl = parse_decl_type(tokens, name);
            token_expect(tokens, T_SEMICOLON);
        } else if ( keyword_matches(tokens, keyword_enum) ) {
            decl = parse_decl_enum(tokens, name);
        } else if ( keyword_matches(tokens, keyword_struct) ) {
            decl = parse_decl_struct(tokens, name);
        } else if ( keyword_matches(tokens, keyword_const) ) {
            decl = parse_decl_const(tokens, name, typespec);
            token_expect(tokens, T_SEMICOLON);
        } else if ( keyword_matches(tokens, keyword_proc) ) {
            decl = parse_decl_proc(tokens, name, typespec);
        } else if ( keyword_matches(tokens, keyword_api) ) {
            decl = parse_decl_api(tokens, name);
        } else if ( keyword_matches(tokens, keyword_impl) ) {
            decl = parse_decl_impl(tokens, name);
        }
    } else {
        Expr *expr = NULL;
        if ( token_match(tokens, T_EQL_ASSIGN) ) {
            expr = parse_expr(tokens, true);
        }

        token_expect(tokens, T_SEMICOLON);
        decl = decl_var(curr, name, typespec, expr);
    }

    return stmt_decl(curr, decl);
}

Stmt_Assign *
parse_stmt_assign(Token_List *tokens, Token *op, Expr *expr) {
    Token *curr = token_get(tokens);
    Stmt_Assign *result = stmt_assign(curr, expr, op, parse_expr(tokens, true));
    token_expect(tokens, T_SEMICOLON);

    return result;
}

Stmt_If *
parse_stmt_if(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *cond  = parse_expr(tokens);
    Stmt *stmt  = parse_stmt(tokens);
    Stmt_If *stmt_else = NULL;

    curr = token_get(tokens);
    if ( keyword_matches(tokens, keyword_else) ) {
        if ( keyword_matches(tokens, keyword_if ) ) {
            stmt_else = parse_stmt_if(tokens);
        } else {
            stmt_else = stmt_if(curr, expr_bool(curr, true), parse_stmt_block(tokens), NULL);
        }
    }

    return stmt_if(curr, cond, stmt, stmt_else);
}

Stmt_For *
parse_stmt_for(Token_List *tokens) {
    Token *curr = token_get(tokens);

    Expr *it    = NULL;
    Expr *cond  = parse_expr(tokens);

    if ( token_match(tokens, T_COLON) ) {
        it = cond;
        cond = parse_expr(tokens);
    }

    Stmt *stmt = parse_stmt(tokens);

    Stmt *stmt_else = NULL;
    if ( keyword_matches(tokens, keyword_else) ) {
        stmt_else = parse_stmt_block(tokens);
    }

    return stmt_for(curr, it, cond, stmt, stmt_else);
}

Stmt_Defer *
parse_stmt_defer(Token_List *tokens) {
    Token *curr = token_get(tokens);

    Stmt_Defer *result = stmt_defer(curr, parse_stmt(tokens));

    return result;
}

Stmt_Ret *
parse_stmt_ret(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Exprs exprs = NULL;

    while ( !token_is(tokens, T_SEMICOLON) ) {
        buf_push(exprs, parse_expr(tokens));
        token_match(tokens, T_COMMA);
    }

    token_expect(tokens, T_SEMICOLON);

    return stmt_ret(curr, exprs, (uint32_t)buf_len(exprs));
}

Stmt_Match *
parse_stmt_match(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *expr = parse_expr(tokens);

    token_expect(tokens, T_LBRACE);
    Match_Lines lines = NULL;

    if ( !token_is(tokens, T_RBRACE) ) {
        do {
#if 0
            Token *ident = token_read(tokens);
            assert(ident->kind == T_IDENT);

            Expr *resolution = NULL;
            if ( token_is(tokens, T_LPAREN) ) {
                resolution = parse_expr_tuple(tokens);
            } else if ( !token_is(tokens, T_COLON) ) {
                resolution = parse_expr(tokens);
            }

            token_expect(tokens, T_COLON);
            Stmt *stmt = parse_stmt_block(tokens);
            buf_push(lines, match_line(ident, ident->val_str, resolution, stmt));

            token_match(tokens, T_COMMA);
#else
            Expr *resolution = parse_expr(tokens);
            token_expect(tokens, T_FAT_ARROW);
            Stmt *stmt = parse_stmt(tokens);

            buf_push(lines, match_line(resolution, resolution, stmt));
#endif
        } while ( !token_is(tokens, T_RBRACE) );
    }

    token_expect(tokens, T_RBRACE);

    return stmt_match(curr, expr, lines, buf_len(lines));
}

Stmt_Using *
parse_stmt_using(Token_List *tokens) {
    Token *curr = token_get(tokens);

    Expr *expr = parse_expr(tokens);
    token_expect(tokens, T_SEMICOLON);

    return stmt_using(curr, expr);
}

Stmt_While *
parse_stmt_while(Token_List *tokens) {
    Token *curr = token_get(tokens);

    Expr *cond = parse_expr(tokens);
    Stmt *block = parse_stmt_block(tokens);

    return stmt_while(curr, cond, block);
}

Directive_Import *
parse_directive_import(Token_List *tokens) {
    Token *curr = token_get(tokens);

    char *scope_name = NULL;
    Token *peek = token_peek(tokens, 1);
    if ( peek->kind == T_EQL_ASSIGN ) {
        token_eat(tokens, 2);
        scope_name = curr->val_str;
    }

    token_match(tokens, T_LPAREN);
    bool wildcard = false;
    Module_Syms syms = NULL;
    while ( !token_match(tokens, T_RPAREN) ) {
        Token *sym = token_read(tokens);

        if ( sym->kind == T_ASTERISK ) {
            wildcard = true;
            token_match(tokens, T_COMMA);
            continue;
        }

        assert(sym->kind == T_IDENT);

        char *alias = NULL;
        if ( keyword_matches(tokens, keyword_as) ) {
            Token *t = token_read(tokens);
            alias = t->val_str;
        }

        buf_push(syms, module_sym(sym, sym->val_str, alias));
        token_match(tokens, T_COMMA);
    }

    keyword_expect(tokens, keyword_from);

    Expr *module = parse_expr(tokens);
    assert(module->kind == EXPR_IDENT);

    token_expect(tokens, T_SEMICOLON);

    char *ident = ((Expr_Ident *)module)->val;
    size_t len = utf8_str_size(ident);

    char *sss_dir = Urq::Os::os_env("SSS_DIR");
    if ( !sss_dir ) {
        report_error(curr, "SSS_DIR umgebungsvariable setzen");
    }

    char *file_name = path_concat(sss_dir, ident, ".sss");

    char *content = "";
    if ( !Urq::Os::os_file_read(file_name, &content) ) {
        report_error(curr, "konnte datei %s nicht lesen", file_name);
    }

    auto imported_tokens = tokenize(file_name, content);
    auto parsed_file     = parse(&imported_tokens);

    return directive_import(curr, scope_name, wildcard, syms, buf_len(syms), parsed_file);
}

Directive_Load *
parse_directive_load(Token_List *tokens) {
    Token *curr = token_get(tokens);

    Expr *file = parse_expr(tokens);
    assert(file->kind == EXPR_STR);
    token_expect(tokens, T_SEMICOLON);

    char *file_name = ((Expr_Str *)file)->val;

    char *content = "";
    if ( !Urq::Os::os_file_read(file_name, &content) ) {
        report_error(curr, "konnte datei %s nicht lesen", file_name);
    }

    auto imported_tokens = tokenize(file_name, content);
    auto parsed_file     = parse(&imported_tokens);

    return directive_load(curr, parsed_file);
}

Directive_Export *
parse_directive_export(Token_List *tokens) {
    Token *curr = token_get(tokens);

    token_expect(tokens, T_LBRACE);
    Module_Syms syms = NULL;

    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            Token *t = token_get(tokens);
            char *sym = token_read_str(tokens);
            char *alias = NULL;

            if ( keyword_matches(tokens, keyword_as) ) {
                alias = token_read_str(tokens);
            }

            buf_push(syms, module_sym(t, sym, alias));
            token_match(tokens, T_COMMA);
        } while ( !token_is(tokens, T_RBRACE) );
    }

    token_expect(tokens, T_RBRACE);

    return directive_export(curr, syms, buf_len(syms));
}

Notes
parse_notes(Token_List *tokens) {
    Notes notes = NULL;

    Token *peek = token_peek(tokens);
    if ( token_is(tokens, T_HASH) && peek->kind == T_IDENT && peek->val_str == keyword_note ) {
        token_eat(tokens, 2);
        Exprs exprs = NULL;

        if ( token_match(tokens, T_LPAREN) ) {
            if ( !token_is(tokens, T_RPAREN) ) {
                do {
                    buf_push(exprs, parse_expr(tokens));
                    token_match(tokens, T_COMMA);
                } while (!token_is(tokens, T_RPAREN));
            }
            token_expect(tokens, T_RPAREN);
        }

        buf_push(notes, note_create(peek, exprs, buf_len(exprs)));
    }

    return notes;
}

Directive *
parse_directive(Token_List *tokens) {
    Directive *result = NULL;

    if ( keyword_matches(tokens, keyword_import) ) {
        result = parse_directive_import(tokens);
    } else if ( keyword_matches(tokens, keyword_load) ) {
        result = parse_directive_load(tokens);
    } else if ( keyword_matches(tokens, keyword_export) ) {
        result = parse_directive_export(tokens);
    }

    return result;
}

Stmt *
parse_stmt(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Stmt *result = NULL;

    Notes notes = parse_notes(tokens);

    if ( token_is(tokens, T_LBRACE) ) {
        result = parse_stmt_block(tokens);
    } else if ( !token_end(tokens) ) {
        Expr *expr = parse_expr(tokens);

        if ( expr ) {
            if ( expr->kind == EXPR_KEYWORD ) {
                Expr_Keyword *keyword = (Expr_Keyword *)expr;

                if ( keyword->val == keyword_if ) {
                    result = parse_stmt_if(tokens);
                } else if ( keyword->val == keyword_for ) {
                    result = parse_stmt_for(tokens);
                } else if ( keyword->val == keyword_return ) {
                    result = parse_stmt_ret(tokens);
                } else if ( keyword->val == keyword_while ) {
                    result = parse_stmt_while(tokens);
                } else if ( keyword->val == keyword_defer ) {
                    result = parse_stmt_defer(tokens);
                } else if ( keyword->val == keyword_match ) {
                    result = parse_stmt_match(tokens);
                } else if ( keyword->val == keyword_using ) {
                    result = parse_stmt_using(tokens);
                }
            } else {
                if ( token_is(tokens, T_COLON) ) {
                    assert(expr->kind == EXPR_IDENT);
                    char *name = ((Expr_Ident *)expr)->val;
                    result = parse_stmt_decl(tokens, name);
                } else if ( token_match(tokens, T_SEMICOLON) ) {
                    result = stmt_expr(expr, expr);
                } else {
                    if ( !token_is_assign(tokens) ) {
                        report_error(token_get(tokens), "unerwartetes token");
                    }

                    Token *op = token_read(tokens);
                    result = parse_stmt_assign(tokens, op, expr);
                }
            }
        } else {
            report_error(curr, "unbekannter ausdruck");
        }
    }

    if ( result ) {
        result->notes = notes;
    }

    return result;
}

Parsed_File *
parse(Token_List *tokens) {
#define KEYWORD_K(Key, Var) keyword_##Var = intern_str(#Key); buf_push(keywords, keyword_##Var)
#define KEYWORD(Key) KEYWORD_K(Key, Key)
    KEYWORD(api);
    KEYWORD(as);
    KEYWORD_K(weg, break);
    KEYWORD_K(als, cast);
    KEYWORD(const);
    KEYWORD_K(defer, defer);
    KEYWORD(enum);
    KEYWORD_K(sonst, else);
    KEYWORD(export);
    KEYWORD_K(falsch, false);
    KEYWORD_K(iter, for);
    KEYWORD(free);
    KEYWORD(from);
    KEYWORD_K(falls, if);
    KEYWORD(impl);
    KEYWORD(import);
    KEYWORD(load);
    KEYWORD_K(zweig, match);
    KEYWORD(new);
    KEYWORD(note);
    KEYWORD(proc);
    KEYWORD_K(res, return);
    KEYWORD_K(ausführen, run);
    KEYWORD_K(struktur, struct);
    KEYWORD_K(wahr, true);
    KEYWORD_K(typedef, type);
    KEYWORD_K(typeof, typeof);
    KEYWORD(typeinfo);
    KEYWORD_K(mit, using);
    KEYWORD(union);
    KEYWORD_K(sizeof, sizeof);
    KEYWORD_K(solange, while);
#undef KEYWORD

    Stmts stmts = NULL;
    Stmt *stmt = NULL;
    Directives directives = NULL;
    Directive *dir = NULL;
    Parsed_File *result = urq_allocs(Parsed_File);

    if ( token_match(tokens, T_HASH) ) {
        dir = parse_directive(tokens);
    } else {
        stmt = parse_stmt(tokens);
    }

    while ( ast_valid(stmt) || ast_valid(dir) ) {
        if ( stmt ) {
            buf_push(stmts, stmt);
        }

        if ( dir ) {
            buf_push(directives, dir);
        }

        if ( token_match(tokens, T_HASH) ) {
            dir = parse_directive(tokens);
            stmt = NULL;
        } else {
            stmt = parse_stmt(tokens);
            dir = NULL;
        }
    }

    result->directives = directives;
    result->stmts      = stmts;

    return result;
}
