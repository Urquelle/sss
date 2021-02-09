char **keywords;
char *keyword_api;
char *keyword_as;
char *keyword_break;
char *keyword_const;
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
char *keyword_note;
char *keyword_proc;
char *keyword_return;
char *keyword_struct;
char *keyword_true;
char *keyword_type;
char *keyword_using;

struct Compound_Elem;
struct Decl;
struct Directive;
struct Enum_Arg;
struct Enum_Field;
struct Expr;
struct Match_Line;
struct Module_Sym;
struct Note;
struct Parsed_File;
struct Proc_Param;
struct Proc_Sign;
struct Stmt;
struct Stmt_Block;
struct Stmt_If;
struct Stmt_Match;
struct Struct_Field;
struct Sym;
struct Type;
struct Typespec;

typedef Compound_Elem**  Compound_Elems;
typedef Decl**           Decls;
typedef Directive**      Directives;
typedef Enum_Arg**       Enum_Args;
typedef Enum_Field**     Enum_Fields;
typedef Expr**           Exprs;
typedef Match_Line**     Match_Lines;
typedef Module_Sym**     Module_Syms;
typedef Note**           Notes;
typedef Proc_Param**     Proc_Params;
typedef Struct_Field**   Struct_Fields;
typedef Stmt**           Stmts;

Parsed_File * parse(Token_List *tokens);
Typespec    * parse_typespec(Token_List *tokens);
Expr        * parse_expr(Token_List *tokens, bool with_stmt = false);
Stmt        * parse_stmt(Token_List *tokens);
Stmt_Block  * parse_stmt_block(Token_List *tokens);
Stmt_If     * parse_stmt_if(Token_List *tokens);
Stmt_Match  * parse_stmt_match(Token_List *tokens);

#define STRUCT(Struct) \
    Struct *result = urq_allocs(Struct); \
    result->file = loc->file; \
    result->line = loc->line; \
    result->col  = loc->col;

#define STRUCTK(Struct, Kind) \
    Struct *result = urq_allocs(Struct); \
    result->kind = Kind; \
    result->file = loc->file; \
    result->line = loc->line; \
    result->col  = loc->col;

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

    EXPR_STR,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_IDENT,
    EXPR_KEYWORD,

    EXPR_BIN,
    EXPR_FIELD,
    EXPR_INDEX,
    EXPR_PAREN,
    EXPR_CALL,
    EXPR_RANGE,
    EXPR_TUPLE,
    EXPR_COMPOUND,
    EXPR_STMT,
};
struct Expr : Ast_Elem {
    Expr_Kind   kind;
    Type      * type;
};

struct Expr_Int : Expr {
    int64_t val;
};

struct Expr_Float : Expr {
    float val;
};

struct Expr_Str : Expr {
    char *val;
};

struct Expr_Ident : Expr {
    Sym  * sym;
    char * val;
};

struct Expr_Keyword : Expr {
    Sym  * sym;
    char * val;
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

struct Expr_Call : Expr {
    Expr  *base;
    Exprs  args;
    size_t num_args;
};

struct Expr_Field : Expr {
    Expr *base;
    char *field;
};

struct Expr_Index : Expr {
    Expr *expr;
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
    size_t num_elems;
};

struct Expr_Stmt : Expr {
    Stmt *stmt;
};

enum Stmt_Kind {
    STMT_NONE,
    STMT_ASSIGN,
    STMT_BLOCK,
    STMT_DECL,
    STMT_EXPR,
    STMT_FOR,
    STMT_IF,
    STMT_MATCH,
    STMT_RET,
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
    Expr *lhs;
    Expr *rhs;
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
    char *ident;
    Expr *resolution;
    Stmt *stmt;
};

struct Stmt_Match : Stmt {
    Expr *expr;
    Match_Lines lines;
    size_t num_lines;
};

struct Enum_Arg : Ast_Elem {
    char     * name;
    Sym      * Sym;
    Typespec * typespec;
};

struct Enum_Field : Ast_Elem {
    char      * name;
    Sym       * sym;
    Enum_Args   args;
    size_t      num_args;
    Expr      * value;
};

struct Struct_Field : Ast_Elem {
    char     * name;
    Sym      * sym;
    Typespec * typespec;
    Expr     * default_value;
};

struct Stmt_Ret : Stmt {
    Expr *expr;
};

enum Decl_Kind {
    DECL_NONE,
    DECL_VAR,
    DECL_CONST,
    DECL_TYPE,
    DECL_ENUM,
    DECL_STRUCT,
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
    Expr *expr;
};

struct Decl_Const : Decl {
    Typespec *typespec;
    Expr *expr;
};

struct Decl_Enum : Decl {
    Enum_Fields fields;
    size_t num_fields;
};

struct Decl_Struct : Decl {
    Struct_Fields fields;
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
};
struct Typespec : Ast_Elem {
    Typespec_Kind   kind;
    Type          * type;
};

struct Typespec_Name : Typespec {
    char * name;
    Sym  * sym;
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
    size_t       num_params;
    Proc_Param * ret;
};

struct Proc_Param {
    char     * name;
    Sym      * sym;
    Typespec * typespec;
    Expr     * default_value;
};

struct Proc_Sign : Ast_Elem {
    Proc_Params params;
    size_t num_params;
    Proc_Param *ret;
};

struct Compound_Elem {
    char     * name;
    Sym      * sym;
    Typespec * typespec;
    Expr     * default_value;
};

struct Parsed_File {
    Stmts      stmts;
    Directives directives;
};

Token empty_token_str = {};
Token * empty_token = &empty_token_str;

Token *
token_get(Token_List *tokens) {
    if ( tokens->curr >= tokens->list.size() ) {
        return empty_token;
    }

    Token *result = tokens->list[tokens->curr];

    return result;
}

Token *
token_peek(Token_List *tokens, size_t i = 1) {
    if ( (tokens->curr + i) >= tokens->list.size() ) {
        return empty_token;
    }

    return tokens->list[tokens->curr + i];
}

void
token_eat(Token_List *tokens, size_t i = 1) {
    if ( (tokens->curr + i) < tokens->list.size() ) {
        tokens->curr += i;
    }
}

bool
token_end(Token_List *tokens) {
    bool result = tokens->curr == ( tokens->list.size() - 1 );

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
        assert(!"nicht das erwartete token");
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
            assert(!"unbekanntes token");
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
        assert(!"keyword erwartet");
        return;
    }
}

Note *
note_create(Ast_Elem *loc, Exprs exprs, size_t num_exprs) {
    STRUCT(Note);

    result->exprs     = (Exprs)MEMDUP(exprs);
    result->num_exprs = num_exprs;

    return result;
}
/* expr {{{ */
Expr_Int *
expr_int(Ast_Elem *loc, int64_t val) {
    STRUCTK(Expr_Int, EXPR_INT);

    result->val  = val;

    return result;
}

Expr_Float *
expr_float(Ast_Elem *loc, float val) {
    STRUCTK(Expr_Float, EXPR_FLOAT);

    result->val  = val;

    return result;
}

Expr_Str *
expr_str(Ast_Elem *loc, char *val) {
    STRUCTK(Expr_Str, EXPR_STR);

    result->val  = val;

    return result;
}

Expr_Ident *
expr_ident(Ast_Elem *loc, char *val) {
    STRUCTK(Expr_Ident, EXPR_IDENT);

    result->sym = NULL;
    result->val = val;

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
expr_index(Ast_Elem *loc, Expr *expr, Expr *index) {
    STRUCTK(Expr_Index, EXPR_INDEX);

    result->expr  = expr;
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
expr_compound(Ast_Elem *loc, Compound_Elems elems, size_t num_elems) {
    STRUCTK(Expr_Compound, EXPR_COMPOUND);

    result->elems     = (Compound_Elems)MEMDUP(elems);
    result->num_elems = num_elems;

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

Expr *
parse_expr_base(Token_List *tokens) {
    Expr *result = NULL;
    Token *curr = token_get(tokens);

    if ( token_is(tokens, T_INT) ) {
        Token *t = token_read(tokens);
        result = expr_int(curr, t->val_int);
    } else if ( token_is(tokens, T_STR) ) {
        Token *t = token_read(tokens);
        result = expr_str(curr, t->val_str);
    } else if ( token_is(tokens, T_FLOAT) ) {
        Token *t = token_read(tokens);
        result = expr_float(curr, t->val_float);
    } else if ( token_is(tokens, T_IDENT) ) {
        if ( token_is_keyword(tokens) ) {
            Token *t = token_read(tokens);
            result = expr_keyword(curr, t->val_str);
        } else {
            Token *t = token_read(tokens);
            result = expr_ident(curr, t->val_str);
        }
    } else if ( token_match(tokens, T_LPAREN) ) {
        result = expr_paren(curr, parse_expr(tokens));
    } else if ( token_match(tokens, T_LBRACE) ) {
        Compound_Elems elems = NULL;

        if ( !token_is(tokens, T_RBRACE) ) {
            do {
                char *name = NULL;
                Typespec *typespec = NULL;
                Expr *value = NULL;

                Token *first = token_get(tokens);
                Token *next  = token_peek(tokens);

                if ( next->kind == T_COLON ) {
                    name = next->val_str;
                    token_eat(tokens);
                    typespec = parse_typespec(tokens);
                } else {
                    typespec = parse_typespec(tokens);
                }

                if ( token_match(tokens, T_EQL_ASSIGN) ) {
                    value = parse_expr(tokens);
                }

                token_match(tokens, T_COMMA);
            } while ( !token_is(tokens, T_RBRACE) );
        }

        token_expect(tokens, T_RBRACE);
        result = expr_compound(curr, elems, buf_len(elems));
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
proc_sign(Ast_Elem *loc, Proc_Params params, size_t num_params, Proc_Param *ret) {
    STRUCT(Proc_Sign);

    result->params     = params;
    result->num_params = num_params;
    result->ret        = ret;

    return result;
}

Match_Line *
match_line(Ast_Elem *loc, char *ident, Expr *resolution, Stmt *stmt) {
    STRUCT(Match_Line);

    result->ident = ident;
    result->resolution = resolution;
    result->stmt = stmt;

    return result;
}

Enum_Arg *
enum_arg(Ast_Elem *loc, char *name, Typespec *typespec) {
    STRUCT(Enum_Arg);

    result->name = name;
    result->typespec = typespec;

    return result;
}

Enum_Field *
enum_field(Ast_Elem *loc, char *name, Enum_Args args, size_t num_args, Expr *value) {
    STRUCT(Enum_Field);

    result->name     = name;
    result->args     = (Enum_Args)MEMDUP(args);
    result->num_args = num_args;
    result->value    = value;

    return result;
}

Struct_Field *
struct_field(Ast_Elem *loc, char *name, Typespec *typespec, Expr *default_value) {
    STRUCT(Struct_Field);

    result->name          = name;
    result->typespec      = typespec;
    result->default_value = default_value;

    return result;
}

Decl_Type *
decl_type(Ast_Elem *loc, char *name, Expr *expr) {
    STRUCTK(Decl_Type, DECL_TYPE);

    result->name = name;
    result->expr = expr;

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
decl_enum(Ast_Elem *loc, char *name, Enum_Fields fields, size_t num_fields) {
    STRUCTK(Decl_Enum, DECL_ENUM);

    result->name       = name;
    result->fields     = (Enum_Fields)MEMDUP(fields);
    result->num_fields = num_fields;

    return result;
}

Decl_Struct *
decl_struct(Ast_Elem *loc, char *name, Struct_Fields fields, size_t num_fields) {
    STRUCTK(Decl_Struct, DECL_STRUCT);

    result->name       = name;
    result->fields     = (Struct_Fields)MEMDUP(fields);
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

Stmt_Assign *
stmt_assign(Ast_Elem *loc, Expr *lhs, Expr *rhs) {
    STRUCTK(Stmt_Assign, STMT_ASSIGN);

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

Stmt_Block *
stmt_block(Ast_Elem *loc, Stmts stmts, size_t num_stmts) {
    STRUCTK(Stmt_Block, STMT_BLOCK);

    result->stmts     = (Stmts)MEMDUP(stmts);
    result->num_stmts = num_stmts;

    return result;
}

Stmt_Ret *
stmt_ret(Ast_Elem *loc, Expr *expr) {
    STRUCTK(Stmt_Ret, STMT_RET);

    result->expr = expr;

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
proc_param(char *name, Typespec *typespec, Expr *default_value = NULL) {
    Proc_Param *result = urq_allocs(Proc_Param);

    result->name = name;
    result->typespec = typespec;

    return result;
}

Proc_Param *
parse_proc_param(Token_List *tokens) {
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

    return proc_param(name, typespec, default_value);
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

Typespec_Array *
typespec_array(Ast_Elem *loc, Typespec *base, Expr *num_elems) {
    STRUCTK(Typespec_Array, TYPESPEC_ARRAY);

    result->base = base;
    result->num_elems = num_elems;

    return result;
}

Typespec_Proc *
typespec_proc(Ast_Elem *loc, Proc_Params params, size_t num_params, Proc_Param *ret) {
    STRUCTK(Typespec_Proc, TYPESPEC_PROC);

    result->params     = (Proc_Params)MEMDUP(params);
    result->num_params = num_params;
    result->ret        = ret;

    return result;
}

Typespec *
parse_typespec(Token_List *tokens) {
    Typespec *result = NULL;
    Token *curr = token_get(tokens);

    if ( token_is(tokens, T_COLON) ) {
        return result;
    }

    if ( curr->kind == T_ASTERISK ) {
        token_eat(tokens);
        result = typespec_ptr(curr, parse_typespec(tokens));
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

        Proc_Param *ret = NULL;
        if ( token_match(tokens, T_ARROW) ) {
            ret = parse_proc_param(tokens);
        }

        result = typespec_proc(curr, params, buf_len(params), ret);
    } else if ( curr->kind == T_IDENT ) {
        Token *ident = token_read(tokens);
        result = typespec_name(curr, ident->val_str);
    }

    return result;
}

Decl_Type *
parse_decl_type(Token_List *tokens, char *name) {
    Expr *expr = parse_expr(tokens);

    return decl_type(expr, name, expr);
}

Decl_Const *
parse_decl_const(Token_List *tokens, char *name, Typespec *typespec) {
    Expr *expr = parse_expr(tokens, true);

    return decl_const(expr, name, typespec, expr);
}

Decl_Enum *
parse_decl_enum(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);
    token_expect(tokens, T_LBRACE);

    Enum_Fields fields = NULL;

    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            Token *field_name = token_get(tokens);
            token_expect(tokens, T_IDENT);
            Enum_Args args = NULL;

            if ( token_match(tokens, T_LPAREN) ) {
                /* @INFO: enum A { FELD_A(u32, u32), FELD_B ... } */
                Token *t = token_get(tokens);
                if ( !token_is(tokens, T_RPAREN) ) {
                    do {
                        Typespec *typespec = parse_typespec(tokens);
                        buf_push(args, enum_arg(t, NULL, typespec));
                    } while ( token_match(tokens, T_COMMA) );
                }

                token_expect(tokens, T_RPAREN);
            } else if ( token_match(tokens, T_LBRACE) ) {
                /* @INFO: enum A { FELD_A { x: u32, y: u32 }, FELD_B ... } */
                if ( !token_is(tokens, T_RBRACE) ) {
                    do {
                        Token *arg_name = token_get(tokens);
                        token_expect(tokens, T_IDENT);
                        token_expect(tokens, T_COLON);
                        Typespec *typespec = parse_typespec(tokens);

                        buf_push(args, enum_arg(arg_name, arg_name->val_str, typespec));
                    } while ( token_match(tokens, T_COMMA) );
                }

                token_expect(tokens, T_RBRACE);
            }

            Expr *value = NULL;
            if ( token_match(tokens, T_EQL_ASSIGN) ) {
                value = parse_expr(tokens);
            }

            buf_push(fields, enum_field(field_name, field_name->val_str, args, buf_len(args), value));
            token_match(tokens, T_COMMA);
        } while ( !token_is(tokens, T_RBRACE) );
    }

    token_expect(tokens, T_RBRACE);

    return decl_enum(curr, name, fields, buf_len(fields));
}

Decl_Struct *
parse_decl_struct(Token_List *tokens, char *name) {
    Token *curr = token_get(tokens);
    token_expect(tokens, T_LBRACE);

    Struct_Fields fields = NULL;
    if ( !token_is(tokens, T_RBRACE) ) {
        do {
            Token **field_names = NULL;
            while ( !token_is(tokens, T_COLON) ) {
                Token *field_name = token_read(tokens);
                assert(field_name->kind == T_IDENT);
                buf_push(field_names, field_name);
                token_match(tokens, T_COMMA);
            }

            token_expect(tokens, T_COLON);
            Typespec *typespec = parse_typespec(tokens);

            Expr *value = NULL;
            if ( token_match(tokens, T_EQL_ASSIGN) ) {
                value = parse_expr(tokens);
            }

            for ( int field_name_index = 0; field_name_index < buf_len(field_names); ++field_name_index ) {
                Token *field_name = field_names[field_name_index];
                buf_push(fields, struct_field(field_name, field_name->val_str, typespec, value));
            }

            token_match(tokens, T_COMMA);
        } while ( !token_is(tokens, T_RBRACE) );
    }
    token_expect(tokens, T_RBRACE);

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

    Proc_Param *ret = NULL;
    if ( token_match(tokens, T_ARROW) ) {
        ret = parse_proc_param(tokens);
    }

    return proc_sign(curr, params, buf_len(params), ret);
}

Decl_Proc *
parse_decl_proc(Token_List *tokens, char *name, Typespec *typespec) {
    Token *curr = token_get(tokens);

    Proc_Sign *sign = parse_proc_sign(tokens);
    Stmt *block = parse_stmt_block(tokens);

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
parse_stmt_block(Token_List *tokens) {
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
        token_expect(tokens, T_EQL_ASSIGN);
        Expr *expr = parse_expr(tokens, true);
        token_expect(tokens, T_SEMICOLON);
        decl = decl_var(curr, name, typespec, expr);
    }

    return stmt_decl(curr, decl);
}

Stmt_Assign *
parse_stmt_assign(Token_List *tokens, Expr *expr) {
    Token *curr = token_get(tokens);
    Stmt_Assign *result = stmt_assign(curr, expr, parse_expr(tokens, true));
    token_expect(tokens, T_SEMICOLON);

    return result;
}

Stmt_If *
parse_stmt_if(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *cond  = parse_expr(tokens);
    Stmt *stmt  = parse_stmt_block(tokens);
    Stmt_If *stmt_else = NULL;

    curr = token_get(tokens);
    if ( keyword_matches(tokens, keyword_else) ) {
        if ( keyword_matches(tokens, keyword_if ) ) {
            stmt_else = parse_stmt_if(tokens);
        } else {
            stmt_else = stmt_if(curr, NULL, parse_stmt_block(tokens), NULL);
        }
    }

    return stmt_if(curr, cond, stmt, stmt_else);
}

Stmt_Ret *
parse_stmt_ret(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *expr = parse_expr(tokens);
    token_expect(tokens, T_SEMICOLON);

    return stmt_ret(curr, expr);
}

Stmt_Match *
parse_stmt_match(Token_List *tokens) {
    Token *curr = token_get(tokens);
    Expr *expr = parse_expr(tokens);

    token_expect(tokens, T_LBRACE);
    Match_Lines lines = NULL;

    if ( !token_is(tokens, T_RBRACE) ) {
        do {
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
        } while ( !token_is(tokens, T_RBRACE) );
    }

    token_expect(tokens, T_RBRACE);

    return stmt_match(curr, expr, lines, buf_len(lines));
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

    char *ident = ((Expr_Ident *)module)->val;
    size_t len = utf8_str_size(ident);
    char *file_name = (char *)urq_alloc(len+5); // plus 5 zeichen weil ".sss\0"

    memcpy(file_name, ident, len);
    memcpy(file_name + len, ".sss", 4);

    char *content = "";
    if ( !Urq::Os::os_file_read(file_name, &content) ) {
        assert(!"konnte datei nicht lesen");
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

    char *file_name = ((Expr_Str *)file)->val;

    char *content = "";
    if ( !Urq::Os::os_file_read(file_name, &content) ) {
        assert(!"konnte datei nicht lesen");
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
                } else if ( keyword->val == keyword_return ) {
                    result = parse_stmt_ret(tokens);
                } else if ( keyword->val == keyword_match ) {
                    result = parse_stmt_match(tokens);
                }
            } else {
                if ( token_is(tokens, T_COLON) ) {
                    assert(expr->kind == EXPR_IDENT);
                    char *name = ((Expr_Ident *)expr)->val;
                    result = parse_stmt_decl(tokens, name);
                } else if ( token_match(tokens, T_SEMICOLON) ) {
                    result = stmt_expr(expr, expr);
                } else {
                    token_expect(tokens, T_EQL_ASSIGN);
                    result = parse_stmt_assign(tokens, expr);
                }
            }
        } else {
            assert(!"unbekannter ausdruck");
        }
    }

    if ( result ) {
        result->notes = notes;
    }

    return result;
}

Parsed_File *
parse(Token_List *tokens) {
#define KEYWORD(Key) keyword_##Key = intern_str(#Key); buf_push(keywords, keyword_##Key)
    KEYWORD(api);
    KEYWORD(as);
    KEYWORD(break);
    KEYWORD(const);
    KEYWORD(enum);
    KEYWORD(else);
    KEYWORD(export);
    KEYWORD(false);
    KEYWORD(for);
    KEYWORD(from);
    KEYWORD(if);
    KEYWORD(impl);
    KEYWORD(import);
    KEYWORD(load);
    KEYWORD(match);
    KEYWORD(note);
    KEYWORD(proc);
    KEYWORD(return);
    KEYWORD(struct);
    KEYWORD(true);
    KEYWORD(type);
    KEYWORD(using);
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
