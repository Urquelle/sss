#ifndef __URQ_SSS__
#define __URQ_SSS__

#define dcAllocMem urq_alloc
#define dcFreeMem urq_dealloc
#include "dyncall.h"

namespace Urq { namespace Sss {

char * entry_point = "master";

struct Compound_Elem;
struct Decl;
struct Directive;
struct Enum_Field;
struct Expr;
struct Match_Line;
struct Module_Sym;
struct Note;
struct Operand;
struct Operand;
struct Parsed_File;
struct Proc_Param;
struct Proc_Sign;
struct Proc_Sign;
struct Resolved_Stmt;
struct Scope;
struct Stmt;
struct Stmt_Block;
struct Stmt_For;
struct Stmt_If;
struct Stmt_Match;
struct Struct_Field;
struct Sym;
struct Token_List;
struct Type;
struct Type_Proc;
struct Type_Struct;
struct Typespec;

typedef Compound_Elem**  Compound_Elems;
typedef Decl**           Decls;
typedef Directive**      Directives;
typedef Enum_Field**     Enum_Fields;
typedef Expr**           Exprs;
typedef Match_Line**     Match_Lines;
typedef Module_Sym**     Module_Syms;
typedef Note**           Notes;
typedef Proc_Param**     Proc_Params;
typedef Struct_Field**   Struct_Fields;
typedef Stmt**           Stmts;
typedef Operand       ** Operands;
typedef Resolved_Stmt ** Resolved_Stmts;
typedef Sym           ** Syms;
typedef Type          ** Types;

Parsed_File    * parse(Token_List *tokens);
Expr           * parse_expr(Token_List *tokens, bool with_stmt = false);
Stmt           * parse_stmt(Token_List *tokens);
Stmt_Block     * parse_stmt_block(Token_List *tokens, Proc_Sign *sign = NULL);
Stmt_If        * parse_stmt_if(Token_List *tokens);
Stmt_For       * parse_stmt_for(Token_List *tokens);
Stmt_Match     * parse_stmt_match(Token_List *tokens);
Typespec       * parse_typespec(Token_List *tokens);
Resolved_Stmts   resolve(Parsed_File *parsed_file);
Type           * resolve_decl(Decl *d);
Type           * resolve_decl_const(Decl *decl);
Type_Proc      * resolve_decl_proc(Decl *decl);
Type           * resolve_decl_type(Decl *decl);
Type           * resolve_decl_var(Decl *decl);
Resolved_Stmts   resolve_stmt(Stmt *stmt);
Type           * resolve_typespec(Typespec *t);
Scope          * scope_new(char *name, Scope *parent = NULL);
Sym            * sym_push_scope(Scope *scope, char *name, Type *type);
void             type_complete(Type *type);

#define EINT(Expr)       ((Expr_Int *)(Expr))
#define EFLOAT(Expr)     ((Expr_Float *)(Expr))
#define ESTR(Expr)       ((Expr_Str *)(Expr))
#define EBOOL(Expr)      ((Expr_Bool *)(Expr))
#define ECMPND(Expr)     ((Expr_Compound *)(Expr))
#define EFIELD(Expr)     ((Expr_Field *)(Expr))
#define ERNG(Expr)       ((Expr_Range *)(Expr))
#define EPAREN(Expr)     ((Expr_Paren *)(Expr))
#define EIDENT(Expr)     ((Expr_Ident *)(Expr))
#define ECAST(Expr)      ((Expr_Cast *)(Expr))
#define ECALL(Expr)      ((Expr_Call *)(Expr))
#define EUNARY(Expr)     ((Expr_Unary *)(Expr))
#define EBIN(Expr)       ((Expr_Bin *)(Expr))
#define EAT(Expr)        ((Expr_At *)(Expr))
#define EINDEX(Expr)     ((Expr_Index *)(Expr))

#define TSNAME(Ts)       ((Typespec_Name *)(Ts))
#define TSPTR(Ts)        ((Typespec_Ptr *)(Ts))
#define TSARRAY(Ts)      ((Typespec_Array *)(Ts))

#define SDECL(Stmt)      ((Stmt_Decl *)(Stmt))
#define SASSIGN(Stmt)    ((Stmt_Assign *)(Stmt))
#define SBLOCK(Stmt)     ((Stmt_Block *)(Stmt))
#define SFOR(Stmt)       ((Stmt_For *)(Stmt))
#define SMATCH(Stmt)     ((Stmt_Match *)(Stmt))
#define SWHILE(Stmt)     ((Stmt_While *)(Stmt))
#define SIF(Stmt)        ((Stmt_If *)(Stmt))
#define SRET(Stmt)       ((Stmt_Ret *)(Stmt))
#define SEXPR(Stmt)      ((Stmt_Expr *)(Stmt))
#define SUSING(Stmt)     ((Stmt_Using *)(Stmt))

#define DVAR(Decl)       ((Decl_Var *)(Decl))
#define DCONST(Decl)     ((Decl_Const *)(Decl))
#define DENUM(Decl)      ((Decl_Enum *)(Decl))
#define DPROC(Decl)      ((Decl_Proc *)(Decl))
#define DSTRUCT(Decl)    ((Decl_Struct *)(Decl))
#define DTYPE(Decl)      ((Decl_Type *)(Decl))

#define DIRIMPORT(Dir)   ((Directive_Import *)(Dir))
#define DIREXPORT(Dir)   ((Directive_Export *)(Dir))
#define DIRLOAD(Dir)     ((Directive_Load *)(Dir))

#define TSTRUCT(T)       ((Type_Struct *)(T))
#define TPROC(T)         ((Type_Proc *)(T))
#define TARRAY(T)        ((Type_Array *)(T))

#define IS_VSTRUCT(Val)  ((Val).kind == VAL_OBJ && (Val).obj_val->kind == OBJ_STRUCT)
#define IS_VSTR(Val)     ((Val).kind == VAL_OBJ && (Val).obj_val->kind == OBJ_STRING)
#define IS_VENUM(Val)    ((Val).kind == VAL_OBJ && (Val).obj_val->kind == OBJ_ENUM)
#define IS_VNS(Val)      ((Val).kind == VAL_OBJ && (Val).obj_val->kind == OBJ_NAMESPACE)

#include "lex.cpp"
#include "parser.cpp"
#include "resolver.cpp"
#include "bytecode.cpp"

void sss_repl() {
    char buf[1000];

    for ( ;; ) {
        printf(">>> ");
        gets_s(buf, sizeof(buf));

        auto tokens   = tokenize("<repl>", buf);
        auto parsed   = parse(&tokens);
        auto resolved = resolve(parsed);
        auto code     = Bytecode::build(parsed);
        Bytecode::eval(code);
    }
}

namespace api {
    using Urq::Sss::Bytecode::Bytecode;

    using Urq::Sss::Bytecode::build;
    using Urq::Sss::Bytecode::optimize;

    using Urq::Sss::parse;
    using Urq::Sss::resolve;
    using Urq::Sss::resolver_init;
    using Urq::Sss::sss_repl;
    using Urq::Sss::tokenize;
}

}}

#endif

