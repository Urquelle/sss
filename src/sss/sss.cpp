#ifndef __URQ_SSS__
#define __URQ_SSS__

#define dcAllocMem urq_alloc
#define dcFreeMem urq_dealloc
#include "dyncall.h"

namespace Urq { namespace Sss {

char * entry_point = "master";

char * prop_id     = "id";
char * prop_size   = "größe";
char * prop_name   = "name";
char * prop_offset = "versatz";
char * prop_base   = "basis";
char * prop_fields = "felder";
char * prop_params = "parameter";
char * prop_rets   = "rückgabewerte";
char * prop_num    = "anzahl";

struct Compound_Elem;
struct Decl;
struct Decl_Var;
struct Directive;
struct Expr;
struct Match_Line;
struct Module_Sym;
struct Note;
struct Operand;
struct Operand;
struct Parsed_File;
struct Proc_Sign;
struct Proc_Sign;
struct Scope;
struct Stmt;
struct Stmt_Block;
struct Stmt_For;
struct Stmt_If;
struct Stmt_Match;
struct Sym;
struct Token_List;
struct Type;
struct Type_Proc;
struct Type_Struct;
struct Typespec;

typedef Compound_Elem ** Compound_Elems;
typedef Decl          ** Decls;
typedef Decl_Var      ** Decl_Vars;
typedef Directive     ** Directives;
typedef Expr          ** Exprs;
typedef Match_Line    ** Match_Lines;
typedef Module_Sym    ** Module_Syms;
typedef Note          ** Notes;
typedef Stmt          ** Stmts;
typedef Operand       ** Operands;
typedef Sym           ** Syms;
typedef Type          ** Types;

Parsed_File            * parse(Token_List *tokens);
Decl_Vars                parse_aggr_block(Token_List *tokens);
Expr                   * parse_expr(Token_List *tokens, bool with_stmt = false);
Stmt                   * parse_stmt(Token_List *tokens, Stmt *parent = NULL);
Stmt_Block             * parse_stmt_block(Token_List *tokens, Proc_Sign *sign = NULL, Stmt *parent = NULL);
Stmt_If                * parse_stmt_if(Token_List *tokens, Stmt *parent);
Stmt_For               * parse_stmt_for(Token_List *tokens);
Stmt_Match             * parse_stmt_match(Token_List *tokens, Stmt *parent);
Typespec               * parse_typespec(Token_List *tokens);
Decl_Var               * parse_proc_param(Token_List *tokens);
void                     resolve(Parsed_File *parsed_file, bool check_entry_point = true);
void                     resolve_file(Parsed_File *parsed_file);
Type                   * resolve_decl(Decl *d);
Type                   * resolve_decl_const(Decl *decl);
Type_Proc              * resolve_decl_proc(Decl *decl);
Type                   * resolve_decl_type(Decl *decl);
Type                   * resolve_decl_var(Decl *decl);
bool                     resolve_stmt(Stmt *stmt, Types rets = NULL, uint32_t num_rets = 0);
Type                   * resolve_typespec(Typespec *t);
void                     resolve_aggr_fields(Decl_Vars fields, uint32_t num_fields);
Scope                  * scope_new(char *name, Scope *parent = NULL);
Sym                    * sym_push_scope(Loc *loc, Scope *scope, char *name, Type *type);
void                     type_complete(Type *type);
void                     type_complete_struct(Type_Struct *type);

#define EBIN(Expr)            ((Expr_Bin *)(Expr))
#define EBOOL(Expr)           ((Expr_Bool *)(Expr))
#define ECALL(Expr)           ((Expr_Call *)(Expr))
#define ECAST(Expr)           ((Expr_Cast *)(Expr))
#define ECHR(Expr)            ((Expr_Char *)(Expr))
#define ECMPND(Expr)          ((Expr_Compound *)(Expr))
#define EDEREF(Expr)          ((Expr_Deref *)(Expr))
#define EFIELD(Expr)          ((Expr_Field *)(Expr))
#define EFLOAT(Expr)          ((Expr_Float *)(Expr))
#define EIDENT(Expr)          ((Expr_Ident *)(Expr))
#define EINDEX(Expr)          ((Expr_Index *)(Expr))
#define EINT(Expr)            ((Expr_Int *)(Expr))
#define ENEW(Expr)            ((Expr_New *)(Expr))
#define ENOT(Expr)            ((Expr_Not *)(Expr))
#define EPAREN(Expr)          ((Expr_Paren *)(Expr))
#define EPTR(Expr)            ((Expr_Ptr *)(Expr))
#define ERNG(Expr)            ((Expr_Range *)(Expr))
#define ESIZEOF(Expr)         ((Expr_Sizeof *)(Expr))
#define ESTR(Expr)            ((Expr_Str *)(Expr))
#define ETYPEINFO(Expr)       ((Expr_Typeinfo *)(Expr))
#define ETYPEOF(Expr)         ((Expr_Typeof *)(Expr))
#define EUNARY(Expr)          ((Expr_Unary *)(Expr))

#define TSNAME(Ts)            ((Typespec_Name *)(Ts))
#define TSPTR(Ts)             ((Typespec_Ptr *)(Ts))
#define TSARRAY(Ts)           ((Typespec_Array *)(Ts))
#define TSUNION(Ts)           ((Typespec_Union *)(Ts))

#define SDECL(Stmt)           ((Stmt_Decl *)(Stmt))
#define SASSIGN(Stmt)         ((Stmt_Assign *)(Stmt))
#define SBLOCK(Stmt)          ((Stmt_Block *)(Stmt))
#define SBREAK(Stmt)          ((Stmt_Break *)(Stmt))
#define SFOR(Stmt)            ((Stmt_For *)(Stmt))
#define SMATCH(Stmt)          ((Stmt_Match *)(Stmt))
#define SWHILE(Stmt)          ((Stmt_While *)(Stmt))
#define SIF(Stmt)             ((Stmt_If *)(Stmt))
#define SRET(Stmt)            ((Stmt_Ret *)(Stmt))
#define SEXPR(Stmt)           ((Stmt_Expr *)(Stmt))
#define SUSING(Stmt)          ((Stmt_Using *)(Stmt))
#define SDEFER(Stmt)          ((Stmt_Defer *)(Stmt))

#define DVAR(Decl)            ((Decl_Var *)(Decl))
#define DCONST(Decl)          ((Decl_Const *)(Decl))
#define DENUM(Decl)           ((Decl_Enum *)(Decl))
#define DUNION(Decl)          ((Decl_Union *)(Decl))
#define DPROC(Decl)           ((Decl_Proc *)(Decl))
#define DSTRUCT(Decl)         ((Decl_Struct *)(Decl))
#define DTYPE(Decl)           ((Decl_Type *)(Decl))

#define IS_DSTRUCT(Decl)      ((Decl)->kind == DECL_STRUCT)
#define IS_DENUM(Decl)        ((Decl)->kind == DECL_ENUM)
#define IS_DUNION(Decl)       ((Decl)->kind == DECL_UNION)
#define IS_DAGGR(Decl)        ((Decl)->kind == DECL_STRUCT || (Decl)->kind == DECL_ENUM || (Decl)->kind == DECL_UNION)

#define DIRIMPORT(Dir)        ((Directive_Import *)(Dir))
#define DIREXPORT(Dir)        ((Directive_Export *)(Dir))
#define DIRLOAD(Dir)          ((Directive_Load *)(Dir))

#define TSTRUCT(T)            ((Type_Struct *)(T))
#define TENUM(T)              ((Type_Enum *)(T))
#define TUNION(T)             ((Type_Union *)(T))
#define TPROC(T)              ((Type_Proc *)(T))
#define TARRAY(T)             ((Type_Array *)(T))
#define TPTR(T)               ((Type_Ptr *)(T))
#define TCMPND(T)             ((Type_Compound *)(T))
#define TVARIADIC(T)          ((Type_Variadic *)(T))

#define IS_TARRAY(T)          ((T)->kind == TYPE_ARRAY)
#define IS_TSTR(T)            ((T)->kind == TYPE_STRING)

#define IS_VOBJ(Val)          ((Val).kind == VAL_OBJ)
#define IS_VARRAY(Val)        (IS_VOBJ(Val) && (Val).o->kind == OBJ_ARRAY)
#define IS_VCMPND(Val)        (IS_VOBJ(Val) && (Val).o->kind == OBJ_COMPOUND)
#define IS_VSTRUCT(Val)       (IS_VOBJ(Val) && (Val).o->kind == OBJ_STRUCT)
#define IS_VSTR(Val)          (IS_VOBJ(Val) && (Val).o->kind == OBJ_STRING)
#define IS_VENUM(Val)         (IS_VOBJ(Val) && (Val).o->kind == OBJ_ENUM)
#define IS_VNS(Val)           (IS_VOBJ(Val) && (Val).o->kind == OBJ_NAMESPACE)
#define IS_VRANGE(Val)        (IS_VOBJ(Val) && (Val).o->kind == OBJ_RANGE)
#define IS_VPROC(Val)         (IS_VOBJ(Val) && (Val).o->kind == OBJ_PROC)
#define IS_VPTR(Val)          (IS_VOBJ(Val) && (Val).o->kind == OBJ_PTR)
#define IS_VITER(Val)         (IS_VOBJ(Val) && (Val).o->kind == OBJ_ITER)

#define VOBJ(Val)             ((Val).o)
#define VNS(Val)              ((Obj_Namespace *)(Val).o)
#define VCMPND(Val)           ((Obj_Compound *)(Val).o)
#define VSTRUCT(Val)          ((Obj_Struct *)(Val).o)
#define VRANGE(Val)           ((Obj_Range *)(Val).o)
#define VARRAY(Val)           ((Obj_Array *)(Val).o)
#define VSTR(Val)             ((Obj_String *)(Val).o)
#define VPROC(Val)            ((Obj_Proc *)(Val).o)
#define VTYPE(Val)            ((Obj_Type *)(Val).o)
#define VPTR(Val)             ((Obj_Ptr *)(Val).o)
#define VAGGRFIELD(Val)       ((Obj_Aggr_Field *)(Val).o)
#define VITER(Val)            ((Obj_Iter *)(Val).o)

#define OITER(Obj)            ((Obj_Iter *)(Obj))
#define OITERRNG(Obj)         ((Obj_Iter_Range *)(Obj))
#define OITERARRAY(Obj)       ((Obj_Iter_Array *)(Obj))
#define OSTR(Obj)             ((Obj_String *)(Obj))

#define IS_OITER(Obj)         ((Obj)->kind == OBJ_ITER)
#define IS_OITERARRAY(Obj)    ((Obj)->kind == OBJ_ITER && OITER(Obj)->iter_kind == ITER_ARRAY)
#define IS_OITERRNG(Obj)      ((Obj)->kind == OBJ_ITER && OITER(Obj)->iter_kind == ITER_RANGE)

#include "lex.cpp"
#include "parser.cpp"
#include "resolver.cpp"
#include "obj.cpp"
#include "vm.cpp"
#include "debug.cpp"

void sss_repl() {
    char buf[1000];

    for ( ;; ) {
        printf(">>> ");
        gets_s(buf, sizeof(buf));

        auto tokens   = tokenize("<repl>", buf);
        auto ast      = parse(&tokens);
                        resolve(ast, false);
        auto bc       = compile(ast);
                        debug(bc, "output.S");
        auto result   = eval(bc);

        printf("-> %lld\n", result);
    }
}

namespace api {
    using Urq::Sss::Mem;

    using Urq::Sss::Vm::compile;
    using Urq::Sss::Vm::eval;
    using Urq::Sss::debug;
    using Urq::Sss::mem_new;
    using Urq::Sss::mem_reset;
    using Urq::Sss::obj_size;
    using Urq::Sss::parse;
    using Urq::Sss::resolve;
    using Urq::Sss::resolver_init;
    using Urq::Sss::resolver_reset;
    using Urq::Sss::sss_repl;
    using Urq::Sss::tokenize;

    using Urq::Sss::Vm::EVAL_REPL;
}

}}

#endif

