
#define TEST_PROC(name) bool name(bool debug_test)
typedef TEST_PROC(Test_Proc);
Test_Proc *test_procs[50];
int test_procs_count;

#define TEST_VALIDATE(Expr) \
    bool success = true; \
    if ( !(Expr) ) { \
        printf(" \x1b[91mFEHLSCHLAG"); \
        success = false; \
    } else { \
        printf(" \x1b[92mIN ORDNUNG"); \
        success = true; \
    } \
    printf("\x1b[0m\n")

#define TEST_SETUP(proc)       test_procs[test_procs_count++] = proc
#define TEST_TOKENIZE(code)    auto tokens = tokenize(__FUNCTION__, code)
#define TEST_PARSE(T)          auto ast    = parse(&tokens)
#define TEST_RESOLVE(A)        resolve(A, false)
#define TEST_COMPILE(A)        auto obj = compile(A, NULL, VM_FLAGS_REPL)
#define TEST_DEBUG()                           \
    if ( debug_test ) {                 \
        debug(obj, __FILE__ "_out.S");  \
    }

#define TEST_EVAL_INT(O)  uint64_t result = eval(obj, VM_FLAGS_REPL)
#define TEST_EVAL_CHAR(O) char result = (char)eval(obj, VM_FLAGS_REPL)

#define TEST_(Label, Content, Expected_Result, Debug, Cast)          \
{                                                                    \
    auto tokens   = tokenize("test_expr", Content);                  \
    auto ast      = parse(&tokens);                                  \
                    resolve(ast, false);                             \
    auto obj      = compile(ast, NULL, VM_FLAGS_REPL);               \
                                                                     \
    if ( Debug ) {                                                   \
        debug(obj, "test_output.S");                                 \
    }                                                                \
                                                                     \
    auto result   = (Cast)eval(obj, VM_FLAGS_REPL);                  \
                                                                     \
    printf("test ausführen: %-20s", Label);                          \
                                                                     \
    bool test_success = (result == Expected_Result);                 \
    if ( test_success ) {                                            \
        printf(" \x1b[92mIN ORDNUNG");                               \
    } else {                                                         \
        printf(" \x1b[91mFEHLSCHLAG");                               \
    }                                                                \
                                                                     \
    printf("\x1b[0m\n");                                             \
                                                                     \
    success = success && test_success;                               \
                                                                     \
    resolver_reset();                                                \
}

#define TEST(Label, Content, Expected_Result) TEST_(Label, Content, Expected_Result, false, uint32_t)
#define TEST_DBG(Label, Content, Expected_Result) TEST_(Label, Content, Expected_Result, true, uint32_t)
#define TEST_CAST(Label, Content, Expected_Result, Cast) TEST_(Label, Content, Expected_Result, false, Cast)

