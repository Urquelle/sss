#define TEST_(Label, Content, Expected_Result, Debug, Cast)       \
{                                                                    \
    auto tokens   = tokenize("test_expr", Content);                  \
    auto ast      = parse(&tokens);                                  \
                    resolve(ast, false);                             \
    auto bc       = compile(ast, mem);                               \
                                                                     \
    if ( Debug ) {                                                   \
        debug(bc, "test_output.S");                                  \
    }                                                                \
                                                                     \
    auto result   = (Cast)eval(bc, mem, EVAL_REPL);                  \
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
    mem_reset(mem);                                                  \
    resolver_reset();                                                \
}

#define TEST(Label, Content, Expected_Result) TEST_(Label, Content, Expected_Result, false, uint32_t)
#define TEST_DBG(Label, Content, Expected_Result) TEST_(Label, Content, Expected_Result, true, uint32_t)
#define TEST_CAST(Label, Content, Expected_Result, Cast) TEST_(Label, Content, Expected_Result, false, Cast)

