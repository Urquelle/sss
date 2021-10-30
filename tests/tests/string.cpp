TEST_PROC(test_string) {
    TEST_SETUP(test_string);
    TEST_TOKENIZE(R"ENDE(master :: proc() -> n32 { x := "abcdef"; y := x[0]; wenn y == 'a' { erg 5; } erg 10; })ENDE");
    TEST_PARSE(tokens);
    TEST_RESOLVE(ast);
    TEST_COMPILE(ast);
    TEST_DEBUG();
    TEST_EVAL_INT(obj);
    TEST_VALIDATE(result == 5);

    return success;
}
