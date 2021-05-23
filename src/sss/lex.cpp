Map interns;

struct Ast_Node : Loc {
    bool   has_error;
    char * error_msg;
    bool   dump_ir;
};

#define TOKENS                        \
    X(T_NONE)                         \
    X(T_AT)                           \
    X(T_HASH)                         \
    X(T_COMMA)                        \
    X(T_COLON)                        \
    X(T_DOT)                          \
    X(T_SEMICOLON)                    \
    X(T_NOTE)                         \
    X(T_NOT)                          \
    X(T_LPAREN)                       \
    X(T_RPAREN)                       \
    X(T_LBRACKET)                     \
    X(T_RBRACKET)                     \
    X(T_LBRACE)                       \
    X(T_RBRACE)                       \
    X(T_PLUS)                         \
    X(T_MINUS)                        \
    X(T_ASTERISK)                     \
    X(T_SLASH)                        \
    X(T_OR)                           \
    X(T_AND)                          \
    X(T_XOR)                          \
    X(T_MODULO)                       \
    X(T_BIT_AND)                      \
    X(T_BIT_OR)                       \
    X(T_RANGE)                        \
    X(T_ELLIPSIS)                     \
    X(T_ARROW)                        \
    X(T_FAT_ARROW)                    \
    X(T_FIRST_CMP)                    \
    X(T_LTE)                          \
    X(T_GT)                           \
    X(T_GTE)                          \
    X(T_EQ)                           \
    X(T_NEQ)                          \
    X(T_EQL_ASSIGN)                   \
    X(T_PLUS_ASSIGN)                  \
    X(T_MINUS_ASSIGN)                 \
    X(T_OR_ASSIGN)                    \
    X(T_AND_ASSIGN)                   \
    X(T_XOR_ASSIGN)                   \
    X(T_MODULO_ASSIGN)                \
    X(T_ASTERISK_ASSIGN)              \
    X(T_SLASH_ASSIGN)                 \
    X(T_LSHIFT)                       \
    X(T_RSHIFT)                       \
    X(T_CHAR)                         \
    X(T_INT)                          \
    X(T_FLOAT)                        \
    X(T_STR)                          \
    X(T_IDENT)

enum Token_Kind {
#define X(Elem) Elem,
    TOKENS
#undef X

    T_LT = T_FIRST_CMP,
    T_LAST_CMP = T_NEQ,
    T_FIRST_ASSIGN = T_EQL_ASSIGN,
    T_LAST_ASSIGN = T_SLASH_ASSIGN,
};

struct Token : Ast_Node {
    Token_Kind kind;
    uint32_t   len;

    char *   val_str;
    int64_t  val_int;
    float    val_float;
};

typedef Token ** Tokens;

struct Token_List : Ast_Node {
    Tokens list;
    size_t curr;
};

bool
digit_valid(char c) {
    bool result = c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F';

    return result;
}

Token *
token_new(Token_Kind kind, char *file, size_t line, size_t col) {
    Token *result = urq_allocs(Token);

    result->kind = kind;
    result->file = file;
    result->line = line;
    result->col  = col;

    return result;
}

void
token_push(Token_List *list, Token *token) {
    buf_push(list->list, token);
}

Token *
token_str(Token_Kind kind, char *val_str, uint32_t len, char *file, size_t line, size_t col) {
    Token *result = token_new(kind, file, line, col);

    result->val_str = val_str;
    result->len     = len;

    return result;
}

Token *
token_int(Token_Kind kind, int64_t val_int, char *file, size_t line, size_t col) {
    Token *result = token_new(kind, file, line, col);

    result->val_int = val_int;

    return result;
}

Token *
token_float(Token_Kind kind, float val_float, char *file, size_t line, size_t col) {
    Token *result = token_new(kind, file, line, col);

    result->val_float = val_float;

    return result;
}

char *
token_val(char *str, size_t len) {
    char *result = (char *)urq_alloc(len+1);
    memcpy(result, str, len);
    result[len] = 0;

    result = intern_str(&interns, result);

    return result;
}

Token_List
tokenize(char *file, char *input) {
    Token_List result = {};

    uint64_t digits['g'];

    digits['0'] = 0;
    digits['1'] = 1;
    digits['2'] = 2;
    digits['3'] = 3;
    digits['4'] = 4;
    digits['5'] = 5;
    digits['6'] = 6;
    digits['7'] = 7;
    digits['8'] = 8;
    digits['9'] = 9;
    digits['A'] = 10;
    digits['B'] = 11;
    digits['C'] = 12;
    digits['D'] = 13;
    digits['E'] = 14;
    digits['F'] = 15;
    digits['a'] = 10;
    digits['b'] = 11;
    digits['c'] = 12;
    digits['d'] = 13;
    digits['e'] = 14;
    digits['f'] = 15;

    char *c = input;
    size_t line = 1;
    size_t col = 1;

#define NEXT_X(N) do { for (int i = 0; i < N; ++i) { size_t len = utf8_char_size(c); c += len; col += len; } } while(false)
#define NEXT() NEXT_X(1)
#define AT(N) (*(c+N) ? *(c+N) : 0)

    for ( ;; ) {
retry:
        while ( AT(0) == ' ' || AT(0) == '\t' || AT(0) == '\n' ) {
            if ( AT(0) == '\n' ) {
                line += 1;
                col = 1;
            } else {
                col++;
            }

            c++;
        }

        if ( AT(0) == '#' && AT(1) == '#' ) {
            NEXT_X(2);

            int recursion = 1;
recurse:
            if ( AT(0) == '(' ) {
                while ( recursion ) {
                    while ( AT(0) != '#' || AT(1) != '#' || AT(2) != ')' ) {
                        if ( AT(0) == '\n' ) {
                            line += 1;
                            col = 1;
                        }

                        NEXT();

                        if ( AT(0) == '#' && AT(1) == '#' && AT(2) == '(') {
                            NEXT_X(2);
                            recursion++;
                            goto recurse;
                        }
                    }

                    recursion--;
                    NEXT_X(3);
                }
            } else {
                while ( AT(0) != '\n' ) {
                    NEXT();
                }
            }

            goto retry;
        }

        if ( AT(0) == '\0' ) {
            break;
        } else if ( utf8_str_eq(c, "@", 1) ) {
            NEXT();
            token_push(&result, token_str(T_AT, "@", 1, file, line, col));
        } else if ( AT(0) == ';' ) {
            NEXT();
            token_push(&result, token_str(T_SEMICOLON, ";", 1, file, line, col));
        } else if ( AT(0) == '#' ) {
            NEXT();
            token_push(&result, token_str(T_HASH, "#", 1, file, line, col));
        } else if ( AT(0) == ',' ) {
            NEXT();
            token_push(&result, token_str(T_COMMA, ",", 1, file, line, col));
        } else if ( AT(0) == '!' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_NEQ, "!=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_NOT, "!", 1, file, line, col));
            }
        } else if ( AT(0) == '\'' ) {
            NEXT();

            char *start = c;
            if ( AT(0) == '\\' ) {
                NEXT();
            }

            NEXT();
            if ( AT(0) != '\'' ) {
                Loc loc = loc_new(file, line, col);
                report_error(&loc, "abschließendes ' erwartet");
            }
            NEXT();

            uint32_t len = 1;
            char *val = token_val(start, len);

            token_push(&result, token_str(T_CHAR, val, 1, file, line, col));
        } else if ( AT(0) == ':' ) {
            NEXT();
            token_push(&result, token_str(T_COLON, ":", 1, file, line, col));
        } else if ( AT(0) == '.' ) {
            NEXT();

            if ( AT(0) == '.' ) {
                NEXT();

                if ( AT(0) == '.' ) {
                    NEXT();
                    token_push(&result, token_str(T_ELLIPSIS, "...", 3, file, line, col));
                } else {
                    token_push(&result, token_str(T_RANGE, "..", 2, file, line, col));
                }
            } else {
                token_push(&result, token_str(T_DOT, ".", 1, file, line, col));
            }
        } else if ( AT(0) == '(' ) {
            NEXT();
            token_push(&result, token_str(T_LPAREN, "(", 1, file, line, col));
        } else if ( AT(0) == ')' ) {
            NEXT();
            token_push(&result, token_str(T_RPAREN, ")", 1, file, line, col));
        } else if ( AT(0) == '[' ) {
            NEXT();
            token_push(&result, token_str(T_LBRACKET, "[", 1, file, line, col));
        } else if ( AT(0) == ']' ) {
            NEXT();
            token_push(&result, token_str(T_RBRACKET, "]", 1, file, line, col));
        } else if ( AT(0) == '{' ) {
            NEXT();
            token_push(&result, token_str(T_LBRACE, "{", 1, file, line, col));
        } else if ( AT(0) == '}' ) {
            NEXT();
            token_push(&result, token_str(T_RBRACE, "}", 1, file, line, col));
        } else if ( AT(0) == '+' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_PLUS_ASSIGN, "+=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_PLUS, "+", 1, file, line, col));
            }
        } else if ( AT(0) == '-' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_MINUS_ASSIGN, "-=", 2, file, line, col));
                NEXT();
            } else if ( AT(0) == '>' ) {
                token_push(&result, token_str(T_ARROW, "->", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_MINUS, "-", 1, file, line, col));
            }
        } else if ( AT(0) == '*' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_ASTERISK_ASSIGN, "*=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_ASTERISK, "*", 1, file, line, col));
            }
        } else if ( AT(0) == '/' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_SLASH_ASSIGN, "/=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_SLASH, "/", 1, file, line, col));
            }
        } else if ( AT(0) == '%' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_MODULO_ASSIGN, "/=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_MODULO, "%", 1, file, line, col));
            }
        } else if ( AT(0) == '^' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_XOR_ASSIGN, "^=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_XOR, "^", 1, file, line, col));
            }
        } else if ( AT(0) == '|' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_OR_ASSIGN, "|=", 2, file, line, col));
                NEXT();
            } else if ( AT(0) == '|' ) {
                token_push(&result, token_str(T_OR, "||", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_BIT_OR, "|", 1, file, line, col));
            }
        } else if ( AT(0) == '&' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_AND_ASSIGN, "&=", 2, file, line, col));
                NEXT();
            } else if ( AT(0) == '&' ) {
                token_push(&result, token_str(T_AND, "&&", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_BIT_AND, "&", 1, file, line, col));
            }
        } else if ( AT(0) == '!' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_NEQ, "!=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_NOT, "!", 1, file, line, col));
            }
        } else if ( AT(0) == '<' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_LTE, "<=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_LT, "<", 1, file, line, col));
            }
        } else if ( AT(0) == '>' ) {
            NEXT();

            if ( AT(0) == '=' ) {
                token_push(&result, token_str(T_GTE, "<=", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_GT, ">", 1, file, line, col));
            }
        } else if ( AT(0) == '=' ) {
            NEXT();

            if ( AT(0) && AT(0) == '=' ) {
                token_push(&result, token_str(T_EQ, "==", 2, file, line, col));
                NEXT();
            } else if ( AT(0) && AT(0) == '>' ) {
                token_push(&result, token_str(T_FAT_ARROW, "=>", 2, file, line, col));
                NEXT();
            } else {
                token_push(&result, token_str(T_EQL_ASSIGN, "=", 1, file, line, col));
            }
        } else if ( AT(0) == '"' ) {
            NEXT();
            char *start = c;

            while ( AT(0) && AT(0) != '"' ) {
                if ( AT(0) == '\\' ) {
                    NEXT();
                }

                NEXT();
            }

            if ( AT(0) == '\0' ) {
                assert(!"fehlendes \" in einer zeichenkette");
            }

            if ( AT(0) == '"' ) {
                NEXT();
            }

            uint32_t len = (uint32_t)(c - start - 1);
            char *val = token_val(start, len);

            token_push(&result, token_str(T_STR, val, len, file, line, col));
        } else if ( utf8_char_isalpha(c) || AT(0) == '_' ) {
            char *start = c;
            NEXT();

            while ( AT(0) && (utf8_char_isalpha(c) || utf8_char_isnum(c)) || AT(0) == '_' ) {
                NEXT();
            }

            uint32_t len = (uint32_t)(c - start);
            char *val = token_val(start, len);

            token_push(&result, token_str(T_IDENT, val, len, file, line, col));
        } else if ( utf8_char_isnum(c) ) {
            size_t val = 0;

            while ( utf8_char_isnum(c) ) {
                val *= 10;
                val += AT(0) - '0';
                NEXT();
            }

            if ( AT(0) == 'b' ) {
                size_t base = val;
                val = 0;

                if ( base > 16 ) {
                    assert(!"basis darf nicht größer 16 sein");
                }

                NEXT();

                while ( digit_valid( AT(0) ) || AT(0) == '_' ) {
                    if ( AT(0) == '_') {
                        NEXT();
                        continue;
                    }

                    size_t num = digits[AT(0)];

                    if ( num >= base ) {
                        assert(!"ziffer darf nicht größer sein als die angegebene basis");
                    }

                    val *= base;
                    val += num;
                    NEXT();
                }
            }

            /* @AUFGABE: fließkomma parsen */
            if ( AT(0) == '.' && AT(1) != '.' ) {
                NEXT();

                float fracture = 0.0f;
                size_t divisor = 1;

                while ( utf8_char_isnum(c) ) {
                    divisor *= 10;
                    fracture += (float)((AT(0) - '0')) / divisor;
                    NEXT();
                }

                float val_float = val + fracture;

                token_push(&result, token_float(T_FLOAT, val_float, file, line, col));
            } else {
                token_push(&result, token_int(T_INT, val, file, line, col));
            }
        } else {
            assert(!"lex: unbekanntes zeichen");
        }
    }

    return result;

#undef AT
#undef NEXT
}

char *
intern_str(char *str) {
    return intern_str(&interns, str);
}

