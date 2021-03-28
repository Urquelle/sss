enum Line_Arg_Flags {
    LINE_ARG_NOT_REQUIRED,
    LINE_ARG_REQUIRED,
};

struct Line_Arg {
    char     ** ptr;
    char      * name;
    char      * option;
    char      * desc;
    char      * default_value;
    uint32_t    flags;
};

typedef Line_Arg ** Line_Args;

Line_Args
line_arg_push(Line_Args args, Line_Arg *arg) {
    buf_push(args, arg);

    return args;
}

Line_Arg *
line_arg(char **ptr, char *name, char *option, char *desc = NULL, uint32_t flags = LINE_ARG_NOT_REQUIRED, char *default_value = NULL) {
    Line_Arg *result = urq_allocs(Line_Arg);

    result->ptr           = ptr;
    result->name          = name;
    result->option        = option;
    result->desc          = desc;
    result->default_value = default_value;
    result->flags         = flags;

    return result;
}

Line_Arg *
line_arg(char *name, char *option, char *default_value) {
    Line_Arg *result = line_arg(NULL, name, option, NULL, LINE_ARG_NOT_REQUIRED, default_value);

    return result;
}

void
line_arg_print(Line_Arg *arg) {
    using namespace Urq::Os::api;

    os_stdout_set_bold();
    os_print(arg->name);
    os_stdout_set_regular();
    os_print(" -%s", arg->option);

    if ( arg->desc ) {
        os_print(" %s", arg->desc);
    }

    os_print("\n");
}

void
print_usage(char **cmd_args, Line_Args args) {
    using namespace Urq::Os::api;

    os_print("Verwendung:\n\t");
    os_stdout_set_bold();
    os_print(cmd_args[0]);
    os_stdout_set_regular();

    int args_num = buf_len(args);
    for ( int i = 0; i < args_num; ++i ) {
        Line_Arg *arg = args[i];

        if ( arg->flags & LINE_ARG_NOT_REQUIRED ) {
            os_print(" [");
        } else {
            os_print(" <");
        }

        os_print(arg->name);

        if ( arg->flags & LINE_ARG_NOT_REQUIRED ) {
            os_print("]");
        } else {
            os_print(">");
        }
    }

    os_print("\nArgumente:\n");

    for ( int i = 0; i < args_num; ++i ) {
        os_print("\t");
        line_arg_print(args[i]);
    }

    os_print("\n");
}

void
parse_args(int32_t count, char **line_args, Line_Args app_args, bool exit_on_error = false) {
    using namespace Urq::Os::api;

    int32_t app_args_num = buf_len(app_args);

    int32_t required_args_num = 0;
    for ( int i = 0; i < app_args_num; ++i ) {
        Line_Arg *arg = app_args[i];

        if ( arg->flags & LINE_ARG_REQUIRED && !arg->default_value ) {
            required_args_num += 1;
        }
    }

    Line_Args given_args = NULL;
    for ( int i = 1; i < count; ++i ) {
        char *arg = line_args[i];

        char *name   = NULL;
        char *option = NULL;
        char *val    = NULL;

        if ( arg[0] == '-' ) {
            arg++;
            option = arg;

            if ( i+1 < count ) {
                if ( line_args[i+1][0] != '-' ) {
                    val = line_args[i+1];
                    i++;
                }
            }

            buf_push(given_args, line_arg(name, option, val));
        } else {
            Line_Arg *app_arg = app_args[i-1];

            buf_push(given_args, line_arg(app_arg->name, app_arg->option, arg));
        }
    }

    int32_t given_args_num = buf_len(given_args);
    if ( required_args_num > given_args_num ) {
        print_usage(line_args, app_args);

        if ( exit_on_error ) {
            assert(0);
            exit(1);
        }
    }

    for ( int i = 0; i < given_args_num; ++i ) {
        Line_Arg *given_arg = given_args[i];
        for ( int j = 0; j < app_args_num; ++j ) {
            Line_Arg *app_arg = app_args[j];

            if ( utf8_str_eq(given_arg->name, app_arg->name) ||
                 utf8_str_eq(given_arg->option, app_arg->option) )
            {
                *app_arg->ptr = given_arg->default_value ? given_arg->default_value : app_arg->default_value;
            }
        }
    }
}
