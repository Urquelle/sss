#ifndef __URQ_SSS__
#define __URQ_SSS__

namespace Urq { namespace Sss {

#include "lex.cpp"
#include "parser.cpp"
#include "resolver.cpp"
#include "bc.cpp"

void sss_repl() {
    char buf[1000];

    for ( ;; ) {
        printf(">>> ");
        gets_s(buf, sizeof(buf));

        resolver_init();

        auto tokens   = tokenize("<repl>", buf);
        auto parsed   = parse(&tokens);
        auto resolved = resolve(parsed);
        auto code     = build(resolved);
        eval(code);
    }
}

namespace api {
    using Urq::Sss::Bytecode;

    using Urq::Sss::build;
    using Urq::Sss::parse;
    using Urq::Sss::resolve;
    using Urq::Sss::resolver_init;
    using Urq::Sss::sss_repl;
    using Urq::Sss::tokenize;
}

}}

#endif

