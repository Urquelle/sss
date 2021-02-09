#ifndef __URQ_SSS__
#define __URQ_SSS__

namespace Urq { namespace Sss {

#include "lex.cpp"
#include "parser.cpp"
#include "vm.cpp"
#include "resolver.cpp"
#include "bc.cpp"

namespace api {
    using Urq::Sss::Bytecode;
    using Urq::Sss::Env;

    using Urq::Sss::build;
    using Urq::Sss::env_new;
    using Urq::Sss::parse;
    using Urq::Sss::resolve;
    using Urq::Sss::resolver_init;
    using Urq::Sss::tokenize;
}

}}

#endif

