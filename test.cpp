//
// Created by Alex on 8/21/2020.
//
#include "lexer.h"
#include "parser.h"
#include "ast.h"
using namespace alex;
int main() {
    const char *string = "import alex.test.value;"
                         "int<int, value> main() {"
                         "  a < b > c;"
                         "}";
    GLRParser<StringIter<char>> parser;
    parser.set_position(true);
    parser.reset(string);
    parser.parse();
    if (parser.accept()) {
        value_t value = parser.value();
        value->dump(std::cout);

    }
    return 0;
}
