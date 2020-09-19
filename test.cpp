//
// Created by Alex on 8/21/2020.
//
#include "lexer.h"
#include "parser.h"
using namespace alex;
int main() {
    //const char *string = "a < b > c;";
    const char *string = "int a = 10;"
                         "int main() {"
                         "  a<b> c;"
                         "}";
    GLRParser<StringIter<char>> parser;
    parser.reset(string);
    parser.parse();
    if (parser.accept()) {
        std::cout << parser.value().dump();
    }

    return 0;
}
