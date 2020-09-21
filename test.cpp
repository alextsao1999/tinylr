//
// Created by Alex on 8/21/2020.
//
#include "lexer.h"
#include "parser.h"
using namespace alex;
int main() {
    //const char *string = "a < b > c;";
    const char *string = "int main() {"
                         "  a<b> c;"
                         "}";
    GLRParser<StringIter<char>> parser;
    parser.set_position(true);
    parser.reset(string);
    parser.parse();
    if (parser.accept()) {
        json value = std::move(parser.value());
        std::cout << value.dump(4);
    }

    return 0;
}
