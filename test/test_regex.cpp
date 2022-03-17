//
// Created by Alex on 2022/2/2.
//
#include "lexer.h"
#include <vector>
int main() {
    using namespace alex;

    Lexer<const char *> lexer;
    lexer.set_whitespace("([ \r\t\n]+)|(/\\*.*\\*/)|(//.*\n)");
    lexer.add_pattern("\"(\\\\.|.)*\"|'(\\\\.|.)*'", 1);
    lexer.add_pattern("[a-zA-Z_][a-zA-Z0-9_]*", 2);
    lexer.add_literal("abc", 3);
//    lexer.add_literal("<", 7);
//    lexer.add_literal(">", 8);
    lexer.generate_states();

    //auto str = "1234 1 aaasdfb 222 333 'a\\'b..asdf' \"abc\" bsdfa { }";
    auto str = ""
               "b a ab abc cc 'abc' aa '\\'xx' ";

    lexer.reset(str, str + strlen(str));
    lexer.dump();
    return 0;
}