//
// Created by Alex on 8/21/2020.
//
#include "lalr.h"
#include "parser.h"
int main() {
    //generate();
    const char *string = "void test() {"
                         "}";
    Parser<> parser;
    parser.reset(string, string + strlen(string));
    parser.parse();
    std::cout << parser.value();

    return 0;
}
