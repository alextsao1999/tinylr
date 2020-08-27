//
// Created by Alex on 8/21/2020.
//
#include "lexer.h"
#include "parser.h"
using namespace alex;
int main() {
    const char *string = "public void test() {\n"
                         "  int value = 100;\n"
                         "  a = get(1,2,3);\n"
                         "}\n";
    //FileStreamWrapper file("test.txt");
    Parser<StringIter<char>> parser;
    parser.reset(string);
    parser.parse();
    std::cout << parser.value().dump(4);

    return 0;
}
