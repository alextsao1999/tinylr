//
// Created by Alex on 8/21/2020.
//
#include "lalr.h"
#include "parser.h"
#include <fstream>
int main() {
    const char *string = "class Object {\n"
                         "    int value = 10;\n"
                         "    int get_value(int abc) {\n"
                         "        return(value);\n"
                         "    }\n"
                         "    int get_value2(int abc) {\n"
                         "        return(value);\n"
                         "    }\n"
                         "}\n"
                         "void test() {\n"
                         "  int value = 100;\n"
                         "  a = get(1,2,3);\n"
                         "}\n";
    Parser<> parser;
    parser.reset(string, string + strlen(string));
    parser.parse();
    std::cout << parser.value().dump(4);
    return 0;
}
