//
// Created by Alex on 8/21/2020.
//
#include "parser.h"
int main() {
    const char *string = "import aaa.bbb.ccc;\n"
                         "int<int, value> main() {\n"
                         "  a < b > c;\n"
                         "  test(a, b, c);"
                         "}";
    GLRParser<> parser(true);
    parser.reset(string, string + strlen(string));
    parser.parse();

    if (parser.accept()) {
        auto value = parser.value();
        std::cout << value.dump(4) << std::endl;
    }
    return 0;
}
