//
// Created by Alex on 8/21/2020.
//
#include "parser.h"
int main() {
    const char *string = "import aaa.bbb.ccc;\n"
                         "int<int, value> main() {\n"
                         "  a < b > c;\n"
                         "}";
    GLRParser<> parser(true);
    parser.reset(string, string + strlen(string));
    parser.parse();

    if (parser.accept()) {
        auto value = parser.value();
        //value->dump(std::cout);
        std::cout << value.dump(4) << std::endl;
    }
    return 0;
}
