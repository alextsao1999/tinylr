//
// Created by Alex on 8/21/2020.
//
#include "lalr.h"
using namespace alex;
int main() {
    LALRGrammarParser lalr("%left '+' '-' '*' '/';"
                      "%start expr;"
                      "expr -> expr '+' expr;"
                      "expr -> expr '-' expr;"
                      "expr -> expr '*' expr;"
                      "expr -> expr '/' expr;"
                      "expr -> \"[0-9]+\"");
    LALRGenerator gen(lalr);
    gen.generate();
    return 0;
}
