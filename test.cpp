//
// Created by Alex on 8/21/2020.
//
#include <sstream>
#include "lalr.h"

using namespace alex;

std::ostream &indent(std::ostream &os, int count = 0) {
    for (int i = 0; i < count * 4; ++i) {
        os << " ";
    }
    return os;
}
std::string parser_emit_c(LALRGenerator &generator) {
    std::stringstream out;
    out << "void parser_parse(const char *first, const char *last) {\n"
           "    int state = 0, symbol = 0;\n"
           "    do {\n"
        << "        if (SYMBOL_TOP() == " << generator.start->index << ") {\n"
        << "            break;\n"
        << "        }\n"
        << "        switch (state) {\n";
    auto &states = generator.get_states();
    for (auto &state : states) {
        indent(out, 3) << "case " << state->index << ":\n";
        for (auto &trans : state->transitions) {
            if (trans.type == TransitionShift && !trans.symbol->is_nonterminal()) {
                indent(out, 4) << "if (SYMBOL_NEXT() == " << trans.symbol->index << ") {\n";
                indent(out, 5) << "SHIFT(" << trans.state->index << ");\n";
                indent(out, 5) << "ADVANCE();\n";
                indent(out, 4) << "}\n";
            }
            if (trans.type == TransitionShift && trans.symbol->is_nonterminal()) {
                indent(out, 4) << "if (SYMBOL_TOP() == " << trans.symbol->index << ") {\n";
                indent(out, 5) << "GOTO(" << trans.state->index << ");\n";
                indent(out, 4) << "}\n";
            }
            if (trans.type == TransitionReduce) {
                indent(out, 4) << "if (SYMBOL_NEXT() == " << trans.symbol->index << ") {\n";
                if (trans.reduce_action) {
                    indent(out, 5) << "ACTION(" << trans.reduce_action->identifier << ");\n";
                }
                indent(out, 5) << "REDUCE(" << trans.reduce_symbol->index
                            << ", " << trans.reduce_length << ");\n";
                indent(out, 4) << "}\n";
            }
        }
        indent(out, 4) << "break;\n";
    }
    out << "        }\n";
    out << "    } while(true);\n";
    out << "}\n";
    return out.str();
}
int main() {
    LALRGrammarParser lalr(
            "%left '+' '-' '*' '/';"
            "%start expr;"
            "expr -> expr '+' expr [add]"
            "       | expr '-' expr [sub]"
            "       | expr '*' expr [mul]"
            "       | expr '/' expr [div];"
            "expr -> \"[0-9]+\" [number];"
    );
    LALRGenerator gen(lalr);
    gen.generate();
    std::cout << parser_emit_c(gen);

    return 0;
}
