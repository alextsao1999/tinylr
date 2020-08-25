//
// Created by Alex on 8/24/2020.
//
#include <sstream>
#include <fstream>
#include "lalr.h"
#include "json.hpp"
#include "parser.h"
using namespace alex;
std::string escape(std::string_view view) {
    std::string result;
    result.reserve(view.length());
    for (auto &chr : view) {
        switch (chr) {
            case '\t':
                result += "\\t";
                break;
            case '\'':
                result += "\\'";
                break;
            case '\"':
                result += "\\\"";
                break;
            case '\n':
                result += "\\n";
                break;
            case '\\':
                result += "\\\\";
                break;
            case '\a':
                result += "\\a";
                break;
            case '\b':
                result += "\\b";
                break;
            case '\f':
                result += "\\f";
                break;
            case '\r':
                result += "\\r";
                break;
            default:
                result += chr;
                break;
        }
    }
    return std::move(result);
}
std::ostream &indent(std::ostream &os, int count = 0) {
    for (int i = 0; i < count * 4; ++i) {
        os << " ";
    }
    return os;
}
std::string parser_emit_c(LALRGenerator &generator) {
    std::stringstream out;
    out << "void parser_parse(const char *first, const char *last) {\n"
           "    #define ADVANCE() \n"
           "    #define GOTO(STATE) \n"
           "    #define REDUCE(SYMBOL, DEPTH) \n"
           "    #define SHIFT(STATE) \n"
           "    #define SYMBOL_NEXT() 0 \n"
           "    #define SYMBOL_TOP() 0 \n"
           "    #define ACTION(ACT, DEPTH) 0 \n"
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
                    indent(out, 5) << "ACTION(" << trans.reduce_action->base << ", " << trans.reduce_length
                                   << ");\n";
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
    out << "    #undef ADVANCE\n"
           "    #undef GOTO\n"
           "    #undef REDUCE\n"
           "    #undef SHIFT\n"
           "    #undef SYMBOL_NEXT\n"
           "    #undef SYMBOL_TOP\n"
           "    #undef ACTION\n"
           "}\n";
    return out.str();
}
std::string lexer_emit_c(LALRGenerator &generator) {
    std::stringstream out;
    Lexer lexer;
    for (auto &symbol : generator.get_symbols()) {
        if (!symbol->is_nonterminal()) {
            auto pattern = symbol->terminal()->pattern;
            bool literal = pattern.front() == '\'';
            pattern.remove_prefix(1);
            pattern.remove_suffix(1);
            if (literal) {
                lexer.add_literal(pattern, (SymbolType) symbol->index);
            } else {
                lexer.add_pattern(pattern, (SymbolType) symbol->index);
            }
        }
    }
    for (auto &node : lexer.generator.nodes) {
        node->print();
        std::cout << std::endl << std::endl << std::endl;
    }
    lexer.generate_states();
    out << "void lexer_advance(const char *first, const char *last) {\n"
           "    int state = 0, symbol = 0;\n"
           "    do {\n"
        << "        switch (state) {\n";
    for (auto &state : lexer.state_machine) {
        indent(out, 3) << "case " << state->index << ":\n";
        for (auto &trans : state->transitions) {
            indent(out, 4) << "if (*first >= " << trans.begin << " && *first <= " << trans.end << ") {\n";
            indent(out, 5) << "GOTO(" << trans.state->index << ");\n";
            indent(out, 4) << "}\n";
        }
        if (state->symbol != SymbolNull) {
            indent(out, 4) << "SYMBOL(" << (int) state->symbol << ");\n";
        }
        indent(out, 4) << "break;\n";
    }
    out << "        }\n";
    out << "    } while(true);\n";
    out << "}\n";
    return out.str();
}
std::string lexer_emit_state_machine(std::vector<std::unique_ptr<RegexState>> &states, const char *trans_name,
                                     const char *states_name) {
    std::stringstream out;
    out << "LexerTransition " << trans_name << "[] = {\n";
    for (auto &state : states) {
        for (auto &trans : state->transitions) {
            out << "    {"
                << trans.begin << ", "
                << trans.end << ", ";
            if (trans.state) {
                out << "&LexerStates[" << trans.state->index << "]";
            } else {
                out << "nullptr";
            }
            out << "}, \n";
        }
    }
    out << "};\n";
    int index = 0;
    out << "LexerState " << states_name << "[] = {\n";
    for (auto &state : states) {
        out << "    {"
            << "&LexerTransitions[" << index << "], "
            << "&LexerTransitions[" << (index + state->transitions.size()) << "], ";
        out << (int) state->symbol;
        out << "}, \n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();

}
std::string parser_emit_lexer(LALRGenerator &generator) {
    std::stringstream out;
    Lexer lexer;
    //lexer.set_whitespace("[ \t\n\r]+");
    for (auto &symbol : generator.get_symbols()) {
        if (!symbol->is_nonterminal()) {
            auto pattern = symbol->terminal()->pattern;
            bool literal = pattern.front() == '\'';
            pattern.remove_prefix(1);
            pattern.remove_suffix(1);
            if (literal) {
                lexer.add_literal(pattern, (SymbolType) symbol->index);
            } else {
                lexer.add_pattern(pattern, (SymbolType) symbol->index);
            }
        }
    }
    lexer.generate_states();
    out << "int LexerWhitespaceSymbol = " << (generator.whitespace ? generator.whitespace->index : -1)
        << ";\n";

    out << "LexerTransition LexerTransitions[] = {\n";
    for (auto &state : lexer.state_machine) {
        for (auto &trans : state->transitions) {
            out << "    {"
                << trans.begin << ", "
                << trans.end << ", ";
            if (trans.state) {
                out << "&LexerStates[" << trans.state->index << "]";
            } else {
                out << "nullptr";
            }
            out << "}, \n";
        }
    }
    out << "};\n";
    int index = 0;
    out << "LexerState LexerStates[] = {\n";
    for (auto &state : lexer.state_machine) {
        out << "    {"
            << "&LexerTransitions[" << index << "], "
            << state->transitions.size() << ", ";
        out << (int) state->symbol;
        out << "}, \n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_action(LALRGenerator &generator) {
    std::stringstream out;
    out << "ReduceAction ParserActions[] = {\n";
    for (auto &action : generator.get_actions()) {
        if (!action->base.empty()) {
            out << "    {"
                   "\"\", \""
                << action->base << "\", ";

            if (action->base.front() == '$') {
                out << "0, ";
            } else if (action->base.front() == '@') {
                out << "1, ";
            }

            out << (std::atoi(action->base.data() + 1) - 1)
                << "}, \n";
        }
        for (auto[key, value] : action->fields) {
            out << "    {\"" << key << "\", ";
            if (value.front() == '$') {
                out << "\"" << value << "\", ";
                out << "2, " << (std::atoi(value.data() + 1) - 1);
            } else if (value.front() == '@') {
                out << "\"" << value << "\", ";
                out << "3, " << (std::atoi(value.data() + 1) - 1);
            } else if (value.front() == '\'') {
                value.remove_prefix(1);
                value.remove_suffix(1);
                out << "\"" << value << "\", ";
                out << "4, 0";
            } else {
                out << "\"" << value << "\", ";
                out << "5, " << std::atoi(value.data());
            }
            out << "}, \n";
        }
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_states(LALRGenerator &generator) {
    std::stringstream out;
    out << "//\n"
           "// Created by Alex's tiny lalr.\n"
           "//\n"
           "#include \"parser.h\"\n";
    out << "ParserSymbol ParserSymbols[] = {\n";
    for (auto &symbol : generator.get_symbols()) {
        out << "    {"
            << symbol->is_nonterminal() << ", "
            << symbol->index << ", "
            << "\"" << escape(symbol->to_str()) << "\""
            << "}, \n";
    }
    out << "};\n";
    out << parser_emit_lexer(generator);
    out << parser_emit_action(generator);
    out << "\n";

    out << "ParserTransition ParserTransitions[] = {\n";
    for (auto &state : generator.get_states()) {
        for (auto &trans : state->transitions) {
            out << "    {"
                << trans.type << ", "
                << trans.symbol->index << ", ";
            if (trans.state) {
                out <<  "&ParserStates[" << trans.state->index << "], ";
            } else {
                out << "nullptr, ";
            }
            out << (trans.reduce_symbol ? trans.reduce_symbol->index : 0) << ", "
                << trans.reduce_length << ", ";
            if (trans.reduce_action) {
                out << "&ParserActions[" << trans.reduce_action->index << "], "
                    << trans.reduce_action->size();
            } else {
                out << "nullptr, 0";
            }
            out << "}, \n";
        }
    }
    out << "};\n";
    int index = 0;
    out << "ParserState ParserStates[] = {\n";
    for (auto &state : generator.get_states()) {
        out << "    {"
            << "&ParserTransitions[" << index << "], "
            << state->transitions.size()
            << "}, \n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}
void generate() {
    LALRGrammarParser lalr(
            "%left '+' '-' '*' '/';"
            "%start programs;"
            "%whitespace \"[ \n\r\t]+\";"
            "programs -> programs program $1{value:$2} | program {kind:'program', value:$1}; "
            "program -> fundef $1 | classdef $1 ;"
            "classdef -> 'class' identifier '{' classbody '}' {kind:'class', name:$2, body:$4} ;"
            "classbody -> classbody classmember $1{member:$2} | classmember {kind:'classbody', member:$1}; "
            "classmember -> fielddef $1 | fundef $1;"
            "fielddef -> vardef ';' $1;"
            "fundef -> type identifier '(' params ')' block {kind:'fundef', type:$1, name:$2, params:$4, block:$6};"
            "params -> params ',' paramdef $1{value:$3} | paramdef {kind:'params', value:$1} | ;"
            "paramdef -> type identifier {kind:'param', type:$1 , name:$2};"
            "vardef -> type identifier {kind:'vardef', type:$1, name:$2} | type identifier '=' expr {kind:'vardef', type:$1, name:$2, init:$4} ;"
            "block -> '{' stmts '}' $2;"
            "stmts -> stmts stmt ';' $1 {value:$2} | stmt ';' {kind:'stmts', value:$1};"
            "stmt -> expr $1 | assign $1 | vardef $1;"
            "assign -> identifier '=' expr {kind:'assign', left:$1, right:$3};"
            "type -> identifier $1;"
            "expr -> expr '+' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | expr '-' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | expr '*' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | expr '/' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | '(' expr ')' $2"
            "       | primary $1"
            ";"
            "primary -> number $1 | invoke $1 | identifier {kind:'var', name:$1};"
            "invoke -> identifier '(' args ')' {kind:'invoke', name:$1, args:$3};"
            "args -> args ',' expr $1{value:$3} | expr {kind:'arg_list', value:$1};"
            "identifier -> \"[a-zA-Z_][a-zA-Z0-9_]*\" @1;"
            "number -> \"[0-9]+\" {kind:'number', value:@1};"
    );
    LALRGenerator gen(lalr);
    gen.generate();
    std::fstream fs;
    fs.open("../parser.cpp", std::ios::trunc | std::ios::out);
    fs << parser_emit_states(gen);
    fs.close();
}

int main() {
    generate();
    return 0;
}