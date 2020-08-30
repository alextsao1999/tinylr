//
// Created by Alex on 8/24/2020.
//
#include <sstream>
#include <fstream>
#include "lalr.h"
#define ACTION_TYPE_INIT 0
#define ACTION_TYPE_INSERT 1
#define ACTION_TYPE_SET 2
#define ACTION_TYPE_INSERT_INT 3
#define ACTION_TYPE_INSERT_BOOL 4
#define ACTION_TYPE_INSERT_STRING 5
#define ACTION_TYPE_SET_INT 6
#define ACTION_TYPE_SET_BOOL 7
#define ACTION_TYPE_SET_STRING 8

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
        << "        if (SYMBOL_TOP() == " << generator.get_start()->index << ") {\n"
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
    Lexer<> lexer;
    for (auto &symbol : generator.get_symbols()) {
        if (!symbol->is_nonterminal()) {
            std::string_view pattern = symbol->terminal()->pattern;
            bool literal = pattern.front() == '\'';
            pattern.remove_prefix(1);
            pattern.remove_suffix(1);
            if (literal) {
                lexer.add_literal(pattern, symbol->index);
            } else {
                lexer.add_pattern(pattern, symbol->index);
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
std::string change_type(const std::string &type) {
    std::string string(type);
    strupr((char *) string.c_str());
    return string;
}
std::string parser_emit_lexer(LALRGenerator &generator) {
    std::stringstream out;
    Lexer<> lexer;
    //lexer.set_whitespace("[ \t\n\r]+");
    for (auto &symbol : generator.get_symbols()) {
        if (!symbol->is_nonterminal()) {
            std::string_view pattern = symbol->terminal()->pattern;
            bool literal = pattern.front() == '\'';
            pattern.remove_prefix(1);
            pattern.remove_suffix(1);
            if (literal) {
                lexer.add_literal(pattern, symbol->index);
            } else {
                lexer.add_pattern(pattern, symbol->index);
            }
        }
    }
    lexer.generate_states();
    out << "int LexerWhitespaceSymbol = " << (generator.get_whitespace() ? generator.get_whitespace()->index : -1)
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
            out << "},\n";
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
        out << "},\n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_symbols(LALRGenerator &generator) {
    std::stringstream out;
    out << "ParserSymbol ParserSymbols[] = {\n";
    for (auto &symbol : generator.get_symbols()) {
        out << "    {"
            << symbol->is_nonterminal() << ", "
            << symbol->index << ", "
            << "\"" << escape(symbol->to_str()) << "\""
            << "},\n";
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_action(LALRGenerator &generator, const char *prefix) {
    std::stringstream out;
    out << "ReduceAction ParserActions[] = {\n";
    for (auto &action : generator.get_actions()) {
        if (!action->base.empty()) {
            out << "    {"
                   "\"\", \""
                << action->base << "\", ";
            out << ACTION_TYPE_INIT << ", ";
            out << (std::atoi(action->base.data() + 1) - 1)
                << "},\n";
        }
        for (auto[key, value, symbol] : action->fields) {
            out << "    {\"" << key << "\", ";
            if (value.front() == '$' || value.front() == '#' || value.front() == '@') {
                // $ set value
                out << "\"" << value << "\", ";
                out << (key.empty() ? ACTION_TYPE_INSERT : ACTION_TYPE_SET) << ", "
                    << (std::atoi(value.data() + 1) - 1);
            } else if (value.front() == '\'' || value.front() == '\"') {
                std::string_view view = value;
                view.remove_prefix(1);
                view.remove_suffix(1);
                out << "\"" << view << "\", ";
                out << (key.empty() ? ACTION_TYPE_INSERT_STRING : ACTION_TYPE_SET_STRING) << ", 0";
            } else if (value == "false") {
                out << "\"false\", ";
                out << (key.empty() ? ACTION_TYPE_INSERT_BOOL : ACTION_TYPE_SET_BOOL) << ", 0";
            } else if (value == "true") {
                out << "\"true\", ";
                out << (key.empty() ? ACTION_TYPE_INSERT_BOOL : ACTION_TYPE_SET_BOOL) << ", 1";
            } else {
                out << "\"" << value << "\", ";
                if (symbol == Token_Identifier) {
                    out << (key.empty() ? ACTION_TYPE_INSERT_INT : ACTION_TYPE_SET_INT)
                        << ", " << prefix << change_type(value);
                } else {
                    out << (key.empty() ? ACTION_TYPE_INSERT_INT : ACTION_TYPE_SET_INT)
                        << ", " << std::atoi(value.data());
                }
            }
            out << "},\n";
        }
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_states(LALRGenerator &generator) {
    std::stringstream out;
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
            out << "},\n";
        }
    }
    out << "};\n";
    int index = 0;
    out << "ParserState ParserStates[] = {\n";
    for (auto &state : generator.get_states()) {
        out << "    {"
            << "&ParserTransitions[" << index << "], "
            << state->transitions.size()
            << "},\n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}
void generate_header(LALRGenerator &generator, const char *output, const char *prefix) {
    const char *header = "struct ParserSymbol {\n"
                         "    int type;\n"
                         "    int symbol;\n"
                         "    const char *text;\n"
                         "};\n"
                         "struct LexerState;\n"
                         "struct LexerTransition {\n"
                         "    int begin;\n"
                         "    int end;\n"
                         "    LexerState *state;\n"
                         "};\n"
                         "struct LexerState {\n"
                         "    LexerTransition *transitions;\n"
                         "    int transition_count;\n"
                         "    int symbol;\n"
                         "    inline LexerTransition *begin() { return transitions; }\n"
                         "    inline LexerTransition *end() { return transitions + transition_count; }\n"
                         "};\n"
                         "struct ReduceAction {\n"
                         "    const char *field;\n"
                         "    const char *desc;\n"
                         "    int type;\n"
                         "    int value;\n"
                         "};\n"
                         "struct ParserState;\n"
                         "struct ParserTransition {\n"
                         "    int type;\n"
                         "    int symbol;\n"
                         "    ParserState *state;\n"
                         "    int reduce_symbol;\n"
                         "    int reduce_length;\n"
                         "    ReduceAction *actions;\n"
                         "    int action_count;\n"
                         "};\n"
                         "struct ParserState {\n"
                         "    ParserTransition *transitions;\n"
                         "    int transition_count;\n"
                         "    inline ParserTransition *begin() { return transitions; }\n"
                         "    inline ParserTransition *end() { return transitions + transition_count; }\n"
                         "};\n"
                         "\n"
                         "extern int LexerWhitespaceSymbol;\n"
                         "extern ParserSymbol ParserSymbols[];\n"
                         "extern LexerState LexerStates[];\n"
                         "extern LexerTransition LexerTransitions[];\n"
                         "extern ParserState ParserStates[];\n"
                         "extern ReduceAction ParserActions[];\n"
                         "extern ParserTransition ParserTransitions[];\n"
                         "\n"
                         "template <class char_t = char, class char_traits = std::char_traits<char_t>>\n"
                         "struct ParserNode {\n"
                         "    ParserState *state;\n"
                         "    int symbol;\n"
                         "    int line = 0;\n"
                         "    int column = 0;\n"
                         "    std::basic_string<char_t, char_traits> lexeme;\n"
                         "    json value;\n"
                         "    ParserNode(ParserState *state, int symbol = 0) : state(state), symbol(symbol) {}\n"
                         "    ParserNode(ParserState *state, int symbol, int line, int column,\n"
                         "               const std::basic_string<char_t, char_traits> &lexeme, const json &value) :\n"
                         "            state(state), symbol(symbol),\n"
                         "            line(line), column(column),\n"
                         "            lexeme(lexeme),\n"
                         "            value(std::move(value)) {}\n"
                         "};\n"
                         "template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>\n"
                         "class ParserLexer {\n"
                         "    using uchar_t = typename std::make_unsigned<char_t>::type;\n"
                         "    using string_t = std::basic_string<char_t, char_traits>;\n"
                         "    LexerState *lexer_state /* = &LexerStates[0]*/;\n"
                         "    int whitespace /*= LexerWhitespaceSymbol*/;\n"
                         "    iter_t current;\n"
                         "    iter_t end;\n"
                         "    int line_start = 0;\n"
                         "    int token_start = 0;\n"
                         "    int token_symbol = 0;\n"
                         "    int line_ = 0;\n"
                         "    int position_ = 0;\n"
                         "    string_t lexeme_;\n"
                         "private:\n"
                         "    inline LexerTransition *find_trans(LexerState *state, uchar_t chr) {\n"
                         "        LexerTransition *dot_trans = nullptr;\n"
                         "        for (auto &trans : *state) {\n"
                         "            if (trans.end == -1) {\n"
                         "                dot_trans = &trans;\n"
                         "            }\n"
                         "            if (trans.begin <= chr && chr <= trans.end) {\n"
                         "                return &trans;\n"
                         "            }\n"
                         "        }\n"
                         "        return dot_trans;\n"
                         "    }\n"
                         "    auto advance_symbol() {\n"
                         "        LexerState *state = lexer_state;\n"
                         "        lexeme_.clear();\n"
                         "        do {\n"
                         "            if (current == end) {\n"
                         "                return state->symbol;\n"
                         "            }\n"
                         "            auto *trans = find_trans(state, *current);\n"
                         "            if (trans) {\n"
                         "                lexeme_ += *current;\n"
                         "                state = trans->state;\n"
                         "                ++position_;\n"
                         "                if (*current == char_t('\\n')) {\n"
                         "                    ++line_;\n"
                         "                    line_start = position_;\n"
                         "                }\n"
                         "                ++current;\n"
                         "            } else {\n"
                         "                break;\n"
                         "            }\n"
                         "        } while (true);\n"
                         "        if (state == lexer_state && *current != '\\0') {\n"
                         "            std::cout << \"Unexpect char: \" << *current << \" line:\" << line() << std::endl;\n"
                         "            ++current;\n"
                         "            return 2; // error symbol\n"
                         "        }\n"
                         "        return state->symbol;\n"
                         "    }\n"
                         "public:\n"
                         "    ParserLexer(LexerState *state, int whitespace = -1) : lexer_state(state), whitespace(whitespace) {}\n"
                         "    void reset(iter_t first, iter_t last) {\n"
                         "        current = first;\n"
                         "        end = last;\n"
                         "    }\n"
                         "    void advance() {\n"
                         "        do {\n"
                         "            token_start = position_;\n"
                         "            token_symbol = advance_symbol();\n"
                         "        } while (token_symbol == whitespace);\n"
                         "    }\n"
                         "    int symbol() { return token_symbol; }\n"
                         "    int line() const { return line_; }\n"
                         "    int column() const { return token_start - line_start; }\n"
                         "    string_t &lexeme() { return lexeme_; }\n"
                         "    void dump() {\n"
                         "        do {\n"
                         "            advance();\n"
                         "            std::cout << lexeme_ << \"  \" << token_symbol\n"
                         "                      << \"[\" << line() << \", \" << column() << \"]\" << std::endl;\n"
                         "\n"
                         "            if (token_symbol == 0) {\n"
                         "                break;\n"
                         "            }\n"
                         "        } while (symbol() != 0);\n"
                         "        exit(0);\n"
                         "    }\n"
                         "};\n"
                         "template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>\n"
                         "class Parser {\n"
                         "    using Lexer = ParserLexer<iter_t>;\n"
                         "    using Node = ParserNode<char_t, char_traits>;\n"
                         "    ParserState *parser_state = &ParserStates[0];\n"
                         "    Lexer parser_lexer = Lexer(&LexerStates[0], LexerWhitespaceSymbol);\n"
                         "    bool position = false;\n"
                         "    inline ParserTransition *find_trans(ParserState *state, int symbol) {\n"
                         "        for (auto &trans : *state) {\n"
                         "            if (trans.symbol == symbol) {\n"
                         "                return &trans;\n"
                         "            }\n"
                         "        }\n"
                         "        return nullptr;\n"
                         "    }\n"
                         "public:\n"
                         "    std::vector<Node> stack;\n"
                         "    Parser() = default;\n"
                         "    void set_position(bool sp) {\n"
                         "        position = sp;\n"
                         "    }\n"
                         "    void reset(iter_t first, iter_t last = iter_t()) {\n"
                         "        parser_lexer.reset(first, last);\n"
                         "    }\n"
                         "    void parse() {\n"
                         "        parser_lexer.advance();\n"
                         "        stack.reserve(32);\n"
                         "        stack.push_back(Node(parser_state));\n"
                         "        do {\n"
                         "            auto *trans = find_trans(stack.back().state, parser_lexer.symbol());\n"
                         "            if (!trans) {\n"
                         "                if (!handle_error()) {\n"
                         "                    break;\n"
                         "                }\n"
                         "                continue;\n"
                         "            }\n"
                         "            if (trans->type == TRANSITION_SHIFT) { //Shift\n"
                         "                shift(trans);\n"
                         "            } else {\n"
                         "                reduce(trans);\n"
                         "                if (stack.back().symbol == 0) {\n"
                         "                    break;\n"
                         "                }\n"
                         "            }\n"
                         "        } while (true);\n"
                         "    }\n"
                         "    json &value() { return stack[0].value; }\n"
                         "    inline void shift(ParserTransition *trans) {\n"
                         "        //debug_shift(trans);\n"
                         "        stack.emplace_back(trans->state,\n"
                         "                           parser_lexer.symbol(),\n"
                         "                           parser_lexer.line(),\n"
                         "                           parser_lexer.column(),\n"
                         "                           parser_lexer.lexeme(),\n"
                         "                           json());\n"
                         "        parser_lexer.advance();\n"
                         "    }\n"
                         "    inline void reduce(ParserTransition *trans) {\n"
                         "        auto stack_start = (stack.size() - trans->reduce_length);\n"
                         "        //debug_reduce(trans, stack_start);\n"
                         "        json reduce_value;\n"
                         "        int line = 0;\n"
                         "        int column = 0;\n"
                         "        if (stack_start != stack.size()) {\n"
                         "            line = stack[stack_start].line;\n"
                         "            column = stack[stack_start].column;\n"
                         "            reduce_value = handle_action(trans->actions, trans->action_count, &stack[stack_start]);\n"
                         "            if (position && reduce_value.is_object()) {\n"
                         "                reduce_value[\"position\"] = {{\"line\",   line},\n"
                         "                                            {\"column\", column}};\n"
                         "            }\n"
                         "            stack.erase(stack.begin() + stack_start, stack.end());\n"
                         "            if (trans->reduce_symbol == 0) {\n"
                         "                stack.back().value = std::move(reduce_value);\n"
                         "                return;\n"
                         "            }\n"
                         "        }\n"
                         "        auto *new_trans = find_trans(stack.back().state, trans->reduce_symbol);\n"
                         "        stack.emplace_back(new_trans->state,\n"
                         "                           trans->reduce_symbol,\n"
                         "                           line,\n"
                         "                           column,\n"
                         "                           std::string(ParserSymbols[trans->reduce_symbol].text),\n"
                         "                           reduce_value);\n"
                         "    }\n"
                         "    inline void expect() {\n"
                         "        Node &node = stack.back();\n"
                         "        std::cout << \"Shift Reduce Error \"\n"
                         "                     \"line: \" << node.line + 1 << \" \"\n"
                         "                  << \"column: \" << node.column + 1 << \" \"\n"
                         "                  << \"token: \" << parser_lexer.lexeme()\n"
                         "                  << std::endl;\n"
                         "        std::cout << \"Expect: \";\n"
                         "        for (auto &trans : *node.state) {\n"
                         "            std::cout << \"\\\"\" << ParserSymbols[trans.symbol].text << \"\\\"\";\n"
                         "            if (&trans != (node.state->end() - 1)) {\n"
                         "                std::cout << \", \";\n"
                         "            }\n"
                         "        }\n"
                         "        std::cout << std::endl;\n"
                         "\n"
                         "    }\n"
                         "    inline void debug_shift(ParserTransition *trans) {\n"
                         "        std::cout << \"shift: \" << stack.size() << \" \"\n"
                         "                  << \"[state \" << (stack.back().state - ParserStates) << \" -> \" << (trans->state - ParserStates)\n"
                         "                  << \"]  [\" << ParserSymbols[parser_lexer.symbol()].text << \"] \"\n"
                         "                  << parser_lexer.lexeme()\n"
                         "                  << std::endl << std::endl;\n"
                         "    }\n"
                         "    inline void debug_reduce(ParserTransition *trans, int start) {\n"
                         "        ParserTransition *goto_trans = find_trans(stack[start - 1].state, trans->reduce_symbol);\n"
                         "        if (!goto_trans) {\n"
                         "            return;\n"
                         "        }\n"
                         "        std::cout << \"reduce: \"\n"
                         "                  << start << \" \"\n"
                         "                  << \"[back to \" /*<< (stack.back().state - ParserStates) << \" -> \"*/\n"
                         "                  << (stack[start - 1].state - ParserStates) << \" -> \"\n"
                         "                  << (goto_trans->state - ParserStates) << \"] \"\n"
                         "                  << ParserSymbols[trans->reduce_symbol].text << \" <- \";\n"
                         "        for (int i = start; i < start + trans->reduce_length; ++i) {\n"
                         "            std::cout << \"[\" << stack[i].lexeme << \"] \" << stack[i].value << \" | \";\n"
                         "        }\n"
                         "        std::cout << std::endl << std::endl;\n"
                         "    }\n"
                         "    inline json handle_action(ReduceAction *actions, int action_count, Node *nodes) {\n"
                         "        if (action_count == 0) {\n"
                         "            return std::move(nodes->value);\n"
                         "        }\n"
                         "        json value;\n"
                         "        for (int i = 0; i < action_count; ++i) {\n"
                         "            auto &action = actions[i];\n"
                         "            if (action.type == ACTION_TYPE_INIT && *action.desc == '$') {  // $n\n"
                         "                value = std::move((nodes + action.value)->value);\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_INIT && *action.desc == '@') { // @n\n"
                         "                value = (nodes + action.value)->lexeme;\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_INIT && *action.desc == '#') { // #n\n"
                         "                value = json::array({std::move((nodes + action.value)->value)});\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_INSERT) { // {$n|@n|#n}\n"
                         "                if (!value.is_array()) {\n"
                         "                    value = json::array({std::move(value)});\n"
                         "                }\n"
                         "                if (*action.desc == '@') {\n"
                         "                    value.emplace_back((nodes + action.value)->lexeme);\n"
                         "                } else {\n"
                         "                    value.emplace_back(std::move((nodes + action.value)->value));\n"
                         "                }\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_SET && *action.desc == '$') { // key:$n set value\n"
                         "                value[std::string(action.field)] = std::move((nodes + action.value)->value);\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_SET && *action.desc == '@') { // key:@n set lexeme\n"
                         "                value[std::string(action.field)] = (nodes + action.value)->lexeme;\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_SET && *action.desc == '#') { // key:#n insert\n"
                         "                value[std::string(action.field)].emplace_back(std::move((nodes + action.value)->value));\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_SET_STRING) {  // key:'string'\n"
                         "                value[std::string(action.field)] = action.desc;\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_SET_BOOL) { // key:bool\n"
                         "                value[std::string(action.field)] = json::boolean_t(action.value);\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_SET_INT) { // key:number\n"
                         "                value[std::string(action.field)] = json::number_integer_t(action.value);\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_INSERT_STRING) {\n"
                         "                if (value.empty()) {\n"
                         "                    value = action.desc;\n"
                         "                } else {\n"
                         "                    if (value.is_array()) {\n"
                         "                        value.emplace_back(action.desc);\n"
                         "                    } else {\n"
                         "                        value = json::array({std::move(value), action.desc});\n"
                         "                    }\n"
                         "                }\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_INSERT_BOOL) { // bool\n"
                         "                value.emplace_back(json::boolean_t(action.value));\n"
                         "            }\n"
                         "            if (action.type == ACTION_TYPE_INSERT_INT) { // number\n"
                         "                value.emplace_back(json::number_integer_t(action.value));\n"
                         "            }\n"
                         "        }\n"
                         "        return std::move(value);\n"
                         "    }\n"
                         "    inline bool handle_error() {\n"
                         "        auto *trans = find_trans(stack.back().state, 2);\n"
                         "        if (trans == nullptr) {\n"
                         "            expect();\n"
                         "            return false;\n"
                         "        }\n"
                         "        json value = json::array();\n"
                         "        ParserState *state = trans->state;\n"
                         "        do {\n"
                         "            value.push_back({{\"lexeme\", parser_lexer.lexeme()},\n"
                         "                                {\"line\",   parser_lexer.line()},\n"
                         "                                {\"column\", parser_lexer.column()}});\n"
                         "            parser_lexer.advance();\n"
                         "            ParserTransition *new_trans = find_trans(state, parser_lexer.symbol());\n"
                         "            if (new_trans) {\n"
                         "                stack.emplace_back(trans->state,\n"
                         "                                   2,\n"
                         "                                   parser_lexer.line(),\n"
                         "                                   parser_lexer.column(),\n"
                         "                                   \"error\",\n"
                         "                                   std::move(value));\n"
                         "                if (new_trans->type == TRANSITION_SHIFT) {\n"
                         "                    shift(new_trans);\n"
                         "                } else {\n"
                         "                    reduce(new_trans);\n"
                         "                }\n"
                         "                break;\n"
                         "            }\n"
                         "        } while (true);\n"
                         "        return true;\n"
                         "    }\n"
                         "};\n"
                         "\n"
                         "#endif //TINYLALR_PARSER_H";
    std::fstream fs;
    fs.open(output, std::ios::trunc | std::ios::out);
    fs << "//\n"
          "// Created by Alex\n"
          "//\n"
          "\n"
          "#ifndef TINYLALR_PARSER_H\n"
          "#define TINYLALR_PARSER_H\n"
          "#include \"json.hpp\"\n"
          "#include <iostream>\n"
          "#define TRANSITION_SHIFT 1\n"
          "#define TRANSITION_REDUCE 2\n"
          "#define ACTION_TYPE_INIT 0\n"
          "#define ACTION_TYPE_INSERT 1\n"
          "#define ACTION_TYPE_SET 2\n"
          "#define ACTION_TYPE_INSERT_INT 3\n"
          "#define ACTION_TYPE_INSERT_BOOL 4\n"
          "#define ACTION_TYPE_INSERT_STRING 5\n"
          "#define ACTION_TYPE_SET_INT 6\n"
          "#define ACTION_TYPE_SET_BOOL 7\n"
          "#define ACTION_TYPE_SET_STRING 8\n"
          "using json = nlohmann::json;\n";
    if (generator.grammar.types.size() > 0) {
        fs << "enum {\n";
        fs << "    " << prefix << "NONE" << ",\n";
        for (auto &type : generator.grammar.types) {
            fs << "    " << prefix << change_type(type) << ",\n";
        }
        fs << "};\n";
        //fs << "#define " << prefix << "LIST_COUNT " << generator.grammar.types.size() << "\n";
        fs << "#define " << prefix << "LIST(v) ";
        for (auto &type : generator.grammar.types) {
            if (type == "true" || type == "false") {

            } else {
                fs << " \\\n  v(" << prefix << change_type(type) << ", " << type << ")";
            }
        }
        fs << "\n";
    }
    fs << header;
    fs.close();
}
void generate(LALRGenerator &gen, const char *output, const char *prefix) {
    std::fstream fs;
    fs.open(output, std::ios::trunc | std::ios::out);
    fs << "//\n"
          "// Created by Alex's tiny lalr.\n"
          "//\n"
          "#include \"parser.h\"\n";
    fs << parser_emit_symbols(gen);
    fs << parser_emit_lexer(gen);
    fs << parser_emit_action(gen, prefix);
    fs << parser_emit_states(gen);
    fs.close();
    std::string file(output);
    std::string str = file.substr(0, file.find_last_of('.')) + ".h";
    generate_header(gen, str.c_str(), prefix);
}
void generate_grammar_file(const char *grammar_file, const char *output, const char *prefix) {
    std::fstream input(grammar_file, std::ios::in);
    LALRGrammarParser<StreamIter> lalr(input);
    input.close();
    LALRGenerator gen(lalr.grammar);
    gen.generate();
    generate(gen, output, prefix);
}
void generate_grammar_string(const char *input, const char *output, const char *prefix) {
    LALRGrammarParser<StringIter<char>> lalr(input);
    LALRGenerator gen(lalr.grammar);
    gen.generate();
    generate(gen, output, prefix);
}

int main(int argc, char **argv) {
    const char *input = "grammar.txt";
    const char *output = "../parser.cpp";
    const char *prefix = "TYPE_";
    bool help = false;
    for (int index = 1; index < argc; index++) {
        if (strcmp(argv[index], "-o") == 0 || strcmp(argv[index], "--output") == 0) {
            output = argv[index + 1];
            index += 2;
        } else if (strcmp(argv[index], "-h") == 0 || strcmp(argv[index], "--help") == 0) {
            help = true;
            index += 1;
        } else if (strcmp(argv[index], "-p") == 0 || strcmp(argv[index], "--prefix") == 0) {
            prefix = argv[index + 1];
            index += 2;
        } else {
            input = argv[index];
        }
    }
    generate_grammar_file(input, output, prefix);

    if (help) {
        //std::cout << "Grammar Generator" << std::endl;
    }
    return 0;
}
