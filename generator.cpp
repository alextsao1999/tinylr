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
struct Options {
    const char *input;
    const char *output;
    const char *prefix;
    int type;
};
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
        for (auto[key, value, symbol] : Reverse(action->fields)) {
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
                << trans.reduce_length << ", "
                << trans.precedence << ", ";
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
    int state_index = 0;
    for (auto &state : generator.get_states()) {
        out << "    {"
            << "&ParserTransitions[" << index << "], "
            << state->transitions.size() << ", "
            << state->conflict << ", ";
        if (state->error) {
            out << "&ParserTransitions[" << state->error->index << "]";
        } else {
            out << "nullptr";
        }
        out << "},\n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}
void generate_header(LALRGenerator &generator, const char *output, const char *prefix, int parser_type) {
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
                         "    int precedence;\n"
                         "    ReduceAction *actions;\n"
                         "    int action_count;\n"
                         "    inline bool accept() const { return reduce_symbol == 0 && type == TRANSITION_REDUCE; }\n"
                         "    inline bool error() const { return symbol == 2 && type == TRANSITION_SHIFT; }\n"
                         "};\n"
                         "struct ParserState {\n"
                         "    ParserTransition *transitions;\n"
                         "    int transition_count;\n"
                         "    int conflict;\n"
                         "    ParserTransition *error;\n"
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
                         "\n";
    const char *parser = "template <class char_t = char, class char_traits = std::char_traits<char_t>>\n"
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
                         "\n";

    const char *glr_parser = "template <class char_t = char, class char_traits = std::char_traits<char_t>>\n"
                             "struct ParserTreeNode {\n"
                             "    struct Link {\n"
                             "        std::shared_ptr<ParserTreeNode> node;\n"
                             "        std::shared_ptr<Link> next;\n"
                             "        Link(const std::shared_ptr<ParserTreeNode> &node) : node(node) {}\n"
                             "        Link(const std::shared_ptr<ParserTreeNode> &node, const std::shared_ptr<Link> &next) : node(node), next(next) {}\n"
                             "        void to_nodes(std::vector<ParserTreeNode *> &nodes) {\n"
                             "            nodes.clear();\n"
                             "            auto start = next;\n"
                             "            while (start) {\n"
                             "                nodes.push_back(start->node.get());\n"
                             "                start = start->next;\n"
                             "            }\n"
                             "        }\n"
                             "    };\n"
                             "    std::shared_ptr<ParserTreeNode> prev;\n"
                             "    std::vector<std::shared_ptr<ParserTreeNode>> prevs;\n"
                             "    ParserState *state = nullptr;\n"
                             "    int symbol = 0;\n"
                             "    int line = 0;\n"
                             "    int column = 0;\n"
                             "    std::basic_string<char_t, char_traits> lexeme;\n"
                             "    json value;\n"
                             "    int depth = 0;\n"
                             "    int merge = 0;\n"
                             "    bool error = false;\n"
                             "    ParserTreeNode(ParserState *state) : state(state) {}\n"
                             "    ParserTreeNode(const std::shared_ptr<ParserTreeNode> &prev, ParserState *state) : prev(prev), state(state) {}\n"
                             "    void add_prev(const std::shared_ptr<ParserTreeNode> &previous) {\n"
                             "        prevs.push_back(previous);\n"
                             "    }\n"
                             "    bool need_lr_reduce(ParserTransition *trans) {\n"
                             "        return state->conflict == CONFLICT_NONE && trans->reduce_length <= depth;\n"
                             "    }\n"
                             "};\n"
                             "template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>\n"
                             "class GLRParser {\n"
                             "    using Lexer = ParserLexer<iter_t>;\n"
                             "    using Node = ParserTreeNode<char_t, char_traits>;\n"
                             "    using NodePtr = std::shared_ptr<Node>;\n"
                             "    using Link = typename Node::Link;\n"
                             "    using LinkPtr = std::shared_ptr<Link>;\n"
                             "    struct ReduceNode {\n"
                             "        LinkPtr link;\n"
                             "        ParserTransition *trans;\n"
                             "        ReduceNode(const LinkPtr &link, ParserTransition &trans) : link(link), trans(&trans) {}\n"
                             "        inline bool operator<(const ReduceNode &rhs) const {\n"
                             "            return trans->precedence < rhs.trans->precedence;\n"
                             "        }\n"
                             "    };\n"
                             "    ParserState *parser_state = &ParserStates[0];\n"
                             "    Lexer lexer_ = Lexer(&LexerStates[0], LexerWhitespaceSymbol);\n"
                             "    bool position = false;\n"
                             "    bool accepted = false;\n"
                             "    std::map<ParserState *, NodePtr> frontier;\n"
                             "    std::vector<NodePtr> shift_list;\n"
                             "    std::vector<LinkPtr> link_list;\n"
                             "    std::vector<Node *> action_nodes;\n"
                             "    std::priority_queue<ReduceNode> reduce_list;\n"
                             "public:\n"
                             "    GLRParser() = default;\n"
                             "    void set_position(bool sp) {\n"
                             "        position = sp;\n"
                             "    }\n"
                             "    void reset(iter_t first, iter_t last = iter_t()) {\n"
                             "        accepted = false;\n"
                             "        frontier.clear();\n"
                             "        lexer_.reset(first, last);\n"
                             "    }\n"
                             "    void parse() {\n"
                             "        frontier.insert(std::pair(parser_state, NodePtr(new Node(parser_state))));\n"
                             "        lexer_.advance();\n"
                             "        do {\n"
                             "            shift();\n"
                             "            reduce();\n"
                             "            if (frontier.size() == 0 || accepted) {\n"
                             "                break;\n"
                             "            }\n"
                             "        } while (true);\n"
                             "    }\n"
                             "    bool accept() {\n"
                             "        return accepted && frontier.size() > 0;\n"
                             "    }\n"
                             "    json &value() {\n"
                             "        return std::get<1>(*frontier.begin())->value;\n"
                             "    }\n"
                             "    void reduce() {\n"
                             "        for (auto &[state, node] : frontier) {\n"
                             "            do_reduce(node);\n"
                             "        }\n"
                             "        while (!reduce_list.empty()) {\n"
                             "            ReduceNode node = reduce_list.top();\n"
                             "            reduce_list.pop();\n"
                             "            do_goto(node);\n"
                             "        }\n"
                             "    }\n"
                             "    void shift() {\n"
                             "        shift_list.clear();\n"
                             "        for (auto &[state, node] : frontier) {\n"
                             "            int shift_count = 0;\n"
                             "            for (auto &trans : *state) {\n"
                             "                if (trans.type == TRANSITION_SHIFT && trans.symbol == lexer_.symbol()) {\n"
                             "                    NodePtr shift_node = NodePtr(new Node(node, trans.state));\n"
                             "                    shift_node->line = lexer_.line();\n"
                             "                    shift_node->column = lexer_.column();\n"
                             "                    shift_node->symbol = lexer_.symbol();\n"
                             "                    shift_node->lexeme = lexer_.lexeme();\n"
                             "                    shift_node->depth = node->depth + 1;\n"
                             "                    shift_list.push_back(shift_node);\n"
                             "                    shift_count++;\n"
                             "                    break;\n"
                             "                }\n"
                             "            }\n"
                             "            if (state->error && shift_count == 0 || node->error) {\n"
                             "                do_error(node);\n"
                             "            }\n"
                             "        }\n"
                             "        if (shift_list.size() == 0) {\n"
                             "            handle_error();\n"
                             "        }\n"
                             "        frontier.clear();\n"
                             "        for (auto &node : shift_list) {\n"
                             "            if (frontier.count(node->state)) {\n"
                             "                frontier[node->state]->add_prev(node->prev);\n"
                             "                frontier[node->state]->depth = 0;\n"
                             "            } else {\n"
                             "                frontier.insert(std::pair(node->state, node));\n"
                             "            }\n"
                             "        }\n"
                             "        lexer_.advance();\n"
                             "    }\n"
                             "    void do_goto(ReduceNode &node) {\n"
                             "        node.link->to_nodes(action_nodes);\n"
                             "        json value;\n"
                             "        if (!action_nodes.empty() && action_nodes.back()->need_lr_reduce(node.trans)) {\n"
                             "            value = handle_action(node.trans->actions, node.trans->action_count, action_nodes.data());\n"
                             "            frontier.erase(action_nodes.back()->state); // del(node)\n"
                             "        } else {\n"
                             "            value = handle_action_copy(node.trans->actions, node.trans->action_count, action_nodes.data()); // dup(node)\n"
                             "        }\n"
                             "        int line = 0, column = 0;\n"
                             "        if (node.trans->reduce_length > 0) {\n"
                             "            line = action_nodes[0]->line;\n"
                             "            column = action_nodes[0]->column;\n"
                             "            if (position && value.is_object()) {\n"
                             "                value[\"position\"] = {{\"line\",   line},\n"
                             "                                     {\"column\", column}};\n"
                             "            }\n"
                             "        }\n"
                             "        if (node.trans->accept()) {\n"
                             "            auto start = NodePtr(new Node(parser_state));\n"
                             "            start->value = std::move(value);\n"
                             "            frontier.insert(std::pair(parser_state, start));\n"
                             "            accepted = true;\n"
                             "            return;\n"
                             "        }\n"
                             "        for (auto &goto_trans : *(node.link->node->state)) {\n"
                             "            if (goto_trans.type == TRANSITION_SHIFT && goto_trans.symbol == node.trans->reduce_symbol) {\n"
                             "                if (frontier.count(goto_trans.state)) {\n"
                             "                    do_merge(frontier[goto_trans.state], value);\n"
                             "                    frontier[goto_trans.state]->add_prev(node.link->node);\n"
                             "                    frontier[goto_trans.state]->depth = 0;\n"
                             "                } else {\n"
                             "                    auto goto_node = NodePtr(new Node(node.link->node, goto_trans.state));\n"
                             "                    goto_node->value = std::move(value);\n"
                             "                    goto_node->line = line;\n"
                             "                    goto_node->column = column;\n"
                             "                    goto_node->symbol = node.trans->reduce_symbol;\n"
                             "                    goto_node->lexeme = ParserSymbols[node.trans->reduce_symbol].text;\n"
                             "                    goto_node->depth = node.link->node->depth + 1;\n"
                             "                    do_reduce(goto_node);\n"
                             "                    frontier.insert(std::pair(goto_node->state, goto_node));\n"
                             "                }\n"
                             "            }\n"
                             "        }\n"
                             "    }\n"
                             "    void do_reduce(NodePtr &node) {\n"
                             "        for (auto &trans : *(node->state)) {\n"
                             "            if (trans.type == TRANSITION_REDUCE && trans.symbol == lexer_.symbol()) {\n"
                             "                if (node->depth >= trans.reduce_length && node->state->conflict == CONFLICT_NONE) {\n"
                             "                    do_lr_reduce(node, trans);\n"
                             "                } else {\n"
                             "                    do_glr_reduce(node, trans);\n"
                             "                }\n"
                             "            }\n"
                             "        }\n"
                             "    }\n"
                             "    void do_lr_reduce(NodePtr node, ParserTransition &trans) {\n"
                             "        auto link = LinkPtr(new Link(node));\n"
                             "        action_nodes.resize(trans.reduce_length);\n"
                             "        for (int i = trans.reduce_length - 1; i >= 0; --i) {\n"
                             "            node = node->prev;\n"
                             "            link = LinkPtr(new Link(node, link));\n"
                             "        }\n"
                             "        reduce_list.emplace(link, trans);\n"
                             "    }\n"
                             "    void do_glr_reduce(NodePtr node, ParserTransition &trans) {\n"
                             "        link_list.clear();\n"
                             "        link_list.push_back(LinkPtr(new Link(node)));\n"
                             "        for (int i = trans.reduce_length - 1; i >= 0; --i) {\n"
                             "            for (int j = 0, length = link_list.size(); j < length; ++j) {\n"
                             "                for (auto &prev : link_list[j]->node->prevs) {\n"
                             "                    link_list.push_back(LinkPtr(new Link(prev, link_list[j])));\n"
                             "                }\n"
                             "                link_list[j] = LinkPtr(new Link(link_list[j]->node->prev, link_list[j]));\n"
                             "            }\n"
                             "        }\n"
                             "        for (auto &link : link_list) {\n"
                             "            reduce_list.emplace(link, trans);\n"
                             "        }\n"
                             "    }\n"
                             "    void do_merge(NodePtr node, json &value) {\n"
                             "        if (node->merge) {\n"
                             "            node->value[\"value\"].push_back(value);\n"
                             "        } else {\n"
                             "            json merge = {{\"kind\",  \"merge\"},\n"
                             "                          {\"value\", json::array({std::move(node->value), value})}};\n"
                             "            node->value = std::move(merge);\n"
                             "        }\n"
                             "        node->merge++;\n"
                             "    }\n"
                             "    void do_error(NodePtr node) {\n"
                             "        // shift error\n"
                             "        if (node->error) {\n"
                             "            node->value.push_back({{\"lexeme\", lexer_.lexeme()},\n"
                             "                                   {\"line\",   lexer_.line()},\n"
                             "                                   {\"column\", lexer_.column()}});\n"
                             "            shift_list.push_back(node);\n"
                             "        } else {\n"
                             "            NodePtr shift_node = NodePtr(new Node(node, node->state->error->state));\n"
                             "            shift_node->symbol = 2;\n"
                             "            shift_node->line = lexer_.line();\n"
                             "            shift_node->column = lexer_.column();\n"
                             "            shift_node->lexeme = \"error\";\n"
                             "            shift_node->error = true;\n"
                             "            shift_node->value = json::array();\n"
                             "            shift_node->depth = node->depth + 1;\n"
                             "            shift_node->value.push_back({{\"lexeme\", lexer_.lexeme()},\n"
                             "                                         {\"line\",   lexer_.line()},\n"
                             "                                         {\"column\", lexer_.column()}});\n"
                             "            shift_list.push_back(shift_node);\n"
                             "        }\n"
                             "    }\n"
                             "    inline json handle_action(ReduceAction *actions, int action_count, Node **nodes) {\n"
                             "        if (action_count == 0) {\n"
                             "            return std::move(nodes[0]->value);\n"
                             "        }\n"
                             "        json value;\n"
                             "        for (int i = 0; i < action_count; ++i) {\n"
                             "            auto &action = actions[i];\n"
                             "            if (action.type == ACTION_TYPE_INIT && *action.desc == '$') {  // $n\n"
                             "                value = std::move(nodes[action.value]->value);\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_INIT && *action.desc == '@') { // @n\n"
                             "                value = nodes[action.value]->lexeme;\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_INIT && *action.desc == '#') { // #n\n"
                             "                value = json::array({std::move(nodes[action.value]->value)});\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_INSERT) { // {$n|@n|#n}\n"
                             "                if (!value.is_array()) {\n"
                             "                    value = json::array({std::move(value)});\n"
                             "                }\n"
                             "                if (*action.desc == '@') {\n"
                             "                    value.emplace_back(nodes[action.value]->lexeme);\n"
                             "                } else {\n"
                             "                    value.emplace_back(std::move(nodes[action.value]->value));\n"
                             "                }\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_SET && *action.desc == '$') { // key:$n set value\n"
                             "                value[std::string(action.field)] = std::move(nodes[action.value]->value);\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_SET && *action.desc == '@') { // key:@n set lexeme\n"
                             "                value[std::string(action.field)] = nodes[action.value]->lexeme;\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_SET && *action.desc == '#') { // key:#n insert\n"
                             "                value[std::string(action.field)].emplace_back(std::move(nodes[action.value]->value));\n"
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
                             "    inline json handle_action_copy(ReduceAction *actions, int action_count, Node **nodes) {\n"
                             "        if (action_count == 0) {\n"
                             "            return {};\n"
                             "        }\n"
                             "        json value;\n"
                             "        for (int i = 0; i < action_count; ++i) {\n"
                             "            auto &action = actions[i];\n"
                             "            if (action.type == ACTION_TYPE_INIT && *action.desc == '$') {  // $n\n"
                             "                value = nodes[action.value]->value;\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_INIT && *action.desc == '@') { // @n\n"
                             "                value = nodes[action.value]->lexeme;\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_INIT && *action.desc == '#') { // #n\n"
                             "                value = json::array({nodes[action.value]->value});\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_INSERT) { // {$n|@n|#n}\n"
                             "                if (!value.is_array()) {\n"
                             "                    value = json::array({value});\n"
                             "                }\n"
                             "                if (*action.desc == '@') {\n"
                             "                    value.emplace_back(nodes[action.value]->lexeme);\n"
                             "                } else {\n"
                             "                    value.emplace_back(nodes[action.value]->value);\n"
                             "                }\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_SET && *action.desc == '$') { // key:$n set value\n"
                             "                value[std::string(action.field)] = nodes[action.value]->value;\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_SET && *action.desc == '@') { // key:@n set lexeme\n"
                             "                value[std::string(action.field)] = nodes[action.value]->lexeme;\n"
                             "            }\n"
                             "            if (action.type == ACTION_TYPE_SET && *action.desc == '#') { // key:#n insert\n"
                             "                value[std::string(action.field)].emplace_back(nodes[action.value]->value);\n"
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
                             "    inline void handle_error() {\n"
                             "        std::cout << \"Unexpected Symbol '\" << lexer_.lexeme() << \"'\"\n"
                             "                  << \" line:\" << lexer_.line() << \" column:\" << lexer_.column()\n"
                             "                  << std::endl;\n"
                             "        std::cout << \"Expect: \";\n"
                             "        for (auto &[state, node] : frontier) {\n"
                             "            for (auto &trans : *state) {\n"
                             "                if (trans.type == TRANSITION_SHIFT && ParserSymbols[trans.symbol].type == SYMBOL_TYPE_NONTERMINAL) {\n"
                             "                    std::cout << \"\\\"\" << ParserSymbols[trans.symbol].text << \"\\\"\" << \", \";\n"
                             "                }\n"
                             "            }\n"
                             "        }\n"
                             "        std::cout << std::endl;\n"
                             "    }\n"
                             "};\n";
    std::fstream fs;
    fs.open(output, std::ios::trunc | std::ios::out);
    fs << "//\n"
          "// Created by Alex\n"
          "//\n"
          "\n"
          "#ifndef TINYLALR_PARSER_H\n"
          "#define TINYLALR_PARSER_H\n"
          "#include \"json.hpp\"\n"
          "#include <queue>\n"
          "#include <map>\n"
          "#include <iostream>\n"
          "#define CONFLICT_NONE 0\n"
          "#define CONFLICT_SHIFT_REDUCE 1\n"
          "#define CONFLICT_REDUCE_REDUCE 2\n"
          "#define SYMBOL_TYPE_TERMINAL 0\n"
          "#define SYMBOL_TYPE_NONTERMINAL 1\n"
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
    fs << parser;
    fs << glr_parser;
    fs << "#endif //TINYLALR_PARSER_H";
    fs.close();
}
void generate(LALRGenerator &gen, Options &opts) {
    std::fstream fs;
    fs.open(opts.output, std::ios::trunc | std::ios::out);
    fs << "//\n"
          "// Created by Alex's tiny lalr.\n"
          "//\n"
          "#include \"parser.h\"\n";
    fs << parser_emit_symbols(gen);
    fs << parser_emit_lexer(gen);
    fs << parser_emit_action(gen, opts.prefix);
    fs << parser_emit_states(gen);
    fs.close();
    std::string file(opts.output);
    std::string str = file.substr(0, file.find_last_of('.')) + ".h";
    generate_header(gen, str.c_str(), opts.prefix, opts.type);
}
void generate_grammar_file(Options &opts) {
    std::fstream input(opts.input, std::ios::in);
    LALRGrammarParser<StreamIter> lalr(input);
    input.close();
    LALRGenerator gen(lalr.grammar);
    gen.generate();
    generate(gen, opts);
}
void generate_grammar_string(const char *input, const char *output, const char *prefix, int type) {
    LALRGrammarParser<StringIter<char>> lalr(input);
    LALRGenerator gen(lalr.grammar);
    gen.generate();
    //generate(gen, output, prefix, type);
}

int main(int argc, char **argv) {
    Options opts;
    opts.input = "grammar.txt";
    opts.output = "../parser.cpp";
    opts.prefix = "TYPE_";
    opts.type = 0;
    for (int index = 1; index < argc; index++) {
        if (strcmp(argv[index], "-o") == 0 || strcmp(argv[index], "--output") == 0) {
            opts.output = argv[index + 1];
            index += 2;
        } else if (strcmp(argv[index], "-h") == 0 || strcmp(argv[index], "--help") == 0) {

            index += 1;
        } else if (strcmp(argv[index], "-p") == 0 || strcmp(argv[index], "--prefix") == 0) {
            opts.prefix = argv[index + 1];
            index += 2;
        } else if (strcmp(argv[index], "-g") == 0 || strcmp(argv[index], "--glr") == 0) {
            opts.type = 1;
            index += 1;
        } else {
            opts.input = argv[index];
        }
    }
    generate_grammar_file(opts);
    return 0;
}
