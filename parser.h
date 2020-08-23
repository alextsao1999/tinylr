//
// Created by Alex on 8/23/2020.
//

#ifndef TINYLALR_PARSER_H
#define TINYLALR_PARSER_H
#include "json.hpp"
#include <iostream>
struct ParserSymbol {
    int type;
    int symbol;
    const char *text;
};

struct LexerState;
struct LexerTransition {
    int begin;
    int end;
    LexerState *state;
};
struct LexerState {
    LexerTransition *transitions;
    int transition_count;
    int symbol;
    inline LexerTransition *begin() { return transitions; }
    inline LexerTransition *end() { return transitions + transition_count; }
};
struct ReduceAction {
    const char *field;
    const char *desc;
    int type;
    int value;
};
struct ParserState;
struct ParserTransition {
    int type;
    int symbol;
    ParserState *state;
    int reduce_symbol;
    int reduce_length;
    ReduceAction *actions;
    int action_count;
};
struct ParserState {
    ParserTransition *transitions;
    int transition_count;
    inline ParserTransition *begin() { return transitions; }
    inline ParserTransition *end() { return transitions + transition_count; }
};

extern int LexerWhitespaceSymbol;
extern ParserSymbol ParserSymbols[];
extern LexerState LexerStates[];
extern LexerTransition LexerTransitions[];
extern ParserState ParserStates[];
extern ReduceAction ParserActions[];
extern ParserTransition ParserTransitions[];

using Value = nlohmann::json;
template <class char_t = char, class char_traits = std::char_traits<char_t>>
struct ParserNode {
    ParserState *state;
    int symbol;
    int line = 0;
    int column = 0;
    std::basic_string<char_t, char_traits> lexeme;
    Value value;
    ParserNode(ParserState *state, int symbol = 0) : state(state), symbol(symbol) {}
    ParserNode(ParserState *state, int symbol, int line, int column,
               const std::basic_string<char_t, char_traits> &lexeme, const Value &value) :
            state(state), symbol(symbol),
            line(line), column(column),
            lexeme(lexeme),
            value(std::move(value)) {}
};
template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>
class ParserLexer {
    using uchar_t = typename std::make_unsigned<char_t>::type;
    using string_t = std::basic_string<char_t, char_traits>;
    LexerState *lexer_state = &LexerStates[0];
    iter_t current;
    iter_t end;
    iter_t line_start;
    iter_t token_start;
    int token_length = 0;
    int token_symbol = 0;
    string_t lexeme_;
    int line_ = 0;
private:
    inline LexerTransition *find_trans(LexerState *state, uchar_t chr) {
        LexerTransition *dot_trans = nullptr;
        for (auto &trans : *state) {
            if (trans.end == -1) {
                dot_trans = &trans;
            }
            if (trans.begin <= chr && chr <= trans.end) {
                return &trans;
            }
        }
        return dot_trans;
    }
    auto advance_symbol() {
        LexerState *state = lexer_state;
        do {
            if (*current == char_t('\0')) {
                break;
            }
            auto *trans = find_trans(state, *current);
            if (trans) {
                state = trans->state;
                if (*current++ == char_t('\n')) {
                    line_++;
                    line_start = current;
                }
            } else {
                break;
            }
        } while (true);
        return state->symbol;
    }
public:
    ParserLexer() = default;
    bool good() { return current < end; }
    void reset(iter_t first, iter_t last) {
        line_start = first;
        current = first;
        end = last;
    }
    void advance() {
        do {
            token_start = current;
            token_symbol = advance_symbol();
            token_length = current - token_start;
        } while (token_symbol == LexerWhitespaceSymbol);
        lexeme_.reserve(token_length);
        lexeme_.assign(token_start, token_length);
    }
    int symbol() { return token_symbol; }
    int line() const { return line_; }
    int column() const { return token_start - line_start; }
    string_t &lexeme() { return lexeme_; }
    void dump() {
        while (good()) {
            advance();
            std::cout << lexeme_ << ", " << token_symbol
                      << "[" << line() << ", " << column() << "]" << std::endl;

            if (token_symbol == 0) {
                break;
            }
        }
    }
};

template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>
class Parser {
    using Lexer = ParserLexer<iter_t>;
    using Node = ParserNode<char_t, char_traits>;
    ParserState *parser_state = &ParserStates[0];
    Lexer parser_lexer;
    ParserTransition *find_trans(ParserState *state, int symbol) {
        for (auto &trans : *state) {
            if (trans.symbol == symbol) {
                return &trans;
            }
        }
        return nullptr;
    }
public:
    std::vector<Node> stack;
    Parser() = default;

    void reset(iter_t first, iter_t last) {
        parser_lexer.reset(first, last);
    }
    void parse() {
        parser_lexer.advance();
        stack.reserve(32);
        stack.push_back(Node(parser_state));
        do {
            auto *trans = find_trans(stack.back().state, parser_lexer.symbol());
            if (!trans) {
                break;
            }
            if (trans->type == 1) { //Shift
                stack.emplace_back(trans->state, parser_lexer.symbol(), parser_lexer.line(), parser_lexer.column(),
                                   parser_lexer.lexeme(), Value());
                parser_lexer.advance();
            } else {
                reduce(trans);
                if (stack.back().symbol == 0) {
                    break;
                }
            }
        } while (true);
    }
    inline void reduce(ParserTransition *trans) {
        Value reduce_value;
        auto stack_start = (stack.end() - trans->reduce_length);
        if (trans->reduce_symbol == 0) {
            reduce_value = std::move(std::move((stack_start)->value));
        }
        for (int i = 0; i < trans->action_count; ++i) {
            auto &action = trans->actions[i];
            if (action.type == 0) {
                reduce_value[std::string(action.field)] = std::move((stack_start + action.value)->value);
            }
            if (action.type == 1) {
                reduce_value[std::string(action.field)] = std::atoi((stack_start + action.value)->lexeme.c_str());
            }
            if (action.type == 2) {
                reduce_value[std::string(action.field)] = std::string(action.desc);
            }
            if (action.type == 3) {
                reduce_value[std::string(action.field)] = action.value;
            }
        }
        int line = stack_start->line;
        int column = stack_start->column;
        stack.erase(stack_start, stack.end());
        if (trans->reduce_symbol == 0) {
            stack.back().value = std::move(reduce_value);
        } else {
            auto *new_trans = find_trans(stack.back().state, trans->reduce_symbol);
            stack.emplace_back(new_trans->state,
                               trans->reduce_symbol,
                               line, column,
                               std::string(ParserSymbols[trans->reduce_symbol].text),
                               reduce_value);
        }
    }
};

#endif //TINYLALR_PARSER_H
