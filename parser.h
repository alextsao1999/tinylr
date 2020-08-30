//
// Created by Alex
//

#ifndef TINYLALR_PARSER_H
#define TINYLALR_PARSER_H
#include "json.hpp"
#include <iostream>
#define TRANSITION_SHIFT 1
#define TRANSITION_REDUCE 2
#define ACTION_TYPE_INIT 0
#define ACTION_TYPE_INSERT 1
#define ACTION_TYPE_SET 2
#define ACTION_TYPE_INSERT_INT 3
#define ACTION_TYPE_INSERT_BOOL 4
#define ACTION_TYPE_INSERT_STRING 5
#define ACTION_TYPE_SET_INT 6
#define ACTION_TYPE_SET_BOOL 7
#define ACTION_TYPE_SET_STRING 8
using json = nlohmann::json;
enum {
    TYPE_NONE,
    TYPE_ARG_LIST,
    TYPE_ARRAY,
    TYPE_ARROW_FUNCTION,
    TYPE_ASSIGN,
    TYPE_BIN,
    TYPE_BINARY,
    TYPE_BREAK,
    TYPE_CASE,
    TYPE_CASES,
    TYPE_CHAR,
    TYPE_CLASS,
    TYPE_DO_WHILE,
    TYPE_DOT,
    TYPE_ERROR,
    TYPE_FIELDDEF,
    TYPE_FLOAT,
    TYPE_FUNDEF,
    TYPE_HEX,
    TYPE_IF,
    TYPE_IMPORT,
    TYPE_INVOKE,
    TYPE_LONG,
    TYPE_METHODDEF,
    TYPE_NEW,
    TYPE_NUMBER,
    TYPE_OVERLOAD,
    TYPE_PARAM,
    TYPE_PROGRAM,
    TYPE_READ,
    TYPE_RETURN,
    TYPE_STMTS,
    TYPE_STRING,
    TYPE_SWITCH,
    TYPE_TYPE,
    TYPE_UNSIGNED,
    TYPE_VARDEF,
    TYPE_VARIABLE,
    TYPE_WHILE,
    TYPE_WRITE,
};
#define TYPE_LIST(v)  \
  v(TYPE_ARG_LIST, arg_list) \
  v(TYPE_ARRAY, array) \
  v(TYPE_ARROW_FUNCTION, arrow_function) \
  v(TYPE_ASSIGN, assign) \
  v(TYPE_BIN, bin) \
  v(TYPE_BINARY, binary) \
  v(TYPE_BREAK, break) \
  v(TYPE_CASE, case) \
  v(TYPE_CASES, cases) \
  v(TYPE_CHAR, char) \
  v(TYPE_CLASS, class) \
  v(TYPE_DO_WHILE, do_while) \
  v(TYPE_DOT, dot) \
  v(TYPE_ERROR, error) \
  v(TYPE_FIELDDEF, fielddef) \
  v(TYPE_FLOAT, float) \
  v(TYPE_FUNDEF, fundef) \
  v(TYPE_HEX, hex) \
  v(TYPE_IF, if) \
  v(TYPE_IMPORT, import) \
  v(TYPE_INVOKE, invoke) \
  v(TYPE_LONG, long) \
  v(TYPE_METHODDEF, methoddef) \
  v(TYPE_NEW, new) \
  v(TYPE_NUMBER, number) \
  v(TYPE_OVERLOAD, overload) \
  v(TYPE_PARAM, param) \
  v(TYPE_PROGRAM, program) \
  v(TYPE_READ, read) \
  v(TYPE_RETURN, return) \
  v(TYPE_STMTS, stmts) \
  v(TYPE_STRING, string) \
  v(TYPE_SWITCH, switch) \
  v(TYPE_TYPE, type) \
  v(TYPE_UNSIGNED, unsigned) \
  v(TYPE_VARDEF, vardef) \
  v(TYPE_VARIABLE, variable) \
  v(TYPE_WHILE, while) \
  v(TYPE_WRITE, write)
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

template <class char_t = char, class char_traits = std::char_traits<char_t>>
struct ParserNode {
    ParserState *state;
    int symbol;
    int line = 0;
    int column = 0;
    std::basic_string<char_t, char_traits> lexeme;
    json value;
    ParserNode(ParserState *state, int symbol = 0) : state(state), symbol(symbol) {}
    ParserNode(ParserState *state, int symbol, int line, int column,
               const std::basic_string<char_t, char_traits> &lexeme, const json &value) :
            state(state), symbol(symbol),
            line(line), column(column),
            lexeme(lexeme),
            value(std::move(value)) {}
};
template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>
class ParserLexer {
    using uchar_t = typename std::make_unsigned<char_t>::type;
    using string_t = std::basic_string<char_t, char_traits>;
    LexerState *lexer_state /* = &LexerStates[0]*/;
    int whitespace /*= LexerWhitespaceSymbol*/;
    iter_t current;
    iter_t end;
    int line_start = 0;
    int token_start = 0;
    int token_symbol = 0;
    int line_ = 0;
    int position_ = 0;
    string_t lexeme_;
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
        lexeme_.clear();
        do {
            if (current == end) {
                return state->symbol;
            }
            auto *trans = find_trans(state, *current);
            if (trans) {
                lexeme_ += *current;
                state = trans->state;
                ++position_;
                if (*current == char_t('\n')) {
                    ++line_;
                    line_start = position_;
                }
                ++current;
            } else {
                break;
            }
        } while (true);
        if (state == lexer_state && *current != '\0') {
            std::cout << "Unexpect char: " << *current << " line:" << line() << std::endl;
            ++current;
            return 2; // error symbol
        }
        return state->symbol;
    }
public:
    ParserLexer(LexerState *state, int whitespace = -1) : lexer_state(state), whitespace(whitespace) {}
    void reset(iter_t first, iter_t last) {
        current = first;
        end = last;
    }
    void advance() {
        do {
            token_start = position_;
            token_symbol = advance_symbol();
        } while (token_symbol == whitespace);
    }
    int symbol() { return token_symbol; }
    int line() const { return line_; }
    int column() const { return token_start - line_start; }
    string_t &lexeme() { return lexeme_; }
    void dump() {
        do {
            advance();
            std::cout << lexeme_ << "  " << token_symbol
                      << "[" << line() << ", " << column() << "]" << std::endl;

            if (token_symbol == 0) {
                break;
            }
        } while (symbol() != 0);
        exit(0);
    }
};
template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>
class Parser {
    using Lexer = ParserLexer<iter_t>;
    using Node = ParserNode<char_t, char_traits>;
    ParserState *parser_state = &ParserStates[0];
    Lexer parser_lexer = Lexer(&LexerStates[0], LexerWhitespaceSymbol);
    bool position = false;
    inline ParserTransition *find_trans(ParserState *state, int symbol) {
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
    void set_position(bool sp) {
        position = sp;
    }
    void reset(iter_t first, iter_t last = iter_t()) {
        parser_lexer.reset(first, last);
    }
    void parse() {
        parser_lexer.advance();
        stack.reserve(32);
        stack.push_back(Node(parser_state));
        do {
            auto *trans = find_trans(stack.back().state, parser_lexer.symbol());
            if (!trans) {
                if (!handle_error()) {
                    break;
                }
                continue;
            }
            if (trans->type == TRANSITION_SHIFT) { //Shift
                shift(trans);
            } else {
                reduce(trans);
                if (stack.back().symbol == 0) {
                    break;
                }
            }
        } while (true);
    }
    json &value() { return stack[0].value; }
    inline void shift(ParserTransition *trans) {
        //debug_shift(trans);
        stack.emplace_back(trans->state,
                           parser_lexer.symbol(),
                           parser_lexer.line(),
                           parser_lexer.column(),
                           parser_lexer.lexeme(),
                           json());
        parser_lexer.advance();
    }
    inline void reduce(ParserTransition *trans) {
        auto stack_start = (stack.size() - trans->reduce_length);
        //debug_reduce(trans, stack_start);
        json reduce_value;
        int line = 0;
        int column = 0;
        if (stack_start != stack.size()) {
            line = stack[stack_start].line;
            column = stack[stack_start].column;
            reduce_value = handle_action(trans->actions, trans->action_count, &stack[stack_start]);
            if (position && reduce_value.is_object()) {
                reduce_value["position"] = {{"line",   line},
                                            {"column", column}};
            }
            stack.erase(stack.begin() + stack_start, stack.end());
            if (trans->reduce_symbol == 0) {
                stack.back().value = std::move(reduce_value);
                return;
            }
        }
        auto *new_trans = find_trans(stack.back().state, trans->reduce_symbol);
        stack.emplace_back(new_trans->state,
                           trans->reduce_symbol,
                           line,
                           column,
                           std::string(ParserSymbols[trans->reduce_symbol].text),
                           reduce_value);
    }
    inline void expect() {
        Node &node = stack.back();
        std::cout << "Shift Reduce Error "
                     "line: " << node.line + 1 << " "
                  << "column: " << node.column + 1 << " "
                  << "token: " << parser_lexer.lexeme()
                  << std::endl;
        std::cout << "Expect: ";
        for (auto &trans : *node.state) {
            std::cout << "\"" << ParserSymbols[trans.symbol].text << "\"";
            if (&trans != (node.state->end() - 1)) {
                std::cout << ", ";
            }
        }
        std::cout << std::endl;

    }
    inline void debug_shift(ParserTransition *trans) {
        std::cout << "shift: " << stack.size() << " "
                  << "[state " << (stack.back().state - ParserStates) << " -> " << (trans->state - ParserStates)
                  << "]  [" << ParserSymbols[parser_lexer.symbol()].text << "] "
                  << parser_lexer.lexeme()
                  << std::endl << std::endl;
    }
    inline void debug_reduce(ParserTransition *trans, int start) {
        ParserTransition *goto_trans = find_trans(stack[start - 1].state, trans->reduce_symbol);
        if (!goto_trans) {
            return;
        }
        std::cout << "reduce: "
                  << start << " "
                  << "[back to " /*<< (stack.back().state - ParserStates) << " -> "*/
                  << (stack[start - 1].state - ParserStates) << " -> "
                  << (goto_trans->state - ParserStates) << "] "
                  << ParserSymbols[trans->reduce_symbol].text << " <- ";
        for (int i = start; i < start + trans->reduce_length; ++i) {
            std::cout << "[" << stack[i].lexeme << "] " << stack[i].value << " | ";
        }
        std::cout << std::endl << std::endl;
    }
    inline json handle_action(ReduceAction *actions, int action_count, Node *nodes) {
        if (action_count == 0) {
            return std::move(nodes->value);
        }
        json value;
        for (int i = 0; i < action_count; ++i) {
            auto &action = actions[i];
            if (action.type == ACTION_TYPE_INIT && *action.desc == '$') {  // $n
                value = std::move((nodes + action.value)->value);
            }
            if (action.type == ACTION_TYPE_INIT && *action.desc == '@') { // @n
                value = (nodes + action.value)->lexeme;
            }
            if (action.type == ACTION_TYPE_INIT && *action.desc == '#') { // #n
                value = json::array({std::move((nodes + action.value)->value)});
            }
            if (action.type == ACTION_TYPE_INSERT) { // {$n|@n|#n}
                if (!value.is_array()) {
                    value = json::array({std::move(value)});
                }
                if (*action.desc == '@') {
                    value.emplace_back((nodes + action.value)->lexeme);
                } else {
                    value.emplace_back(std::move((nodes + action.value)->value));
                }
            }
            if (action.type == ACTION_TYPE_SET && *action.desc == '$') { // key:$n set value
                value[std::string(action.field)] = std::move((nodes + action.value)->value);
            }
            if (action.type == ACTION_TYPE_SET && *action.desc == '@') { // key:@n set lexeme
                value[std::string(action.field)] = (nodes + action.value)->lexeme;
            }
            if (action.type == ACTION_TYPE_SET && *action.desc == '#') { // key:#n insert
                value[std::string(action.field)].emplace_back(std::move((nodes + action.value)->value));
            }
            if (action.type == ACTION_TYPE_SET_STRING) {  // key:'string'
                value[std::string(action.field)] = action.desc;
            }
            if (action.type == ACTION_TYPE_SET_BOOL) { // key:bool
                value[std::string(action.field)] = json::boolean_t(action.value);
            }
            if (action.type == ACTION_TYPE_SET_INT) { // key:number
                value[std::string(action.field)] = json::number_integer_t(action.value);
            }
            if (action.type == ACTION_TYPE_INSERT_STRING) {
                if (value.empty()) {
                    value = action.desc;
                } else {
                    if (value.is_array()) {
                        value.emplace_back(action.desc);
                    } else {
                        value = json::array({std::move(value), action.desc});
                    }
                }
            }
            if (action.type == ACTION_TYPE_INSERT_BOOL) { // bool
                value.emplace_back(json::boolean_t(action.value));
            }
            if (action.type == ACTION_TYPE_INSERT_INT) { // number
                value.emplace_back(json::number_integer_t(action.value));
            }
        }
        return std::move(value);
    }
    inline bool handle_error() {
        auto *trans = find_trans(stack.back().state, 2);
        if (trans == nullptr) {
            expect();
            return false;
        }
        json value = json::array();
        ParserState *state = trans->state;
        do {
            value.push_back({{"lexeme", parser_lexer.lexeme()},
                                {"line",   parser_lexer.line()},
                                {"column", parser_lexer.column()}});
            parser_lexer.advance();
            ParserTransition *new_trans = find_trans(state, parser_lexer.symbol());
            if (new_trans) {
                stack.emplace_back(trans->state,
                                   2,
                                   parser_lexer.line(),
                                   parser_lexer.column(),
                                   "error",
                                   std::move(value));
                if (new_trans->type == TRANSITION_SHIFT) {
                    shift(new_trans);
                } else {
                    reduce(new_trans);
                }
                break;
            }
        } while (true);
        return true;
    }
};

#endif //TINYLALR_PARSER_H