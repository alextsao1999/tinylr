//
// Created by Alex
//

#ifndef TINYLALR_PARSER_H
#define TINYLALR_PARSER_H
#include <string>
#include <iostream>
#include <map>
#include <queue>
#include <cassert>
#include <functional>

#define LR_UNREACHED() assert(!"unreached here")

#define CONFLICT_NONE 0
#define CONFLICT_SHIFT_REDUCE 1
#define CONFLICT_REDUCE_REDUCE 2
#define SYMBOL_TYPE_TERMINAL 0
#define SYMBOL_TYPE_NONTERMINAL 1
#define TRANSITION_SHIFT 1
#define TRANSITION_REDUCE 2

enum ActionOpcode {
    OpcodeCreateObj,
    OpcodeCreateArr,
    OpcodePushValue, /// $n
    OpcodePushInt, /// int
    OpcodePushStr, /// str
    OpcodePushBool, /// bool
    OpcodePushToken, /// token
    OpcodePopSet,
    OpcodePopInsertArr,
    OpcodePopInsertObj,
};
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
    int opcode;
    int index;
    const char *value;
};

struct ParserState;
struct ParserTransition {
    int type;
    int symbol;
    ParserState *state;
    int reduce_symbol;
    int reduce_length;
    int precedence;
    ReduceAction *actions;
    int action_count;
    inline bool accept() const { return reduce_symbol == 0 && type == TRANSITION_REDUCE; }
    inline bool error() const { return symbol == 2 && type == TRANSITION_SHIFT; }
};
struct ParserState {
    int index;
    ParserTransition *transitions;
    int transition_count;
    int conflict;
    ParserTransition *error;
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

struct Location {
    int line_start = 0;
    int line_end = 0;
    int column_start = 0;
    int column_end = 0;
    bool empty() const { return line_start == line_end && column_start == column_end; }
    Location merge(const Location &rhs) const {
        if (rhs.empty()) {
            return *this;
        }
        if (empty()) {
            return rhs;
        }
        return {
                std::min(line_start, rhs.line_start),
                std::max(line_end, rhs.line_end),
                std::min(column_start, rhs.column_start),
                std::max(column_end, rhs.column_end)
        };
    }
};
template <class iter_t = const char *,
        class char_t = typename std::iterator_traits<iter_t>::value_type,
        class char_traits = std::char_traits<char_t>>
class ParserLexer {
public:
    using string_t = std::basic_string<char_t, char_traits>;
private:
    LexerState *lexer_state /* = &LexerStates[0]*/;
    int whitespace /*= LexerWhitespaceSymbol*/;
    iter_t current;
    iter_t end;
    int line_ = 0;
    int line_start_position_ = 0;
    int position_ = 0;
    int token_line_start_ = 0;
    int token_column_start_ = 0;
    int token_symbol = 0;
    string_t lexeme_;
private:
    inline LexerTransition *find_trans(LexerState *state, char_t chr) {
        for (auto &trans : *state) {
            if (trans.begin <= chr && chr < trans.end) {
                return &trans;
            }
        }
        return nullptr;
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
                if (*current == '\n') {
                    ++line_;
                    line_start_position_ = position_;
                }
                ++current;
            } else {
                break;
            }
        } while (true);
        if (state == lexer_state && *current != '\0') {
            std::cout << "Unexpect char: " << *current << " line:" << line_end() << std::endl;
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
            token_line_start_ = line_end();
            token_column_start_ = column_end();
            token_symbol = advance_symbol();
        } while (token_symbol == whitespace);
    }
    int symbol() { return token_symbol; }
    inline int line_start() const { return token_line_start_; }
    inline int line_end() const { return line_; }
    inline int column_start() const { return token_column_start_; }
    inline int column_end() const { return position_ - line_start_position_; }
    inline Location location() const { return {line_start(), line_end(), column_start(), column_end()}; }
    inline string_t &lexeme() { return lexeme_; }
    void dump() {
        do {
            advance();
            std::cout << lexeme_ << "  " << token_symbol
                      << "[" << line_start() << ", " << column_start() << "]" << std::endl;
            if (token_symbol == 0) {
                break;
            }
        } while (symbol() != 0);
        exit(0);
    }
};

#include "json.hpp"
using value_t = nlohmann::json;

template<bool Move = true, typename NodeGetter>
inline void HandleReduceAction(ReduceAction &action, std::vector<value_t> &arr, NodeGetter nodes) {
    switch (action.opcode) {
        default:
            LR_UNREACHED();
        case OpcodeCreateObj:
            arr.push_back(value_t::object_t());
            break;
        case OpcodeCreateArr:
            arr.push_back(value_t::array_t());
            break;
        case OpcodePushValue:
            arr.push_back(Move ? std::move(nodes[action.index].value) : nodes[action.index].value);
            break;
        case OpcodePushInt:
            arr.push_back(value_t::number_integer_t(action.index));
            break;
        case OpcodePushStr:
            arr.push_back(value_t::string_t(action.value));
            break;
        case OpcodePushBool:
            arr.push_back(value_t::boolean_t(action.index));
            break;
        case OpcodePushToken:
            arr.push_back(value_t::string_t(nodes[action.index].lexeme));
            break;
        case OpcodePopSet: {
            auto poped = std::move(arr.back());
            arr.pop_back();
            auto &top = arr.back();
            top[std::string(action.value)] = std::move(poped);
            break;
        }
        case OpcodePopInsertArr: {
            auto poped = std::move(arr.back());
            arr.pop_back();
            auto &top = arr.back();
            top.insert(top.end(), std::move(poped));
            break;
        }
        case OpcodePopInsertObj: {
            auto poped = std::move(arr.back());
            arr.pop_back();
            auto &top = arr.back()[std::string(action.value)];
            top.insert(top.end(), std::move(poped));
            break;
        }
    }
}

template<class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>
class LRParser {
public:
    using Lexer = ParserLexer<iter_t>;
    using string_t = typename Lexer::string_t;
private:
    struct ParserNode {
        ParserState *state;
        int symbol;
        value_t value;
        string_t lexeme;
        Location location;
        ParserNode(ParserState *state, int symbol, const value_t &value, Location loc) : state(state), symbol(symbol),
                                                                                         value(value), location(loc) {
#ifdef DEBUG
            lexeme = ParserSymbols[symbol].text;
#endif
        }
        ParserNode(ParserState *state, int symbol, const string_t &lexeme, Location loc) : state(state), symbol(symbol),
                                                                                           lexeme(lexeme), location(loc) {}
    };

    using Node = ParserNode;
public:
    ParserState *parser_state = &ParserStates[0];
    Lexer parser_lexer = Lexer(&LexerStates[0], LexerWhitespaceSymbol);
    bool position = false;
    inline ParserTransition *find_trans(ParserState *state, int symbol) {
        for (auto &trans: *state) {
            if (trans.symbol == symbol) {
                return &trans;
            }
        }
        return nullptr;
    }
public:
    std::vector<Node> stack;
    std::vector<value_t> values;
    LRParser() = default;
    explicit LRParser(bool position) : position(position) {}
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
            if (auto *trans = find_trans(stack.back().state, parser_lexer.symbol())) {
                if (trans->type == TRANSITION_SHIFT) { // Shift
                    shift(trans);
                } else { // Reduce
                    reduce(trans);
                    if (trans->accept()) {
                        break;
                    }
                }
            } else {
                if (!handle_error()) {
                    break;
                }
            }
        } while (true);
    }
    value_t &value() { return stack[0].value; }

    inline void shift(ParserTransition *trans) {
        // debug_shift(trans);
        stack.emplace_back(trans->state, parser_lexer.symbol(), parser_lexer.lexeme(), parser_lexer.location());
        parser_lexer.advance();
    }
    inline void reduce(ParserTransition *trans) {
        values.clear();
        // if the reduce length is zero, the location is the lexer current location.
        Location loc = parser_lexer.location();
        if (trans->reduce_length) {
            auto first = stack.size() - trans->reduce_length;
            // merge the locations
            loc = std::accumulate(stack.begin() + first, stack.end(), Location(), [](Location &i, Node *node) {
                return i.merge(node->location);
            });
            // handle `reduce` action
            handle_action(trans->actions, trans->action_count, &stack[first]);
            // record the position
            value_t &reduce_value = values.back();
            if (position && reduce_value.is_object()) {
                reduce_value["position"] = {{"line",   loc.line_start},
                                            {"column", loc.column_start}};
            }
            stack.erase(stack.begin() + first, stack.end());
            if (trans->accept()) {
                stack.back().value = std::move(values.back());
                return;
            }
        }
        // goto a new state by the reduced symbol
        if (auto *Goto = find_trans(stack.back().state, trans->reduce_symbol)) {
            stack.emplace_back(Goto->state, trans->reduce_symbol, std::move(values.back()), loc);
            values.pop_back();
        } else {
            expect();
        }
    }

    inline void expect() {
        Node &node = stack.back();
        std::cout << "Shift Reduce Error "
                     "line: " << node.location.line_start + 1 << " "
                  << "column: " << node.location.column_start + 1 << " "
                  << "token: " << parser_lexer.lexeme()
                  << std::endl;
        std::cout << "Expect: ";
        for (auto &trans: *node.state) {
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

    inline void handle_action(ReduceAction *actions, int action_count, Node *nodes) {
        if (action_count == 0) {
            values.push_back(std::move(nodes->value));
            return;
        }

        struct Getter {
            Node *nodes;
            Getter(Node *nodes) : nodes(nodes) {}
            inline Node &operator[](size_t index) {
                return nodes[index];
            }
        } getter{nodes};

        for (int i = 0; i < action_count; ++i) {
            HandleReduceAction(actions[i], values, getter);
        }
    }
    inline bool handle_error() {
        auto *trans = find_trans(stack.back().state, 2);
        if (trans == nullptr) {
            expect();
            return false;
        }
        value_t value = value_t::array();
        ParserState *state = trans->state;
        do {
            value.push_back({{"lexeme", parser_lexer.lexeme()},
                             {"line",   parser_lexer.line_start()},
                             {"column", parser_lexer.column_start()}});
            parser_lexer.advance();
            if (ParserTransition *Goto = find_trans(state, parser_lexer.symbol())) {
                stack.emplace_back(trans->state, 2, std::move(value), parser_lexer.location());
                if (Goto->type == TRANSITION_SHIFT) {
                    shift(Goto);
                } else {
                    reduce(Goto);
                }
                break;
            }
        } while (true);
        return true;
    }
};

template <class iter_t = const char *,
        class char_t = typename std::iterator_traits<iter_t>::value_type,
        class char_traits = std::char_traits<char_t>>
class GLRParser {
    using Lexer = ParserLexer<iter_t>;
    using string_t = typename Lexer::string_t;
    struct ParserGraphNode {
        using NodePtr = std::shared_ptr<ParserGraphNode>;
        static auto Create(ParserState *state) {
            return std::make_shared<ParserGraphNode>(state);
        }
        static auto Create(ParserState *state, const NodePtr &prev) {
            return std::make_shared<ParserGraphNode>(state, prev);
        }
        std::vector<NodePtr> prevs;
        ParserState *state = nullptr;
        int symbol = 0;
        value_t value;
        string_t lexeme;
        Location location;
        int depth = 0;
        int merge = 0;
        bool error = false;
        ParserGraphNode(ParserState *state) : state(state) {}
        ParserGraphNode(ParserState *state, const NodePtr &prev) : state(state), prevs({prev}) {}
        void add_prev(const NodePtr &previous) {
            prevs.push_back(previous);
        }
        bool need_lr_reduce(ParserTransition *trans) {
            return state->conflict == CONFLICT_NONE && trans->reduce_length <= depth;
        }
    };
private:
    using Node = ParserGraphNode;
    using NodePtr = std::shared_ptr<Node>;
    struct ReduceNode {
        std::vector<Node *> paths;
        ParserTransition *trans;
        NodePtr prev;
        ReduceNode(const std::vector<Node *> &paths, ParserTransition *trans, const NodePtr &prev) : paths(paths.rbegin(), paths.rend()),
                                                                                trans(trans), prev(prev) {}
        inline bool operator<(const ReduceNode &rhs) const {
            return trans->precedence < rhs.trans->precedence;
        }
        inline Node *get_first() const {
            return paths[0];
        }
        inline Node *get_last() const {
            return paths.back();
        }
    };
    ParserState *parser_state = &ParserStates[0];
    Lexer lexer_ = Lexer(&LexerStates[0], LexerWhitespaceSymbol);
    bool position = false;
    bool accepted = false;
    std::map<ParserState *, NodePtr> frontier;
    std::vector<NodePtr> shift_list;
    std::vector<value_t> values;
    std::priority_queue<ReduceNode> reduce_list;
public:
    GLRParser() = default;
    explicit GLRParser(bool position) : position(position) {}
    explicit GLRParser(iter_t first, iter_t last = iter_t()) {
        reset(first, last);
    }
    void set_position(bool sp) {
        position = sp;
    }
    void reset(iter_t first, iter_t last = iter_t()) {
        accepted = false;
        frontier.clear();
        lexer_.reset(first, last);
    }
    void parse() {
        frontier.insert(std::pair(parser_state, Node::Create(parser_state)));
        lexer_.advance();
        do {
            shift();
            reduce();
            if (frontier.size() == 0 || accepted) {
                break;
            }
        } while (true);
    }
    bool accept() {
        return accepted && frontier.size() > 0;
    }
    value_t &value() {
        return std::get<1>(*frontier.begin())->value;
    }

    void reduce() {
        for (auto &[state, node] : frontier) {
            do_reduce(node);
        }
        while (!reduce_list.empty()) {
            ReduceNode node = std::move(reduce_list.top());
            reduce_list.pop();
            do_goto(node);
        }
    }
    void shift() {
        shift_list.clear();
        for (auto &[state, node] : frontier) {
            int shift_count = 0;
            for (auto &trans : *state) {
                if (trans.type == TRANSITION_SHIFT && trans.symbol == lexer_.symbol()) {
                    NodePtr shift_node = Node::Create(trans.state, node);
                    shift_node->symbol = lexer_.symbol();
                    shift_node->lexeme = lexer_.lexeme();
                    shift_node->location = lexer_.location();
                    shift_node->depth = node->depth + 1;
                    shift_list.push_back(shift_node);
                    shift_count++;
                    break;
                }
            }
            if (state->error && shift_count == 0 || node->error) {
                do_error(node);
            }
        }
        if (shift_list.size() == 0) {
            handle_error();
        }
        frontier.clear();
        for (auto &node : shift_list) {
            if (frontier.count(node->state)) {
                // merge the previous nodes if they are the same state
                auto &prevs = frontier[node->state]->prevs;
                prevs.insert(prevs.end(), node->prevs.begin(), node->prevs.end());
                frontier[node->state]->depth = 0;
            } else {
                frontier.insert(std::pair(node->state, node));
            }
        }
        lexer_.advance();
    }

    void do_goto(ReduceNode &node) {
        values.clear();
        // handle `reduce` action
        if (!node.paths.empty() && node.get_last()->need_lr_reduce(node.trans)) {
            handle_action<true>(node, node.trans->actions, node.trans->action_count);
            frontier.erase(node.get_last()->state);
            // del(node)
        } else {
            handle_action<false>(node, node.trans->actions, node.trans->action_count);
            // dup(node)
        }
        // record location
        Location loc = lexer_.location();
        value_t value;
        if (!values.empty()) {
            value = std::move(values.back());
            values.pop_back();
        }
        if (node.trans->reduce_length) {
            // merge the locations
            loc = std::accumulate(node.paths.begin(), node.paths.end(), Location(), [](Location &loc, Node *node) {
                return loc.merge(node->location);
            });
            if (position && value.is_object()) {
                value["position"] = {{"line",   loc.line_start},
                                     {"column", loc.column_start}};
            }
        }
        if (node.trans->accept()) {
            auto start = Node::Create(parser_state);
            start->value = std::move(value);
            frontier.insert(std::pair(parser_state, start));
            accepted = true;
            return;
        }
        for (auto &Goto: *(node.prev->state)) {
            if (Goto.type == TRANSITION_SHIFT && Goto.symbol == node.trans->reduce_symbol) {
                if (frontier.count(Goto.state)) {
                    do_merge(frontier[Goto.state], value);
                    frontier[Goto.state]->add_prev(node.prev);
                    frontier[Goto.state]->depth = 0;
                } else {
                    auto goto_node = Node::Create(Goto.state, node.prev);
                    goto_node->symbol = node.trans->reduce_symbol;
                    goto_node->value = std::move(value);
                    goto_node->lexeme = ParserSymbols[node.trans->reduce_symbol].text;
                    goto_node->location = loc;
                    goto_node->depth = node.prev->depth + 1;
                    do_reduce(goto_node);
                    frontier.insert(std::pair(goto_node->state, goto_node));
                }
            }
        }
    }
    void do_reduce(NodePtr &node) {
        for (auto &trans : *(node->state)) {
            if (trans.type == TRANSITION_REDUCE && trans.symbol == lexer_.symbol()) {
                enumerate_path(node, &trans);
            }
        }
    }

    void enumerate_path(const NodePtr &start, ParserTransition *trans) {
        std::vector<Node *> path;
        std::function<void(const NodePtr &, unsigned)> DFS = [&](auto &node, unsigned length) {
            if (length-- == 0) {
                reduce_list.push(ReduceNode(path, trans, node));
                return;
            }
            path.push_back(node.get());
            for (auto &prev: node->prevs) {
                DFS(prev, length);
            }
            path.pop_back();
        };
        DFS(start, trans->reduce_length);
    }
    void do_merge(NodePtr node, value_t &value) {
        if (node->merge) {
            node->value["value"].push_back(std::move(value));
        } else {
            value_t merge = {{"kind",  "merge"},
                             {"value", value_t::array({std::move(node->value), value})}};
            node->value = std::move(merge);
        }
        node->merge++;
    }
    void do_error(NodePtr node) {
        // shift error
        if (node->error) {
            node->value.push_back({{"lexeme", lexer_.lexeme()},
                                   {"line",   lexer_.line_start()},
                                   {"column", lexer_.column_start()}});
            shift_list.push_back(node);
        } else {
            NodePtr shift_node = Node::Create(node->state->error->state, node);
            shift_node->symbol = 2;
            shift_node->value = value_t::array();
            shift_node->lexeme = "error";
            shift_node->location = lexer_.location();
            shift_node->error = true;
            shift_node->depth = node->depth + 1;
            shift_node->value.push_back({{"lexeme", lexer_.lexeme()},
                                         {"line",   lexer_.line_start()},
                                         {"column", lexer_.column_start()}});
            shift_list.push_back(shift_node);
        }
    }

    template<bool Move = false>
    inline void handle_action(ReduceNode &node, ReduceAction *actions, int action_count) {
        if (action_count == 0) {
            if (!node.paths.empty() && Move) {
                values.push_back(node.paths[0]->value); // default action -> $1
            }
            return;
        }

        struct Getter {
            Node **nodes;
            Getter(Node **nodes) : nodes(nodes) {}
            inline Node &operator[](size_t index) {
                return *nodes[index];
            }
        } getter{node.paths.data()};

        std::vector<ReduceAction *> reductions;
        for (int i = 0; i < action_count; ++i) {
            reductions.push_back(&actions[i]);
        }

        for (int i = 0; i < action_count; ++i) {
            HandleReduceAction<Move>(actions[i], values, getter);
        }
    }
    inline void handle_error() {
        std::cout << "Unexpected Symbol '" << lexer_.lexeme() << "'"
                  << " line:" << lexer_.line_start() << " column:" << lexer_.column_start()
                  << std::endl;
        std::cout << "Expect: ";
        for (auto &[state, node] : frontier) {
            for (auto &trans : *state) {
                if (trans.type == TRANSITION_SHIFT && ParserSymbols[trans.symbol].type == SYMBOL_TYPE_NONTERMINAL) {
                    std::cout << "\"" << ParserSymbols[trans.symbol].text << "\"" << ", ";
                }
            }
        }
        std::cout << std::endl;
    }
};
#endif //TINYLALR_PARSER_H