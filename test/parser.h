//
// Created by Alex
//

#ifndef TINYLALR_PARSER_H
#define TINYLALR_PARSER_H
#include "ast.h"
#include <queue>
#include <map>
#define CONFLICT_NONE 0
#define CONFLICT_SHIFT_REDUCE 1
#define CONFLICT_REDUCE_REDUCE 2
#define SYMBOL_TYPE_TERMINAL 0
#define SYMBOL_TYPE_NONTERMINAL 1
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
#define ACTION_TYPE_CREATE 9
using value_t = ASTNodePtr;
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
    int opcode;
    int value;
    int index;
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
template <class char_t = char, class char_traits = std::char_traits<char_t>>
struct ParserTreeNode {
    struct Link {
        std::shared_ptr<ParserTreeNode> node;
        std::shared_ptr<Link> next;
        Link(const std::shared_ptr<ParserTreeNode> &node) : node(node) {}
        Link(const std::shared_ptr<ParserTreeNode> &node, const std::shared_ptr<Link> &next) : node(node), next(next) {}
        void to_nodes(std::vector<ParserTreeNode *> &nodes) {
            nodes.clear();
            auto start = next;
            while (start) {
                nodes.push_back(start->node.get());
                start = start->next;
            }
        }
    };
    std::shared_ptr<ParserTreeNode> prev;
    std::vector<std::shared_ptr<ParserTreeNode>> prevs;
    ParserState *state = nullptr;
    int symbol = 0;
    int line = 0;
    int column = 0;
    std::basic_string<char_t, char_traits> lexeme;
    value_t value;
    int depth = 0;
    int merge = 0;
    bool error = false;
    ParserTreeNode(ParserState *state) : state(state) {}
    ParserTreeNode(const std::shared_ptr<ParserTreeNode> &prev, ParserState *state) : prev(prev), state(state) {}
    void add_prev(const std::shared_ptr<ParserTreeNode> &previous) {
        prevs.push_back(previous);
    }
    bool need_lr_reduce(ParserTransition *trans) {
        return state->conflict == CONFLICT_NONE && trans->reduce_length <= depth;
    }
};
template <class iter_t = const char *, class char_t = typename std::iterator_traits<iter_t>::value_type, class char_traits = std::char_traits<char_t>>
class GLRParser {
    using Lexer = ParserLexer<iter_t>;
    using Node = ParserTreeNode<char_t, char_traits>;
    using NodePtr = std::shared_ptr<Node>;
    using Link = typename Node::Link;
    using LinkPtr = std::shared_ptr<Link>;
    struct ReduceNode {
        LinkPtr link;
        ParserTransition *trans;
        ReduceNode(const LinkPtr &link, ParserTransition &trans) : link(link), trans(&trans) {}
        inline bool operator<(const ReduceNode &rhs) const {
            return trans->precedence < rhs.trans->precedence;
        }
    };
    ParserState *parser_state = &ParserStates[0];
    Lexer lexer_ = Lexer(&LexerStates[0], LexerWhitespaceSymbol);
    bool position = false;
    bool accepted = false;
    std::map<ParserState *, NodePtr> frontier;
    std::vector<NodePtr> shift_list;
    std::vector<LinkPtr> link_list;
    std::vector<Node *> action_nodes;
    std::priority_queue<ReduceNode> reduce_list;
public:
    GLRParser() = default;
    void set_position(bool sp) {
        position = sp;
    }
    void reset(iter_t first, iter_t last = iter_t()) {
        accepted = false;
        frontier.clear();
        lexer_.reset(first, last);
    }
    void parse() {
        frontier.insert(std::pair(parser_state, NodePtr(new Node(parser_state))));
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
            ReduceNode node = reduce_list.top();
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
                    NodePtr shift_node = NodePtr(new Node(node, trans.state));
                    shift_node->line = lexer_.line();
                    shift_node->column = lexer_.column();
                    shift_node->symbol = lexer_.symbol();
                    shift_node->lexeme = lexer_.lexeme();
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
                frontier[node->state]->add_prev(node->prev);
                frontier[node->state]->depth = 0;
            } else {
                frontier.insert(std::pair(node->state, node));
            }
        }
        lexer_.advance();
    }
    void do_goto(ReduceNode &node) {
        node.link->to_nodes(action_nodes);
        if (node.trans->accept()) {
            auto start = NodePtr(new Node(parser_state));
            start->value = action_nodes[0]->value;
            frontier.insert(std::pair(parser_state, start));
            accepted = true;
            return;
        }
        int line = 0, column = 0;
        value_t value = handle_action(node.trans->actions, node.trans->action_count, action_nodes.data());
        if (!action_nodes.empty()) {
            line = action_nodes[0]->line;
            column = action_nodes[0]->column;
            if (action_nodes.back()->need_lr_reduce(node.trans)) {
                frontier.erase(action_nodes.back()->state); // del(node)
            }
        }
        for (auto &goto_trans : *(node.link->node->state)) {
            if (goto_trans.type == TRANSITION_SHIFT && goto_trans.symbol == node.trans->reduce_symbol) {
                if (frontier.count(goto_trans.state)) {
                    do_merge(frontier[goto_trans.state], value);
                    frontier[goto_trans.state]->add_prev(node.link->node);
                    frontier[goto_trans.state]->depth = 0;
                } else {
                    auto goto_node = NodePtr(new Node(node.link->node, goto_trans.state));
                    goto_node->value = value;
                    goto_node->line = line;
                    goto_node->column = column;
                    goto_node->symbol = node.trans->reduce_symbol;
                    goto_node->lexeme = ParserSymbols[node.trans->reduce_symbol].text;
                    goto_node->depth = node.link->node->depth + 1;
                    do_reduce(goto_node);
                    frontier.insert(std::pair(goto_node->state, goto_node));
                }
            }
        }
    }
    void do_reduce(NodePtr &node) {
        for (auto &trans : *(node->state)) {
            if (trans.type == TRANSITION_REDUCE && trans.symbol == lexer_.symbol()) {
                if (node->depth >= trans.reduce_length && node->state->conflict == CONFLICT_NONE) {
                    do_lr_reduce(node, trans);
                } else {
                    do_glr_reduce(node, trans);
                }
            }
        }
    }
    void do_lr_reduce(NodePtr node, ParserTransition &trans) {
        auto link = LinkPtr(new Link(node));
        action_nodes.resize(trans.reduce_length);
        for (int i = trans.reduce_length - 1; i >= 0; --i) {
            node = node->prev;
            link = LinkPtr(new Link(node, link));
        }
        reduce_list.emplace(link, trans);
    }
    void do_glr_reduce(NodePtr node, ParserTransition &trans) {
        link_list.clear();
        link_list.push_back(LinkPtr(new Link(node)));
        for (int i = trans.reduce_length - 1; i >= 0; --i) {
            for (int j = 0, length = link_list.size(); j < length; ++j) {
                for (auto &prev : link_list[j]->node->prevs) {
                    link_list.push_back(LinkPtr(new Link(prev, link_list[j])));
                }
                link_list[j] = LinkPtr(new Link(link_list[j]->node->prev, link_list[j]));
            }
        }
        for (auto &link : link_list) {
            reduce_list.emplace(link, trans);
        }
    }
    void do_merge(NodePtr node, value_t &value) {
        if (node->merge) {
            std::dynamic_pointer_cast<ASTMerge>(node->value)->add(value);
        } else {
            auto merge = std::make_shared<ASTMerge>();
            merge->add(node->value);
            merge->add(value);
            node->value = merge;
        }
        node->merge++;
    }
    void do_error(NodePtr node) {
        // shift error
        if (node->error) {
            std::dynamic_pointer_cast<ASTError>(node->value)->add(
                    std::make_shared<ASTLexeme<char_t, char_traits>>(lexer_.line(), lexer_.column(), lexer_.lexeme()));
        } else {
            NodePtr shift_node = NodePtr(new Node(node, node->state->error->state));
            shift_node->symbol = 2;
            shift_node->line = lexer_.line();
            shift_node->column = lexer_.column();
            shift_node->lexeme = "error";
            shift_node->error = true;
            shift_node->depth = node->depth + 1;
            auto ptr = std::make_shared<ASTError>();
            ptr->add(std::make_shared<ASTLexeme<char_t, char_traits>>(lexer_.lexeme()));
            shift_node->value = ptr;
            shift_list.push_back(shift_node);
        }
    }
    inline value_t handle_action(ReduceAction *actions, int action_count, Node **nodes) {
        if (action_count == 0) {
            return value_t();
        }
        value_t value;
        for (int i = 0; i < action_count; ++i) {
            auto &action = actions[i];
            if (action.opcode == ACTION_TYPE_CREATE && !value) {
                value = CreateASTById(action.value);
            }
            if (action.opcode == ACTION_TYPE_INIT) {  // $n
                if (*action.desc == '@') {
                    value = std::make_shared<ASTLexeme<char_t, char_traits>>(nodes[action.value]->lexeme);
                } else {
                    value = nodes[action.value]->value;
                }
            }
            if (action.opcode == ACTION_TYPE_INSERT) { // {$n|@n|#n}
                if (!value) {
                    value = std::make_shared<ASTList>();
                }
                if (*action.desc == '@') {
                    std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTLexeme<char_t, char_traits>>(
                            nodes[action.value]->line,
                            nodes[action.value]->column,
                            nodes[action.value]->lexeme));
                } else {
                    std::dynamic_pointer_cast<ASTList>(value)->add(nodes[action.value]->value);
                }
            }
            if (action.opcode == ACTION_TYPE_SET && *action.desc == '$') { // key:$n set value
                std::dynamic_pointer_cast<ASTList>(value)->set(action.index, nodes[action.value]->value);
            }
            if (action.opcode == ACTION_TYPE_SET && *action.desc == '@') { // key:@n set lexeme
                std::dynamic_pointer_cast<ASTList>(value)->set(action.index,
                                                               std::make_shared<ASTLexeme<char_t, char_traits>>(
                                                                       nodes[action.value]->line,
                                                                       nodes[action.value]->column,
                                                                       nodes[action.value]->lexeme));
            }
            if (action.opcode == ACTION_TYPE_SET && *action.desc == '#') { // key:#n insert
                auto ptr = std::dynamic_pointer_cast<ASTList>(value)->get(action.value);
                std::dynamic_pointer_cast<ASTList>(ptr)->add(nodes[action.value]->value);
            }
            if (action.opcode == ACTION_TYPE_SET_STRING) {  // key:'string'
                std::dynamic_pointer_cast<ASTList>(value)->set(action.index, std::make_shared<ASTString>(action.desc));
            }
            if (action.opcode == ACTION_TYPE_SET_BOOL) { // key:bool
                std::dynamic_pointer_cast<ASTList>(value)->set(action.index, std::make_shared<ASTInteger>(action.value));
            }
            if (action.opcode == ACTION_TYPE_SET_INT) { // key:number
                std::dynamic_pointer_cast<ASTList>(value)->set(action.index, std::make_shared<ASTInteger>(action.value));
            }
            if (action.opcode == ACTION_TYPE_INSERT_STRING) {
                std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTString>(action.desc));
            }
            if (action.opcode == ACTION_TYPE_INSERT_BOOL) { // bool
                std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTInteger>(action.value));
            }
            if (action.opcode == ACTION_TYPE_INSERT_INT) { // number
                std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTInteger>(action.value));
            }
        }
        return std::move(value);
    }
    inline void handle_error() {
        std::cout << "Unexpected Symbol '" << lexer_.lexeme() << "'"
                  << " line:" << lexer_.line() << " column:" << lexer_.column()
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