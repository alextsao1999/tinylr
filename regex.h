//
// Created by Alex
//

#ifndef ALEX_LIBS_REGEX_H
#define ALEX_LIBS_REGEX_H

#include <utility>
#include <tuple>
#include <memory>
#include <vector>
#include <map>
#include <set>
#include <iostream>

#define RegexNodeDecl() int accept(RegexVisitor *) override;
#define RegexNodeList(V) \
            V(RegexConcat) \
            V(RegexBracket) \
            V(RegexOr) \
            V(RegexRange) \
            V(RegexPlus) \
            V(RegexQuestion) \
            V(RegexStar)
namespace alex {
    using SymbolType = void *;
    constexpr SymbolType SymbolNull = nullptr;
    using RegexChar = int;
    constexpr RegexChar RegexMaxChar = -1;
    class RegexVisitor;
    class RegexRange;
    class RegexNode {
    public:
        std::vector<RegexRange *> firstpos;
        std::vector<RegexRange *> lastpos;
        std::vector<RegexRange *> followpos;
        virtual ~RegexNode() = default;
        virtual int accept(RegexVisitor *) = 0;
        virtual void print() {}
        virtual bool nullable() { return false; }
    };
    class RegexConcat : public RegexNode {
    public:
        RegexNodeDecl();
        RegexConcat() = default;
        virtual ~RegexConcat() = default;
        RegexConcat(const std::shared_ptr<RegexNode> &lhs, const std::shared_ptr<RegexNode> &rhs) {
            add(lhs);
            add(rhs);
        }

        std::vector <std::shared_ptr<RegexNode>> nodes;
        void add(const std::shared_ptr<RegexNode>& node) {
            nodes.push_back(node);
        }
        void print() override {
            std::cout << "(";
            for (auto &item : nodes) {
                item->print();
            }
            std::cout << ")";
        }
        bool nullable() override {
            for (auto &item : nodes) {
                if (!item->nullable()) {
                    return false;
                }
            }
            return true;
        }
    };
    class RegexBracket : public RegexNode {
    public:
        RegexNodeDecl();
        RegexBracket() = default;
        virtual ~RegexBracket() = default;
        std::vector <std::shared_ptr<RegexNode>> nodes;
        void add(const std::shared_ptr<RegexNode>& node) {
            nodes.push_back(node);
        }
        void print() override {
            std::cout << "[";
            for (auto &item : nodes) {
                item->print();
            }
            std::cout << "]";
        }
    };
    class RegexOr : public RegexNode {
    public:
        RegexNodeDecl();
        RegexOr() = default;
        virtual ~RegexOr() = default;
        std::shared_ptr<RegexNode> lhs;
        std::shared_ptr<RegexNode> rhs;
        RegexOr(std::shared_ptr <RegexNode> lhs, std::shared_ptr <RegexNode> rhs) : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
        void print() override {
            lhs->print();
            std::cout << "|";
            rhs->print();
        }
        bool nullable() override { return rhs->nullable() || rhs->nullable(); }
    };
    class RegexRange : public RegexNode {
    public:
        RegexNodeDecl();
        RegexRange(RegexChar begin, RegexChar end) : begin(begin), end(end) {}
        RegexRange(RegexChar chr) : begin(chr), end(chr)  {}
        virtual ~RegexRange() = default;
        RegexChar begin;
        RegexChar end;
        SymbolType symbol = SymbolNull;
        bool contain(RegexChar begin_, RegexChar end_) {
            return this->begin <= begin_ && this->end >= end_;
        }
        bool is_priority(RegexChar begin_, RegexChar end_) {
            return begin == begin_ && end == end_ && symbol != SymbolNull;
        }
        void print() override {
            if (begin == end) {
                std::cout << (char) begin;
            } else {
                std::cout << (char) begin << "-" << (char) end;
            }
            //std::cout << " #" << index << followpos << " ";
        }
    };
    class RegexStar : public RegexNode {
    public:
        RegexNodeDecl();
        std::shared_ptr <RegexNode> node;
        RegexStar(std::shared_ptr <RegexNode> node) : node(std::move(node)) {}
        virtual ~RegexStar() = default;
        void print() override {
            node->print();
            std::cout << "*";
        }
        bool nullable() override { return true; }
    };
    class RegexPlus : public RegexNode {
    public:
        RegexNodeDecl();
        std::shared_ptr <RegexNode> node;
        RegexPlus(std::shared_ptr <RegexNode> node) : node(std::move(node)) {}
        virtual ~RegexPlus() = default;
        void print() override {
            node->print();
            std::cout << "+";
        }
    };
    class RegexQuestion : public RegexNode {
    public:
        RegexNodeDecl();
        std::shared_ptr <RegexNode> node; // 问号
        RegexQuestion(std::shared_ptr <RegexNode> node) : node(std::move(node)) {}
        virtual ~RegexQuestion() = default;
        void print() override {
            node->print();
            std::cout << "?";
        }
        bool nullable() override { return true; }
    };
    class RegexVisitor {
    public:
#define DefineVisitor(Type) virtual int visit(Type *node) { return node->accept(this); }
        RegexNodeList(DefineVisitor)
#undef  DefineVisitor
    };
#define DefineVisitor(Type) int Type::accept(RegexVisitor *visitor) { return visitor->visit(this); }
    RegexNodeList(DefineVisitor)
#undef  DefineVisitor
    class RegexState;
    struct RegexTransition {
        RegexState *state = nullptr;
        RegexChar begin;
        RegexChar end;
        RegexTransition(RegexState *state, RegexChar begin, RegexChar end) : state(state), begin(begin), end(end) {}
        RegexTransition(RegexChar begin, RegexChar end) : begin(begin), end(end) {}
    };
    struct RegexState {
        int index = 0;
        SymbolType symbol = SymbolNull;
        std::vector<RegexTransition> transitions;
        std::vector<RegexRange *> gotos; // list index of the leaf
        RegexState() = default;
        inline const RegexTransition *find_trans(const uint8_t chr) const {
            const RegexTransition *dot = nullptr;
            for (auto &item : transitions) {
                if (item.end == RegexMaxChar) {
                    dot = &item;
                }
                if (chr >= item.begin && chr <= item.end) {
                    return &item;
                }
            }
            return dot;
        }
    };
    class RegexGenerator : private RegexVisitor {
    public:
        std::vector<RegexRange *> firstpos; // all first pos
        std::vector<std::unique_ptr<RegexState>> states;
        std::vector<std::shared_ptr<RegexNode>> nodes;
        RegexGenerator() = default;
        void feed(std::shared_ptr<RegexNode> node, SymbolType symbol) {
            node->accept(this);
            firstpos.insert(firstpos.end(), node->firstpos.begin(), node->firstpos.end());
            for (auto &item : node->lastpos) {
                item->symbol = symbol;
            }
            nodes.push_back(node);
        }
        std::vector<std::unique_ptr<RegexState>> generate() {
            get_goto_state(firstpos);
            int visit_count = 0;
            while (visit_count < states.size()) {
                generate_transition(states[visit_count++].get());
            }
            return std::move(states);
        }
    private:
        bool contains(std::vector<RegexRange *> &vec, RegexRange *node) {
            for (auto &item :vec) {
                if (item == node) {
                    return true;
                }
            }
            return false;
        }
        bool is_same(std::vector<RegexRange *> &lhs, std::vector<RegexRange *> &rhs) {
            for (auto &slice : rhs) {
                if (!contains(lhs, slice)) {
                    return false;
                }
            }
            if (!lhs.empty() && rhs.empty()) {
                return false;
            }
            return true;
        }
        bool is_intersect(RegexState *state, RegexChar begin, RegexChar end) {
            for (auto &item : state->gotos) {
                if (item->contain(begin, end)) {
                    return true;
                }
            }
            return false;
        }
        RegexState *get_goto_state(std::vector<RegexRange *> &gotos, SymbolType symbol = SymbolNull) {
            for (int i = 0; i < states.size(); ++i) {
                if (is_same(states[i]->gotos, gotos) && symbol == states[i]->symbol) {
                    return states[i].get();
                }
            }
            auto *state = new RegexState();
            state->index = states.size();
            state->symbol = symbol;
            state->gotos = gotos;
            states.emplace_back(state);
            return state;
        }
        void generate_transition_range(RegexState *state, RegexChar begin, RegexChar end) {
            std::vector<RegexRange *> gotos;
            std::vector<RegexRange *> priority;
            SymbolType symbol = SymbolNull;
            for (auto &item : state->gotos) {
                if (item->contain(begin, end)) {
                    for (auto &follow : item->followpos) {
                        if (!contains(gotos, follow)) {
                            gotos.push_back(follow);
                        }
                    }
                    if (item->symbol != SymbolNull) {
                        symbol = item->symbol;
                    }
                    if (item->is_priority(begin, end)) {
                        priority.push_back(item);
                    }
                }
            }
            if (!priority.empty()) {
                for (auto &item : priority) {
                    symbol = item->symbol;
                }
            }
            auto *goto_state = get_goto_state(gotos, symbol);
            if (goto_state) {
                state->transitions.push_back(RegexTransition(goto_state, begin, end));
            }
        }
        void generate_transition(RegexState *state) {
            std::map<RegexChar, int> bounds;
            for (auto &item : state->gotos) { // 将go_to相同的状态合并 之后生成新go_to
                bounds.insert(std::pair(item->begin, 1));
                bounds.insert(std::pair(item->end + 1, -1));
            }
            auto iter = bounds.begin();
            while (iter != bounds.end()) {
                auto count = iter->second;
                auto begin = (iter++)->first;
                if (iter == bounds.end()) {
                    break;
                }
                int end = (iter)->first - 1;
                if (count < 0 && iter->second > 0) {
                    if (is_intersect(state, begin, end)) {
                        generate_transition_range(state, begin, end);
                    }
                } else {
                    generate_transition_range(state, begin, end);
                }
            }
        }
        void add_follow(std::vector<RegexRange *> &elements, std::vector<RegexRange *> &follow) {
            for (auto &item : elements) {
                item->followpos.insert(item->followpos.end(), follow.begin(), follow.end());
            }
        }
        int visit(RegexConcat *node) override {
            for (auto &item : node->nodes) {
                item->accept(this);
            }
            for (auto &item : node->nodes) {
                node->firstpos.insert(node->firstpos.end(), item->firstpos.begin(), item->firstpos.end());
                if (!item->nullable()) {
                    break;
                }
            }
            for (auto iter = node->nodes.rbegin();iter != node->nodes.rend();++iter) {
                node->lastpos.insert(node->lastpos.end(), (*iter)->lastpos.begin(), (*iter)->lastpos.end());
                if (!(*iter)->nullable()) {
                    break;
                }
            }
            for (int i = 1; i < node->nodes.size(); ++i) {
                for (int j = i; j < node->nodes.size(); ++j) {
                    add_follow(node->nodes[i - 1]->lastpos, node->nodes[j]->firstpos);
                    if (!node->nodes[j]->nullable()) {
                        break;
                    }
                }
            }
            return 0;
        }
        int visit(RegexBracket *node) override {
            for (auto &item : node->nodes) {
                item->accept(this);
                node->firstpos.insert(node->firstpos.end(), item->firstpos.begin(), item->firstpos.end());
                node->lastpos.insert(node->lastpos.end(), item->lastpos.begin(), item->lastpos.end());
            }
            return 0;
        }
        int visit(RegexRange *node) override {
            node->firstpos.push_back(node);
            node->lastpos.push_back(node);
            return 0;
        }
        int visit(RegexOr *node) override {
            node->lhs->accept(this);
            node->rhs->accept(this);
            node->firstpos.insert(node->firstpos.end(), node->lhs->firstpos.begin(), node->lhs->firstpos.end());
            node->firstpos.insert(node->firstpos.end(), node->rhs->firstpos.begin(), node->rhs->firstpos.end());
            node->lastpos.insert(node->lastpos.end(), node->lhs->lastpos.begin(), node->lhs->lastpos.end());
            node->lastpos.insert(node->lastpos.end(), node->rhs->lastpos.begin(), node->rhs->lastpos.end());
            return 0;
        }
        int visit(RegexPlus *node) override {
            node->node->accept(this);
            node->firstpos = node->node->firstpos;
            node->lastpos = node->node->lastpos;
            add_follow(node->node->lastpos, node->node->firstpos);
            return 0;
        }
        int visit(RegexStar *node) override {
            node->node->accept(this);
            node->firstpos = node->node->firstpos;
            node->lastpos = node->node->lastpos;
            add_follow(node->node->lastpos, node->node->firstpos);
            return 0;
        }
        int visit(RegexQuestion *node) override {
            node->node->accept(this);
            node->firstpos = node->node->firstpos;
            node->lastpos = node->node->lastpos;
            return 0;
        }
    };
    class RegexParser {
    public:
        using char_t = char;
        const char_t *m_first = nullptr;
        const char_t *m_last = nullptr;
        const char_t *m_current = nullptr;
        RegexParser() = default;
        RegexParser(const char_t *first, const char_t *last) : m_first(first), m_last(last), m_current(first) {}
        RegexParser(const char_t *s) : m_first(s), m_last(s + strlen(s)), m_current(s) {}
        inline void reset(const char_t *string) {
            m_first = m_current = string;
            m_last = string + strlen(string);
        }
        inline bool has() const { return m_current < m_last; }
        RegexChar parse_character() {
            if (*m_current == '\\') {
                switch (*++m_current) {
                    case 't':
                        m_current++;
                        return '\t';
                    case 'n':
                        m_current++;
                        return '\n';
                    case '\\':
                        m_current++;
                        return '\\';
                    case 'a':
                        m_current++;
                        return '\a';
                    case 'b':
                        m_current++;
                        return '\b';
                    case 'f':
                        m_current++;
                        return '\f';
                    case 'r':
                        m_current++;
                        return '\r';
                    case 'u':
                        m_current += 5;
                        return ((FromHex(*(m_current - 4)) << 12) + (FromHex(*(m_current - 3)) << 8) +
                               (FromHex(*(m_current - 2)) << 4) + FromHex(*(m_current - 1)));
                    case 'x':
                        m_current += 3;
                        return (FromHex(*(m_current - 2)) << 4) + FromHex(*(m_current - 1));
                    default:
                        break;
                }
            }
            return uint8_t(*m_current++);
        }
        int parse_integer() {
            int value = 0;
            if (isdigit(*m_current)) {
                do {
                    value *= 10;
                    value += (*m_current - '0');
                } while (isdigit(*++m_current));
            }
            if (*m_current == ',' || *m_current == '}') {
                m_current++;
            }
            return value;
        }
        std::shared_ptr<RegexNode> parse_choice() {
            auto chr = parse_character();
            if (*m_current == '-') {
                m_current++;
                return std::make_shared<RegexRange>(chr, parse_character());
            } else {
                return std::make_shared<RegexRange>(chr);
            }
        }
        std::shared_ptr<RegexNode> parse_bracket() {
            std::shared_ptr<RegexBracket> bracket(new RegexBracket());
            while (has()) {
                switch (*m_current) {
                    case ']':
                        m_current++;
                        return bracket;
                    default: {
                        bracket->add(parse_choice());
                    }
                }
            }
            return bracket;
        }
        std::shared_ptr<RegexNode> parse_char() {
            switch (*m_current) {
                case '.':
                    m_current++;
                    return std::make_shared<RegexRange>(RegexMaxChar);
                default:
                    return std::make_shared<RegexRange>(parse_character());
            }
        }
        // concat item
        std::shared_ptr<RegexNode> parse_item() {
            std::shared_ptr<RegexNode> node;
            switch(*m_current) {
                case '(':
                    m_current++;
                    node = parse_concat();
                    break;
                case '[':
                    m_current++;
                    node = parse_bracket();
                    break;
                default:
                    node = parse_char();
                    break;
            }
            return parse_postfix(node);
        }
        std::shared_ptr<RegexNode> parse_concat() {
            std::shared_ptr<RegexConcat> concat(new RegexConcat());
            while (has()) {
                switch (*m_current) {
                    case ')':
                        m_current++;
                        return concat;
                    case '|':
                        m_current++;
                        return std::make_shared<RegexOr>(concat, parse_concat());
                    default: {
                        concat->add(parse_item());
                    }
                }
            }
            return concat;
        }
        std::shared_ptr<RegexNode> parse_postfix(const std::shared_ptr<RegexNode> &node) {
            switch (*m_current) {
                case '*':
                    m_current++;
                    return std::make_shared<RegexStar>(node);
                case '+':
                    m_current++;
                    return std::make_shared<RegexPlus>(node);
                case '?':
                    m_current++;
                    return std::make_shared<RegexQuestion>(node);
                default:
                    break;
            }
            return node;
        }
        std::shared_ptr<RegexNode> parse_literal() {
            std::shared_ptr<RegexConcat> concat(new RegexConcat());
            while (has()) {
                concat->add(std::make_shared<RegexRange>(parse_character()));
            }
            return concat;
        }
        inline static uint8_t FromHex(const char digit) {
            if (digit >= '0' && digit <= '9')
                return digit - '0';
            if (digit >= 'a' && digit <= 'f')
                return digit - 'a' + 10;
            if (digit >= 'A' && digit <= 'F')
                return digit - 'A' + 10;
            return 0;
        }
    };
    template <typename It>
    SymbolType regex_match(std::vector <std::unique_ptr<RegexState>> &state_machine, It string) {
        auto *state = state_machine[0].get();
        do {
            auto *trans = state->find_trans(*(string));
            if (trans == nullptr) {
                return state->symbol;
            }
            state = trans->state;
            if ((*++string) == '\0') {
                return state->symbol;
            }
        } while (*string != '\0');
        return state->symbol;
    }

}

#endif //ALEX_LIBS_REGEX_H
