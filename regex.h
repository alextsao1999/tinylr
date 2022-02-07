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
#include <algorithm>
#include <climits>

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
    using RegexChar = int;
    using SymbolType = int;
    constexpr SymbolType SymbolNull = 0;
    constexpr RegexChar RegexMinChar = std::numeric_limits<RegexChar>::min();
    constexpr RegexChar RegexMaxChar = std::numeric_limits<RegexChar>::max();

    class RegexVisitor;
    class RegexRange;
    class RegexState;

    class RegexNode {
    public:
        std::set<RegexRange *> firstpos;
        std::set<RegexRange *> lastpos;
        std::set<RegexRange *> followpos;
        virtual ~RegexNode() = default;
        virtual int accept(RegexVisitor *) = 0;
        virtual void print() {}
        virtual bool nullable() { return false; }
    };

    class RegexConcat : public RegexNode {
    public:
        RegexNodeDecl();
        RegexConcat() = default;
        RegexConcat(const std::shared_ptr<RegexNode> &lhs, const std::shared_ptr<RegexNode> &rhs) {
            add(lhs);
            add(rhs);
        }
        ~RegexConcat() override = default;
        std::vector<std::shared_ptr<RegexNode>> nodes;
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
        ~RegexBracket() override = default;
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
        ~RegexOr() override = default;
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
        explicit RegexRange(RegexChar chr) : begin(chr), end(chr)  {}
        ~RegexRange() override = default;
        RegexChar begin;
        RegexChar end;

        inline bool contain(RegexChar begin_, RegexChar end_) const {
            return this->begin <= begin_ && this->end > end_;
        }

        inline bool is_any() const {
            return begin == RegexMinChar && end == RegexMaxChar;
        }

        inline bool is_last() const {
            return followpos.empty();
        }

        void print() override {
            if (begin == end) {
                std::cout << (char) begin;
            } else {
                std::cout << (char) begin << "-" << (char) end;
            }
        }

        inline static auto FromRange(RegexChar begin, RegexChar end) {
            return std::make_shared<RegexRange>(begin, end + 1);
        }

        inline static auto FromChar(RegexChar chr) {
            return std::make_shared<RegexRange>(chr, chr + 1);
        }

        inline static auto FromAny() {
            return std::make_shared<RegexRange>(RegexMinChar, RegexMaxChar);
        }

    };

    class RegexStar : public RegexNode {
    public:
        RegexNodeDecl();
        explicit RegexStar(std::shared_ptr <RegexNode> node) : node(std::move(node)) {}
        ~RegexStar() override = default;
        std::shared_ptr<RegexNode> node;
        void print() override {
            node->print();
            std::cout << "*";
        }

        bool nullable() override { return true; }
    };

    class RegexPlus : public RegexNode {
    public:
        RegexNodeDecl();
        explicit RegexPlus(std::shared_ptr <RegexNode> node) : node(std::move(node)) {}
        ~RegexPlus() override = default;
        std::shared_ptr<RegexNode> node;
        void print() override {
            node->print();
            std::cout << "+";
        }
    };

    class RegexQuestion : public RegexNode {
    public:
        RegexNodeDecl();
        explicit RegexQuestion(std::shared_ptr <RegexNode> node) : node(std::move(node)) {}
        ~RegexQuestion() override = default;
        std::shared_ptr<RegexNode> node;
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

    struct RegexRangeCompare {
        bool operator() (const RegexRange *lhs, const RegexRange *rhs) const {
            return lhs->begin < rhs->begin ||
                   (lhs->begin == rhs->begin && lhs->end < rhs->end);
        }
    };
    using RangeSet = std::set<RegexRange *, RegexRangeCompare>;

    struct RegexTransition {
        RegexState *state = nullptr;
        RegexChar begin;
        RegexChar end;
        RegexTransition(RegexState *state, RegexChar begin, RegexChar end) : state(state), begin(begin), end(end) {}
        RegexTransition(RegexChar begin, RegexChar end) : begin(begin), end(end) {}

        bool operator<(const RegexTransition &other) const {
            return begin < other.begin || (begin == other.begin && end < other.end);
        }
    };
    struct RegexState {
        size_t index = 0;
        SymbolType symbol = SymbolNull;
        std::set<RegexRange *> gotos; // list index of the leaf
        std::set<RegexTransition> transitions;

        RegexState() = default;
        RegexState(size_t index, SymbolType symbol, std::set<RegexRange *> gotos) : index(index), symbol(symbol),
                                                                                    gotos(std::move(gotos)) {}

        inline const RegexTransition *find_trans(const uint8_t chr) const {
            for (auto &item: transitions) {
                if (chr >= item.begin && chr < item.end) {
                    return &item;
                }
            }
            return nullptr;
        }
    };

    class RegexGenerator : private RegexVisitor {
    public:
        std::set<RegexRange *> firstpos; // all first pos
        std::vector<std::unique_ptr<RegexState>> states;
        std::vector<std::shared_ptr<RegexNode>> nodes;
        std::map<RegexRange *, SymbolType> symbols;
        RegexGenerator() = default;

        void feed(const std::shared_ptr<RegexNode>& node, SymbolType symbol) {
            node->accept(this);
            firstpos.insert(node->firstpos.begin(), node->firstpos.end());
            for (auto &item : node->lastpos) {
                symbols[item] = symbol;
            }
            nodes.push_back(node);
        }

        std::vector<std::unique_ptr<RegexState>> &generate() {
            generate_state(firstpos);
            int visit_count = 0;
            while (visit_count < states.size()) {
                generate_transition(states[visit_count++].get());
            }
            return states;
        }

    private:
        RegexState *generate_state(const std::set<RegexRange *> &gotos, SymbolType symbol = SymbolNull) {
            for (auto &state : states) {
                if (state->gotos == gotos && symbol == state->symbol) {
                    return state.get();
                }
            }
            auto *state = new RegexState(states.size(), symbol, gotos);
            states.emplace_back(state);
            return state;
        }

        void generate_goto(RegexState *state, RegexChar begin, RegexChar end) {
            std::set<RegexRange *> gotos; // 下一步要goto的range
            std::set<RegexRange *> matched;
            RegexRange *prior = nullptr;
            SymbolType symbol = SymbolNull;

            for (auto &item : state->gotos) {
                if (item->begin <= begin && item->end >= end && !item->is_any()) {
                    if (auto find = symbols.find(item);find != symbols.end()) {
                        if (!prior || prior && prior->contain(item->begin, item->end)) {
                            prior = item;
                        }
                    }
                    gotos.insert(item->followpos.begin(), item->followpos.end());
                }
                if (item->is_any()) {
                    matched.insert(item->followpos.begin(), item->followpos.end());
                }
            }

            if (prior) {
                symbol = symbols[prior];
            } else if (gotos.empty()) {
                gotos = matched;
            }

            if (auto *goto_state = generate_state(gotos, symbol)) {
                state->transitions.insert(RegexTransition(goto_state, begin, end));
            }
        }

        void generate_transition(RegexState *state) {
            RangeSet ranges;
            ranges.insert(state->gotos.begin(), state->gotos.end());

            std::vector<std::pair<RegexChar, RegexChar>> merged;
            for (auto &item: ranges) {
                if (merged.empty() || item->begin > merged.back().second) {
                    merged.emplace_back(item->begin, item->end);
                } else {
                    merged.back().second = std::max(merged.back().second, item->end);
                }
            }

            std::map<std::pair<RegexChar, RegexChar>, std::set<int>> splited;
            for (auto &item: state->gotos) {
                for (auto &merge: merged) {
                    if (item->begin >= merge.first && item->end <= merge.second) {
                        splited[merge].insert(item->begin);
                        splited[merge].insert(item->end);
                    }
                }
            }

            merged.clear();
            for (auto &item: splited) {
                for (auto iter = item.second.begin(), e = --item.second.end(); iter != e; ) {
                    auto begin = *iter;
                    auto end = *(++iter);
                    merged.emplace_back(begin, end);
                }
            }

            for (auto &item: merged) {
                generate_goto(state, item.first, item.second);
            }
        }

        void add_follow(std::set<RegexRange *> &elements, std::set<RegexRange *> &follow) {
            for (auto &item : elements) {
                item->followpos.insert(follow.begin(), follow.end());
            }
        }

        int visit(RegexConcat *node) override {
            for (auto &item : node->nodes) {
                item->accept(this);
            }
            for (auto &item : node->nodes) {
                node->firstpos.insert(item->firstpos.begin(), item->firstpos.end());
                if (!item->nullable()) {
                    break;
                }
            }
            for (auto iter = node->nodes.rbegin();iter != node->nodes.rend();++iter) {
                node->lastpos.insert((*iter)->lastpos.begin(), (*iter)->lastpos.end());
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
                node->firstpos.insert(item->firstpos.begin(), item->firstpos.end());
                node->lastpos.insert(item->lastpos.begin(), item->lastpos.end());
            }
            return 0;
        }
        int visit(RegexRange *node) override {
            node->firstpos.insert(node);
            node->lastpos.insert(node);
            return 0;
        }
        int visit(RegexOr *node) override {
            node->lhs->accept(this);
            node->rhs->accept(this);
            node->firstpos.insert(node->lhs->firstpos.begin(), node->lhs->firstpos.end());
            node->firstpos.insert(node->rhs->firstpos.begin(), node->rhs->firstpos.end());
            node->lastpos.insert(node->lhs->lastpos.begin(), node->lhs->lastpos.end());
            node->lastpos.insert(node->rhs->lastpos.begin(), node->rhs->lastpos.end());
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
                    // FIXME: overflow
                    value = value * 10 + (*m_current - '0');
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
                return RegexRange::FromRange(chr, parse_character());
            } else {
                return RegexRange::FromChar(chr);
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
                    return RegexRange::FromAny();
                default:
                    return RegexRange::FromChar(parse_character());
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
                concat->add(RegexRange::FromChar(parse_character()));
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
    SymbolType regex_match(std::vector<std::unique_ptr<RegexState>> &state_machine, It string) {
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
