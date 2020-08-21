//
// Created by Alex on 8/5/2020.
//

#ifndef MYLIBS_LALR_H
#define MYLIBS_LALR_H
#include <unordered_map>
#include <set>
#include <iostream>
#include "lexer.h"

namespace alex {
    template<typename It>
    class ReverseWrapper {
        It first;
        It last;
    public:
        constexpr ReverseWrapper(It first, It last) : first(first), last(last) {}
        constexpr It begin() { return first; }
        constexpr It end() { return last; }
    };
    template<typename T>
    constexpr decltype(auto) Reverse(const T &container) {
        return ReverseWrapper<decltype(container.rbegin())>(container.rbegin(), container.rend());
    }

    template<typename K, typename V>
    using Map = std::unordered_map<K, V>;
    using viewer_t = std::string_view;
    struct Production;
    struct Symbol;
    struct Nonterminal;
    struct Terminal;
    struct SymbolVisitor {
        virtual int visit(Terminal *) = 0;
        virtual int visit(Nonterminal *) = 0;
    };
    enum Associativity {
        AssoNone,
        AssoLeft,
        AssoRight,
    };
    struct Action {
        int index;
        viewer_t identifier;
        Action(int index, const viewer_t &identifier) : index(index), identifier(identifier) {}
    };
    struct Symbol {
        int index = 0;
        int line = 0;
        int column = 0;
        int precedence = 0;
        Associativity associativity = AssoNone;
        bool nullable = false;
        std::set<Symbol *> first;
        std::set<Symbol *> follow;
        virtual ~Symbol() = default;
        virtual bool is_nonterminal() const { return false; }
        virtual int accept(SymbolVisitor *visitor) { return 0; };
        virtual void dump() const {}
        virtual Nonterminal *nonterminal() { return nullptr; }
        virtual Terminal *terminal() { return nullptr; }
        virtual std::string to_str() { return "(null)"; }
    };
    struct Production {
        Symbol *lhs; // left hand side of the production
        std::vector<Symbol *> symbols;
        Action *action = nullptr;
        Production(Symbol *lhs) : lhs(lhs) {}
        Production(Symbol *lhs, const std::vector<Symbol *> &symbols) : lhs(lhs), symbols(symbols) {}
        Symbol *rightmost_teminal() {
            for (auto iter = symbols.rbegin(); iter != symbols.rend(); ++iter) {
                if (!(*iter)->is_nonterminal()) {
                    return *iter;
                }
            }
            return nullptr;
        }
    };
    struct Nonterminal : Symbol {
        viewer_t identifier;
        std::vector<std::unique_ptr<Production>> productions;
        Nonterminal() = default;
        Nonterminal(const viewer_t &identifier) : identifier(identifier) {}
        virtual ~Nonterminal() = default;
        bool is_nonterminal() const override { return true; }
        Nonterminal *nonterminal() override { return this; }
        int accept(SymbolVisitor *visitor) override { return visitor->visit(this); }
        void push_production(Production *production) {
            productions.push_back(std::unique_ptr<Production>(production));
        }
        void push_symbol(Symbol *sym) {
            productions.back()->symbols.push_back(sym);
        }
        void push_start(Symbol *sym) {
            push_production(new Production(this, {sym}));
        }
        void dump() const override {
            std::cout << "<" << identifier << ">";
        }
        std::string to_str() override { return std::string(identifier); }
    };
    struct Terminal : Symbol {
        viewer_t pattern;
        Terminal(const viewer_t &pattern) : pattern(pattern) {}
        virtual ~Terminal() = default;
        int accept(SymbolVisitor *visitor) override { return visitor->visit(this); }
        void dump() const override {
            std::cout << "(" << pattern << ")";
        }
        Terminal *terminal() override { return this; }
        std::string to_str() override { return std::string(pattern); }
    };
    struct GrammarItem {
        Production *production;
        int position; // the dot position
        mutable std::set<Symbol *> lookahead_symbols;
        inline int add_lookahead(std::set<Symbol *> &symbols) const {
            auto nSize = lookahead_symbols.size();
            lookahead_symbols.insert(symbols.begin(), symbols.end());
            return lookahead_symbols.size() - nSize;
        }
        inline bool is_lookahead(Symbol *symbol) const {
            return lookahead_symbols.count(symbol);
        }
        inline bool is_same_lookahead(const GrammarItem &rhs) const {
            return lookahead_symbols == rhs.lookahead_symbols;
        }
        inline bool dot_reduce() const { return position >= production->symbols.size(); }
        inline bool dot_next(Symbol *symbol) const {
            return production->symbols[position] == symbol /*&& (position + 1 < production->symbols.size())*/;
        }
        inline bool dot_last() const { return position + 1 == production->symbols.size(); }
        Symbol *dot_symbol() const {
            if (position >= production->symbols.size()) {
                return nullptr;
            }
            return production->symbols[position];
        }
        GrammarItem(Production *production, int position) : production(production), position(position) {}
        GrammarItem(Production *production, int position, const std::set<Symbol *> &lookaheadSymbols) : production(
                production), position(position), lookahead_symbols(lookaheadSymbols) {}
        inline bool operator<(const GrammarItem &rhs) const {
            return production < rhs.production || (production == rhs.production && position < rhs.position);
        }
        inline bool operator==(const GrammarItem &rhs) const {
            return production == rhs.production && position == rhs.position;
        }
    };
    struct GrammarState;
    enum TransitionType {
        TransitionGoto,
        TransitionShift,
        TransitionReduce
    };
    struct GrammarTransition {
        GrammarState *state;
        Symbol *symbol;
        //int index = -1;
        mutable TransitionType type = TransitionShift;
        mutable Symbol *reduce_symbol = nullptr;
        mutable int reduce_length = 0;
        mutable Action *reduce_action = nullptr;
        GrammarTransition(GrammarState *state, Symbol *symbol) : state(state), symbol(symbol) {}
        GrammarTransition(GrammarState *state, Symbol *symbol, TransitionType type, Symbol *reduce_symbol = nullptr, Action *reduce_action = nullptr, int reduce_length = 0)
                : state(state), symbol(symbol), type(type), reduce_symbol(reduce_symbol), reduce_action(reduce_action), reduce_length(reduce_length) {
        }
        inline bool operator<(const GrammarTransition&rhs) const {
            return symbol < rhs.symbol;
        }
    };
    struct GrammarState {
        int index = 0;
        std::set<GrammarItem> items; // Item set
        std::set<GrammarTransition> transitions;
        GrammarState() = default;
        GrammarState(const std::set<GrammarItem> &items) : items(std::move(items)) {}
        const GrammarTransition *find_trans(Symbol *symbol) {
            auto iter = transitions.find(GrammarTransition(nullptr, symbol));
            return &(*iter);
        }
    };
    class LALRGrammarParser {
    public:
        Lexer lexer;
        std::vector<std::unique_ptr<Symbol>> symbols;
        std::vector<std::unique_ptr<Action>> actions;
        std::map<viewer_t, Nonterminal *> nonterminal_table;
        std::map<viewer_t, Terminal *> terminal_table;
        std::map<viewer_t, Action *> action_table;
        Nonterminal *start = nullptr;
        Nonterminal *end = nullptr;
        Nonterminal *active = nullptr;
        Nonterminal *error = nullptr;
        int precedence = 0;
        enum TokenType {
            Token_Null,
            Token_String,
            Token_Identifier,
            Token_Bar,
            Token_Arrow,
            Token_None,
            Token_Left,
            Token_Right,
            Token_LeftBraket,
            Token_RightBraket,
            Token_Start,
            Token_End,
        };
        LALRGrammarParser(const char *grammer) {
            lexer.set_whitespace("([ \r\t\n]+)|(/\\*.*\\*/)|(//.*\n)");
            lexer.add_pattern("\"(\\\\.|.)*\"|'(\\\\.|.)*'", (SymbolType) Token_String);
            lexer.add_pattern("([a-zA-Z_]|([\\x81-\\xfe][\\x40-\\xfe]))([a-zA-Z0-9_]|[\\x81-\\xfe][\\x40-\\xfe])*", (SymbolType) Token_Identifier);
            lexer.add_pattern("\\|", (SymbolType) Token_Bar);
            lexer.add_pattern("\\->", (SymbolType) Token_Arrow);
            lexer.add_pattern("%none", (SymbolType) Token_None);
            lexer.add_pattern("%left", (SymbolType) Token_Left);
            lexer.add_pattern("%right", (SymbolType) Token_Right);
            lexer.add_pattern("%start", (SymbolType) Token_Start);
            lexer.add_pattern("\\[", (SymbolType) Token_LeftBraket);
            lexer.add_pattern("\\]", (SymbolType) Token_RightBraket);
            lexer.add_pattern(";", (SymbolType) Token_End);
            lexer.generate_states();

            start = new Nonterminal("@start");
            symbols.push_back(std::unique_ptr<Symbol>(start));

            end = new Nonterminal("@end");
            symbols.push_back(std::unique_ptr<Symbol>(end));

            error = new Nonterminal("error");
            symbols.push_back(std::unique_ptr<Symbol>(error));

            lexer.reset(grammer);
            parse_rules();
        }
        bool match(TokenType token) {
            if (lexer.symbol() == (SymbolType) token) {
                lexer.advance();
                return true;
            }
            return false;
        }
        void expect(const char *str) {
            std::cout << "expect:" << str << " line:"
                      << lexer.line() << " column:" << lexer.column() << std::endl;
            std::cout << ">>> " << lexer.lexeme() << std::endl;
            std::cout << "    " << "^^^" << std::endl;
        }
        Nonterminal *get_nonterminal(viewer_t name) {
            if (nonterminal_table.count(name)) {
                return nonterminal_table[name];
            }
            auto *nt = new Nonterminal(name);
            nt->index = symbols.size() + 1;
            nt->line = lexer.line();
            nt->column = lexer.column();
            symbols.push_back(std::unique_ptr<Symbol>(nt));
            nonterminal_table[name] = nt;
            //start->push_start(nt);
            return nt;
        }
        Terminal *get_terminal(viewer_t name) {
            if (terminal_table.count(name)) {
                return terminal_table[name];
            } else {
                auto *nt = new Terminal(name);
                nt->index = symbols.size() + 1;
                nt->line = lexer.line();
                nt->column = lexer.column();
                symbols.push_back(std::unique_ptr<Symbol>(nt));
                terminal_table[name] = nt;
                return nt;
            }
        }
        void parse_rules() {
            while (lexer.good() && parse_expression()) {
            }
        }
        bool parse_expression() {
            lexer.advance();
            if (lexer.symbol() == (SymbolType) Token_Identifier) {
                active = get_nonterminal(lexer.lexeme());
                lexer.advance();
                if (!match(Token_Arrow)) {
                    expect("->");
                    return false;
                }
                active->push_production(new Production(active));
                while (parse_production());
            } else if (lexer.symbol() >= (SymbolType) Token_None && lexer.symbol() <= (SymbolType) Token_Right) {
                Associativity asso;
                if (lexer.symbol() == (SymbolType) Token_None) {
                    asso = AssoNone;
                }
                if (lexer.symbol() == (SymbolType) Token_Left) {
                    asso = AssoLeft;
                }
                if (lexer.symbol() == (SymbolType) Token_Right) {
                    asso = AssoRight;
                }
                lexer.advance();
                do {
                    Symbol *symbol = parse_symbol();
                    if (!symbol) {
                        break;
                    }
                    symbol->associativity = asso;
                    symbol->precedence = ++precedence;
                } while (true);
            } else if (lexer.symbol() == (SymbolType) Token_Start) {
                lexer.advance();
                do {
                    Symbol *symbol = parse_symbol();
                    if (!symbol) {
                        break;
                    }
                    start->push_start(symbol);
                } while (true);
            } else {
                return false;
            }
            return true;
        }
        bool parse_production() {
            do {
                if (lexer.symbol() == (SymbolType) Token_Bar) {
                    lexer.advance();
                    active->push_production(new Production(active));
                    return true;
                } else if (lexer.symbol() == (SymbolType) Token_LeftBraket) {
                    lexer.advance();
                    if (action_table.count(lexer.lexeme())) {
                        active->productions.back()->action = action_table[lexer.lexeme()];
                    } else {
                        auto *action = new Action(actions.size(), lexer.lexeme());
                        actions.push_back(std::unique_ptr<Action>(action));
                        action_table[lexer.lexeme()] = action;
                        active->productions.back()->action = action;
                    }
                    lexer.advance();
                    match(Token_RightBraket);
                    continue;
                } else if (lexer.symbol() == (SymbolType) Token_End) {
                    return false;
                }
                Symbol *symbol = parse_symbol();
                if (!symbol) {
                    expect("regex or identifier");
                    return false;
                }
                active->push_symbol(symbol);
            } while (true);
        }
        Symbol *parse_symbol() {
            Symbol *symbol = nullptr;
            if (lexer.symbol() == (SymbolType) Token_Identifier) {
                symbol = (get_nonterminal(lexer.lexeme()));
                lexer.advance();
            } else if (lexer.symbol() == (SymbolType) Token_String) {
                symbol = (get_terminal(lexer.lexeme()));
                lexer.advance();
            }
            return symbol;
        }
        std::vector<std::unique_ptr<Symbol>> &get_symbols() { return symbols; }
    };
    class LALRGenerator : SymbolVisitor {
        std::vector<std::unique_ptr<Symbol>> symbols;
        std::vector<std::unique_ptr<Action>> actions;
        std::vector<std::unique_ptr<GrammarState>> states;
    public:
        Nonterminal *start;
        Nonterminal *error;
        Nonterminal *end;
        LALRGenerator(LALRGrammarParser &parser) : symbols(std::move(parser.symbols)),
                                                   actions(std::move(parser.actions)), start(parser.start),
                                                   error(parser.error), end(parser.end) {
            parser.start->follow.insert(end);
            int delta;
            do {
                delta = 0;
                for (auto &symbol : symbols) {
                    delta += symbol->accept(this);
                }
            } while (delta > 0);
            generate_start(parser.start);
        }
        void generate() {
            int visit_count = 0;
            while (visit_count < states.size()) {
                states[visit_count]->index = visit_count;
                generate_transition(states[visit_count++].get());
            }
            for (auto &state : states) {
                generate_reduce(state.get());
            }
        }
        inline std::vector<std::unique_ptr<Symbol>> &get_symbols() { return symbols; }
        inline std::vector<std::unique_ptr<Action>> &get_actions() { return actions; }
        inline std::vector<std::unique_ptr<GrammarState>> &get_states() { return states; }
    private:
        void closure(std::set<GrammarItem> &items) {
            int delta;
            do {
                delta = 0;
                for (auto &item : items) {
                    auto *symbol = item.dot_symbol();
                    if (symbol && symbol->is_nonterminal()) {
                        for (auto &production : symbol->nonterminal()->productions) {
                            auto iter = items.find(GrammarItem(production.get(), 0));
                            if (iter == items.end()) {
                                auto pair = items.insert(GrammarItem(production.get(), 0));
                                delta += lookahead(item, *std::get<0>(pair));
                            } else {
                                delta += lookahead(item, *iter);
                            }
                        }
                    }
                }
            } while (delta > 0);
        }
        void propagate(GrammarState *state, const GrammarItem &item) {
            for (auto &trans : state->transitions) {
                if (!item.dot_reduce()) {
                    auto iter = trans.state->items.find(GrammarItem(item.production, item.position + 1));
                    if (iter != trans.state->items.end()) {
                        if (iter->add_lookahead(item.lookahead_symbols)) {
                            propagate(trans.state, *iter);
                        }
                    }
                }
            }
        }
        void merge(GrammarState *state, std::set<GrammarItem> &items) {
            for (auto &item : items) {
                auto iter = state->items.find(item);
                if (iter != state->items.end()) {
                    if (iter->add_lookahead(item.lookahead_symbols) && !iter->dot_reduce()) {
                        propagate(state, *iter);
                    }
                }
            }
        }
        int lookahead(const GrammarItem &inherit, const GrammarItem &item) {
            int index;
            auto nSize = item.lookahead_symbols.size();
            for (index = inherit.position + 1; index < inherit.production->symbols.size(); ++index) {
                item.lookahead_symbols.insert(inherit.production->symbols[index]->first.begin(),
                                              inherit.production->symbols[index]->first.end());
                if (!inherit.production->symbols[index]->nullable) {
                    break;
                }
            }
            if (index == inherit.production->symbols.size()) {
                item.lookahead_symbols.insert(inherit.lookahead_symbols.begin(), inherit.lookahead_symbols.end());
            }
            return item.lookahead_symbols.size() - nSize;
        }
        GrammarState *get_item_state(std::set<GrammarItem> &items) {
            for (auto &st : states) {
                if (st->items == items) {
                    return st.get();
                }
            }
            return nullptr;
        }
        GrammarState *get_goto_state(std::set<GrammarItem> &items, Symbol *symbol) {
            std::set<GrammarItem> gotos;
            for (auto &item : items) {
                if (item.dot_reduce()) {
                    continue;
                }
                if (item.dot_next(symbol)) {
                    gotos.insert(GrammarItem(item.production, item.position + 1, item.lookahead_symbols));
                }
            }
            if (gotos.empty()) {
                return nullptr;
            }
            closure(gotos);
            if (auto *find = get_item_state(gotos)) {
                merge(find, gotos);
                return find;
            }
            GrammarState *goto_state = new GrammarState(gotos);
            goto_state->index = states.size();
            states.push_back(std::unique_ptr<GrammarState>(goto_state));
            return goto_state;
        }
        GrammarState *generate_start(Nonterminal *symbol) {
            //start->follow.insert(end);
            GrammarState *state = new GrammarState();
            for (auto &production : symbol->productions) {
                auto[iter, _] = state->items.insert(GrammarItem(production.get(), 0));
                iter->lookahead_symbols.insert(end);
            }
            states.push_back(std::unique_ptr<GrammarState>(state));
            closure(state->items);
            return state;
        }
        void generate_transition(GrammarState *state) {
            for (auto &symbol : symbols) {
                auto *goto_state = get_goto_state(state->items, symbol.get());
                if (goto_state) {
                    state->transitions.insert(GrammarTransition(goto_state, symbol.get()));
                }
            }
        }
        void generate_reduce(GrammarState *state) {
            for (auto &item : state->items) {
                if (item.dot_reduce()) {
                    for (auto &symbol : item.lookahead_symbols) {
                        //check_conflict(state, item, symbol);
                        auto *rightmost = item.production->rightmost_teminal();
                        if (rightmost && rightmost->precedence != 0 && symbol->precedence != 0 &&
                            (symbol->precedence > rightmost->precedence ||
                             (symbol->precedence == rightmost->precedence &&
                              rightmost->associativity == AssoRight))) {

                        } else {
                            auto iter = state->transitions.find(GrammarTransition(nullptr, symbol));
                            if (iter == state->transitions.end()) {
                                state->transitions.insert(GrammarTransition(
                                        nullptr, symbol, TransitionReduce, item.production->lhs,
                                        item.production->action, item.production->symbols.size()));
                            } else {
                                iter->type = TransitionReduce;
                                iter->reduce_symbol = item.production->lhs;
                                iter->reduce_action = item.production->action;
                                iter->reduce_length = item.production->symbols.size();
                            }
                        }
                    }
                }
            }
        }
        int add_first(Symbol *symbol, std::set<Symbol *> &first) {
            auto nSize = symbol->first.size();
            symbol->first.insert(first.begin(), first.end());
            return symbol->first.size() - nSize;
        }
        int add_follow(Symbol *symbol, std::set<Symbol *> &follow) {
            auto nSize = symbol->follow.size();
            symbol->follow.insert(follow.begin(), follow.end());
            return symbol->follow.size() - nSize;
        }
        int visit(Terminal *terminal) override {
            if (!terminal->first.empty()) {
                return 0;
            }
            terminal->first.insert(terminal);
            return 1;
        }
        int visit(Nonterminal *nonterminal) override {
            int delta = 0;
            for (auto &production : nonterminal->productions) {
                if (production->symbols.empty()) {
                    nonterminal->nullable = true;
                }
                for (auto &symbol : production->symbols) {
                    delta += add_first(nonterminal, symbol->first);
                    if (!symbol->nullable) {
                        break;
                    }
                }
                for (auto &symbol : Reverse(production->symbols)) {
                    delta += add_follow(nonterminal, symbol->follow);
                    delta += add_follow(symbol, nonterminal->follow);
                    if (!symbol->nullable) {
                        break;
                    }
                }
                for (int i = 1; i < production->symbols.size(); ++i) {
                    for (int j = i; j < production->symbols.size(); ++j) {
                        delta += add_follow(production->symbols[i - 1], production->symbols[j]->first);
                        if (!production->symbols[i]->nullable) {
                            break;
                        }
                    }
                }
            }
            return delta;
        }
    };

}
#endif //MYLIBS_LALR_H
