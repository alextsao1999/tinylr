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
    using string_t = std::string;
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
        string_t base;
        std::map<string_t, string_t> fields;
        Action() = default;
        inline size_t size() {
            return fields.size() + (base.empty() ? 0 : 1);
        }
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
        string_t identifier;
        std::vector<std::unique_ptr<Production>> productions;
        Nonterminal() = default;
        Nonterminal(const string_t &identifier) : identifier(identifier) {}
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
        string_t pattern;
        Terminal(const string_t &pattern) : pattern(pattern) {}
        virtual ~Terminal() = default;
        bool is_nonterminal() const override { return false; }
        int accept(SymbolVisitor *visitor) override { return visitor->visit(this); }
        void dump() const override {
            std::cout << "(" << pattern << ")";
        }
        Terminal *terminal() override { return this; }
        std::string to_str() override {
            std::string_view result = pattern;
            result.remove_prefix(1);
            result.remove_suffix(1);
            return std::string(result);
        }
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
        TransitionNone,
        TransitionShift,
        TransitionReduce
    };
    struct GrammarTransition {
        GrammarState *state;
        Symbol *symbol;
        mutable int index = -1;
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
        bool visited = false;
        std::set<GrammarItem> items; // Item set
        std::set<GrammarTransition> transitions;
        GrammarState() = default;
        GrammarState(const std::set<GrammarItem> &items) : items(std::move(items)) {}
        const GrammarTransition *find_trans(Symbol *symbol) {
            auto iter = transitions.find(GrammarTransition(nullptr, symbol));
            return &(*iter);
        }
    };
    struct LALRGrammar {
        std::vector<std::unique_ptr<Symbol>> symbols;
        std::vector<std::unique_ptr<Action>> actions;
        Nonterminal *start = nullptr;
        Nonterminal *end = nullptr;
        Nonterminal *error = nullptr;
        Symbol *whitespace = nullptr;
    };
    template <class iter_t = StringIter<>>
    class LALRGrammarParser {
    public:
        Lexer<iter_t> lexer;
        LALRGrammar grammar;
        Nonterminal *active = nullptr;
        std::map<string_t, Nonterminal *> nonterminal_table;
        std::map<string_t, Terminal *> terminal_table;
        int precedence = 0;
        enum TokenType {
            Token_Null,
            Token_String,
            Token_Identifier,
            Token_Pipe,
            Token_Arrow,
            Token_None,
            Token_Left,
            Token_Right,
            Token_LeftBrace,
            Token_RightBrace,
            Token_Comma,
            Token_Colon,
            Token_Number,
            Token_Desc,
            Token_Start,
            Token_WhiteSpace,
            Token_Semicolon,
        };
        LALRGrammarParser(iter_t first, iter_t last = iter_t()) {
            lexer.set_whitespace("([ \r\t\n]+)|(/\\*.*\\*/)|(//.*\n)");
            lexer.add_pattern("\"(\\\\.|.)*\"|'(\\\\.|.)*'", Token_String);
            lexer.add_pattern("[a-zA-Z_][a-zA-Z0-9_]*", Token_Identifier);
            lexer.add_pattern("\\|", Token_Pipe);
            lexer.add_pattern("\\->", Token_Arrow);
            lexer.add_pattern("%none", Token_None);
            lexer.add_pattern("%left", Token_Left);
            lexer.add_pattern("%right", Token_Right);
            lexer.add_pattern("%start", Token_Start);
            lexer.add_pattern("%whitespace", Token_WhiteSpace);
            lexer.add_pattern("\\{", Token_LeftBrace);
            lexer.add_pattern("\\}", Token_RightBrace);
            lexer.add_pattern(",", Token_Comma);
            lexer.add_pattern("[0-9]+(\\.[0-9]+)?", Token_Number);
            lexer.add_pattern("($|@)+[0-9]+", Token_Desc);
            lexer.add_pattern(":", Token_Colon);
            lexer.add_pattern(";", Token_Semicolon);
            lexer.generate_states();

            grammar.start = new Nonterminal("@start");
            grammar.start->index = 0;
            grammar.symbols.push_back(std::unique_ptr<Symbol>(grammar.start));

            grammar.end = new Nonterminal("@end");
            grammar.end->index = 0;
            grammar.symbols.push_back(std::unique_ptr<Symbol>(grammar.end));

            grammar.error = get_nonterminal("error");

            lexer.reset(first, last);
            parse_rules();
        }
        bool match(TokenType token) {
            if (lexer.symbol() == token) {
                lexer.advance();
                return true;
            }
            return false;
        }
        void expect(const char *str) {
            std::cout << "Unexpected Token '" << str
                      << "' line:" << lexer.line() + 1
                      << " column:" << lexer.column() + 1 << std::endl;
            std::cout << ">>> " << lexer.lexeme() << std::endl;
            std::cout << "    " << "^^^" << std::endl;
        }
        Nonterminal *get_nonterminal(string_t name) {
            if (nonterminal_table.count(name)) {
                return nonterminal_table[name];
            }
            auto *nt = new Nonterminal(name);
            nt->index = grammar.symbols.size();
            nt->line = lexer.line();
            nt->column = lexer.column();
            grammar.symbols.push_back(std::unique_ptr<Symbol>(nt));
            nonterminal_table[name] = nt;
            //start->push_start(nt);
            return nt;
        }
        Terminal *get_terminal(string_t name) {
            if (terminal_table.count(name)) {
                return terminal_table[name];
            } else {
                auto *nt = new Terminal(name);
                nt->index = grammar.symbols.size();
                nt->line = lexer.line();
                nt->column = lexer.column();
                grammar.symbols.push_back(std::unique_ptr<Symbol>(nt));
                terminal_table[name] = nt;
                return nt;
            }
        }
        void parse_rules() {
            while (parse_expression()) {
            }
        }
        bool parse_expression() {
            lexer.advance();
            if (lexer.symbol() == Token_Null) {
                return false;
            } else if (lexer.symbol() == Token_Semicolon) {
                return true;
            } else if (lexer.symbol() == Token_Identifier) {
                active = get_nonterminal(lexer.lexeme());
                lexer.advance();
                if (!match(Token_Arrow)) {
                    expect("->");
                    return false;
                }
                active->push_production(new Production(active));
                while (parse_production());
            } else if (lexer.symbol() >= Token_None && lexer.symbol() <= Token_Right) {
                Associativity asso;
                if (lexer.symbol() == Token_None) {
                    asso = AssoNone;
                }
                if (lexer.symbol() == Token_Left) {
                    asso = AssoLeft;
                }
                if (lexer.symbol() == Token_Right) {
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
            } else if (lexer.symbol() == Token_Start) {
                lexer.advance();
                do {
                    Symbol *symbol = parse_symbol();
                    if (!symbol) {
                        break;
                    }
                    grammar.start->push_start(symbol);
                } while (true);
            } else if (lexer.symbol() == Token_WhiteSpace) {
                lexer.advance();
                Symbol *symbol = parse_symbol();
                if (!symbol) {
                    expect("symbol");
                    return false;
                }
                if (symbol->is_nonterminal()) {
                    expect("terminal");
                    return false;
                }
                grammar.whitespace = symbol;
            } else {
                expect("identifier, associativity or start");
                return false;
            }
            return true;
        }
        bool parse_production() {
            do {
                if (lexer.symbol() == Token_Pipe) {
                    active->push_production(new Production(active));
                    lexer.advance();
                    return true;
                } else if (lexer.symbol() == Token_LeftBrace ||
                           lexer.symbol() == Token_Desc) {
                    active->productions.back()->action = parse_action();
                    continue;
                } else if (lexer.symbol() == Token_Semicolon) {
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
        Action *parse_action() {
            Action *action = new Action();
            grammar.actions.emplace_back(action);
            if (lexer.symbol() == Token_Desc) {
                action->base = lexer.lexeme();
                lexer.advance();
                if (lexer.symbol() != Token_LeftBrace) {
                    return action;
                }
            }
            do {
                lexer.advance();
                auto field = lexer.lexeme();
                lexer.advance();
                if (lexer.symbol() != Token_Colon) {
                    expect(":");
                }
                lexer.advance();
                action->fields[field] = lexer.lexeme();
                lexer.advance();
            } while (lexer.symbol() == Token_Comma);
            if (!match(Token_RightBrace)) {
                expect("}");
            }
            return action;
        }
        Symbol *parse_symbol() {
            Symbol *symbol = nullptr;
            if (lexer.symbol() == Token_Identifier) {
                symbol = (get_nonterminal(lexer.lexeme()));
                lexer.advance();
            } else if (lexer.symbol() == Token_String) {
                symbol = (get_terminal(lexer.lexeme()));
                lexer.advance();
            }
            return symbol;
        }
    };
    class LALRGenerator : SymbolVisitor {
        LALRGrammar &grammar;
        std::vector<std::unique_ptr<GrammarState>> states;
    public:
        LALRGenerator(LALRGrammar &grammar) : grammar(grammar) {
            grammar.start->follow.insert(grammar.end);
            int delta;
            do {
                delta = 0;
                for (auto &symbol : grammar.symbols) {
                    delta += symbol->accept(this);
                }
            } while (delta > 0);
            generate_start(grammar.start);
        }
        void generate() {
            check_undefine_symbol();
            int visit_count = 0;
            while (visit_count < states.size()) {
                states[visit_count]->index = visit_count;
                if (!states[visit_count]->visited) {
                    generate_transition(states[visit_count].get());
                }
                visit_count++;
            }
            for (auto &state : states) {
                generate_reduce(state.get());
            }
            generate_index();
        }
        inline std::vector<std::unique_ptr<Symbol>> &get_symbols() { return grammar.symbols; }
        inline std::vector<std::unique_ptr<Action>> &get_actions() { return grammar.actions; }
        inline std::vector<std::unique_ptr<GrammarState>> &get_states() { return states; }
        inline Nonterminal *get_start() { return grammar.start; }
        inline Nonterminal *get_end() { return grammar.end; }
        inline Nonterminal *get_error() { return grammar.error; }
        inline Symbol *get_whitespace() { return grammar.whitespace; }
    private:
        void check_undefine_symbol() {
            for (auto &symbol : grammar.symbols) {
                if (symbol->is_nonterminal()) {
                    auto *nonterminal = symbol->nonterminal();
                    if (nonterminal == grammar.end || nonterminal == grammar.error) {
                        continue;
                    }
                    if (nonterminal->productions.size() == 0) {
                        std::cout << "Undefine Symbol: " << nonterminal->identifier << std::endl;
                    }
                }
            }
        }
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
            if (!state->visited) {
                generate_transition(state);
            }
            for (auto &trans : state->transitions) {
                if (!item.dot_reduce()) {
                    auto iter = trans.state->items.find(GrammarItem(item.production, item.position + 1));
                    if (iter != trans.state->items.end()) {
                        if (iter->add_lookahead(item.lookahead_symbols)) {
                            closure(trans.state->items);
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
                        closure(state->items);
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
            states.emplace_back(goto_state);
            return goto_state;
        }
        GrammarState *generate_start(Nonterminal *symbol) {
            //start->follow.insert(end);
            GrammarState *state = new GrammarState();
            for (auto &production : symbol->productions) {
                auto[iter, _] = state->items.insert(GrammarItem(production.get(), 0));
                iter->lookahead_symbols.insert(grammar.end);
            }
            states.push_back(std::unique_ptr<GrammarState>(state));
            closure(state->items);
            return state;
        }
        void generate_transition(GrammarState *state) {
            state->visited = true;
            for (auto &symbol : grammar.symbols) {
                auto *goto_state = get_goto_state(state->items, symbol.get());
                if (goto_state) {
                    state->transitions.insert(GrammarTransition(goto_state, symbol.get()));
                }
            }
        }
        void generate_reduce(GrammarState *state) {
            for (auto &item : state->items) {
                if (item.dot_reduce()) {
                    auto *rightmost = item.production->rightmost_teminal();
                    for (auto &symbol : item.lookahead_symbols) {
                        //check_conflict(state, item, symbol);
                        if (rightmost && rightmost->precedence > 0 &&
                            (rightmost->precedence < symbol->precedence ||
                            (rightmost->precedence == symbol->precedence &&
                             rightmost->associativity == AssoRight))) {
                            // Shift
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
        void generate_index() {
            int index = 0;
            for (auto &action : grammar.actions) {
                action->index = index;
                index += action->size();
            }
            index = 0;
            for (auto &state : states) {
                for (auto &trans : state->transitions) {
                    trans.index = index++;
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
