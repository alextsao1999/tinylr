//
// Created by Alex
//

#ifndef MYLIBS_LEXER_H
#define MYLIBS_LEXER_H

#include <string_view>
#include "regex.h"
namespace alex {
    class Lexer {
    public:
        using iter_t = const char *;
        using char_t = char;
        using string_t = std::string_view;
        RegexGenerator generator;
        std::vector<std::unique_ptr<RegexState>> state_machine;
        std::vector<std::unique_ptr<RegexState>> whitespace;
        iter_t first;
        iter_t last;
        iter_t current;
        SymbolType token_symbol = SymbolNull;
        int token_line = 0;
        int token_length = 0;
        iter_t token_start = 0;
        iter_t token_line_start = 0;
        Lexer() = default;
        void set_whitespace(string_t pattern) {
            RegexParser parser(pattern.data(), pattern.data() + pattern.size());
            RegexGenerator space_generator;
            space_generator.feed(parser.parse_concat(), (SymbolType) 1);
            whitespace = std::move(space_generator.generate());
        }
        void add_pattern(string_t pattern, SymbolType symbol) {
            RegexParser parser(pattern.data(), pattern.data() + pattern.size());
            generator.feed(parser.parse_concat(), symbol);
        }
        void add_literal(string_t pattern, SymbolType symbol) {
            RegexParser parser(pattern.data(), pattern.data() + pattern.size());
            generator.feed(parser.parse_literal(), symbol);
        }
        void generate_states() {
            state_machine = std::move(generator.generate());
        }
        void reset(string_t str) {
            reset(str.data(), str.data() + str.length());
        }
        void reset(iter_t begin, iter_t end) {
            this->current = this->first = this->token_line_start = begin;
            this->last = end;
        }
        bool skip() {
            if (whitespace.empty()) {
                return false;
            }
            auto *space_start = current;
            auto *state = whitespace[0].get();
            do {
                auto *trans = state->find_trans(*current);
                if (!trans) {
                    break;
                }
                if (*current++ == '\n') {
                    token_line++;
                    token_line_start = current;
                }
                state = trans->state;
            } while (*current != '\0');
            if (state->symbol == SymbolNull) {
                current = space_start;
            }
            return state->symbol != SymbolNull;
        }
        void advance() {
            while (skip());
            token_start = current;
            auto *state = state_machine[0].get();
            auto *last_start = current;
            auto *last_state = state;
            do {
                if (state->symbol != SymbolNull) {
                    last_start = current;
                    last_state = state;
                }
                auto *trans = state->find_trans(*current);
                if (trans == nullptr) {
                    break;
                }
                if (*current++ == '\n') {
                    token_line++;
                    token_line_start = current;
                }
                state = trans->state;
            } while (*current != '\0');
            if (state->symbol == SymbolNull) {
                state = last_state;
                current = last_start;
            }
            token_symbol = state->symbol;
            token_length = current - token_start;
        }
        inline bool good() { return current < last; }
        inline string_t lexeme() { return string_t(&*token_start, token_length); }
        inline SymbolType symbol() { return token_symbol; }
        inline int line() { return token_line; }
        inline int column() { return token_start - token_line_start; }
        void dump() {
            while (good()) {
                advance();
                std::cout << "{" << lexeme() << "} -> " << int(symbol()) << std::endl;
                if (symbol() == SymbolNull) {
                    break;
                }
            }
        }
    };

}
#endif //MYLIBS_LEXER_H
