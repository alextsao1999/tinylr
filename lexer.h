//
// Created by Alex
//

#ifndef MYLIBS_LEXER_H
#define MYLIBS_LEXER_H

#include <string_view>
#include "regex.h"
namespace alex {
    class StreamIter {
        std::istream *stream = nullptr;
        int chr = 0;
    public:
        using iterator_category = typename std::input_iterator_tag;
        using value_type = char;
        using difference_type = long;
        using pointer = const char *;
        using reference = char &;
        constexpr StreamIter() = default;
        StreamIter(std::istream &os) : stream(&os) {
            ++*this;
        }
        inline bool empty() const { return stream == nullptr; }
        inline bool eof() const { return  !stream || (stream && stream->eof()); }
        inline const int operator*() const { return chr; }
        inline StreamIter&operator++() {
            chr = (!empty()) ? stream->get() : 0;
            return *this;
        }
        inline bool operator==(const StreamIter &rhs) {
            return rhs.empty() && eof();
        }
        inline bool operator!=(const StreamIter &rhs) {
            return !(*this == rhs);
        }
    };
    template<class char_t = char>
    class StringIter {
        const char_t *current = nullptr;
    public:
        using iterator_category = typename std::input_iterator_tag;
        using value_type = char_t;
        using difference_type = long;
        using pointer = const char_t *;
        using reference = char_t &;
        constexpr StringIter() = default;
        constexpr StringIter(const char_t *current) : current(current) {}
        inline bool empty() const { return current == nullptr; }
        inline const char operator*() const { return current ? *current : 0; }
        inline StringIter&operator++() {
            current++;
            return *this;
        }
        inline bool operator==(const StringIter &rhs) {
            return rhs.empty() && **this == '\0';
        }
        inline bool operator!=(const StringIter &rhs) {
            return !(*this == rhs);
        }
    };
    template <class iter_t = StringIter<>>
    class Lexer {
    public:
        using char_t = typename std::iterator_traits<iter_t>::value_type;
        using string_t = std::basic_string<char_t>;
        using symbol_t = int;
        RegexGenerator generator;
        std::vector<std::unique_ptr<RegexState>> state_machine;
        std::vector<std::unique_ptr<RegexState>> whitespace;
        iter_t current;
        iter_t last;
        string_t lexeme_;
        int line_ = 0;
        int position_ = 0;
        int token_start = 0;
        int token_line_start = 0;
        symbol_t token_symbol;
    public:
        Lexer() = default;
        void set_whitespace(std::string_view pattern) {
            RegexParser parser(pattern.data(), pattern.data() + pattern.size());
            RegexGenerator space_generator;
            space_generator.feed(parser.parse_concat(), (SymbolType) 1);
            whitespace = std::move(space_generator.generate());
        }
        void add_pattern(std::string_view pattern, symbol_t symbol) {
            RegexParser parser(pattern.data(), pattern.data() + pattern.size());
            generator.feed(parser.parse_concat(), (SymbolType) symbol);
        }
        void add_literal(std::string_view pattern, symbol_t symbol) {
            RegexParser parser(pattern.data(), pattern.data() + pattern.size());
            generator.feed(parser.parse_literal(), (SymbolType) symbol);
        }
        void generate_states() {
            state_machine = std::move(generator.generate());
        }
        void reset(iter_t begin, iter_t end) {
            this->current = begin;
            this->last = end;
        }
        void advance() {
            while (advance_symbol(whitespace[0].get()) != SymbolNull);
            token_start = position_;
            SymbolType symbol = advance_symbol(state_machine[0].get());
            if (symbol == SymbolNull && current != last) {
                std::cout << "Unexpect char: " << *current << " line:" << line() << std::endl;
            }
            token_symbol = (symbol_t) symbol;
        }
        inline string_t &lexeme() { return lexeme_; }
        inline symbol_t symbol() { return token_symbol; }
        inline int line() { return line_; }
        inline int column() { return token_start - token_line_start; }
        void dump() {
            do {
                advance();
                std::cout << "{" << lexeme() << "} -> " << int(symbol()) << std::endl;
            } while (symbol() != 0);
            exit(0);
        }
    private:
        SymbolType advance_symbol(RegexState *begin) {
            lexeme_.clear();
            RegexState *state = begin;
            do {
                if (current == last) {
                    return state->symbol;
                }
                auto *trans = state->find_trans(*current);
                if (trans) {
                    lexeme_ += *current;
                    state = trans->state;
                    ++position_;
                    if (*current == char_t('\n')) {
                        ++line_;
                        token_line_start = position_;
                    }
                    ++current;
                } else {
                    break;
                }
            } while (true);
            return state->symbol;
        }
    };
}

#endif //MYLIBS_LEXER_H
