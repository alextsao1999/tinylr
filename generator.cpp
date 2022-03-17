//
// Created by Alex on 8/24/2020.
//
#include <sstream>
#include <fstream>
#include <algorithm>
#include "lalr.h"

using namespace alex;
enum OptionType {
    TypeJson,
    TypeAst
};
struct Options {
    const char *input;
    const char *output;
    const char *prefix;
    const char *ast_header;
    int type;
};

inline std::ostream &indent(std::ostream &os, int count = 0) {
    os << std::string(' ', count * 4);
    return os;
}
inline std::string string_escape(std::string_view view) {
    std::string result;
    result.reserve(view.length());
    for (auto &chr : view) {
        switch (chr) {
            case '\t':
                result += "\\t";
                break;
            case '\'':
                result += "\\'";
                break;
            case '\"':
                result += "\\\"";
                break;
            case '\n':
                result += "\\n";
                break;
            case '\\':
                result += "\\\\";
                break;
            case '\a':
                result += "\\a";
                break;
            case '\b':
                result += "\\b";
                break;
            case '\f':
                result += "\\f";
                break;
            case '\r':
                result += "\\r";
                break;
            default:
                result += chr;
                break;
        }
    }
    return std::move(result);
}
inline std::string string_upper(const std::string &type) {
    std::string string(type);
    std::transform(type.begin(), type.end(), string.begin(), std::toupper);
    return string;
}

inline std::string get_file_name(const std::string &file) {
    auto pos = file.find_last_of('/');
    if (pos == std::string::npos) {
        return file;
    }
    return file.substr(pos + 1);
}
inline std::string drop_file_ext(const std::string &file) {
    auto pos = file.find_last_of('.');
    if (pos == std::string::npos) {
        return file;
    }
    return file.substr(0, pos);
}

std::string parser_emit_symbols(LALRGenerator &generator) {
    std::stringstream out;
    out << "ParserSymbol ParserSymbols[] = {\n";
    for (auto &symbol : generator.get_symbols()) {
        out << "    {"
            << symbol->is_nonterminal() << ", "
            << symbol->index << ", "
            << "\"" << string_escape(symbol->to_str()) << "\""
            << "},\n";
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_action(LALRGenerator &generator) {
    std::stringstream out;
    out << "ReduceAction ParserActions[] = {\n";
    for (auto &action : generator.get_actions()) {
        for (auto &inst: action->insts) {
            out << "    {"
                << inst.opcode << ", "
                << inst.value_int << ", ";
            out << '\"' << inst.value_str << '\"';
            out << "},\n";
        }
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_lexer_states(LALRGenerator &generator) {
    std::stringstream out;
    Lexer<> lexer;
    //lexer.set_whitespace("[ \t\n\r]+");
    for (auto &symbol : generator.get_symbols()) {
        if (!symbol->is_nonterminal()) {
            std::string_view pattern = symbol->terminal()->pattern;
            bool literal = pattern.front() == '\'';
            pattern.remove_prefix(1);
            pattern.remove_suffix(1);
            if (literal) {
                lexer.add_literal(pattern, symbol->index);
            } else {
                lexer.add_pattern(pattern, symbol->index);
            }
        }
    }
    lexer.generate_states();
    out << "int LexerWhitespaceSymbol = " << (generator.get_whitespace() ? generator.get_whitespace()->index : -1)
        << ";\n";

    out << "LexerTransition LexerTransitions[] = {\n";
    for (auto &state : lexer.state_machine) {
        for (auto &trans : state->transitions) {
            out << "    {"
                << trans.begin << ", "
                << trans.end << ", ";
            if (trans.state) {
                out << "&LexerStates[" << trans.state->index << "]";
            } else {
                out << "nullptr";
            }
            out << "},\n";
        }
    }
    out << "};\n";
    int index = 0;
    out << "LexerState LexerStates[] = {\n";
    for (auto &state : lexer.state_machine) {
        out << "    {"
            << "&LexerTransitions[" << index << "], "
            << state->transitions.size() << ", ";
        out << (int) state->symbol;
        out << "},\n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}
std::string parser_emit_parser_states(LALRGenerator &generator) {
    std::stringstream out;
    out << "ParserTransition ParserTransitions[] = {\n";
    for (auto &state : generator.get_states()) {
        for (auto &trans : state->transitions) {
            out << "    {"
                << trans.type << ", "
                << trans.symbol->index << ", ";
            if (trans.state) {
                out <<  "&ParserStates[" << trans.state->index << "], ";
            } else {
                out << "nullptr, ";
            }
            out << (trans.reduce_symbol ? trans.reduce_symbol->index : 0) << ", "
                << trans.reduce_length << ", "
                << trans.precedence << ", ";
            if (trans.reduce_action) {
                out << "&ParserActions[" << trans.reduce_action->index << "], "
                    << trans.reduce_action->size();
            } else {
                out << "nullptr, 0";
            }
            out << "},\n";
        }
    }
    out << "};\n";
    int index = 0;
    out << "ParserState ParserStates[] = {\n";
    int state_index = 0;
    for (auto &state : generator.get_states()) {
        out << "    {" << state_index++ << ", "
            << "&ParserTransitions[" << index << "], "
            << state->transitions.size() << ", "
            << state->conflict << ", ";
        if (state->error) {
            out << "&ParserTransitions[" << state->error->index << "]";
        } else {
            out << "nullptr";
        }
        out << "},\n";
        index += state->transitions.size();
    }
    out << "};\n";
    return out.str();
}

class Generator {
public:
    Generator(Options &opts) : options(opts) {
        LALRGrammarParser<StreamIter> lalr(grammar);

        // parse the input file
        std::fstream input(opts.input, std::ios::in);
        lalr.parse(input);
        input.close();

        // new generator
        generator = std::make_unique<LALRGenerator>(grammar);
        generator->generate();
    }
    ~Generator() {}

    /// generate all files
    void generate() {
        std::fstream fs;

        // generate parser.cpp
        fs.open(options.output, std::ios::trunc | std::ios::out);
        fs << "//\n"
           << "// Created by Alex's tiny lr.\n"
           << "//\n"
           << "#include \"" << drop_file_ext(get_file_name(options.output)) << ".h" << "\"\n";
        generate_state_machine(fs);
        fs.close();

        // generate parser.h
        std::string header = drop_file_ext(options.output) + ".h";
        fs.open(header, std::ios::trunc | std::ios::out);
        generate_common_header(fs);
        fs.close();

        // generate ast.h
        if (options.type == TypeAst) {
            fs.open(options.ast_header, std::ios::trunc | std::ios::out);
            generate_ast_header(fs);
            fs.close();
        }

    }

    /// generate lexer states and parser states(parser.cpp)
    void generate_state_machine(std::ostream &os) {
        os << parser_emit_symbols(*generator)
           << parser_emit_lexer_states(*generator)
           << parser_emit_action(*generator)
           << parser_emit_parser_states(*generator);
    }

    /// generate common header(parser.h)
    void generate_common_header(std::ostream &os) {
        os << "//\n"
              "// Created by Alex\n"
              "//\n"
              "\n"
              "#ifndef TINYLALR_PARSER_H\n"
              "#define TINYLALR_PARSER_H\n";

        os << "#include <string>\n"
              "#include <iostream>\n"
              "#include <map>\n"
              "#include <queue>\n"
              "#include <cassert>\n"
              "#include <functional>\n\n";

        os << "#define LR_UNREACHED() assert(!\"unreached here\")\n\n";

        os << "#define CONFLICT_NONE 0\n"
              "#define CONFLICT_SHIFT_REDUCE 1\n"
              "#define CONFLICT_REDUCE_REDUCE 2\n"
              "#define SYMBOL_TYPE_TERMINAL 0\n"
              "#define SYMBOL_TYPE_NONTERMINAL 1\n"
              "#define TRANSITION_SHIFT 1\n"
              "#define TRANSITION_REDUCE 2\n\n";

        os << "enum ActionOpcode {\n"
              "    OpcodeCreateObj,\n"
              "    OpcodeCreateArr,\n"
              "    OpcodePushValue, /// $n\n"
              "    OpcodePushInt, /// int\n"
              "    OpcodePushStr, /// str\n"
              "    OpcodePushBool, /// bool\n"
              "    OpcodePushToken, /// token\n"
              "    OpcodePopSet,\n"
              "    OpcodePopInsertArr,\n"
              "    OpcodePopInsertObj,\n"
              "};";

        // emit common structs
        os << R"cpp(
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
};)cpp" << std::endl << std::endl;

        if (options.type == TypeJson) {
            generate_json_parser(os);
        } else {
            generate_ast_parser(os);
        }

        os << "#endif //TINYLALR_PARSER_H";

    }
    void generate_json_parser(std::ostream &os) {
        os << "#include \"json.hpp\"\n"
           << "using value_t = nlohmann::json;\n\n";

        /*if (grammar.types.size() > 0) {
            os << "enum {\n";
            os << "    " << options.prefix << "NONE" << ",\n";
            for (auto &type : grammar.types) {
                os << "    " << options.prefix << string_upper(type) << ",\n";
            }
            os << "};\n";
        }*/

        // HandleReduceAction
        os << R"cpp(
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
)cpp";

        // LRParser
        os << R"cpp(
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
)cpp" << std::endl;

        // GLRParser
        os << R"cpp(template <class iter_t = const char *,
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
)cpp";
    }
    void generate_ast_parser(std::ostream &os) {
        os << "#include \"" << get_file_name(options.ast_header) << "\"\n"
           << "using value_t = ASTNodePtr;\n";

        if (grammar.types.size() > 0) {
            os << "enum {\n";
            os << "    " << options.prefix << "NONE" << ",\n";
            for (auto &type : grammar.types) {
                os << "    " << options.prefix << string_upper(type) << ",\n";
            }
            os << "};\n";
        }

        // GLRParser
        os << R"cpp(
template <class char_t = char, class char_traits = std::char_traits<char_t>>
struct ParserGraphNode {
    struct Link {
        std::shared_ptr<ParserGraphNode> node;
        std::shared_ptr<Link> next;
        Link(const std::shared_ptr<ParserGraphNode> &node) : node(node) {}
        Link(const std::shared_ptr<ParserGraphNode> &node, const std::shared_ptr<Link> &next) : node(node), next(next) {}
        void to_nodes(std::vector<ParserGraphNode *> &nodes) {
            nodes.clear();
            auto start = next;
            while (start) {
                nodes.push_back(start->node.get());
                start = start->next;
            }
        }
    };
    static auto CreateLink(const std::shared_ptr<ParserGraphNode> &node) {
        return std::make_shared<Link>(node);
    }

    static auto CreateLink(const std::shared_ptr<ParserGraphNode> &node, const std::shared_ptr<Link> &next) {
        return std::make_shared<Link>(node, next);
    }

    static auto Create(ParserState *state) {
        return std::make_shared<ParserGraphNode>(state);
    }

    static auto Create(const std::shared_ptr<ParserGraphNode> &prev, ParserState *state) {
        return std::make_shared<ParserGraphNode>(prev, state);
    }

    std::shared_ptr<ParserGraphNode> prev;
    std::vector<std::shared_ptr<ParserGraphNode>> prevs;
    ParserState *state = nullptr;
    int symbol = 0;
    int line = 0;
    int column = 0;
    std::basic_string<char_t, char_traits> lexeme;
    value_t value;
    int depth = 0;
    int merge = 0;
    bool error = false;
    ParserGraphNode(ParserState *state) : state(state) {}
    ParserGraphNode(const std::shared_ptr<ParserGraphNode> &prev, ParserState *state) : prev(prev), state(state) {}
    void add_prev(const std::shared_ptr<ParserGraphNode> &previous) {
        prevs.push_back(previous);
    }
    bool need_lr_reduce(ParserTransition *trans) {
        return state->conflict == CONFLICT_NONE && trans->reduce_length <= depth;
    }
};
template <class iter_t = const char *,
        class char_t = typename std::iterator_traits<iter_t>::value_type,
        class char_traits = std::char_traits<char_t>>
class GLRParser {
    using Lexer = ParserLexer<iter_t>;
    using Node = ParserGraphNode<char_t, char_traits>;
    using NodePtr = std::shared_ptr<Node>;
    using LinkPtr = std::shared_ptr<typename Node::Link>;
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
            auto start = Node::Create(parser_state);
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
                    auto goto_node = Node::Create(node.link->node, goto_trans.state);
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
        auto link = Node::CreateLink(node);
        action_nodes.resize(trans.reduce_length);
        for (int i = trans.reduce_length - 1; i >= 0; --i) {
            node = node->prev;
            link = Node::CreateLink(node, link);
        }
        reduce_list.emplace(link, trans);
    }
    void do_glr_reduce(NodePtr node, ParserTransition &trans) {
        link_list.clear();
        link_list.push_back(Node::CreateLink(node));
        for (int i = trans.reduce_length - 1; i >= 0; --i) {
            for (int j = 0, length = link_list.size(); j < length; ++j) {
                for (auto &prev : link_list[j]->node->prevs) {
                    link_list.push_back(Node::CreateLink(prev, link_list[j]));
                }
                link_list[j] = Node::CreateLink(link_list[j]->node->prev, link_list[j]);
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
            NodePtr shift_node = Node::Create(node, node->state->error->state);
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
        if (action_count == 0)
            return value_t();
        value_t value;
        for (int i = 0; i < action_count; ++i) {
            auto &action = actions[i];
            switch (action.opcode) {
                default: LR_UNREACHED();
                case ACTION_TYPE_CREATE:
                    value = CreateASTById(action.value);
                    break;
                case ACTION_TYPE_INIT:
                    if (*action.desc == '@') {
                        value = std::make_shared<ASTLexeme<char_t, char_traits>>(nodes[action.value]->lexeme);
                    } else {
                        value = nodes[action.value]->value;
                    }
                    break;
                case ACTION_TYPE_INSERT:
                    if (!value)
                        value = std::make_shared<ASTList>();
                    if (*action.desc == '@') {
                        std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTLexeme<char_t, char_traits>>(
                                nodes[action.value]->line,
                                nodes[action.value]->column,
                                nodes[action.value]->lexeme));
                    } else {
                        std::dynamic_pointer_cast<ASTList>(value)->add(nodes[action.value]->value);
                    }
                    break;
                case ACTION_TYPE_SET:
                    switch (*action.desc) {
                        default: LR_UNREACHED();
                        case '$': // key:$n set value
                            std::dynamic_pointer_cast<ASTList>(value)->set(action.index, nodes[action.value]->value);
                            break;
                        case '@': // key:@n set lexeme
                            std::dynamic_pointer_cast<ASTList>(value)->set(action.index,
                                                                           std::make_shared<ASTLexeme<char_t, char_traits>>(
                                                                                   nodes[action.value]->line,
                                                                                   nodes[action.value]->column,
                                                                                   nodes[action.value]->lexeme));
                            break;
                        case '#': // key:#n insert
                        {
                            auto ptr = std::dynamic_pointer_cast<ASTList>(value)->get(action.value);
                            std::dynamic_pointer_cast<ASTList>(ptr)->add(nodes[action.value]->value);
                        }
                            break;
                    }
                    break;
                case ACTION_TYPE_SET_STRING: // key:'string'
                    std::dynamic_pointer_cast<ASTList>(value)->set(action.index, std::make_shared<ASTString>(action.desc));
                    break;
                case ACTION_TYPE_SET_INT:  // key:number
                    std::dynamic_pointer_cast<ASTList>(value)->set(action.index,
                                                                   std::make_shared<ASTInteger>(action.value));
                    break;
                case ACTION_TYPE_SET_BOOL: // key:bool
                    std::dynamic_pointer_cast<ASTList>(value)->set(action.index,
                                                                   std::make_shared<ASTInteger>(action.value));
                    break;
                case ACTION_TYPE_INSERT_STRING:
                    std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTString>(action.desc));
                    break;
                case ACTION_TYPE_INSERT_INT:
                    std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTInteger>(action.value));
                    break;
                case ACTION_TYPE_INSERT_BOOL:
                    std::dynamic_pointer_cast<ASTList>(value)->add(std::make_shared<ASTInteger>(action.value));
                    break;
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
)cpp";

    }

    /// generate ast.h
    void generate_ast_header(std::ostream &os) {
        os << "//\n"
              "// Created by Alex on 9/29/2020.\n"
              "//\n"
              "\n"
              "#ifndef TINYLALR_AST_H\n"
              "#define TINYLALR_AST_H\n"
              "#include <iostream>\n"
              "#include <vector>\n";

        os << "#define ASTLIST(v)";
        for (auto &[type, info] : grammar.type_info) {
            if (type.empty()) {
                continue;
            }
            auto *type_name = type.c_str() + 1;
            os << " \\\n    v(" << type_name << ")";
        }
        os << "\n";
        os << "#define DeclareASTList(Type) class Type;\n"
              "ASTLIST(DeclareASTList);\n"
              "template <class char_t, class char_traits>\n"
              "class ASTLexeme;\n"
              "class ASTVisitor {\n"
              "public:\n"
              "#define VisitItem(Type) virtual void visit(Type *) = 0;\n"
              "    ASTLIST(VisitItem);\n"
              "};\n"
              "class ASTNode {\n"
              "protected:\n"
              "    int line_ = 0;\n"
              "    int column_ = 0;\n"
              "public:\n"
              "    ASTNode(int line, int column) : line_(line), column_(column) {}\n"
              "    ASTNode() {}\n"
              "    virtual ~ASTNode() {}\n"
              "    int line () {\n"
              "        return line_;\n"
              "    }\n"
              "    int column () {\n"
              "        return column_;\n"
              "    }\n"
              "    virtual bool is_leaf() { return false; }\n"
              "    virtual bool is_list() { return false; }\n"
              "    virtual bool is_error() { return false; }\n"
              "    template <class char_t = char, class char_traits = std::char_traits<char_t>>\n"
              "    ASTLexeme<char_t, char_traits> *token() {\n"
              "        return dynamic_cast<ASTLexeme<char_t, char_traits> *>(this);\n"
              "    }\n"
              "    virtual void dump(std::ostream &os) {}\n"
              "    virtual void accept(ASTVisitor *) {}\n"
              "};\n"
              "class ASTLeaf : public ASTNode {\n"
              "public:\n"
              "    ASTLeaf() {}\n"
              "    ASTLeaf(int line, int column) : ASTNode(line, column) {}\n"
              "    bool is_leaf() override { return true; }\n"
              "};\n"
              "class ASTInteger : public ASTLeaf {\n"
              "protected:\n"
              "    int value;\n"
              "public:\n"
              "    ASTInteger(int value) : value(value) {}\n"
              "    void dump(std::ostream &os) override {\n"
              "        os << value;\n"
              "    }\n"
              "};\n"
              "class ASTString : public ASTLeaf {\n"
              "protected:\n"
              "    std::string value;\n"
              "public:\n"
              "    ASTString(const std::string &value) : value(value) {}\n"
              "    void dump(std::ostream &os) override {\n"
              "        os << value;\n"
              "    }\n"
              "};\n"
              "template <class char_t, class char_traits = std::char_traits<char_t>>\n"
              "class ASTLexeme : public ASTLeaf {\n"
              "    std::basic_string<char_t, char_traits> lexeme_;\n"
              "public:\n"
              "    ASTLexeme(const std::basic_string<char_t, char_traits> &lexeme) : lexeme_(lexeme) {}\n"
              "    ASTLexeme(int line, int column, const std::basic_string<char_t, char_traits> &lexeme) : ASTLeaf(line, column),\n"
              "                                                                                            lexeme_(lexeme) {}\n"
              "    std::basic_string<char_t, char_traits> &lexeme() const { return lexeme_; }\n"
              "    void dump(std::ostream &os) override {\n"
              "        os << lexeme_;\n"
              "    }\n"
              "};\n"
              "using ASTNodePtr = std::shared_ptr<ASTNode>;\n"
              "using ASTLeafPtr = std::shared_ptr<ASTLeaf>;\n"
              "class ASTList : public ASTNode {\n"
              "    using iterator = std::vector<ASTNodePtr>::iterator;\n"
              "    int kind;\n"
              "    std::vector<ASTNodePtr> nodes;\n"
              "public:\n"
              "    ASTList() {}\n"
              "    ASTList(int kind, int size) : kind(kind), nodes(size) {}\n"
              "    bool is_list() override { return true; }\n"
              "    inline iterator begin() { return nodes.begin(); }\n"
              "    inline iterator end() { return nodes.end(); }\n"
              "    size_t size() { return nodes.size(); }\n"
              "    ASTNodePtr &get(size_t index) { return nodes[index]; }\n"
              "    void set(size_t index , ASTNodePtr value) {\n"
              "        nodes[index] = value;\n"
              "    }\n"
              "    void add(ASTNodePtr value) {\n"
              "        nodes.push_back(value);\n"
              "    }\n"
              "    template <class char_t = char, class char_traits = std::char_traits<char_t>>\n"
              "    ASTLexeme<char_t, char_traits> *token(size_t index = 0) {\n"
              "        return dynamic_cast<ASTLexeme<char_t, char_traits> *>(get(index).get());\n"
              "    }\n"
              "    void dump(std::ostream &os) override {\n"
              "        os << \"[\";\n"
              "        for (auto &node : *this) {\n"
              "            node->dump(os);\n"
              "            if (nodes.back() != node) {\n"
              "                os << \", \";\n"
              "            }\n"
              "        }\n"
              "        os << \"]\";\n"
              "    }\n"
              "    void accept(ASTVisitor *visitor) override {\n"
              "        for (auto &item : *this) {\n"
              "            if (item) {\n"
              "                item->accept(visitor);\n"
              "            }\n"
              "        }\n"
              "    }\n"
              "};\n"
              "using ASTListPtr = std::shared_ptr<ASTList>;\n"
              "class ASTError : public ASTList {\n"
              "public:\n"
              "    bool is_error() override { return true; }\n"
              "    void dump(std::ostream &os) override {\n"
              "        os << \"error\";\n"
              "        ASTList::dump(os);\n"
              "    }\n"
              "};\n"
              "class ASTMerge : public ASTList {\n"
              "public:\n"
              "    void dump(std::ostream &os) override {\n"
              "        os << \"merge\";\n"
              "        ASTList::dump(os);\n"
              "    }\n"
              "};\n\n";

        for (auto &[type, info] : grammar.type_info) {
            auto *type_name = type.c_str() + 1;
            if (type.empty()) {
                continue;
            }
            os << "class " << type_name << " : public ASTList {\n"
               << "public:\n"
               << "    " << type_name << "() : ASTList(" << info.index << ", " << info.fields.size() << ") {}\n";
            for (auto &[field, index] : info.fields) {
                os << "    ASTNodePtr " << field << "() { return get(" << index << "); }\n";
            }
            os << "    void accept(ASTVisitor *visitor) override { visitor->visit(this); }\n";
            if (info.fields.size() > 0) {
                os << "    void dump(std::ostream &os) override {\n"
                      "        os << \"" << type_name << "\" << \"{\";\n";
                bool first = true;
                for (auto &[field, index] : info.fields) {
                    os << (first ? "        os << \"" : "        os << \", ") << field << ": \";\n";
                    os << "        if (" << field << "()) " << field << "()->dump(os); else os << \"null\";\n";
                    first = false;
                }
                os << "        os << \"}\";\n"
                      "    }\n";
            }
            os << "};\n";
        }

        os << "inline ASTListPtr CreateASTById(int id) {\n"
              "    switch (id) {\n";
        for (auto &[type, info] : grammar.type_info) {
            if (type.empty()) {
                continue;
            }
            auto *type_name = type.c_str() + 1;
            os << "        case " << info.index << ":\n"
               << "            return std::make_shared<" << type_name << ">();\n";
        }
        os << "    }\n"
              "    return ASTListPtr();\n"
              "}\n";
        os << "#endif //TINYLALR_AST_H";
    }

private:
    LALRGrammar grammar; ///> parsed grammar
    std::unique_ptr<LALRGenerator> generator; ///> lalr state machine generator
    Options &options; ///> cmdline options

};

int main(int argc, char **argv) {
    Options opts;
    opts.input = "../test/grammar.new.y";
    opts.output = "../test/parser.cpp";
    opts.ast_header = "../test/ast.h";
    opts.prefix = "TYPE_";
    opts.type = TypeJson;
    for (int index = 1; index < argc; index++) {
        if (strcmp(argv[index], "-o") == 0 || strcmp(argv[index], "--output") == 0) {
            opts.output = argv[index + 1];
            index += 2;
        } else if (strcmp(argv[index], "-h") == 0 || strcmp(argv[index], "--help") == 0) {
            std::cout << "Usage: " << argv[0] << " [options]\n"
                      << "Options:\n"
                      << "  -o, --output <file>\n"
                      << "  -h, --help\n"
                      << "  -a, --ast\n"
                      << "  -p, --prefix <prefix>\n";
            return 0;
        } else if (strcmp(argv[index], "-p") == 0 || strcmp(argv[index], "--prefix") == 0) {
            opts.prefix = argv[index + 1];
            index += 2;
        } else if (strcmp(argv[index], "-a") == 0 || strcmp(argv[index], "--ast") == 0) {
            opts.type = TypeAst;
            index += 1;
        } else {
            opts.input = argv[index];
        }
    }
    Generator generator(opts);
    generator.generate();
    return 0;
}
