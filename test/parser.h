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

enum {
    TYPE_NONE = -1,
    TYPE_ACCESSEXPR = 33,
    TYPE_ARROWEXPR = 28,
    TYPE_ASSIGNEXPR = 27,
    TYPE_BIN = 41,
    TYPE_BINARYEXPR = 25,
    TYPE_BLOCKSTMT = 14,
    TYPE_BREAKSTMT = 17,
    TYPE_CASESTMT = 22,
    TYPE_CASTMETHODDECLARE = 7,
    TYPE_CHAR = 42,
    TYPE_CLASSDECLARE = 4,
    TYPE_CONTINUESTMT = 16,
    TYPE_DOWHILESTMT = 20,
    TYPE_DOTEXPR = 32,
    TYPE_ERROR = 3,
    TYPE_EXPRSTMT = 13,
    TYPE_FIELDDECLARE = 5,
    TYPE_FLOAT = 37,
    TYPE_FUNCTIONDECLARE = 11,
    TYPE_HEX = 40,
    TYPE_IFSTMT = 18,
    TYPE_IMPORT = 2,
    TYPE_INVOKEEXPR = 34,
    TYPE_LONG = 39,
    TYPE_METHODDECLARE = 6,
    TYPE_NEWEXPR = 30,
    TYPE_NUMBER = 36,
    TYPE_OVERLOADOPERATOR = 10,
    TYPE_PARAM = 12,
    TYPE_PROGRAM = 1,
    TYPE_READPROPERTYDECLARE = 8,
    TYPE_RETURNSTMT = 15,
    TYPE_STRING = 35,
    TYPE_SWITCHSTMT = 21,
    TYPE_TERNARYEXPR = 26,
    TYPE_TYPECAST = 29,
    TYPE_TYPESPECIFIER = 24,
    TYPE_UNSIGNED = 38,
    TYPE_VARIABLEDECLARE = 23,
    TYPE_VARIABLEEXPR = 31,
    TYPE_WHILESTMT = 19,
    TYPE_WRITEPROPERTYDECLARE = 9,
};
class JsonASTBase {
protected:
    value_t &value_;
public:
    JsonASTBase(value_t &value) : value_(value) {}
    int getID() { return value_["id"].get<int>(); }
    std::string getKind() { return value_["name"].get<std::string>(); }
    operator value_t &() { return value_; }
};
class AccessExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getField() { return value_["field"]; }
    value_t &getLhs() { return value_["lhs"]; }
};
class ArrowExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBlock() { return value_["block"]; }
    value_t &getParams() { return value_["params"]; }
};
class AssignExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getLeft() { return value_["left"]; }
    value_t &getRight() { return value_["right"]; }
};
class Bin : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class BinaryExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getLeft() { return value_["left"]; }
    value_t &getOp() { return value_["op"]; }
    value_t &getRight() { return value_["right"]; }
};
class BlockStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class BreakStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
};
class CaseStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBody() { return value_["body"]; }
    value_t &getBranches() { return value_["branches"]; }
    value_t &getCondition() { return value_["condition"]; }
};
class CastMethodDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBlock() { return value_["block"]; }
    value_t &getType() { return value_["type"]; }
};
class Char : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class ClassDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBody() { return value_["body"]; }
    std::string getName() { return value_["name"].get<std::string>(); }
    value_t &getSuper() { return value_["super"]; }
    value_t &getTemplate() { return value_["template"]; }
};
class ContinueStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
};
class DoWhileStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBody() { return value_["body"]; }
    value_t &getCondition() { return value_["condition"]; }
};
class DotExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getField() { return value_["field"]; }
    value_t &getLhs() { return value_["lhs"]; }
};
class Error : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    std::string getContent() { return value_["content"].get<std::string>(); }
    value_t &getValue() { return value_["value"]; }
};
class ExprStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class FieldDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
};
class Float : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class FunctionDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBlock() { return value_["block"]; }
    bool getInline() { return value_["inline"].get<bool>(); }
    value_t &getName() { return value_["name"]; }
    value_t &getParams() { return value_["params"]; }
    bool getPublic() { return value_["public"].get<bool>(); }
    bool getStatic() { return value_["static"].get<bool>(); }
    value_t &getType() { return value_["type"]; }
};
class Hex : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class IfStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getCondition() { return value_["condition"]; }
    value_t &getElse() { return value_["else"]; }
    value_t &getThen() { return value_["then"]; }
};
class Import : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getItems() { return value_["items"]; }
};
class InvokeExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getArgs() { return value_["args"]; }
    value_t &getName() { return value_["name"]; }
};
class Long : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class MethodDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
};
class NewExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getArgs() { return value_["args"]; }
    value_t &getType() { return value_["type"]; }
};
class Number : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class OverloadOperator : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBlock() { return value_["block"]; }
    value_t &getOp() { return value_["op"]; }
    value_t &getParams() { return value_["params"]; }
    value_t &getType() { return value_["type"]; }
};
class Param : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getName() { return value_["name"]; }
    value_t &getType() { return value_["type"]; }
};
class Program : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class ReadPropertyDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBlock() { return value_["block"]; }
    value_t &getName() { return value_["name"]; }
    bool getPublic() { return value_["public"].get<bool>(); }
    value_t &getType() { return value_["type"]; }
};
class ReturnStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class String : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getValue() { return value_["value"]; }
};
class SwitchStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getCases() { return value_["cases"]; }
    value_t &getCondition() { return value_["condition"]; }
    value_t &getDefault() { return value_["default"]; }
};
class TernaryExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getCondition() { return value_["condition"]; }
    value_t &getLeft() { return value_["left"]; }
    value_t &getRight() { return value_["right"]; }
};
class TypeCast : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getCast_to() { return value_["cast_to"]; }
    value_t &getValue() { return value_["value"]; }
};
class TypeSpecifier : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getArgs() { return value_["args"]; }
    value_t &getType() { return value_["type"]; }
};
class Unsigned : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    bool getUnsigned() { return value_["unsigned"].get<bool>(); }
    value_t &getValue() { return value_["value"]; }
};
class VariableDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getInit() { return value_["init"]; }
    value_t &getName() { return value_["name"]; }
    bool getPublic() { return value_["public"].get<bool>(); }
    bool getStatic() { return value_["static"].get<bool>(); }
    value_t &getType() { return value_["type"]; }
};
class VariableExpr : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getName() { return value_["name"]; }
};
class WhileStmt : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBody() { return value_["body"]; }
    value_t &getCondition() { return value_["condition"]; }
};
class WritePropertyDeclare : public JsonASTBase {
public:
    using JsonASTBase::JsonASTBase;
    value_t &getBlock() { return value_["block"]; }
    value_t &getName() { return value_["name"]; }
    value_t &getParams() { return value_["params"]; }
    bool getPublic() { return value_["public"].get<bool>(); }
    value_t &getType() { return value_["type"]; }
};
template<typename SubTy, typename RetTy = void>
struct Visitor {
    RetTy visit(value_t &value) {
        if (value.is_null()) {
            return RetTy();
        }
        if (value.is_array()) {
            for (auto &val : value) {
                visit(val);
            }
            return RetTy();
        }
        switch (value["id"].get<int>()) {
            case TYPE_ACCESSEXPR:
                return static_cast<SubTy *>(this)->visitAccessExpr(value);
            case TYPE_ARROWEXPR:
                return static_cast<SubTy *>(this)->visitArrowExpr(value);
            case TYPE_ASSIGNEXPR:
                return static_cast<SubTy *>(this)->visitAssignExpr(value);
            case TYPE_BIN:
                return static_cast<SubTy *>(this)->visitBin(value);
            case TYPE_BINARYEXPR:
                return static_cast<SubTy *>(this)->visitBinaryExpr(value);
            case TYPE_BLOCKSTMT:
                return static_cast<SubTy *>(this)->visitBlockStmt(value);
            case TYPE_BREAKSTMT:
                return static_cast<SubTy *>(this)->visitBreakStmt(value);
            case TYPE_CASESTMT:
                return static_cast<SubTy *>(this)->visitCaseStmt(value);
            case TYPE_CASTMETHODDECLARE:
                return static_cast<SubTy *>(this)->visitCastMethodDeclare(value);
            case TYPE_CHAR:
                return static_cast<SubTy *>(this)->visitChar(value);
            case TYPE_CLASSDECLARE:
                return static_cast<SubTy *>(this)->visitClassDeclare(value);
            case TYPE_CONTINUESTMT:
                return static_cast<SubTy *>(this)->visitContinueStmt(value);
            case TYPE_DOWHILESTMT:
                return static_cast<SubTy *>(this)->visitDoWhileStmt(value);
            case TYPE_DOTEXPR:
                return static_cast<SubTy *>(this)->visitDotExpr(value);
            case TYPE_ERROR:
                return static_cast<SubTy *>(this)->visitError(value);
            case TYPE_EXPRSTMT:
                return static_cast<SubTy *>(this)->visitExprStmt(value);
            case TYPE_FIELDDECLARE:
                return static_cast<SubTy *>(this)->visitFieldDeclare(value);
            case TYPE_FLOAT:
                return static_cast<SubTy *>(this)->visitFloat(value);
            case TYPE_FUNCTIONDECLARE:
                return static_cast<SubTy *>(this)->visitFunctionDeclare(value);
            case TYPE_HEX:
                return static_cast<SubTy *>(this)->visitHex(value);
            case TYPE_IFSTMT:
                return static_cast<SubTy *>(this)->visitIfStmt(value);
            case TYPE_IMPORT:
                return static_cast<SubTy *>(this)->visitImport(value);
            case TYPE_INVOKEEXPR:
                return static_cast<SubTy *>(this)->visitInvokeExpr(value);
            case TYPE_LONG:
                return static_cast<SubTy *>(this)->visitLong(value);
            case TYPE_METHODDECLARE:
                return static_cast<SubTy *>(this)->visitMethodDeclare(value);
            case TYPE_NEWEXPR:
                return static_cast<SubTy *>(this)->visitNewExpr(value);
            case TYPE_NUMBER:
                return static_cast<SubTy *>(this)->visitNumber(value);
            case TYPE_OVERLOADOPERATOR:
                return static_cast<SubTy *>(this)->visitOverloadOperator(value);
            case TYPE_PARAM:
                return static_cast<SubTy *>(this)->visitParam(value);
            case TYPE_PROGRAM:
                return static_cast<SubTy *>(this)->visitProgram(value);
            case TYPE_READPROPERTYDECLARE:
                return static_cast<SubTy *>(this)->visitReadPropertyDeclare(value);
            case TYPE_RETURNSTMT:
                return static_cast<SubTy *>(this)->visitReturnStmt(value);
            case TYPE_STRING:
                return static_cast<SubTy *>(this)->visitString(value);
            case TYPE_SWITCHSTMT:
                return static_cast<SubTy *>(this)->visitSwitchStmt(value);
            case TYPE_TERNARYEXPR:
                return static_cast<SubTy *>(this)->visitTernaryExpr(value);
            case TYPE_TYPECAST:
                return static_cast<SubTy *>(this)->visitTypeCast(value);
            case TYPE_TYPESPECIFIER:
                return static_cast<SubTy *>(this)->visitTypeSpecifier(value);
            case TYPE_UNSIGNED:
                return static_cast<SubTy *>(this)->visitUnsigned(value);
            case TYPE_VARIABLEDECLARE:
                return static_cast<SubTy *>(this)->visitVariableDeclare(value);
            case TYPE_VARIABLEEXPR:
                return static_cast<SubTy *>(this)->visitVariableExpr(value);
            case TYPE_WHILESTMT:
                return static_cast<SubTy *>(this)->visitWhileStmt(value);
            case TYPE_WRITEPROPERTYDECLARE:
                return static_cast<SubTy *>(this)->visitWritePropertyDeclare(value);
            default:
                LR_UNREACHED();
        }
    }
    RetTy visitAccessExpr(AccessExpr value) {
        return RetTy();
    }
    RetTy visitArrowExpr(ArrowExpr value) {
        return RetTy();
    }
    RetTy visitAssignExpr(AssignExpr value) {
        return RetTy();
    }
    RetTy visitBin(Bin value) {
        return RetTy();
    }
    RetTy visitBinaryExpr(BinaryExpr value) {
        return RetTy();
    }
    RetTy visitBlockStmt(BlockStmt value) {
        return RetTy();
    }
    RetTy visitBreakStmt(BreakStmt value) {
        return RetTy();
    }
    RetTy visitCaseStmt(CaseStmt value) {
        return RetTy();
    }
    RetTy visitCastMethodDeclare(CastMethodDeclare value) {
        return RetTy();
    }
    RetTy visitChar(Char value) {
        return RetTy();
    }
    RetTy visitClassDeclare(ClassDeclare value) {
        return RetTy();
    }
    RetTy visitContinueStmt(ContinueStmt value) {
        return RetTy();
    }
    RetTy visitDoWhileStmt(DoWhileStmt value) {
        return RetTy();
    }
    RetTy visitDotExpr(DotExpr value) {
        return RetTy();
    }
    RetTy visitError(Error value) {
        return RetTy();
    }
    RetTy visitExprStmt(ExprStmt value) {
        return RetTy();
    }
    RetTy visitFieldDeclare(FieldDeclare value) {
        return RetTy();
    }
    RetTy visitFloat(Float value) {
        return RetTy();
    }
    RetTy visitFunctionDeclare(FunctionDeclare value) {
        return RetTy();
    }
    RetTy visitHex(Hex value) {
        return RetTy();
    }
    RetTy visitIfStmt(IfStmt value) {
        return RetTy();
    }
    RetTy visitImport(Import value) {
        return RetTy();
    }
    RetTy visitInvokeExpr(InvokeExpr value) {
        return RetTy();
    }
    RetTy visitLong(Long value) {
        return RetTy();
    }
    RetTy visitMethodDeclare(MethodDeclare value) {
        return RetTy();
    }
    RetTy visitNewExpr(NewExpr value) {
        return RetTy();
    }
    RetTy visitNumber(Number value) {
        return RetTy();
    }
    RetTy visitOverloadOperator(OverloadOperator value) {
        return RetTy();
    }
    RetTy visitParam(Param value) {
        return RetTy();
    }
    RetTy visitProgram(Program value) {
        return RetTy();
    }
    RetTy visitReadPropertyDeclare(ReadPropertyDeclare value) {
        return RetTy();
    }
    RetTy visitReturnStmt(ReturnStmt value) {
        return RetTy();
    }
    RetTy visitString(String value) {
        return RetTy();
    }
    RetTy visitSwitchStmt(SwitchStmt value) {
        return RetTy();
    }
    RetTy visitTernaryExpr(TernaryExpr value) {
        return RetTy();
    }
    RetTy visitTypeCast(TypeCast value) {
        return RetTy();
    }
    RetTy visitTypeSpecifier(TypeSpecifier value) {
        return RetTy();
    }
    RetTy visitUnsigned(Unsigned value) {
        return RetTy();
    }
    RetTy visitVariableDeclare(VariableDeclare value) {
        return RetTy();
    }
    RetTy visitVariableExpr(VariableExpr value) {
        return RetTy();
    }
    RetTy visitWhileStmt(WhileStmt value) {
        return RetTy();
    }
    RetTy visitWritePropertyDeclare(WritePropertyDeclare value) {
        return RetTy();
    }
};

template<bool Move = true, typename NodeGetter>
inline void HandleReduceAction(ReduceAction &action, std::vector<value_t> &arr, NodeGetter nodes) {
    switch (action.opcode) {
        default:
            LR_UNREACHED();
        case OpcodeCreateObj:
            arr.push_back(value_t::object_t(
                    {{"kind", value_t::string_t(action.value)},
                     {"id", value_t::number_integer_t(action.index)}}));
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
        std::vector<NodePtr> paths;
        ParserTransition *trans;
        NodePtr prev;
        ReduceNode(const std::vector<NodePtr> &paths, ParserTransition *trans, const NodePtr &prev) : paths(paths.rbegin(), paths.rend()),
                                                                                trans(trans), prev(prev) {}
        inline bool operator<(const ReduceNode &rhs) const {
            return trans->precedence < rhs.trans->precedence;
        }
        inline NodePtr get_last() const {
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
            loc = std::accumulate(node.paths.begin(), node.paths.end(), Location(), [](Location &loc, NodePtr &node) {
                return loc.merge(node->location);
            });
            if (position && value.is_object()) {
                value["position"] = {{"lineStart",   loc.line_start},
                                     {"columnStart", loc.column_start},
                                     {"lineEnd",     loc.line_end},
                                     {"columnEnd",   loc.column_end}};
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
        std::vector<NodePtr> path;
        std::function<void(const NodePtr &, unsigned)> DFS = [&](auto &node, unsigned length) {
            if (length-- == 0) {
                reduce_list.push(ReduceNode(path, trans, node));
                return;
            }
            path.push_back(node);
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
        if (!node->error) {
            // there is error state, goto error state
            node = Node::Create(node->state->error->state, node);
            node->symbol = 2;
            node->value = value_t::array();
            node->lexeme = "error";
            node->location = lexer_.location();
            node->error = true;
            node->depth = node->depth + 1;
        }
        node->value.push_back({{"lexeme",      lexer_.lexeme()},
                               {"lineStart",   lexer_.line_start()},
                               {"columnStart", lexer_.column_start()},
                               {"lineEnd",     lexer_.line_end()},
                               {"columnEnd",   lexer_.column_end()}});
        shift_list.push_back(node);
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
            NodePtr *nodes;
            Getter(NodePtr *nodes) : nodes(nodes) {}
            inline Node &operator[](size_t index) {
                return *nodes[index];
            }
        } getter{node.paths.data()};

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