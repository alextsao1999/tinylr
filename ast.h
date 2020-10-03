//
// Created by Alex on 9/29/2020.
//

#ifndef TINYLALR_AST_H
#define TINYLALR_AST_H
#include <iostream>
#include <vector>
#define ASTLIST(v) \
    v(ArgumentList) \
    v(ArrayExpr) \
    v(ArrayFuncExpr) \
    v(BinExpr) \
    v(BinaryExpr) \
    v(BlockStatement) \
    v(BreakStatement) \
    v(CastMethodDeclare) \
    v(CharExpr) \
    v(Class) \
    v(ContinueStatement) \
    v(DotExpr) \
    v(Error) \
    v(ExprStatement) \
    v(Float) \
    v(FunctionDeclare) \
    v(HexExpr) \
    v(IfStatement) \
    v(Import) \
    v(Invoke) \
    v(Long) \
    v(MethodDeclare) \
    v(NewExpr) \
    v(Number) \
    v(OverloadOperator) \
    v(Param) \
    v(Program) \
    v(PropertyReadDeclare) \
    v(PropertyWriteDeclare) \
    v(ReturnStatement) \
    v(String) \
    v(TypeArgs) \
    v(TypeCastExpr) \
    v(TypeSpecifier) \
    v(Unsigned) \
    v(VariableDeclare) \
    v(VariableExpr) \
    v(WhileStatement)
#define DeclareASTList(Type) class Type;
ASTLIST(DeclareASTList);
class ASTVisitor {
public:
#define VisitItem(Type) virtual void visit(Type *) = 0;
    ASTLIST(VisitItem);
};
class ASTNode {
protected:
    int line_ = 0;
    int column_ = 0;
public:
    ASTNode(int line, int column) : line_(line), column_(column) {}
    ASTNode() {}
    virtual ~ASTNode() {}
    int line () {
        return line_;
    }
    int column () {
        return column_;
    }
    virtual bool is_leaf() { return false; }
    virtual bool is_list() { return false; }
    virtual bool is_error() { return false; }
    virtual void dump(std::ostream &os) {}
    virtual void accept(ASTVisitor *) {}
};
class ASTLeaf : public ASTNode {
public:
    ASTLeaf() {}
    ASTLeaf(int line, int column) : ASTNode(line, column) {}
    bool is_leaf() override { return true; }
};
class ASTInteger : public ASTLeaf {
protected:
    int value;
public:
    ASTInteger(int value) : value(value) {}
    void dump(std::ostream &os) override {
        os << value;
    }
};
class ASTString : public ASTLeaf {
protected:
    std::string value;
public:
    ASTString(const std::string &value) : value(value) {}
    void dump(std::ostream &os) override {
        os << value;
    }
};
template <class char_t, class char_traits = std::char_traits<char_t>>
class ASTLexeme : public ASTLeaf {
    std::basic_string<char_t, char_traits> lexeme_;
public:
    ASTLexeme(const std::basic_string<char_t, char_traits> &lexeme) : lexeme_(lexeme) {}
    ASTLexeme(int line, int column, const std::basic_string<char_t, char_traits> &lexeme) : ASTLeaf(line, column),
                                                                                            lexeme_(lexeme) {}
    void dump(std::ostream &os) override {
        os << lexeme_;
    }
};
using ASTNodePtr = std::shared_ptr<ASTNode>;
using ASTLeafPtr = std::shared_ptr<ASTLeaf>;
class ASTList : public ASTNode {
    using iterator = std::vector<ASTNodePtr>::iterator;
    int kind;
    std::vector<ASTNodePtr> nodes;
public:
    ASTList() {}
    ASTList(int kind, int size) : kind(kind), nodes(size) {}
    bool is_list() override { return true; }
    inline iterator begin() { return nodes.begin(); }
    inline iterator end() { return nodes.end(); }
    size_t size() { return nodes.size(); }
    ASTNodePtr &get(size_t index) { return nodes[index]; }
    void set(size_t index , ASTNodePtr value) {
        nodes[index] = value;
    }
    void add(ASTNodePtr value) {
        nodes.push_back(value);
    }
    template <class char_t = char, class char_traits = std::char_traits<char_t>>
    ASTLexeme<char_t, char_traits> *token(size_t index = 0) {
        return dynamic_cast<ASTLexeme<char_t, char_traits> *>(get(index).get());
    }
    void dump(std::ostream &os) override {
        os << "[";
        for (auto &node : *this) {
            node->dump(os);
            if (nodes.back() != node) {
                os << ", ";
            }
        }
        os << "]";
    }
    void accept(ASTVisitor *visitor) override {
        for (auto &item : *this) {
            item->accept(visitor);
        }
    }
};
using ASTListPtr = std::shared_ptr<ASTList>;
class ASTError : public ASTList {
public:
    bool is_error() override { return true; }
    void dump(std::ostream &os) override {
        os << "error";
        ASTList::dump(os);
    }
};
class ASTMerge : public ASTList {
public:
    void dump(std::ostream &os) override {
        os << "merge";
        ASTList::dump(os);
    }
};

class ArgumentList : public ASTList {
public:
    ArgumentList() : ASTList(1, 0) {}
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
};
class ArrayExpr : public ASTList {
public:
    ArrayExpr() : ASTList(2, 2) {}
    ASTNodePtr field() { return get(1); }
    ASTNodePtr lhs() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "ArrayExpr" << "{";
        os << "field: ";
        if (field()) field()->dump(os); else os << "null";
        os << ", lhs: ";
        if (lhs()) lhs()->dump(os); else os << "null";
        os << "}";
    }
};
class ArrayFuncExpr : public ASTList {
public:
    ArrayFuncExpr() : ASTList(3, 2) {}
    ASTNodePtr block() { return get(1); }
    ASTNodePtr params() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "ArrayFuncExpr" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", params: ";
        if (params()) params()->dump(os); else os << "null";
        os << "}";
    }
};
class BinExpr : public ASTList {
public:
    BinExpr() : ASTList(4, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "BinExpr" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class BinaryExpr : public ASTList {
public:
    BinaryExpr() : ASTList(5, 3) {}
    ASTNodePtr left() { return get(0); }
    ASTNodePtr op() { return get(1); }
    ASTNodePtr right() { return get(2); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "BinaryExpr" << "{";
        os << "left: ";
        if (left()) left()->dump(os); else os << "null";
        os << ", op: ";
        if (op()) op()->dump(os); else os << "null";
        os << ", right: ";
        if (right()) right()->dump(os); else os << "null";
        os << "}";
    }
};
class BlockStatement : public ASTList {
public:
    BlockStatement() : ASTList(6, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "BlockStatement" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class BreakStatement : public ASTList {
public:
    BreakStatement() : ASTList(7, 0) {}
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
};
class CastMethodDeclare : public ASTList {
public:
    CastMethodDeclare() : ASTList(8, 2) {}
    ASTNodePtr block() { return get(1); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "CastMethodDeclare" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class CharExpr : public ASTList {
public:
    CharExpr() : ASTList(9, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "CharExpr" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class Class : public ASTList {
public:
    Class() : ASTList(10, 4) {}
    ASTNodePtr body() { return get(3); }
    ASTNodePtr name() { return get(0); }
    ASTNodePtr super() { return get(2); }
    ASTNodePtr template_args() { return get(1); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Class" << "{";
        os << "body: ";
        if (body()) body()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", super: ";
        if (super()) super()->dump(os); else os << "null";
        os << ", template_args: ";
        if (template_args()) template_args()->dump(os); else os << "null";
        os << "}";
    }
};
class ContinueStatement : public ASTList {
public:
    ContinueStatement() : ASTList(11, 0) {}
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
};
class DotExpr : public ASTList {
public:
    DotExpr() : ASTList(12, 2) {}
    ASTNodePtr field() { return get(1); }
    ASTNodePtr lhs() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "DotExpr" << "{";
        os << "field: ";
        if (field()) field()->dump(os); else os << "null";
        os << ", lhs: ";
        if (lhs()) lhs()->dump(os); else os << "null";
        os << "}";
    }
};
class Error : public ASTList {
public:
    Error() : ASTList(13, 2) {}
    ASTNodePtr content() { return get(0); }
    ASTNodePtr value() { return get(1); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Error" << "{";
        os << "content: ";
        if (content()) content()->dump(os); else os << "null";
        os << ", value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class ExprStatement : public ASTList {
public:
    ExprStatement() : ASTList(14, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "ExprStatement" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class Float : public ASTList {
public:
    Float() : ASTList(15, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Float" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class FunctionDeclare : public ASTList {
public:
    FunctionDeclare() : ASTList(16, 7) {}
    ASTNodePtr block() { return get(3); }
    ASTNodePtr is_inline() { return get(6); }
    ASTNodePtr is_public() { return get(4); }
    ASTNodePtr is_static() { return get(5); }
    ASTNodePtr name() { return get(1); }
    ASTNodePtr params() { return get(2); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "FunctionDeclare" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", is_inline: ";
        if (is_inline()) is_inline()->dump(os); else os << "null";
        os << ", is_public: ";
        if (is_public()) is_public()->dump(os); else os << "null";
        os << ", is_static: ";
        if (is_static()) is_static()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", params: ";
        if (params()) params()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class HexExpr : public ASTList {
public:
    HexExpr() : ASTList(17, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "HexExpr" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class IfStatement : public ASTList {
public:
    IfStatement() : ASTList(18, 3) {}
    ASTNodePtr condition() { return get(0); }
    ASTNodePtr else_block() { return get(2); }
    ASTNodePtr then_block() { return get(1); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "IfStatement" << "{";
        os << "condition: ";
        if (condition()) condition()->dump(os); else os << "null";
        os << ", else_block: ";
        if (else_block()) else_block()->dump(os); else os << "null";
        os << ", then_block: ";
        if (then_block()) then_block()->dump(os); else os << "null";
        os << "}";
    }
};
class Import : public ASTList {
public:
    Import() : ASTList(19, 0) {}
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
};
class Invoke : public ASTList {
public:
    Invoke() : ASTList(20, 2) {}
    ASTNodePtr args() { return get(1); }
    ASTNodePtr name() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Invoke" << "{";
        os << "args: ";
        if (args()) args()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << "}";
    }
};
class Long : public ASTList {
public:
    Long() : ASTList(21, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Long" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class MethodDeclare : public ASTList {
public:
    MethodDeclare() : ASTList(22, 7) {}
    ASTNodePtr block() { return get(3); }
    ASTNodePtr is_inline() { return get(6); }
    ASTNodePtr is_public() { return get(4); }
    ASTNodePtr is_static() { return get(5); }
    ASTNodePtr name() { return get(1); }
    ASTNodePtr params() { return get(2); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "MethodDeclare" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", is_inline: ";
        if (is_inline()) is_inline()->dump(os); else os << "null";
        os << ", is_public: ";
        if (is_public()) is_public()->dump(os); else os << "null";
        os << ", is_static: ";
        if (is_static()) is_static()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", params: ";
        if (params()) params()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class NewExpr : public ASTList {
public:
    NewExpr() : ASTList(23, 2) {}
    ASTNodePtr args() { return get(1); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "NewExpr" << "{";
        os << "args: ";
        if (args()) args()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class Number : public ASTList {
public:
    Number() : ASTList(24, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Number" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class OverloadOperator : public ASTList {
public:
    OverloadOperator() : ASTList(25, 4) {}
    ASTNodePtr block() { return get(2); }
    ASTNodePtr op() { return get(0); }
    ASTNodePtr params() { return get(1); }
    ASTNodePtr type() { return get(3); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "OverloadOperator" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", op: ";
        if (op()) op()->dump(os); else os << "null";
        os << ", params: ";
        if (params()) params()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class Param : public ASTList {
public:
    Param() : ASTList(26, 2) {}
    ASTNodePtr name() { return get(1); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Param" << "{";
        os << "name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class Program : public ASTList {
public:
    Program() : ASTList(27, 0) {}
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
};
class PropertyReadDeclare : public ASTList {
public:
    PropertyReadDeclare() : ASTList(28, 3) {}
    ASTNodePtr block() { return get(2); }
    ASTNodePtr name() { return get(1); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "PropertyReadDeclare" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class PropertyWriteDeclare : public ASTList {
public:
    PropertyWriteDeclare() : ASTList(29, 4) {}
    ASTNodePtr block() { return get(3); }
    ASTNodePtr name() { return get(1); }
    ASTNodePtr params() { return get(2); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "PropertyWriteDeclare" << "{";
        os << "block: ";
        if (block()) block()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", params: ";
        if (params()) params()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class ReturnStatement : public ASTList {
public:
    ReturnStatement() : ASTList(30, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "ReturnStatement" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class String : public ASTList {
public:
    String() : ASTList(31, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "String" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class TypeArgs : public ASTList {
public:
    TypeArgs() : ASTList(32, 0) {}
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
};
class TypeCastExpr : public ASTList {
public:
    TypeCastExpr() : ASTList(33, 2) {}
    ASTNodePtr cast_to() { return get(0); }
    ASTNodePtr value() { return get(1); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "TypeCastExpr" << "{";
        os << "cast_to: ";
        if (cast_to()) cast_to()->dump(os); else os << "null";
        os << ", value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class TypeSpecifier : public ASTList {
public:
    TypeSpecifier() : ASTList(34, 2) {}
    ASTNodePtr args() { return get(1); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "TypeSpecifier" << "{";
        os << "args: ";
        if (args()) args()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class Unsigned : public ASTList {
public:
    Unsigned() : ASTList(35, 1) {}
    ASTNodePtr value() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "Unsigned" << "{";
        os << "value: ";
        if (value()) value()->dump(os); else os << "null";
        os << "}";
    }
};
class VariableDeclare : public ASTList {
public:
    VariableDeclare() : ASTList(36, 3) {}
    ASTNodePtr init() { return get(2); }
    ASTNodePtr name() { return get(1); }
    ASTNodePtr type() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "VariableDeclare" << "{";
        os << "init: ";
        if (init()) init()->dump(os); else os << "null";
        os << ", name: ";
        if (name()) name()->dump(os); else os << "null";
        os << ", type: ";
        if (type()) type()->dump(os); else os << "null";
        os << "}";
    }
};
class VariableExpr : public ASTList {
public:
    VariableExpr() : ASTList(37, 1) {}
    ASTNodePtr name() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "VariableExpr" << "{";
        os << "name: ";
        if (name()) name()->dump(os); else os << "null";
        os << "}";
    }
};
class WhileStatement : public ASTList {
public:
    WhileStatement() : ASTList(38, 2) {}
    ASTNodePtr body() { return get(1); }
    ASTNodePtr condition() { return get(0); }
    void accept(ASTVisitor *visitor) override { visitor->visit(this); }
    void dump(std::ostream &os) override {
        os << "WhileStatement" << "{";
        os << "body: ";
        if (body()) body()->dump(os); else os << "null";
        os << ", condition: ";
        if (condition()) condition()->dump(os); else os << "null";
        os << "}";
    }
};
inline ASTListPtr CreateASTById(int id) {
    switch (id) {
        case 1:
            return std::make_shared<ArgumentList>();
        case 2:
            return std::make_shared<ArrayExpr>();
        case 3:
            return std::make_shared<ArrayFuncExpr>();
        case 4:
            return std::make_shared<BinExpr>();
        case 5:
            return std::make_shared<BinaryExpr>();
        case 6:
            return std::make_shared<BlockStatement>();
        case 7:
            return std::make_shared<BreakStatement>();
        case 8:
            return std::make_shared<CastMethodDeclare>();
        case 9:
            return std::make_shared<CharExpr>();
        case 10:
            return std::make_shared<Class>();
        case 11:
            return std::make_shared<ContinueStatement>();
        case 12:
            return std::make_shared<DotExpr>();
        case 13:
            return std::make_shared<Error>();
        case 14:
            return std::make_shared<ExprStatement>();
        case 15:
            return std::make_shared<Float>();
        case 16:
            return std::make_shared<FunctionDeclare>();
        case 17:
            return std::make_shared<HexExpr>();
        case 18:
            return std::make_shared<IfStatement>();
        case 19:
            return std::make_shared<Import>();
        case 20:
            return std::make_shared<Invoke>();
        case 21:
            return std::make_shared<Long>();
        case 22:
            return std::make_shared<MethodDeclare>();
        case 23:
            return std::make_shared<NewExpr>();
        case 24:
            return std::make_shared<Number>();
        case 25:
            return std::make_shared<OverloadOperator>();
        case 26:
            return std::make_shared<Param>();
        case 27:
            return std::make_shared<Program>();
        case 28:
            return std::make_shared<PropertyReadDeclare>();
        case 29:
            return std::make_shared<PropertyWriteDeclare>();
        case 30:
            return std::make_shared<ReturnStatement>();
        case 31:
            return std::make_shared<String>();
        case 32:
            return std::make_shared<TypeArgs>();
        case 33:
            return std::make_shared<TypeCastExpr>();
        case 34:
            return std::make_shared<TypeSpecifier>();
        case 35:
            return std::make_shared<Unsigned>();
        case 36:
            return std::make_shared<VariableDeclare>();
        case 37:
            return std::make_shared<VariableExpr>();
        case 38:
            return std::make_shared<WhileStatement>();
    }
    return ASTListPtr();
}
#endif //TINYLALR_AST_H