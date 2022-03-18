//
// Created by Alex on 8/21/2020.
//
#include "parser.h"

struct MyVisitor : public Visitor<MyVisitor> {
    void visitProgram(Program value) {
        visit(value.getValue());
    }
    void visitBlockStmt(BlockStmt value) {
        visit(value.getValue());
    }
    void visitExprStmt(ExprStmt value) {
        visit(value.getValue());
    }
    void visitAssignExpr(AssignExpr value) {
        visit(value.getLeft());
        std::cout << " = ";
        visit(value.getRight());
        std::cout << std::endl;
    }
    void visitVariableExpr(VariableExpr value) {
        std::cout << value.getName();
    }

    void visitFunctionDeclare(FunctionDeclare value) {
        visit(value.getBlock());
    }

    void visitParam(Param value) {
        Visitor::visitParam(value);
    }

};

int main() {
    const char *string = "import aaa.bbb.ccc;\n"
                         "int<int, value> main() {\n"
                         "  a = 1 + 2 + 5;\n"
                         "}";

    GLRParser<> parser(true);
    parser.reset(string, string + strlen(string));
    parser.parse();

    if (parser.accept()) {
        auto value = parser.value();
        //MyVisitor visitor;
        //visitor.visit(value);
        std::cout << value.dump(4) << std::endl;
    }
    return 0;
}
