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

    void visitParam(ParamDef value) {
        Visitor::visitParamDef(value);
    }

};

int main() {
    const char *string = "int a() {"
                         "  int *b = 10;"
                         "}";

    GLRParser<> parser(false);
    parser.reset(string, string + strlen(string));
    parser.parse();
    assert(parser.accept());
    auto value = std::move(parser.value());
    //MyVisitor visitor;
    //visitor.visit(value);
    std::cout << value.dump(4) << std::endl;
    return 0;
}
