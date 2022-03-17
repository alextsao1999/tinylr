%right '=';
%left '||' '&&' '==' '<' '>' '+' '-' '*' '/' '->' ;
%right ':' '?';
%none '(' ')' ;
%right 'else' ;
%whitespace "[ \n\r]+";
%start programs;

programs -> programs program @Program $1{value:#2}
          | program @Program {value: [$1]};

program -> function_declare $1
         | class_declare $1
         | variable_declare $1
         | import $1
         ;

import -> 'import' import_items ';' @Import {items:$2};
import_items -> import_items '.' import_item $1[$3]
              | import_item [$1]
              | @Error{content:'null import item'}
              ;
import_item -> identifier $1
             | '*' @1
             | error @Error{content:'error import content', value:$1}
             ;

class_declare -> 'class' identifier template super '{' classbody '}'
	@ClassDeclare{@std::string name: $2, template:$3, super:$4, body:$6};

super -> ':' identifier $2
       |
       ;

template -> '<' template_args '>' $2
          |
          ;
template_args -> template_args ',' identifier $1[$3]
               | identifier [$1]
               ;

classbody  -> classbody classmember $1[$2]
            | classmember [$1];

classmember -> variable_declare @FieldDeclare $1
             | function_declare @MethodDeclare $1
             | cast_method_declare $1
             | overload $1
             | property $1
             ;
cast_method_declare -> type block @CastMethodDeclare{type:$1, block:$2};

property -> type identifier block @ReadPropertyDeclare{type:$1, name:$2, block:$3, public: false}
          | type identifier '=' '(' params ')' block @WritePropertyDeclare{type:$1, name:$2, params:$5, block:$7, public:false}
          | 'public' property $2{public:true}
          | 'private' property $2{public:false}
          ;

overload -> overload_op '(' params ')' block @OverloadOperator{op:$1, params:$3, block:$5}
          | overload_op '(' params ')' '->' type block @OverloadOperator{op:$1, params:$3, type:$6, block:$7}
          ;

overload_op -> '[' ']' @1
             | '(' ')' @1
             | '+' @1
             | '-' @1
             | '*' @1
             | '/' @1
             | '<' @1
             | '>' @1
             | '++' @1
             | '--' @1
             | '==' @1
             | '->' @1
             | '<<' @1
             | '>>' @1
             ;

function_declare -> type identifier '(' params ')' block @FunctionDeclare{type:$1, name:$2, params:$4, block:$6}
        | 'public' function_declare @FunctionDeclare $2{public:true}
        | 'private' function_declare @FunctionDeclare $2{public:false}
        | 'static' function_declare @FunctionDeclare $2{static:true}
        | 'inline' function_declare @FunctionDeclare $2{inline:true}
        ;

params -> params ',' param $1[$3]
        | param [$1]
        |
        ;

param -> type identifier @Param{type:$1 , name:$2};

block  -> '{' stmts '}' $2
        | '{' '}'
        | error @Error{content:'error block', value:$1}
        ;

stmts  -> stmts stmt $1[$2]
        | stmt [$1]
        ;

stmt   -> expr ';' @ExprStmt{value:$1}
        | block @BlockStmt{value:$1}
        | 'return' expr ';' @ReturnStmt{value:$2}
        | 'continue' ';' @ContinueStmt{}
        | 'break' ';' @BreakStmt{}
        | variable_declare $1
        | if_stmt $1
        | while_stmt $1
        | do_while_stmt $1
        ;

if_stmt -> 'if' '(' expr ')' stmt @IfStmt{condition:$3, then:$5}
         | 'if' '(' expr ')' stmt 'else' stmt @IfStmt{condition:$3, then:$5, else:$7}
         ;

while_stmt -> 'while' '(' expr ')' stmt @WhileStmt{condition:$3, body:$5};

do_while_stmt -> 'do' stmt 'while' '(' expr ')' ';' @DoWhileStmt{condition:$5, body:$2};

switch_stmt -> 'switch' '(' expr ')' '{' cases default '}' @SwitchStmt{condition:$3, cases:$6, default:$7} ;
cases -> cases case $1{branches:#2}
       | case @CaseStmt{branches:[$1]}
       ;
case -> 'case' literal ':' stmts @CaseStmt{condition:$2, body:$4} ;
default -> 'default' ':' stmts $3
         |
         ;

lhs -> variable $1
     | dot $1
     | array $1
     ;

variable_declare -> type identifier ';' @VariableDeclare{type:$1, name:$2}
        | type identifier '=' expr ';' @VariableDeclare{type:$1, name:$2, init:$4}
        | 'public' variable_declare @VariableDeclare $2{public:true}
        | 'private' variable_declare @VariableDeclare $2{public:false}
        | 'static' variable_declare @VariableDeclare $2{static:true}
        ;

type   -> identifier @TypeSpecifier{type:$1}
        | identifier '<' type_args '>' @TypeSpecifier{type:$1, args:$3}
        ;
type_args -> type [$1]
           | type_args ',' type $1[$3]
           ;

expr -> expr '<' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '>' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '+' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '-' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '*' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '/' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '==' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '&&' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '||' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '?' expr ':' expr @TernaryExpr{condition:$1, left:$3, right:$5}
      | expr '=' expr @AssignExpr{left:$1, right:$3}
      | '(' params ')' '->' block @ArrowExpr{params:$2, block:$5}
      | '(' type ')' expr @TypeCast{cast_to:$2, value:$4}
      | '(' expr ')' $2
      | primary $1
      ;

primary -> literal $1
         | invoke $1
         | variable $1
         | dot $1
         | array $1
         | 'new' type '(' args ')' @NewExpr{type:$2, args:$4}
         ;

variable -> identifier @VariableExpr{name:$1};
dot   -> expr '.' identifier @DotExpr{lhs:$1, field:$3};
array -> expr '[' expr ']' @AccessExpr{lhs:$1, field:$3};
invoke -> identifier '(' args ')' @InvokeExpr{name:$1, args:$3};

args   -> args ',' expr $1[$3]
        | expr [$1]
        |
        ;

literal -> "\".*\"" @String{value:@1}
         | "[0-9]+" @Number{value:@1}
         | "[0-9]+\.[0-9]*f?" @Float{value:@1}
         | "[0-9]+u" @Unsigned{unsigned:true, value:@1}
         | "[0-9]+L" @Long{value:@1}
         | "0x[0-9A-F]+" @Hex{value:@1}
         | "0b[0-1]+" @Bin{value:@1}
         | "'(.|\\\\.)'" @Char{value:@1}
         ;

identifier -> "[a-zA-Z_][a-zA-Z0-9_]*" @1;
