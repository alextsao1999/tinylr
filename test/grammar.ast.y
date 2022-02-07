%right '=';
%left '||' '&&' '==' '<' '>' '+' '-' '*' '/' '->' ;
%right ':' '?';
%none '(' ')' ;
%right 'else' ;
%whitespace "[ \n\r]+";
%start programs;
programs -> programs program $1{$2}
          | program @Program {$1};

program -> function_declare $1
         | class_declare $1
         | variable_declare $1
         | import $1
         ;

import -> 'import' import_items ';' $2 ;
import_items -> import_items '.' import_item $1{$3}
              | import_item @Import{$1}
              | @Error{content:'null import item'}
              ;
import_item -> identifier $1
             | '*' @1
             | error $1
             ;

class_declare -> 'class' identifier template super '{' classbody '}' @Class {name:$2, template_args:$3, super:$4, body:$6};

super -> ':' identifier $2
       |
       ;

template -> '<' template_args '>' $2
          |
          ;
template_args -> template_args ',' identifier $1{$3}
               | identifier {$1}
               ;

classbody  -> classbody classmember $1{$2}
            | classmember {$1};

classmember -> field_declare $1
             | method_declare $1
             | cast_method_declare $1
             | property_read $1
             | property_write $1
             | overload $1
             ;
field_declare -> type identifier ';' @FieldDeclare{type:$1, name:$2}
        | type identifier '=' expr ';' @FieldDeclare{type:$1, name:$2, init:$4}
        | 'public' field_declare @FieldDeclare $2{is_public:true}
        | 'private' field_declare @FieldDeclare $2{is_public:false}
        | 'static' field_declare @FieldDeclare $2{is_static:true}
        ;

method_declare -> type identifier '(' params ')' block @MethodDeclare {type:$1, name:$2, params:$4, block:$6}
        | 'public' method_declare @MethodDeclare $1{is_public:true}
        | 'private' method_declare @MethodDeclare $2{is_public:false}
        | 'static' method_declare @MethodDeclare $2{is_static:true}
        | 'inline' method_declare @MethodDeclare $2{is_inline:true}
        ;
cast_method_declare -> type block @CastMethodDeclare{type:$1, block:$2};
property_read -> type identifier block @PropertyReadDeclare{type:$1, name:$2, block:$3}
          | 'public' property_read @PropertyReadDeclare $2{is_public:true}
          | 'private' property_read @PropertyReadDeclare $2{is_public:false}
          ;
property_write -> type identifier '=' '(' params ')' block @PropertyWriteDeclare{type:$1, name:$2, params:$5, block:$7}
                | 'public' property_write @PropertyWriteDeclare $2{is_public:true}
                | 'private' property_write @PropertyWriteDeclare $2{is_public:false}
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
        | 'public' function_declare @FunctionDeclare $2{is_public:true}
        | 'private' function_declare @FunctionDeclare $2{is_public:false}
        | 'static' function_declare @FunctionDeclare $2{is_static:true}
        | 'inline' function_declare @FunctionDeclare $2{is_inline:true}
        ;

params -> params ',' param $1{$3}
        | param {$1}
        |
        ;

param -> type identifier @Param{type:$1 , name:$2};

block  -> '{' stmts '}' $2
        | '{' '}'
        | error @Error{content:'error block', value:$1}
        ;

stmts  -> stmts stmt $1{$2}
        | stmt {$1}
        ;

stmt   -> expr ';' @ExprStatement{value:$1}
        | block @BlockStatement{value:$1}
        | 'return' expr ';' @ReturnStatement{value:$2}
        | 'continue' ';' @ContinueStatement
        | 'break' ';' @BreakStatement
        | variable_declare $1
        | if_stmt $1
        | while_stmt $1
        ;

if_stmt -> 'if' '(' expr ')' stmt @IfStatement{condition:$3, then_block:$5}
         | 'if' '(' expr ')' stmt 'else' stmt @IfStatement{condition:$3, then_block:$5, else_block:$7}
         ;

while_stmt -> 'while' '(' expr ')' stmt @WhileStatement{condition:$3, body:$5};

lhs -> variable $1
     | dot $1
     | array $1
     ;

variable_declare -> type identifier ';' @VariableDeclare{type:$1, name:$2}
        | type identifier '=' expr ';' @VariableDeclare{type:$1, name:$2, init:$4}
        ;

type   -> identifier @TypeSpecifier{type:$1}
        | identifier '<' type_args '>' @TypeSpecifier{type:$1, args:$3}
        ;
type_args -> type @TypeArgs{$1}
           | type_args ',' type $1{$3}
           ;

expr -> expr '<' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '>' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '+' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '-' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '*' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '/' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '=' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '==' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '&&' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '||' expr @BinaryExpr{left:$1, op:@2, right:$3}
      | expr '?' expr ':' expr @TernaryExpr{condition:$1, left:$3, right:$5}
      | '(' params ')' '->' block @ArrayFuncExpr{params:$2, block:$5}
      | '(' type ')' expr @TypeCastExpr{cast_to:$2, value:$4}
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
array -> expr '[' expr ']' @ArrayExpr{lhs:$1, field:$3};
invoke -> identifier '(' args ')' @InvokeExpr{name:$1, args:$3};

args   -> args ',' expr $1{$3}
        | expr @ArgumentList{$1}
        |
        ;

literal -> "\".*\"" @String{value:@1}
         | "[0-9]+" @Number{value:@1}
         | "[0-9]+\.[0-9]*f?" @Float{value:@1}
         | "[0-9]+u" @Unsigned{value:@1}
         | "[0-9]+L" @Long{value:@1}
         | "0x[0-9A-F]+" @HexExpr{value:@1}
         | "0b[0-1]+" @BinExpr{value:@1}
         | "'(.|\\\\.)'" @CharExpr{value:@1}
         ;

identifier -> "[a-zA-Z_][a-zA-Z0-9_]*" @1;
