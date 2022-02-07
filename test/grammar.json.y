%right '=';
%left '||' '&&' '==' '<' '>' '+' '-' '*' '/' '->' ;
%right ':' '?';
%none '(' ')' ;
%right 'else' ;
%whitespace "[ \n\r]+";
%start programs;

programs -> programs program $1{value:#2}
          | program {id:Program, kind:'program', value:#1};

program -> function_declare $1
         | class_declare $1
         | variable_declare $1
         | import $1
         ;

import -> 'import' import_items ';' {id:Import, kind:'import', items:$2} ;
import_items -> import_items '.' import_item $1{$3}
              | import_item {$1}
              | {kind:'error', content:'null import item'}
              ;
import_item -> identifier $1
             | '*' @1
             | error {id:Error, kind:'error', content:'error import content', value:$1}
             ;

class_declare -> 'class' identifier template super '{' classbody '}' {id:Class, kind:'class_declare', name:$2, template:$3, super:$4, body:$6};

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

classmember -> variable_declare $1{id:FieldDeclare, kind:'field_declare'}
             | function_declare $1{id:MethodDeclare, kind:'method_declare'}
             | cast_method_declare $1
             | overload $1
             | property $1
             ;
cast_method_declare -> type block {id:CastMethodDeclare, kind:'cast_method_declare', type:$1, block:$2};

property -> type identifier block {id:PropertyDeclare, kind:'read', type:$1, name:$2, block:$3}
          | type identifier '=' '(' params ')' block {id:PropertyDeclare, kind:'write', type:$1, name:$2, params:$5, block:$7} $1
          | 'public' property $2{public:true}
          | 'private' property $2{public:false}
          ;

overload -> overload_op '(' params ')' block {id:OverloadOperator, kind:'overload', op:$1, params:$3, block:$5}
          | overload_op '(' params ')' '->' type block {id:OverloadOperator, kind:'overload', op:$1, params:$3, type:$6, block:$7}
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

function_declare -> type identifier '(' params ')' block {id:FunctionDeclare, kind:'function_declare', type:$1, name:$2, params:$4, block:$6}
        | 'public' function_declare $2{public:true}
        | 'private' function_declare $2{public:false}
        | 'static' function_declare $2{static:true}
        | 'inline' function_declare $2{inline:true}
        ;

params -> params ',' param $1{$3}
        | param {$1}
        |
        ;

param -> type identifier {id:Param, kind:'param', type:$1 , name:$2};

block  -> '{' stmts '}' $2
        | '{' '}'
        | error {id:Error, kind:'error', content:'error block', value:$1}
        ;

stmts  -> stmts stmt $1{$2}
        | stmt {$1}
        ;

stmt   -> expr ';' {id:expr_statement, kind:'expr', value:$1}
        | block {id:block_statement, kind:'block', value:$1}
        | 'return' expr ';' {id:Statement, kind:'return', value:$2}
        | 'continue' ';' {id:Statement, kind:'continue'}
        | 'break' ';' {id:Statement, kind:'break'}
        | variable_declare $1
        | if_stmt $1
        | while_stmt $1
        | do_while_stmt $1
        ;

if_stmt -> 'if' '(' expr ')' stmt {id:Statement, kind:'if', condition:$3, then:$5}
         | 'if' '(' expr ')' stmt 'else' stmt {id:Statement, kind:'if', condition:$3, then:$5, else:$7}
         ;

while_stmt -> 'while' '(' expr ')' stmt {id:Statement, kind:'while', condition:$3, body:$5};

do_while_stmt -> 'do' stmt 'while' '(' expr ')' ';' {id:Statement, kind:'do_while', condition:$5, body:$2};

switch_stmt -> 'switch' '(' expr ')' '{' cases default '}' {id:Statement, kind:'switch', condition:$3, cases:$6, default:$7} ;
cases -> cases case $1{branches:#2}
       | case {id:Statement, kind:'cases', branches:#1}
       ;
case -> 'case' literal ':' stmts {id:Statement, kind:'case', condition:$2, body:$4} ;
default -> 'default' ':' stmts $3
         |
         ;

lhs -> variable $1
     | dot $1
     | array $1
     ;

variable_declare -> type identifier ';' {id:VariableDeclare, kind:'variable_declare', type:$1, name:$2}
        | type identifier '=' expr ';' {id:VariableDeclare, kind:'variable_declare', type:$1, name:$2, init:$4}
        | 'public' variable_declare $2{public:true}
        | 'private' variable_declare $2{public:false}
        | 'static' variable_declare $2{static:true}
        ;

type   -> identifier {id:TypeSpecifier, kind:'type_specifier', type:$1}
        | identifier '<' type_args '>' {id:TypeSpecifier, kind:'type_specifier', type:$1, args:$3}
        ;
type_args -> type {$1}
           | type_args ',' type $1{$3}
           ;

expr -> expr '<' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '>' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '+' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '-' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '*' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '/' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '==' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '&&' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '||' expr {id:Expression, kind:'binary', left:$1, op:@2, right:$3}
      | expr '?' expr ':' expr {id:Expression, kind:'ternary', condition:$1, left:$3, right:$5}
      | expr '=' expr {id:Expression, kind:'assign', left:$1, right:$3}
      | '(' params ')' '->' block {id:Expression, kind:'arrow_function', params:$2, block:$5}
      | '(' type ')' expr {id:Expression, kind:'type_cast', cast_to:$2, value:$4}
      | '(' expr ')' $2
      | primary $1
      ;

primary -> literal $1
         | invoke $1
         | variable $1
         | dot $1
         | array $1
         | 'new' type '(' args ')' {id:Expression, kind:'new', type:$2, args:$4}
         ;

variable -> identifier {id:Expression, kind:'variable', name:$1};
dot   -> expr '.' identifier {id:Expression, kind:'dot', lhs:$1, field:$3};
array -> expr '[' expr ']' {id:Expression, kind:'array', lhs:$1, field:$3};
invoke -> identifier '(' args ')' {id:Expression, kind:'invoke', name:$1, args:$3};

args   -> args ',' expr $1{value:#3}
        | expr {id:ArgumentList, kind:'argument_list', value:#1}
        |
        ;

literal -> "\".*\"" {id:Expression, kind:'string', string:true, value:@1}
         | "[0-9]+" {id:Expression, kind:'number', number:true, value:@1}
         | "[0-9]+\.[0-9]*f?" {id:Expression, kind:'float', float:true, value:@1}
         | "[0-9]+u" {id:Expression, kind:'unsigned', unsigned:true, value:@1}
         | "[0-9]+L" {id:Expression, kind:'long', long:true, value:@1}
         | "0x[0-9A-F]+" {id:Expression, kind:'hex', hex:true, value:@1}
         | "0b[0-1]+" {id:Expression, kind:'bin', bin:true, value:@1}
         | "'(.|\\\\.)'" {id:Expression, kind:'char', char:true, value:@1}
         ;

identifier -> "[a-zA-Z_][a-zA-Z0-9_]*" @1;
