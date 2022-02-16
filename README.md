# tinylr
a tiny lalr generator inspired by Charles Baker's lalr

generate value_t format ast from grammar

glr parser can handle the shift-reduce conflict and reduce-reduce conflict grammar easily

# Grammar Example
```c++
%none '(' ')' ;
%right 'else' '=' ;
%left '||' '&&' '==' '<' '>' '+' '-' '*' '/' '->' ;
%whitespace "[ \n\r]+";
%start programs;

programs -> programs program $1{value:#2}
          | program {id:program, kind:'program', value:#1};

program -> fundef $1
         | classdef $1
         | vardef $1
         | import $1
         ;

import -> 'import' import_items ';' {id:import, kind:'import', items:$2} ;
import_items -> import_items '.' import_item $1{$3}
              | import_item #1
              | {kind:'error', content:'null import item'}
              ;
import_item -> identifier $1
             | '*' @1
             | error {id:error, kind:'error', content:'error import content', value:$1}
             ;

classdef -> 'class' identifier template super '{' classbody '}' {id:class, kind:'class', name:$2, template:$3, super:$4, body:$6};

super -> ':' identifier $2
       |
       ;

template -> '<' template_args '>' $2
          |
          ;
template_args -> template_args ',' identifier $1{$3}
               | identifier #1
               ;

classbody  -> classbody classmember $1{$2}
            | classmember #1;

classmember -> fielddef $1
             | fundef $1{id:methoddef, kind:'methoddef'}
             | overload $1
             | property $1
             ;

property -> read $1
          | write $1
          | 'public' property $2{public:true}
          | 'private' property $2{public:false}
          ;
read -> type identifier block {id:read, kind:'read', type:$1, name:$2, block:$3};
write -> type identifier '=' '(' params ')' block {id:write, kind:'write', type:$1, name:$2, params:$5, block:$7};

overload -> overload_op '(' params ')' block {id:overload, kind:'overload', op:$1, params:$3, block:$5}
          | overload_op '(' params ')' '->' type block {id:overload, kind:'overload', op:$1, params:$3, type:$6, block:$7}
          ;

overload_op -> '[' ']' {'[]'}
             | '+' @1
             | '-' @1
             | '*' @1
             | '->' @1
             | '==' @1
             | '/' @1
             ;

fielddef -> vardef $1{id:fielddef, kind:'fielddef'};

fundef -> type identifier '(' params ')' block {id:fundef, kind:'fundef', type:$1, name:$2, params:$4, block:$6}
        | 'public' fundef $2{public:true}
        | 'private' fundef $2{public:false}
        | 'static' fundef $2{static:true}
        | 'inline' fundef $2{inline:true}
        ;

params -> params ',' param $1{$3}
        | param #1
        |
        ;

param -> type identifier {id:param, kind:'param', type:$1 , name:$2};

block  -> '{' stmts '}' $2
        | '{' '}'
        ;

stmts  -> stmts stmt $1{value:#2}
        | stmt {id:stmts, kind:'stmts', value:#1}
        ;

stmt   -> assign ';' $1
        | expr ';'
        | 'return' expr ';' {id:return, kind:'return', value:$2}
        | 'continue' ';' {id:return, kind:'continue'}
        | 'break' ';' {id:break, kind:'break'}
        | vardef $1
        | block $1
        | while_stmt $1
        | do_while_stmt $1
        | if_stmt $1
        ;

if_stmt -> 'if' '(' expr ')' stmt {id:if, kind:'if', condition:$3, then:$5}
         | 'if' '(' expr ')' stmt 'else' stmt {id:if, kind:'if', condition:$3, then:$5, else:$7}
         ;

while_stmt -> 'while' '(' expr ')' stmt {id:while, kind:'while', condition:$3, body:$5};

do_while_stmt -> 'do' stmt 'while' '(' expr ')' ';' {id:do_while, kind:'do_while', condition:$5, body:$2};

switch_stmt -> 'switch' '(' expr ')' '{' cases default '}' {id:switch, kind:'switch', condition:$3, cases:$6, default:$7} ;
cases -> cases case $1{branches:#2}
       | case {id:cases, kind:'cases', branches:#1}
       ;
case -> 'case' literal ':' stmts {id:case, kind:'case', condition:$2, body:$4} ;
default -> 'default' ':' stmts $3
         |
         ;

lhs -> variable $1
     | dot $1
     | array $1
     ;

assign -> lhs '=' expr {id:assign, kind:'assign', left:$1, right:$3};

vardef -> type identifier ';' {id:vardef, kind:'vardef', type:$1, name:$2}
        | type identifier '=' expr ';' {id:vardef, kind:'vardef', type:$1, name:$2, init:$4}
        | 'public' vardef $2{public:true}
        | 'private' vardef $2{public:false}
        | 'static' vardef $2{static:true}
        ;

type   -> identifier {id:type, kind:'type', type:$1}
        | identifier '<' type_args '>' {id:type, kind:'type', type:$1, args:$3}
        ;

type_args -> type #1
           | type_args ',' type $1{$3}
           ;
expr -> expr '<' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '>' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '+' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '-' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '*' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '/' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '==' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '&&' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | expr '||' expr {id:binary, kind:'binary', left:$1, op:@2, right:$3}
      | '(' expr ')' $2
      | '(' params ')' '->' block {id:arrow_function, kind:'arrow_function', params:$2, block:$5}
      | primary $1
      ;

primary -> literal $1
         | invoke $1
         | variable $1
         | dot $1
         | array $1
         | 'new' type '(' args ')' {id:new, kind:'new', type:$2, args:$4}
         ;

variable -> identifier {id:variable, kind:'variable', name:$1};

dot   -> expr '.' identifier {id:dot, kind:'dot', lhs:$1, field:$3};

array -> expr '[' expr ']' {id:array, kind:'array', lhs:$1, field:$3};

invoke -> identifier '(' args ')' {id:invoke, kind:'invoke', name:$1, args:$3};

args   -> args ',' expr $1{value:#3}
        | expr {id:arg_list, kind:'arg_list', value:#1}
        |
        ;

literal -> "\".*\"" {id:string, kind:'string', value:@1}
         | "[0-9]+" {id:number, kind:'number', value:@1}
         | "[0-9]+\.[0-9]*f?" {id:float, kind:'float', value:@1}
         | "[0-9]+u" {id:unsigned, kind:'unsigned', value:@1}
         | "[0-9]+L" {id:long, kind:'long', value:@1}
         | "0x[0-9A-F]+" {id:hex, kind:'hex', value:@1}
         | "0b[0-1]+" {id:bin, kind:'bin', value:@1}
         | "'(.|\\\\.)'" {id:char, kind:'char', value:@1}
         ;

identifier -> "[a-zA-Z_][a-zA-Z0-9_]*" @1;

```

```c++
//
// Created by Alex on 8/21/2020.
//
#include "lexer.h"
#include "parser.h"
using namespace alex;
int main() {
    const char *string = "int main() {"
                         "  a<b> c;"
                         "}";
    GLRParser<StringIter<char>> parser;
    parser.set_position(true);
    parser.reset(string);
    parser.parse();
    if (parser.accept()) {
        value_t value = std::move(parser.value());
        std::cout << value.dump(4);
    }
    return 0;
}

```

## Output

```value_t
{
    "id": 28,
    "kind": "program",
    "position": {
        "column": 0,
        "line": 0
    },
    "value": [
        {
            "block": {
                "id": 31,
                "kind": "stmts",
                "position": {
                    "column": 11,
                    "line": 0
                },
                "value": [
                    {
                        "kind": "merge",
                        "value": [
                            {
                                "id": 6,
                                "kind": "binary",
                                "left": {
                                    "id": 37,
                                    "kind": "variable",
                                    "name": "a",
                                    "position": {
                                        "column": 14,
                                        "line": 0
                                    }
                                },
                                "op": "<",
                                "position": {
                                    "column": 14,
                                    "line": 0
                                },
                                "right": {
                                    "id": 6,
                                    "kind": "binary",
                                    "left": {
                                        "id": 37,
                                        "kind": "variable",
                                        "name": "b",
                                        "position": {
                                            "column": 16,
                                            "line": 0
                                        }
                                    },
                                    "op": ">",
                                    "position": {
                                        "column": 16,
                                        "line": 0
                                    },
                                    "right": {
                                        "id": 37,
                                        "kind": "variable",
                                        "name": "c",
                                        "position": {
                                            "column": 19,
                                            "line": 0
                                        }
                                    }
                                }
                            },
                            {
                                "id": 36,
                                "kind": "vardef",
                                "name": "c",
                                "position": {
                                    "column": 14,
                                    "line": 0
                                },
                                "type": {
                                    "args": [
                                        {
                                            "id": 34,
                                            "kind": "type",
                                            "position": {
                                                "column": 16,
                                                "line": 0
                                            },
                                            "type": "b"
                                        }
                                    ],
                                    "id": 34,
                                    "kind": "type",
                                    "position": {
                                        "column": 14,
                                        "line": 0
                                    },
                                    "type": "a"
                                }
                            }
                        ]
                    }
                ]
            },
            "id": 17,
            "kind": "fundef",
            "name": "main",
            "params": null,
            "position": {
                "column": 0,
                "line": 0
            },
            "type": {
                "id": 34,
                "kind": "type",
                "position": {
                    "column": 0,
                    "line": 0
                },
                "type": "int"
            }
        }
    ]
}
```


# Generator AST Directly from grammar

