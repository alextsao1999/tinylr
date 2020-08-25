# tinylalr
a tiny lalr generator inspired by Charles Baker's lalr

generate json format ast from grammar
```c++
int main() {
    LALRGrammarParser lalr(
            "%left '+' '-' '*' '/';"
            "%start programs;"
            "%whitespace \"[ \n\r\t]+\";"
            "programs -> programs program $1{value:$2} | program {kind:'program', value:$1}; "
            "program -> fundef $1 | classdef $1 ;"
            "classdef -> 'class' identifier '{' classbody '}' {kind:'class', name:$2, body:$4} ;"
            "classbody -> classbody classmember $1{member:$2} | classmember {kind:'classbody', member:$1}; "
            "classmember -> fielddef $1 | fundef $1;"
            "fielddef -> vardef ';' $1;"
            "fundef -> type identifier '(' params ')' block {kind:'fundef', type:$1, name:$2, params:$4, block:$6};"
            "params -> params ',' paramdef $1{value:$3} | paramdef {kind:'params', value:$1} | ;"
            "paramdef -> type identifier {kind:'param', type:$1 , name:$2};"
            "vardef -> type identifier {kind:'vardef', type:$1, name:$2} | type identifier '=' expr {kind:'vardef', type:$1, name:$2, init:$4} ;"
            "block -> '{' stmts '}' $2;"
            "stmts -> stmts stmt ';' $1 {value:$2} | stmt ';' {kind:'stmts', value:$1};"
            "stmt -> expr $1 | assign $1 | vardef $1;"
            "assign -> identifier '=' expr {kind:'assign', left:$1, right:$3};"
            "type -> identifier $1;"
            "expr -> expr '+' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | expr '-' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | expr '*' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | expr '/' expr {kind:'binary', left:$1, op:@2, right:$3}"
            "       | '(' expr ')' $2"
            "       | primary $1"
            ";"
            "primary -> number $1 | invoke $1 | identifier {kind:'var', name:$1};"
            "invoke -> identifier '(' args ')' {kind:'invoke', name:$1, args:$3};"
            "args -> args ',' expr $1{value:$3} | expr {kind:'arg_list', value:$1};"
            "identifier -> \"[a-zA-Z_][a-zA-Z0-9_]*\" @1;"
            "number -> \"[0-9]+\" {kind:'number', value:@1};"
    );
    LALRGenerator gen(lalr);
    gen.generate();
    std::fstream fs;
    fs.open("../parser.cpp", std::ios::trunc | std::ios::out);
    fs << parser_emit_states(gen);
    fs.close();
    return 0;
}
```

```c++
int main() {
    const char *string = "class Object {\n"
                         "    int value = 10;\n"
                         "    int get_value(int abc) {\n"
                         "        return(value);\n"
                         "    }\n"
                         "    int get_value2(int abc) {\n"
                         "        return(value);\n"
                         "    }\n"
                         "}\n"
                         "void test() {\n"
                         "  int value = 100;\n"
                         "  a = get(1,2,3);\n"
                         "}\n";
    Parser<> parser;
    parser.reset(string, string + strlen(string));
    parser.parse();
    std::cout << parser.value();
    return 0;
}
```

## Output

```json
{
	"kind": "program",
	"value": [{
		"body": {
			"kind": "classbody",
			"member": [{
				"init": {
					"kind": "number",
					"value": "10"
				},
				"kind": "vardef",
				"name": "value",
				"type": "int"
			}, {
				"block": {
					"kind": "stmts",
					"value": {
						"args": {
							"kind": "arg_list",
							"value": {
								"kind": "var",
								"name": "value"
							}
						},
						"kind": "invoke",
						"name": "return"
					}
				},
				"kind": "fundef",
				"name": "get_value",
				"params": {
					"kind": "params",
					"value": {
						"kind": "param",
						"name": "abc",
						"type": "int"
					}
				},
				"type": "int"
			}, {
				"block": {
					"kind": "stmts",
					"value": {
						"args": {
							"kind": "arg_list",
							"value": {
								"kind": "var",
								"name": "value"
							}
						},
						"kind": "invoke",
						"name": "return"
					}
				},
				"kind": "fundef",
				"name": "get_value2",
				"params": {
					"kind": "params",
					"value": {
						"kind": "param",
						"name": "abc",
						"type": "int"
					}
				},
				"type": "int"
			}]
		},
		"kind": "class",
		"name": "Object"
	}, {
		"block": {
			"kind": "stmts",
			"value": [{
				"init": {
					"kind": "number",
					"value": "100"
				},
				"kind": "vardef",
				"name": "value",
				"type": "int"
			}, {
				"kind": "assign",
				"left": "a",
				"right": {
					"args": {
						"kind": "arg_list",
						"value": [{
							"kind": "number",
							"value": "1"
						}, {
							"kind": "number",
							"value": "2"
						}, {
							"kind": "number",
							"value": "3"
						}]
					},
					"kind": "invoke",
					"name": "get"
				}
			}]
		},
		"kind": "fundef",
		"name": "test",
		"params": null,
		"type": "void"
	}]
}

```
