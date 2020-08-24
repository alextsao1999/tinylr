# tinylalr
a tiny lalr generator inspired by Charles Baker's lalr

generate json format ast from grammar
```c++
int main() {
    LALRGrammarParser lalr(
            "%left '+' '-' '*' '/';"
            "%start expr;"
            "%whitespace \"[ \n\r\t]+\";"
            "%none '(';"
            "expr -> expr '+' expr {left:$1, right:$3, op:'+'}"
            "       | expr '-' expr {left:$1, right:$3, op:'-'}"
            "       | expr '*' expr {left:$1, right:$3, op:'*'}"
            "       | expr '/' expr {left:$1, right:$3, op:'/'}"
            "       | '(' expr ')' $2"
            ";"
            "expr -> number $1 | invoke $1;"
            "invoke -> identifier '(' arg_list ')' {type:'invoke', name:$1, args:$3} ;"
            "arg_list -> arg_list ',' expr $1{value:$3} | expr {type:'arg_list', value:$1} ;"
            "identifier -> \"[a-zA-Z][a-zA-Z0-9]*\" {type:'identifier', value:@1};"
            "number -> \"[0-9]+\" {type:'number', value:@1};"
    );
    LALRGenerator gen(lalr);
    gen.generate();
    std::fstream fs;
    fs.open("parser.cpp", std::ios::trunc | std::ios::out);
    fs << parser_emit_states(gen);
    fs.close();
}
```

```c++
int main() {
    const char *string = "1234 + (342 * 2)*2+3 + 22+ add(1,sub(4, 5, 6),3)+10";
    Parser<> parser;
    parser.reset(string, string + strlen(string));
    parser.parse();
    for (auto &value : parser.stack) {
        std::cout << value.value;
    }
    return 0;
}
```

## Output

```json
{
	"left": {
		"left": {
			"left": {
				"left": {
					"left": {
						"position": {
							"column": 0,
							"line": 0
						},
						"type": "number",
						"value": "1234"
					},
					"op": "+",
					"position": {
						"column": 0,
						"line": 0
					},
					"right": {
						"left": {
							"left": {
								"position": {
									"column": 8,
									"line": 0
								},
								"type": "number",
								"value": "342"
							},
							"op": "*",
							"position": {
								"column": 7,
								"line": 0
							},
							"right": {
								"position": {
									"column": 14,
									"line": 0
								},
								"type": "number",
								"value": "2"
							}
						},
						"op": "*",
						"position": {
							"column": 7,
							"line": 0
						},
						"right": {
							"position": {
								"column": 17,
								"line": 0
							},
							"type": "number",
							"value": "2"
						}
					}
				},
				"op": "+",
				"position": {
					"column": 0,
					"line": 0
				},
				"right": {
					"position": {
						"column": 19,
						"line": 0
					},
					"type": "number",
					"value": "3"
				}
			},
			"op": "+",
			"position": {
				"column": 0,
				"line": 0
			},
			"right": {
				"position": {
					"column": 23,
					"line": 0
				},
				"type": "number",
				"value": "22"
			}
		},
		"op": "+",
		"position": {
			"column": 0,
			"line": 0
		},
		"right": {
			"args": {
				"position": {
					"column": 31,
					"line": 0
				},
				"type": "arg_list",
				"value": [{
					"position": {
						"column": 31,
						"line": 0
					},
					"type": "number",
					"value": "1"
				}, {
					"args": {
						"position": {
							"column": 37,
							"line": 0
						},
						"type": "arg_list",
						"value": [{
							"position": {
								"column": 37,
								"line": 0
							},
							"type": "number",
							"value": "4"
						}, {
							"position": {
								"column": 40,
								"line": 0
							},
							"type": "number",
							"value": "5"
						}, {
							"position": {
								"column": 43,
								"line": 0
							},
							"type": "number",
							"value": "6"
						}]
					},
					"name": {
						"position": {
							"column": 33,
							"line": 0
						},
						"type": "identifier",
						"value": "sub"
					},
					"position": {
						"column": 33,
						"line": 0
					},
					"type": "invoke"
				}, {
					"position": {
						"column": 46,
						"line": 0
					},
					"type": "number",
					"value": "3"
				}]
			},
			"name": {
				"position": {
					"column": 27,
					"line": 0
				},
				"type": "identifier",
				"value": "add"
			},
			"position": {
				"column": 27,
				"line": 0
			},
			"type": "invoke"
		}
	},
	"op": "+",
	"position": {
		"column": 0,
		"line": 0
	},
	"right": {
		"position": {
			"column": 49,
			"line": 0
		},
		"type": "number",
		"value": "10"
	}
}
```
