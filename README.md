# tinylr
[简体中文](README-zh.md)

a tiny lalr generator inspired by Charles Baker's lalr

generate json format ast from grammar

glr parser can handle the shift-reduce conflict and reduce-reduce conflict grammar easily

# Grammar Example
[grammar file](test/grammar.json.y)

## JSON Parser Example
```c++
#include "parser.h"
int main() {
    const char *string = "import aaa.bbb.ccc;\n"
                         "int<int, value> main() {\n"
                         "  a < b > c;\n"
                         "  test(a, b, c);"
                         "}";
    GLRParser<> parser(true);
    parser.reset(string, string + strlen(string));
    parser.parse();
    if (parser.accept()) {
        auto value = parser.value();
        std::cout << value.dump(4) << std::endl;
    }
    return 0;
}
```

## Output

```json
{
    "id": 12,
    "kind": "program",
    "position": {
        "column": 0,
        "line": 0
    },
    "value": [
        {
            "id": 8,
            "items": [
                "aaa",
                "bbb",
                "ccc"
            ],
            "kind": "import",
            "position": {
                "column": 0,
                "line": 0
            }
        },
        {
            "block": [
                {
                    "kind": "merge",
                    "value": [
                        {
                            "id": 18,
                            "kind": "expr",
                            "position": {
                                "column": 2,
                                "line": 2
                            },
                            "value": {
                                "id": 5,
                                "kind": "binary",
                                "left": {
                                    "id": 5,
                                    "kind": "variable",
                                    "name": "a",
                                    "position": {
                                        "column": 2,
                                        "line": 2
                                    }
                                },
                                "op": "<",
                                "position": {
                                    "column": 2,
                                    "line": 2
                                },
                                "right": {
                                    "id": 5,
                                    "kind": "binary",
                                    "left": {
                                        "id": 5,
                                        "kind": "variable",
                                        "name": "b",
                                        "position": {
                                            "column": 6,
                                            "line": 2
                                        }
                                    },
                                    "op": ">",
                                    "position": {
                                        "column": 6,
                                        "line": 2
                                    },
                                    "right": {
                                        "id": 5,
                                        "kind": "variable",
                                        "name": "c",
                                        "position": {
                                            "column": 10,
                                            "line": 2
                                        }
                                    }
                                }
                            }
                        },
                        {
                            "id": 16,
                            "kind": "variable_declare",
                            "name": "c",
                            "position": {
                                "column": 2,
                                "line": 2
                            },
                            "type": {
                                "args": [
                                    {
                                        "id": 15,
                                        "kind": "type_specifier",
                                        "position": {
                                            "column": 6,
                                            "line": 2
                                        },
                                        "type": "b"
                                    }
                                ],
                                "id": 15,
                                "kind": "type_specifier",
                                "position": {
                                    "column": 2,
                                    "line": 2
                                },
                                "type": "a"
                            }
                        }
                    ]
                },
                {
                    "id": 18,
                    "kind": "expr",
                    "position": {
                        "column": 2,
                        "line": 3
                    },
                    "value": {
                        "args": {
                            "id": 1,
                            "kind": "argument_list",
                            "position": {
                                "column": 7,
                                "line": 3
                            },
                            "value": [
                                {
                                    "id": 5,
                                    "kind": "variable",
                                    "name": "a",
                                    "position": {
                                        "column": 7,
                                        "line": 3
                                    }
                                },
                                {
                                    "id": 5,
                                    "kind": "variable",
                                    "name": "b",
                                    "position": {
                                        "column": 10,
                                        "line": 3
                                    }
                                },
                                {
                                    "id": 5,
                                    "kind": "variable",
                                    "name": "c",
                                    "position": {
                                        "column": 13,
                                        "line": 3
                                    }
                                }
                            ]
                        },
                        "id": 5,
                        "kind": "invoke",
                        "name": "test",
                        "position": {
                            "column": 2,
                            "line": 3
                        }
                    }
                }
            ],
            "id": 7,
            "kind": "function_declare",
            "name": "main",
            "params": null,
            "position": {
                "column": 0,
                "line": 1
            },
            "type": {
                "args": [
                    {
                        "id": 15,
                        "kind": "type_specifier",
                        "position": {
                            "column": 4,
                            "line": 1
                        },
                        "type": "int"
                    },
                    {
                        "id": 15,
                        "kind": "type_specifier",
                        "position": {
                            "column": 9,
                            "line": 1
                        },
                        "type": "value"
                    }
                ],
                "id": 15,
                "kind": "type_specifier",
                "position": {
                    "column": 0,
                    "line": 1
                },
                "type": "int"
            }
        }
    ]
}
```

# Generator AST Directly from grammar

