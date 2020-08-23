# tinylalr
a tiny lalr generator inspired by Charles Baker's lalr

generate json format ast from grammar
```c++
int main() {
    LALRGrammarParser lalr(
            "%left '+' '-' '*' '/';"
            "%start expr;"
            "%whitespace \"[ \n\r\t]+\";"
            "expr -> expr '+' expr {left:$1, right:$3, op:'+'}"
            "       | expr '-' expr {left:$1, right:$3, op:'-'}"
            "       | expr '*' expr {left:$1, right:$3, op:'*'}"
            "       | expr '/' expr {left:$1, right:$3, op:'/'}"
            "       | '(' expr ')' {compound:$2}"
            ";"
            "expr -> \"[0-9]+\" {type:'number', number:@1};"
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
    const char *string = "1234 + (342 * 2)*2+3 + 22";
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
{"left":{"left":{"left":{"number":1234,"type":"number"},"op":"+","right":{"left":{"compound":{"left":{"number":342,"type":"number"},"op":"*","right":{"number":2,"type":"number"}}},"op":"*","right":{"number":2,"type":"number"}}},"op":"+","right":{"number":3,"type":"number"}},"op":"+","right":{"number":22,"type":"number"}}
```
