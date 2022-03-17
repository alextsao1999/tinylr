# tinylr
## 特点
* 迷你的parser生成器, 生成文件只有两个, (parser.h, parser.cpp)
* 支持GLR, LALR解析器, 支持的语法范围广
* 自动生成语法树和visitor, 减少劳动

## 使用
产生式与bison, yacc相似, 不过为了美观我们的产生式使用箭头 `->` (可能看起来也不美观, 不过后期可以改 ε=ε=

```
优先级定义:
%none  无结合性
%left  左结合
%right 右结合

%start 定义开始的非终结符
%whitespace 定义空白符, 词法分析器自动跳过

"" 内的为正则表达式
'' 内的为字面量
```

## 例子
### 四则运算
```goyacc
%start expr; // 定义开始的非终结符
%left '+' '-' '*' '/';  // 这里分别为 + - * / 定义了结合性, 并且让从左到右分配优先级
expr ->
    expr '+' expr {left: $1, right: $3, op: '+'}
    | expr '-' expr {left: $1, right: $3, op: '-'}
    | expr '*' expr {left: $1, right: $3, op: '*'}
    | expr '/' expr {left: $1, right: $3, op: '/'}
    | factor $1 // 将$1取出
    ;
factor ->
    '(' expr ')'
    | "[0-9]+" @1 // @n 表示取出token文本 
    ;
```

[更复杂的例子](test/grammar.json.y)
