//
// Created by Alex on 8/23/2020.
//

#ifndef TINYLALR_PARSER_H
#define TINYLALR_PARSER_H
struct LexerState;
struct LexerTransition {
    int begin;
    int end;
    LexerState *state;
};
struct LexerState {
    LexerTransition *first;
    LexerTransition *last;
    int symbol;
};

struct ReduceAction {
    const char *field;
    const char *value;
};
struct ParserState;
struct ParserSymbol {
    int type;
    int symbol;
    const char *text;
};
struct ParserTransition {
    int type;
    int symbol;
    ParserState *state;
    int reduce_symbol;
    int reduce_length;
    ReduceAction *first_action;
    ReduceAction *last_action;
};
struct ParserState {
    ParserTransition *first;
    ParserTransition *last;
};

extern LexerState LexerStates[];
extern LexerTransition LexerTransitions[];
extern ParserState ParserStates[];
extern ReduceAction ParserActions[];
extern ParserSymbol ParserSymbols[];
extern ParserTransition ParserTransitions[];

#endif //TINYLALR_PARSER_H
