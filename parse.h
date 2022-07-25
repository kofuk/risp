#ifndef RISP_LEX_H
#define RISP_LEX_H

#include <stdbool.h>
#include <stdio.h>

#include "risp.h"

typedef enum {
    TK_LPAREN,
    TK_RPAREN,
    TK_DOT,
    TK_QUOTE,
    TK_BACKQUOTE,
    TK_UNQUOTE,
    TK_SPLICE,
    TK_FUNQUOTE,
    TK_INT,
    TK_STRING,
    TK_KWSYMBOL,
    TK_SYMBOL
} token_type;

typedef struct {
    token_type type;
    char *text;
} token;

typedef struct {
    bool has_error;
    char *message;
    int line;
    int column;
} risp_error;

typedef struct {
    int line;
    int column;
} lex_state;

typedef struct lexer {
    char *in_name;
    FILE *infile;
    lex_state *state;
    token *tk;

    int (*read_char)(struct lexer *);
    int (*unread_char)(int, struct lexer *);
#ifdef HAVE_READLINE
    char *rl_prompt;
    char *rl_line;
    size_t rl_cursor;
    int rl_unget;
    bool rl_nul_read;
#endif
    bool repl;
} lexer;

void token_free(token *tk);
void risp_error_init(risp_error *err);
void lex_state_init(lex_state *state);
token *get_token(lexer *lex, risp_error *err);
void unget_token(lexer *lex, token *tk);

#endif
