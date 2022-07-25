#include <ctype.h>
#include <string.h>

#include "parse.h"

void token_free(token *tk) {
    if (tk != NULL) {
        free(tk->text);
        free(tk);
    }
}

void risp_error_init(risp_error *err) {
    err->has_error = false;
    err->message = NULL;
    err->line = 0;
    err->column = 0;
}

void lex_state_init(lex_state *state) {
    state->line = 1;
    state->column = 0;
}

token *get_token(lexer *lex, risp_error *err) {
    if (lex->tk != NULL) {
        token *tk = lex->tk;
        lex->tk = NULL;
        return tk;
    }

    int c;
    bool inside_comment = false;
    for (;;) {
        c = lex->read_char(lex);
        if (c == EOF) {
            return NULL;
        }
        if (c == ' ' || c == '\t' || c == '\r') {
            lex->state->column++;
        } else if (c == '\n') {
            lex->state->line++;
            lex->state->column = 0;
            inside_comment = false;
        } else if (c == ';') {
            inside_comment = true;
            lex->state->column++;
        } else if (inside_comment) {
            lex->state->column++;
        } else {
            lex->state->column++;
            break;
        }
    }

    token *result = malloc(sizeof(token));
    result->text = NULL;
    switch (c) {
    case '(':
        result->type = TK_LPAREN;
        break;

    case ')':
        result->type = TK_RPAREN;
        break;

    case '.':
        result->type = TK_DOT;
        break;

    case '\'':
        result->type = TK_QUOTE;
        break;

    case '`':
        result->type = TK_BACKQUOTE;
        break;

    case ',': {
        bool is_unquote;
        c = lex->read_char(lex);
        if (c == EOF) {
            is_unquote = true;
        } else if (c == '@') {
            is_unquote = false;
        } else {
            lex->unread_char(c, lex);
            is_unquote = true;
        }

        if (is_unquote) {
            result->type = TK_UNQUOTE;
        } else {
            result->type = TK_SPLICE;
        }
        break;
    }
    case '#':
        c = lex->read_char(lex);
        if (c == EOF) {
            free(result);

            err->has_error = true;
            err->message = "Unexpected EOF";
            err->line = lex->state->line;
            err->column = lex->state->column;
            return NULL;
        } else if (c != '\'') {
            free(result);

            err->has_error = true;
            err->message = "Unexpected char";
            err->line = lex->state->line;
            err->column = lex->state->column;
            return NULL;
        }

        lex->state->column++;

        result->type = TK_FUNQUOTE;
        break;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '-': {
        usize len = 1;
        usize cap = 4;
        char *text = malloc(sizeof(char) * cap);
        text[0] = c;

        char first_char = c;

        c = lex->read_char(lex);
        if (first_char == '-') {
            if (c == EOF) {
                free(text);
                goto scan_as_sym;
            } else if (!isdigit(c)) {
                free(text);
                lex->unread_char(c, lex);
                c = '-';
                goto scan_as_sym;
            }
        } else {
            if (c != EOF && isdigit(c)) {
                text[1] = c;
                len++;
            } else {
                lex->unread_char(c, lex);
            }
        }

        for (;;) {
            c = lex->read_char(lex);
            if (c == EOF) {
                break;
            }

            if (!isdigit(c)) {
                lex->unread_char(c, lex);
                break;
            }

            lex->state->column++;

            if (len + 2 >= cap) {
                cap <<= 1;
                text = realloc(text, sizeof(char) * cap);
            }
            text[len] = (char)c;
            len++;
        }
        text[len] = '\0';

        result->type = TK_INT;
        result->text = text;
        break;
    }

    case '"': {
        usize len = 0;
        usize cap = 4;
        char *text = malloc(sizeof(char) * cap);
        for (;;) {
            c = lex->read_char(lex);
            if (c == EOF) {
                free(result);
                free(text);

                err->has_error = true;
                err->message = "Unexpected EOF";
                err->line = lex->state->line;
                err->column = lex->state->column;
                return NULL;
            }
            if (c == '\n') {
                lex->state->line++;
                lex->state->column = 0;
            } else {
                lex->state->column++;
                if (c == '"') {
                    break;
                }
            }

            if (len + 2 >= cap) {
                cap <<= 1;
                text = realloc(text, sizeof(char) * cap);
            }
            text[len] = (char)c;
            len++;
        }
        text[len] = '\0';

        result->type = TK_STRING;
        result->text = text;
        break;
    }

    default: {
    scan_as_sym:;
        usize len = 1;
        usize cap = 4;
        char *text = malloc(sizeof(char) * cap);
        text[0] = c;
        for (;;) {
            c = lex->read_char(lex);
            if (c == EOF) {
                break;
            }
            if (!(isalnum(c) || strchr("+-*/=<>:$@", c))) {
                lex->unread_char(c, lex);
                break;
            }

            lex->state->column++;

            if (len + 2 >= cap) {
                cap <<= 1;
                text = realloc(text, sizeof(char) * cap);
            }
            text[len] = (char)c;
            len++;
        }
        text[len] = '\0';

        if (text[0] == ':') {
            result->type = TK_KWSYMBOL;
        } else {
            result->type = TK_SYMBOL;
        }
        result->text = text;
    }
    }

    return result;
}

void unget_token(lexer *lex, token *tk) {
    token_free(lex->tk);
    lex->tk = tk;
}
