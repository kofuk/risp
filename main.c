#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>

typedef enum {
    TK_LPAREN,
    TK_RPAREN,
    TK_QUOTE,
    TK_BACKQUOTE,
    TK_UNQUOTE,
    TK_SPLICE,
    TK_FUNQUOTE,
    TK_INT,
    TK_STRING,
    TK_KWSYMBOL,
    TK_SYMBOL,
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

static void token_type_print(token_type type, FILE *out) {
    char const *repr;
    switch (type) {
    case TK_LPAREN:
        repr = "TK_LPAREN";
        break;
    case TK_RPAREN:
        repr = "TK_RPAREN";
        break;
    case TK_QUOTE:
        repr = "TK_QUOTE";
        break;
    case TK_BACKQUOTE:
        repr = "TK_BACKQUOTE";
        break;
    case TK_UNQUOTE:
        repr = "TK_UNQUOTE";
        break;
    case TK_SPLICE:
        repr = "TK_SPLICE";
        break;
    case TK_FUNQUOTE:
        repr = "TK_FUNQUOTE";
        break;
    case TK_INT:
        repr = "TK_INT";
        break;
    case TK_STRING:
        repr = "TK_STRING";
        break;
    case TK_KWSYMBOL:
        repr = "TK_KWSYMBOL";
        break;
    case TK_SYMBOL:
        repr = "TK_SYMBOL";
        break;
    default:
        repr = "<unknown>";
        break;
    }
    fputs(repr, out);
}

static void token_free(token *tk) {
    free(tk->text);
    free(tk);
}

static void risp_error_init(risp_error *err) {
    err->has_error = false;
    err->message = NULL;
    err->line = 0;
    err->column = 0;
}

static void lex_state_init(lex_state *state) {
    state->line = 1;
    state->column = 0;
}

static token *next_token(lex_state *state, FILE *infile, risp_error *err) {
    int c;
    bool inside_comment = false;
    for (;;) {
        c = fgetc(infile);
        if (c == EOF) {
            return NULL;
        }
        if (c == ' ' || c == '\t' || c == '\r') {
            state->column++;
        } else if (c == '\n') {
            state->line++;
            state->column = 0;
            inside_comment = false;
        } else if (c == ';') {
            inside_comment = true;
            state->column++;
        } else if (inside_comment) {
            state->column++;
        } else {
            break;
        }
    }

    token *result = malloc(sizeof(token));
    result->text = NULL;
    if (c == '(') {
        result->type = TK_LPAREN;
    } else if (c == ')') {
        result->type = TK_RPAREN;
    } else if (c == '\'') {
        result->type = TK_QUOTE;
    } else if (c == '`') {
        result->type = TK_BACKQUOTE;
    } else if (c == ',') {
        bool is_unquote;
        c = fgetc(infile);
        if (c == EOF) {
            is_unquote = true;
        } else if (c == '@') {
            is_unquote = false;
        } else {
            ungetc(c, infile);
            is_unquote = true;
        }

        if (is_unquote) {
            result->type = TK_UNQUOTE;
        } else {
            result->type = TK_SPLICE;
        }
    } else if (c == '#') {
        c = fgetc(infile);
        if (c == EOF) {
            free(result);

            err->has_error = true;
            err->message = "Unexpected EOF";
            err->line = state->line;
            err->column = state->column;
            return NULL;
        } else if (c != '\'') {
            free(result);

            err->has_error = true;
            err->message = "Unexpected char";
            err->line = state->line;
            err->column = state->column;
            return NULL;
        }

        state->column++;

        result->type = TK_FUNQUOTE;
    } else if (isdigit(c) || c == '-') {
        size_t len = 1;
        size_t cap = 4;
        char *text = malloc(sizeof(char) * cap);
        text[0] = c;

        char first_char = c;

        c = fgetc(infile);
        if (first_char == '-') {
            if (c == EOF) {
                free(text);
                goto scan_as_sym;
            } else if (!isdigit(c)) {
                free(text);
                ungetc(c, infile);
                c = '-';
                goto scan_as_sym;
            }
        } else {
            if (c != EOF && isdigit(c)) {
                text[1] = c;
                len++;
            }
        }

        for (;;) {
            c = fgetc(infile);
            if (c == EOF) {
                break;
            }

            if (!isdigit(c)) {
                ungetc(c, infile);
                break;
            }

            state->column++;

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
    }else if (c == '"') {
        size_t len = 0;
        size_t cap = 4;
        char *text = malloc(sizeof(char) * cap);
        for (;;) {
            c = fgetc(infile);
            if (c == EOF) {
                free(result);
                free(text);

                err->has_error = true;
                err->message = "Unexpected EOF";
                err->line = state->line;
                err->column = state->column;
                return NULL;
            }
            if (c == '\n') {
                state->line++;
                state->column = 0;
            } else {
                state->column++;
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
    } else {
    scan_as_sym:;
        size_t len = 1;
        size_t cap = 4;
        char *text = malloc(sizeof(char) * cap);
        text[0] = c;
        for (;;) {
            c = fgetc(infile);
            if (c == EOF) {
                break;
            }
            if (!(isalnum(c) || strchr("+-*/=<>:$@", c))) {
                ungetc(c, infile);
                break;
            }

            state->column++;

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
    return result;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fputs("usage: risp <infile>\n", stderr);
        return 1;
    }

    FILE *infile = fopen(argv[1], "r");
    if (!infile) {
        perror("Failed to open infile");
        return 1;
    }

    lex_state state;
    risp_error err;
    for (;;) {
        lex_state_init(&state);
        risp_error_init(&err);

        token *tk = next_token(&state, infile, &err);
        if (tk == NULL) {
            if (err.has_error) {
                fprintf(stderr, "%s: %d: %d: %s\n", argv[1], err.line, err.column, err.message);
                return 1;
            } else {
                break;
            }
        }

        token_type_print(tk->type, stdout);
        if (tk->text) {
            printf(": %s", tk->text);
        }
        puts("");
        token_free(tk);
    }

    fclose(infile);
}
