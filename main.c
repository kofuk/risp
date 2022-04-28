#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <inttypes.h>
#include <stddef.h>

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

typedef enum {
    T_CONS,
    T_STRING,
    T_SYMBOL,
    T_INT,
    T_FUNC,
    T_NATIVE_FUNC
} risp_type;

struct risp_object;

typedef struct risp_object *(*risp_native_func)(struct risp_object *);

typedef struct risp_object {
    risp_type type;
    size_t size;                // should be pow of sizeof(void *)
    struct risp_object *forwarding;
    union {
        struct {
            struct risp_object *car;
            struct risp_object *cdr;
        } cons;
        uint8_t str[0];
        uint64_t integer;
        struct {
            struct risp_object *body;
            struct risp_object *arglist;
            uint32_t level;
        } func;
        struct {
            risp_native_func func;
            uint64_t nargs;
        } native_func;
    } d;
} risp_object;

typedef struct risp_vars {
    struct risp_object *vars;   // alist of symbol and its value.
    struct risp_vars *parent;
    struct risp_vars *prev;
} risp_vars;

typedef struct {
    void *heap;
    size_t heap_len;
    size_t heap_cap;
    risp_vars *var_list;        // last element of variable list
} risp_env;

static inline size_t copy_object(risp_object *free_ptr, risp_object *old_obj) {
    memcpy(free_ptr, old_obj, old_obj->size);
    old_obj->forwarding = free_ptr;
    return old_obj->size;
}

static void run_gc(risp_env *env) {
    void *new_heap = malloc(env->heap_cap);
    size_t new_len = 0;
    risp_object *free_ptr = new_heap;

    for (risp_vars *vars = env->var_list; vars != NULL; vars = vars->prev) {
        for (risp_object *cons = vars->vars; cons != NULL; cons = cons->d.cons.cdr) {
            assert(cons->type == T_CONS);
            assert(cons->forwarding == NULL);

            new_len += copy_object(free_ptr, cons->d.cons.car);
            cons->d.cons.car->forwarding = free_ptr;
            cons->d.cons.car = free_ptr;
            free_ptr = new_heap + new_len;
        }
    }

    risp_object *scan_ptr = new_heap;
    while (scan_ptr < free_ptr) {
        switch (scan_ptr->type) {
        case T_CONS:
            if (scan_ptr->d.cons.car->forwarding == NULL) {
                new_len += copy_object(free_ptr, scan_ptr->d.cons.car);
                scan_ptr->d.cons.car->forwarding = free_ptr;
                scan_ptr->d.cons.car = free_ptr;
                free_ptr = new_heap + new_len;
            } else {
                scan_ptr->d.cons.car = scan_ptr->d.cons.car->forwarding;
            }
            if (scan_ptr->d.cons.cdr->forwarding == NULL) {
                new_len += copy_object(free_ptr, scan_ptr->d.cons.cdr);
                scan_ptr->d.cons.cdr->forwarding = free_ptr;
                scan_ptr->d.cons.cdr = free_ptr;
                free_ptr = new_heap + new_len;
            } else {
                scan_ptr->d.cons.cdr = scan_ptr->d.cons.cdr->forwarding;
            }
            break;

        case T_STRING:
            break;

        case T_SYMBOL:
            break;

        case T_INT:
            break;

        case T_FUNC:
            if (scan_ptr->d.func.arglist->forwarding == NULL) {
                new_len += copy_object(free_ptr, scan_ptr->d.func.arglist);
                scan_ptr->d.cons.car->forwarding = free_ptr;
                scan_ptr->d.func.arglist = free_ptr;
                free_ptr = new_heap + new_len;
            } else {
                scan_ptr->d.func.arglist = scan_ptr->d.func.arglist->forwarding;
            }
            break;

        case T_NATIVE_FUNC:
            break;
        }
    }

    free(env->heap);
    env->heap = new_heap;
    env->heap_len = new_len;
}

static inline size_t align_to_word(size_t size) {
    return (size + sizeof(void *)) & ~(sizeof(void *) - 1);
}

static void ensure_allocatable(risp_env *env, size_t size) {
    if (env->heap_cap - env->heap_len >= size) {
        return;
    }

    //TODO: more efficient way.

    run_gc(env);
    if (env->heap_cap - env->heap_len >= size) {
        return;
    }

    size_t required_cap = env->heap_cap << 1;
    for (;;) {
        if (required_cap - env->heap_len >= size) {
            break;
        }
        required_cap <<= 1;
    }

    env->heap_cap = required_cap;
    run_gc(env);
}

static risp_object *alloc_string(risp_env *env, size_t len) {
    size_t str_offset = offsetof(risp_object, d.str);
    size_t alloc_size;
    if (sizeof(risp_object) - str_offset <= len) {
        alloc_size = align_to_word(sizeof(risp_object));
    } else {
        alloc_size = align_to_word(str_offset + len);
    }

    ensure_allocatable(env, alloc_size);

    risp_object *r = env->heap + env->heap_len;
    env->heap_len += alloc_size;
    return r;
}

static risp_object *alloc_object(risp_env *env) {
    size_t size = align_to_word(sizeof(risp_object));
    ensure_allocatable(env, size);

    risp_object *r = env->heap + env->heap_len;
    env->heap_len += size;
    return r;
}

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
