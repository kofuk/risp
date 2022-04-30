#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <inttypes.h>
#include <stddef.h>

#define FLAG_ALWAYS_GC (1)

#define UNUSED(x) (void)x

typedef uint8_t u8;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;
typedef size_t usize;

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

typedef struct {
    char *in_name;
    FILE *infile;
    lex_state *state;
    token *tk;
} lexer;

typedef enum {
    T_CONS = 1,
    T_STRING,
    T_SYMBOL,
    T_KWSYMBOL,
    T_INT,
    T_FUNC,
    T_NATIVE_FUNC
} risp_type;

typedef struct risp_object risp_object;
typedef struct risp_env risp_env;

typedef risp_object *(*risp_native_func)(risp_env *env, risp_object *args, u32 caller_level);

struct risp_object {
    risp_type type;
    usize size;                // should be pow of sizeof(void *)
    struct risp_object *forwarding;
    union {
        struct {
            struct risp_object *car;
            struct risp_object *cdr;
        } cons;
        usize str_len;
        i64 integer;
        struct {
            struct risp_object *body;
            struct risp_object *arglist;
            u32 level;
        } func;
        risp_native_func native_func;
    } d;
    u8 str_data[];
};

typedef struct risp_vars {
    struct risp_object *vars;   // alist of symbol and its value.
    struct risp_vars *parent;
    struct risp_vars *prev;
} risp_vars;

// risp_eobject is a wrapper for risp_object to avoid ephemeral objects to be freed and keep track of them.
typedef struct risp_eobject {
    risp_object *o;
    struct risp_eobject *next;
} risp_eobject;

struct risp_env {
    void *heap;
    usize heap_len;
    usize heap_cap;
    risp_vars *var_list;        // last element of variable list
    risp_eobject *ephemeral;
    risp_object *obarray;       // interned symbols
    risp_object *error;
    u32 flags;
};

static inline usize copy_object(risp_object *free_ptr, risp_object *old_obj) {
    memcpy(free_ptr, old_obj, old_obj->size);
    old_obj->forwarding = free_ptr;
    return old_obj->size;
}

static void run_gc(risp_env *env) {
    void *new_heap = malloc(env->heap_cap);
    usize new_len = 0;
    risp_object *free_ptr = new_heap;

    for (risp_eobject *eo = env->ephemeral; eo; eo = eo->next) {
        if (eo->o == NULL) {
            continue;
        }

        if (eo->o->forwarding == NULL) {
            new_len += copy_object(free_ptr, eo->o);
            eo->o = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            eo->o = eo->o->forwarding;
        }
    }

    for (risp_vars *vars = env->var_list; vars != NULL; vars = vars->prev) {
        if (vars->vars->forwarding == NULL) {
            new_len += copy_object(free_ptr, vars->vars);
            vars->vars = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            vars->vars = vars->vars->forwarding;
        }
    }

    if (env->error != NULL) {
        if (env->error->forwarding == NULL) {
            new_len += copy_object(free_ptr, env->error);
            env->error = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            env->error = env->error->forwarding;
        }
    }

    if (env->obarray != NULL) {
        if (env->obarray->forwarding == NULL) {
            new_len += copy_object(free_ptr, env->obarray);
            env->obarray = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            env->obarray = env->obarray->forwarding;
        }
    }

    risp_object *scan_ptr = new_heap;
    while (scan_ptr < free_ptr) {
        switch (scan_ptr->type) {
        case T_CONS:
            if (scan_ptr->d.cons.car->forwarding == NULL) {
                new_len += copy_object(free_ptr, scan_ptr->d.cons.car);
                scan_ptr->d.cons.car = free_ptr;
                free_ptr = (void *)((char *)new_heap + new_len);
            } else {
                scan_ptr->d.cons.car = scan_ptr->d.cons.car->forwarding;
            }
            if (scan_ptr->d.cons.cdr->forwarding == NULL) {
                new_len += copy_object(free_ptr, scan_ptr->d.cons.cdr);
                scan_ptr->d.cons.cdr = free_ptr;
                free_ptr = (void *)((char *)new_heap + new_len);
            } else {
                scan_ptr->d.cons.cdr = scan_ptr->d.cons.cdr->forwarding;
            }
            break;

        case T_STRING:
            break;

        case T_SYMBOL:
            break;

        case T_KWSYMBOL:
            break;

        case T_INT:
            break;

        case T_FUNC:
            if (scan_ptr->d.func.arglist->forwarding == NULL) {
                new_len += copy_object(free_ptr, scan_ptr->d.func.arglist);
                scan_ptr->d.func.arglist = free_ptr;
                free_ptr = (void *)((char *)new_heap + new_len);
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

static risp_eobject *register_ephemeral_object(risp_env *env, risp_object *obj) {
    risp_eobject *eo = malloc(sizeof(risp_eobject));
    eo->o = obj;
    eo->next = env->ephemeral;
    return eo;
}

static void unregister_ephemeral_object(risp_env *env, risp_eobject *registered) {
    for (risp_eobject *eo = env->ephemeral, *prev = NULL; eo; eo = eo->next) {
        if (eo == registered) {
            if (prev == NULL) {
                env->ephemeral = eo;
            } else {
                prev->next = eo->next;
            }
            free(registered);
            return;
        }
        prev = eo;
    }
}

static void ephemeral_object_free_all(risp_env *env) {
    for (risp_eobject *eo = env->ephemeral, *next; eo; eo = next) {
        next = eo->next;
        free(eo);
    }
}

static risp_object *get_error(risp_env *env) {
    return env->error;
}

static void clear_error(risp_env *env) {
    env->error = NULL;
}

static inline usize align_to_word(usize size) {
    return (size + sizeof(void *) - 1) & ~(sizeof(void *) - 1);
}

static void ensure_allocatable(risp_env *env, usize size) {
    if ((env->flags & FLAG_ALWAYS_GC) || env->heap_cap - env->heap_len >= size) {
        return;
    }

    //TODO: more efficient way.

    run_gc(env);
    if (env->heap_cap - env->heap_len >= size) {
        return;
    }

    usize required_cap = env->heap_cap << 1;
    for (;;) {
        if (required_cap - env->heap_len >= size) {
            break;
        }
        required_cap <<= 1;
    }

    env->heap_cap = required_cap;
    run_gc(env);
}

static risp_object *alloc_str_like(risp_env *env, usize len) {
    usize str_offset = offsetof(risp_object, str_data);
    usize alloc_size;
    if (len <= sizeof(risp_object) - str_offset) {
        alloc_size = align_to_word(sizeof(risp_object));
    } else {
        alloc_size = align_to_word(str_offset + len);
    }

    ensure_allocatable(env, alloc_size);

    risp_object *r = (void *)((char *)env->heap + env->heap_len);
    r->size = alloc_size;
    r->forwarding = NULL;
    r->d.str_len = len;
    env->heap_len += alloc_size;
    return r;
}

static risp_object *alloc_object(risp_env *env) {
    usize size = align_to_word(sizeof(risp_object));
    ensure_allocatable(env, size);

    risp_object *r = (void *)((char *)env->heap + env->heap_len);
    r->size = size;
    r->forwarding = NULL;
    env->heap_len += size;
    return r;
}

static void signal_error(risp_env *env, risp_object *err) {
    env->error = err;
}

static void signal_error_s(risp_env *env, const char *msg) {
    size_t len = strlen(msg);
    risp_object *err = alloc_str_like(env, len);
    err->type = T_STRING;
    memcpy(err->str_data, msg, len);
    err->d.str_len = len;
    signal_error(env, err);
}

static void push_frame(risp_env *env, u32 caller_level, u32 callee_level) {
    risp_vars *parent_scope;

    if (caller_level < callee_level) {
        parent_scope = env->var_list;
    } else {
        u32 diff = caller_level - callee_level;
        parent_scope = env->var_list;
        for (u32 i = 0; i <= diff; ++i) {
            parent_scope = parent_scope->parent;
        }
    }

    risp_vars *vars = malloc(sizeof(risp_vars));
    vars->parent = parent_scope;
    vars->prev = env->var_list;
    env->var_list = vars;
}

static void env_init(risp_env *env) {
    env->heap_len = 0;
    env->heap_cap = 64 * 1024 * 1024;
    env->heap = malloc(env->heap_cap);
    env->var_list = NULL;
    env->ephemeral = NULL;
    env->obarray = NULL;
    env->error = NULL;
    env->flags = 0;

    if (strlen(getenv("RISP_ALWAYS_GC"))) {
        env->flags |= FLAG_ALWAYS_GC;
    }

    push_frame(env, 0, 1);
}

static risp_object *lookup_variable_cons(risp_env *env, risp_object *symbol) {
    assert(symbol->type == T_SYMBOL);

    for (risp_vars *target = env->var_list; target; target = target->parent) {
        for (risp_object *vars = target->vars; vars; vars = vars->d.cons.cdr) {
            assert(vars->type == T_CONS);

            risp_object *var = vars->d.cons.car;
            assert(var->type == T_CONS);

            if (symbol == var->d.cons.car) {
                return var;
            }
        }
    }

    return NULL;
}

static risp_object *lookup_symbol(risp_env *env, risp_object *symbol) {
    risp_object *cons = lookup_variable_cons(env, symbol);
    if (cons == NULL) {
        return NULL;
    }
    return cons->d.cons.cdr;
}

static void make_variable(risp_env *env, risp_vars *vars, risp_eobject *symbol, risp_eobject *value) {
    assert(symbol->o->type == T_SYMBOL);

    for (risp_object *var = vars->vars; var; var = var->d.cons.cdr) {
        assert(var->type == T_CONS);

        risp_object *var_cons = var->d.cons.car;
        assert(var_cons->type == T_CONS);
        assert(var_cons->d.cons.car->type == T_SYMBOL);

        if (symbol->o == var_cons->d.cons.car) {
            var_cons->d.cons.cdr = value->o;
            return;
        }
    }

    risp_eobject *list_element = register_ephemeral_object(env, alloc_object(env));
    list_element->o->type = T_CONS;
    list_element->o->d.cons.cdr = vars->vars;
    vars->vars = list_element->o;

    risp_object *cons = alloc_object(env);
    cons->type = T_CONS;
    cons->d.cons.car = symbol->o;
    cons->d.cons.cdr = value->o;

    list_element->o->d.cons.car = cons;

    unregister_ephemeral_object(env, list_element);
}

static void make_local_variable(risp_env *env, risp_eobject *symbol, risp_eobject *value) {
    make_variable(env, env->var_list, symbol, value);
}

static void make_global_variable(risp_env *env, risp_eobject *symbol, risp_eobject *value) {
    risp_vars *vars = env->var_list;
    while (vars->prev) {
        vars = vars->prev;
    }

    make_variable(env, vars, symbol, value);
}

static void scoped_set(risp_env *env, risp_eobject *symbol, risp_eobject *value) {
    risp_object *cons = lookup_variable_cons(env, symbol->o);
    if (cons == NULL) {
        make_global_variable(env, symbol, value);
        return;
    }
    cons->d.cons.cdr = value->o;
}

static risp_object *intern_symbol(risp_env *env, const char *name) {
    usize name_len = strlen(name);
    assert(name_len > 0);

    if (env->obarray != NULL) {
        for (risp_object *obj = env->obarray; obj; obj = obj->d.cons.cdr) {
            assert(obj->type == T_CONS);

            risp_object *sym = obj->d.cons.car;
            assert(sym->type == T_SYMBOL || sym->type == T_KWSYMBOL);

            if (sym->d.str_len == name_len) {
                if (!memcmp(sym->str_data, name, name_len)) {
                    return sym;
                }
            }
        }
    }

    risp_eobject *cons = register_ephemeral_object(env, alloc_object(env));
    cons->o->type = T_CONS;
    cons->o->d.cons.cdr = env->obarray;

    risp_object *sym = alloc_str_like(env, name_len);
    if (name[0] == ':') {
        sym->type = T_KWSYMBOL;
    } else {
        sym->type = T_SYMBOL;
    }
    memcpy(sym->str_data, name, name_len);
    cons->o->d.cons.car = sym;

    env->obarray = cons->o;

    unregister_ephemeral_object(env, cons);

    return sym;
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

static token *get_token(lexer *lex, risp_error *err) {
    if (lex->tk != NULL) {
        token *tk = lex->tk;
        lex->tk = NULL;
        return tk;
    }

    int c;
    bool inside_comment = false;
    for (;;) {
        c = fgetc(lex->infile);
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
    if (c == '(') {
        result->type = TK_LPAREN;
    } else if (c == ')') {
        result->type = TK_RPAREN;
    } else if (c == '.') {
        result->type = TK_DOT;
    } else if (c == '\'') {
        result->type = TK_QUOTE;
    } else if (c == '`') {
        result->type = TK_BACKQUOTE;
    } else if (c == ',') {
        bool is_unquote;
        c = fgetc(lex->infile);
        if (c == EOF) {
            is_unquote = true;
        } else if (c == '@') {
            is_unquote = false;
        } else {
            ungetc(c, lex->infile);
            is_unquote = true;
        }

        if (is_unquote) {
            result->type = TK_UNQUOTE;
        } else {
            result->type = TK_SPLICE;
        }
    } else if (c == '#') {
        c = fgetc(lex->infile);
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
    } else if (isdigit(c) || c == '-') {
        usize len = 1;
        usize cap = 4;
        char *text = malloc(sizeof(char) * cap);
        text[0] = c;

        char first_char = c;

        c = fgetc(lex->infile);
        if (first_char == '-') {
            if (c == EOF) {
                free(text);
                goto scan_as_sym;
            } else if (!isdigit(c)) {
                free(text);
                ungetc(c, lex->infile);
                c = '-';
                goto scan_as_sym;
            }
        } else {
            if (c != EOF && isdigit(c)) {
                text[1] = c;
                len++;
            } else {
                ungetc(c, lex->infile);
            }
        }

        for (;;) {
            c = fgetc(lex->infile);
            if (c == EOF) {
                break;
            }

            if (!isdigit(c)) {
                ungetc(c, lex->infile);
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
    }else if (c == '"') {
        usize len = 0;
        usize cap = 4;
        char *text = malloc(sizeof(char) * cap);
        for (;;) {
            c = fgetc(lex->infile);
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
    } else {
    scan_as_sym:;
        usize len = 1;
        usize cap = 4;
        char *text = malloc(sizeof(char) * cap);
        text[0] = c;
        for (;;) {
            c = fgetc(lex->infile);
            if (c == EOF) {
                break;
            }
            if (!(isalnum(c) || strchr("+-*/=<>:$@", c))) {
                ungetc(c, lex->infile);
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
    return result;
}

static inline void unget_token(lexer *lex, token *tk) {
    lex->tk = tk;
}

static risp_object *eval_exp(risp_env *env, risp_object *exp) {
    if (exp == NULL) {
        return NULL;
    } else if (exp->type == T_CONS) {
        if (exp->d.cons.car->type != T_SYMBOL) {
            signal_error_s(env, "void function");
            return NULL;
        }

        risp_object *func = lookup_symbol(env, exp->d.cons.car);
        if (func == NULL) {
            signal_error_s(env, "void function");
            return NULL;
        }

        if (func->type == T_NATIVE_FUNC) {
            return func->d.native_func(env, exp->d.cons.cdr, 0);
        } else if (func->type == T_FUNC) {
            //TODO: call functions in lisp world
        } else {
            signal_error_s(env, "void function");
        }
        return NULL;
    } else if (exp->type == T_STRING || exp->type == T_KWSYMBOL ||
               exp->type == T_INT || exp->type == T_FUNC || exp->type == T_NATIVE_FUNC) {
        return exp;
    } else if (exp->type == T_SYMBOL) {
        risp_object *obj = lookup_symbol(env, exp);
        if (obj == NULL) {
            signal_error_s(env, "void variable");
            return NULL;
        }
        return obj;
    }
    assert(false);
}

static risp_object *read_exp(lexer *lex, risp_error *err, risp_env *env);

// read whole expression assuming that LPAREN is already read.
static risp_object *read_sexp_inner(lexer *lex, risp_error *err, risp_env *env) {
    risp_eobject *root = register_ephemeral_object(env, alloc_object(env));
    root->o->type = T_CONS;

    token *tk = get_token(lex, err);
    if (tk == NULL) {
        goto lex_err;
    }
    if (tk->type == TK_RPAREN) {
        token_free(tk);
        unregister_ephemeral_object(env, root);
        return NULL;
    }
    unget_token(lex, tk);

    risp_object *obj = read_exp(lex, err, env);
    if (err->has_error) {
        unregister_ephemeral_object(env, root);
        return NULL;
    }
    root->o->d.cons.car = obj;

    tk = get_token(lex, err);
    if (tk == NULL) {
        goto lex_err;
    }
    if (tk->type == TK_RPAREN) {
        token_free(tk);
        risp_object *r = root->o;
        unregister_ephemeral_object(env, root);

        r->d.cons.cdr = NULL;
        return r;
    } else if (tk->type == TK_DOT) {
        // this is cons.
        token_free(tk);
        risp_object *obj = read_exp(lex, err, env);
        if (err->has_error) {
            unregister_ephemeral_object(env, root);
            return NULL;
        }
        root->o->d.cons.cdr = obj;

        tk = get_token(lex, err);
        if (tk == NULL) {
            goto lex_err;
        }
        if (tk->type != TK_RPAREN) {
            unget_token(lex, tk);

            unregister_ephemeral_object(env, root);

            err->has_error = err;
            err->message = "RPAREN expected";

            return NULL;
        }

        risp_object *result = root->o;
        unregister_ephemeral_object(env, root);
        return result;
    }
    unget_token(lex, tk);

    risp_eobject *prev = register_ephemeral_object(env, root->o);

    for (;;) {
        token *tk = get_token(lex, err);
        if (tk == NULL) {
            unregister_ephemeral_object(env, prev);
            unregister_ephemeral_object(env, root);

            err->has_error = err;
            err->message = "unexpected end of file";

            return NULL;
        } else if (tk->type == TK_RPAREN) {
            token_free(tk);
            break;
        }

        unget_token(lex, tk);

        risp_eobject *cons = register_ephemeral_object(env, alloc_object(env));
        cons->o->type = T_CONS;

        risp_object *obj = read_exp(lex, err, env);
        if (err->has_error) {
            unregister_ephemeral_object(env, cons);
            unregister_ephemeral_object(env, prev);
            unregister_ephemeral_object(env, root);
            return NULL;
        }

        cons->o->d.cons.car = obj;

        prev->o->d.cons.cdr = cons->o;
        unregister_ephemeral_object(env, prev);
        prev = cons;
    }

    risp_object *first = root->o;
    unregister_ephemeral_object(env, root);
    unregister_ephemeral_object(env, prev);
    return first;

lex_err:
    unregister_ephemeral_object(env, root);

    err->has_error = err;
    err->message = "unexpected end of file";

    return NULL;
}

static risp_object *read_exp(lexer *lex, risp_error *err, risp_env *env) {
    token *tk = get_token(lex, err);
    if (tk == NULL) {
        return NULL;
    }

    if (tk->type == TK_LPAREN) {
        token_free(tk);
        return read_sexp_inner(lex, err, env);
    } else if (tk->type == TK_QUOTE) {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env));
        risp_eobject *equote = register_ephemeral_object(env, intern_symbol(env, "quote"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *quote = equote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, equote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->type = T_CONS;
        cons->d.cons.car = quote;
        cons->d.cons.cdr = inner;
        return cons;
    } else if (tk->type == TK_BACKQUOTE) {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env));
        risp_eobject *ebackquote = register_ephemeral_object(env, intern_symbol(env, "backquote"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *backquote = ebackquote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, ebackquote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->type = T_CONS;
        cons->d.cons.car = backquote;
        cons->d.cons.cdr = inner;
        return cons;
    } else if (tk->type == TK_UNQUOTE) {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env));
        risp_eobject *eunquote = register_ephemeral_object(env, intern_symbol(env, "unquote"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *unquote = eunquote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, eunquote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->type = T_CONS;
        cons->d.cons.car = unquote;
        cons->d.cons.cdr = inner;
        return cons;
    } else if (tk->type == TK_SPLICE) {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env));
        risp_eobject *esplice = register_ephemeral_object(env, intern_symbol(env, "splice"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *splice = esplice->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, esplice);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->type = T_CONS;
        cons->d.cons.car = splice;
        cons->d.cons.cdr = inner;
        return cons;
    } else if (tk->type == TK_FUNQUOTE) {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env));
        risp_eobject *efunction = register_ephemeral_object(env, intern_symbol(env, "function"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *function = efunction->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, efunction);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->type = T_CONS;
        cons->d.cons.car = function;
        cons->d.cons.cdr = inner;
        return cons;
    } else if (tk->type == TK_INT) {
        risp_object *r = alloc_object(env);
        r->type = T_INT;
        r->d.integer = strtoll(tk->text, NULL, 0);
        token_free(tk);
        return r;
    } else if (tk->type == TK_STRING) {
        size_t len = strlen(tk->text);
        risp_object *r = alloc_str_like(env, len);
        r->type = T_STRING;
        memcpy(r->str_data, tk->text, len);
        token_free(tk);
        return r;
    } else if (tk->type == TK_KWSYMBOL) {
        risp_object *r = intern_symbol(env, tk->text);
        token_free(tk);
        return r;
    } else if (tk->type == TK_SYMBOL) {
        if (!strcmp(tk->text, "nil")) {
            token_free(tk);
            return NULL;
        }
        risp_object *r = intern_symbol(env, tk->text);
        token_free(tk);
        return r;
    } else {
        err->has_error = true;
        err->line = lex->state->line;
        err->column = lex->state->column;
        err->message = "unexpected token";

        return NULL;
    }

    assert(false);
    return NULL;
}

static void repr_object(risp_env *env, risp_object *obj);

static void repr_list(risp_env *env, risp_object *obj) {
    if (obj == NULL) {
        putchar(')');
    } else if (obj->type == T_CONS) {
        putchar(' ');
        repr_object(env, obj->d.cons.car);
        repr_list(env, obj->d.cons.cdr);
    } else {
        fputs(" . ", stdout);
        repr_object(env, obj);
        putchar(')');
    }
}

static void repr_object(risp_env *env, risp_object *obj) {
    if (obj == NULL) {
        fputs("nil", stdout);
        return;
    }

    risp_eobject *eobj = register_ephemeral_object(env, obj);
    switch (obj->type) {
    case T_CONS:
        if (eobj->o->d.cons.car == intern_symbol(env, "quote")) {
            putchar('\'');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->d.cons.cdr);
        } else if (eobj->o->d.cons.car == intern_symbol(env, "function")) {
            putchar('#');
            putchar('\'');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->d.cons.cdr);
        } else if (eobj->o->d.cons.car == intern_symbol(env, "unquote")) {
            putchar(',');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->d.cons.cdr);
        } else if (eobj->o->d.cons.car == intern_symbol(env, "splice")) {
            putchar(',');
            putchar('@');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->d.cons.cdr);
        } else {
            putchar('(');
            repr_object(env, eobj->o->d.cons.car);
            repr_list(env, obj->d.cons.cdr);
        }
        break;

    case T_STRING:
        printf("\"%.*s\"", (int)eobj->o->d.str_len, eobj->o->str_data);
        break;

    case T_SYMBOL:
    case T_KWSYMBOL:
        printf("%.*s", (int)eobj->o->d.str_len, eobj->o->str_data);
        break;

    case T_INT:
        printf("%ld", eobj->o->d.integer);
        break;

    case T_FUNC:
        fputs("<func>", stderr);
        break;

    case T_NATIVE_FUNC:
        printf("<native_func@%p>", ((union {void *p; risp_native_func func;} *)&eobj->o->d.native_func)->p);
        break;
    }
}

static i32 read_and_eval(lexer *lex, risp_env *env) {
    risp_error err;
    risp_error_init(&err);
    risp_object *sexp = read_exp(lex, &err, env);
    if (sexp == NULL) {
        if (err.has_error) {
            fprintf(stderr, "%s: %d: %d: %s\n", lex->in_name, err.line, err.column, err.message);
            return -1;
        }
        return 0;
    }

    risp_object *result = eval_exp(env, sexp);

    risp_object *runtime_err = get_error(env);
    if (runtime_err != NULL) {
        fputs("Fatal error: ", stderr);
        repr_object(env, runtime_err);
        putchar('\n');
        clear_error(env);

        return -1;
    }

    repr_object(env, result);
    putchar('\n');

    return 1;
}

static risp_object *Fprint(risp_env *env, risp_object *args, u32 caller_level) {
    UNUSED(caller_level);

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != NULL) {
        risp_object *target = eval_exp(env, cur->o->d.cons.car);
        if (get_error(env) != NULL) {
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        if (target->type == T_INT) {
            printf("%ld\n", target->d.integer);
        } else if (target->type == T_STRING) {
            printf("%.*s\n", (int)target->d.str_len, target->str_data);
        } else {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "argument type must be stings or ints");
            return NULL;
        }

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->d.cons.cdr);
    }

    unregister_ephemeral_object(env, cur);

    return NULL;
}

static i64 list_length(risp_env *env, risp_object *list) {
    i64 result = 0;
    for (risp_object *arg = list; arg != NULL; arg = arg->d.cons.cdr) {
        ++result;

        risp_object *next = arg->d.cons.cdr;
        if (next != NULL && next->type != T_CONS) {
            signal_error_s(env, "argument must be a list or string");
            return 0;
        }
    }
    return result;
}

static risp_object *Flength(risp_env *env, risp_object *args, u32 caller_level) {
    UNUSED(caller_level);

    if (list_length(env, args) != 1) {
        signal_error_s(env, "just 1 argument expected");
        return NULL;
    }

    i64 result = 0;
    risp_object *target = eval_exp(env, args->d.cons.car);

    if (get_error(env) != NULL) {
        return NULL;
    }

    if (target == NULL) {
        result = 0;
    } else if (target->type == T_CONS) {
        result = list_length(env, target);
    } else if (target->type == T_STRING) {
        result = target->d.str_len;
    }

    risp_object *r = alloc_object(env);
    r->type = T_INT;
    r->d.integer = result;

    return r;
}

static risp_object *Fplus(risp_env *env, risp_object *args, u32 caller_level) {
    UNUSED(caller_level);

    risp_eobject *result = register_ephemeral_object(env, alloc_object(env));
    result->o->type = T_INT;
    result->o->d.integer = 0;

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != NULL) {
        risp_object *target = eval_exp(env, cur->o->d.cons.car);
        if (get_error(env) != NULL) {
            unregister_ephemeral_object(env, cur);
            unregister_ephemeral_object(env, result);
            return NULL;
        }

        if (target->type == T_INT) {
            result->o->d.integer += target->d.integer;
        } else {
            unregister_ephemeral_object(env, cur);
            unregister_ephemeral_object(env, result);

            signal_error_s(env, "argument type must be int");
            return NULL;
        }

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->d.cons.cdr);
    }

    unregister_ephemeral_object(env, cur);
    risp_object *r = result->o;
    unregister_ephemeral_object(env, result);
    return r;
}

static risp_object *Fquote(risp_env *env, risp_object *args, u32 caller_level) {
    UNUSED(env);
    UNUSED(caller_level);
    return args;
}

static risp_object *Fsetq(risp_env *env, risp_object *args, u32 caller_level) {
    UNUSED(caller_level);

    if (list_length(env, args) % 2 != 0) {
        signal_error_s(env, "wrong argument count");
        return NULL;
    }

    risp_eobject *cur = register_ephemeral_object(env, args);
    risp_eobject *last = register_ephemeral_object(env, NULL);
    while (cur->o != NULL) {
        if (cur->o->d.cons.car->type != T_SYMBOL) {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "variable must be symbol");
            return NULL;
        }
        risp_eobject *sym = register_ephemeral_object(env, cur->o->d.cons.car);
        risp_object *val = eval_exp(env, cur->o->d.cons.cdr->d.cons.car);
        if (get_error(env) != NULL) {
            unregister_ephemeral_object(env, sym);
            unregister_ephemeral_object(env, last);
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        risp_eobject *e_val = register_ephemeral_object(env, val);
        scoped_set(env, sym, e_val);
        unregister_ephemeral_object(env, last);
        unregister_ephemeral_object(env, sym);

        last = e_val;

        risp_eobject *next = register_ephemeral_object(env, cur->o->d.cons.cdr->d.cons.cdr);
        unregister_ephemeral_object(env, cur);
        cur = next;
    }

    unregister_ephemeral_object(env, cur);
    risp_object *r = last->o;
    unregister_ephemeral_object(env, last);
    return r;
}

static inline void register_native_function(risp_env *env, const char *name, risp_native_func func) {
    risp_eobject *func_var = register_ephemeral_object(env, alloc_object(env));
    risp_eobject *sym = register_ephemeral_object(env, intern_symbol(env, name));

    func_var->o->type = T_NATIVE_FUNC;
    func_var->o->d.native_func = func;

    make_global_variable(env, sym, func_var);

    unregister_ephemeral_object(env, sym);
    unregister_ephemeral_object(env, func_var);
}

static void init_native_functions(risp_env *env) {
    register_native_function(env, "+", &Fplus);
    register_native_function(env, "function", &Fquote);
    register_native_function(env, "length", &Flength);
    register_native_function(env, "print", &Fprint);
    register_native_function(env, "quote", &Fquote);
    register_native_function(env, "setq", &Fsetq);
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
    lex_state_init(&state);
    lexer lex = {
        .in_name = argv[1],
        .infile = infile,
        .state = &state,
        .tk = NULL
    };
    risp_env env;
    env_init(&env);
    init_native_functions(&env);

    int exit_code = 0;

    for (;;) {
        i32 status = read_and_eval(&lex, &env);
        if (status <= 0) {
            exit_code = -status;
            goto clean;
        }
    }

clean:
    free(env.heap);
    ephemeral_object_free_all(&env);

    fclose(infile);

    return exit_code;
}
