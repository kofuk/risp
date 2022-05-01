#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include "parse.h"
#include "rt.h"
#include "primitive.h"

#define FLAG_ALWAYS_GC (1)

risp_object Qnil = {
    .type = T_CONS,
    .size = sizeof(risp_object),
    .d.cons.car = NULL,
    .d.cons.cdr = NULL,
};

risp_object Qt = {
    .type = T_CONS,
    .size = sizeof(risp_object),
    .d.cons.car = NULL,
    .d.cons.cdr = NULL,
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
        if (eo->o == NULL || eo->o == &Qnil) {
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
        if (vars->vars == &Qnil) {
            continue;
        }

        if (vars->vars->forwarding == NULL) {
            new_len += copy_object(free_ptr, vars->vars);
            vars->vars = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            vars->vars = vars->vars->forwarding;
        }
    }

    if (env->error != &Qnil) {
        if (env->error->forwarding == NULL) {
            new_len += copy_object(free_ptr, env->error);
            env->error = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            env->error = env->error->forwarding;
        }
    }

    if (env->obarray != &Qnil) {
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
            if (scan_ptr->d.cons.car != &Qnil) {
                if (scan_ptr->d.cons.car->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->d.cons.car);
                    scan_ptr->d.cons.car = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->d.cons.car = scan_ptr->d.cons.car->forwarding;
                }
            }
            if (scan_ptr->d.cons.cdr != &Qnil) {
                if (scan_ptr->d.cons.cdr->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->d.cons.cdr);
                    scan_ptr->d.cons.cdr = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->d.cons.cdr = scan_ptr->d.cons.cdr->forwarding;
                }
            }
            break;

        case T_FUNC:
            if (scan_ptr->d.func.arglist != &Qnil) {
                if (scan_ptr->d.func.arglist->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->d.func.arglist);
                    scan_ptr->d.func.arglist = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->d.func.arglist = scan_ptr->d.func.arglist->forwarding;
                }
            }
            if (scan_ptr->d.func.body != &Qnil) {
                if (scan_ptr->d.func.body->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->d.func.body);
                    scan_ptr->d.func.body = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->d.func.body = scan_ptr->d.func.body->forwarding;
                }
            }
            break;

        case T_STRING:
        case T_SYMBOL:
        case T_KWSYMBOL:
        case T_INT:
        case T_NATIVE_FUNC:
        case T_NATIVE_HANDLE:
            break;
        }
    }

    memset(env->heap, 0, env->heap_len);
    free(env->heap);
    env->heap = new_heap;
    env->heap_len = new_len;
}

risp_eobject *register_ephemeral_object(risp_env *env, risp_object *obj) {
    risp_eobject *eo = malloc(sizeof(risp_eobject));
    eo->o = obj;
    eo->next = env->ephemeral;
    env->ephemeral = eo;
    return eo;
}

void unregister_ephemeral_object(risp_env *env, risp_eobject *registered) {
    for (risp_eobject *eo = env->ephemeral, *prev = NULL; eo != NULL; eo = eo->next) {
        if (eo == registered) {
            if (prev == NULL) {
                env->ephemeral = eo->next;
            } else {
                prev->next = eo->next;
            }
            free(registered);
            return;
        }
        prev = eo;
    }
}

risp_object *get_error(risp_env *env) { return env->error; }

void clear_error(risp_env *env) { env->error = &Qnil; }

static inline usize align_to_word(usize size) { return (size + sizeof(void *) - 1) & ~(sizeof(void *) - 1); }

static void ensure_allocatable(risp_env *env, usize size) {
    if ((env->flags & FLAG_ALWAYS_GC) || env->heap_cap - env->heap_len >= size) {
        return;
    }

    // TODO: more efficient way.

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

risp_object *alloc_str_like(risp_env *env, usize len) {
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

risp_object *alloc_object(risp_env *env) {
    usize size = align_to_word(sizeof(risp_object));
    ensure_allocatable(env, size);

    risp_object *r = (void *)((char *)env->heap + env->heap_len);
    r->size = size;
    r->forwarding = NULL;
    env->heap_len += size;
    return r;
}

void signal_error(risp_env *env, risp_object *err) { env->error = err; }

void signal_error_s(risp_env *env, const char *msg) {
    size_t len = strlen(msg);
    risp_object *err = alloc_str_like(env, len);
    err->type = T_STRING;
    memcpy(err->str_data, msg, len);
    err->d.str_len = len;
    signal_error(env, err);
}

static void push_var_frame(risp_env *env, u32 caller_level, u32 callee_level) {
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
    vars->vars = &Qnil;
    vars->parent = parent_scope;
    vars->prev = env->var_list;
    env->var_list = vars;
}

void var_frame_free_all(risp_env *env) {
    for (risp_vars *vars = env->var_list; vars != NULL;) {
        risp_vars *prev = vars->prev;
        free(vars);
        vars = prev;
    }
}

void env_init(risp_env *env) {
    env->heap_len = 0;
    env->heap_cap = 64 * 1024 * 1024;
    env->heap = malloc(env->heap_cap);
    env->var_list = NULL;
    env->ephemeral = NULL;
    env->obarray = &Qnil;
    env->error = &Qnil;
    env->flags = 0;

    if (getenv("RISP_ALWAYS_GC") != NULL) {
        env->flags |= FLAG_ALWAYS_GC;
    }

    push_var_frame(env, 0, 1);
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

    return &Qnil;
}

static risp_object *lookup_symbol(risp_env *env, risp_object *symbol) {
    risp_object *cons = lookup_variable_cons(env, symbol);
    if (cons == &Qnil) {
        return &Qnil;
    }
    return cons->d.cons.cdr;
}

static void make_variable(risp_env *env, risp_vars *vars, risp_eobject *symbol, risp_eobject *value) {
    assert(symbol->o->type == T_SYMBOL);

    for (risp_object *var = vars->vars; var != &Qnil; var = var->d.cons.cdr) {
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

void make_local_variable(risp_env *env, risp_eobject *symbol, risp_eobject *value) {
    make_variable(env, env->var_list, symbol, value);
}

void make_global_variable(risp_env *env, risp_eobject *symbol, risp_eobject *value) {
    risp_vars *vars = env->var_list;
    while (vars->prev) {
        vars = vars->prev;
    }

    make_variable(env, vars, symbol, value);
}

void scoped_set(risp_env *env, risp_eobject *symbol, risp_eobject *value) {
    risp_object *cons = lookup_variable_cons(env, symbol->o);
    if (cons == &Qnil) {
        make_global_variable(env, symbol, value);
        return;
    }
    cons->d.cons.cdr = value->o;
}

static risp_object *intern_symbol(risp_env *env, const char *name) {
    usize name_len = strlen(name);
    assert(name_len > 0);

    if (env->obarray != &Qnil) {
        for (risp_object *obj = env->obarray; obj != &Qnil; obj = obj->d.cons.cdr) {
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

risp_object *eval_exp(risp_env *env, risp_object *exp) {
    if (exp == &Qnil) {
        return &Qnil;
    } else if (exp == &Qt) {
        return &Qt;
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
            // TODO: call functions in lisp world
        } else {
            signal_error_s(env, "void function");
        }
        return NULL;
    } else if (exp->type == T_STRING || exp->type == T_KWSYMBOL || exp->type == T_INT || exp->type == T_FUNC ||
               exp->type == T_NATIVE_FUNC) {
        return exp;
    } else if (exp->type == T_SYMBOL) {
        risp_object *obj = lookup_symbol(env, exp);
        if (obj == &Qnil) {
            signal_error_s(env, "void variable");
            return NULL;
        }
        return obj;
    }
    assert(false);
    return &Qnil;
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
        return &Qnil;
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

        r->d.cons.cdr = &Qnil;
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

    prev->o->d.cons.cdr = &Qnil;
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
    lex->rl_prompt = "... ";

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
            return &Qnil;
        } else if (!strcmp(tk->text, "t")) {
            token_free(tk);
            return &Qt;
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
    if (obj == &Qnil) {
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
    if (obj == &Qnil) {
        fputs("nil", stdout);
        return;
    } else if (obj == &Qt) {
        fputs("t", stdout);
        return;
    }

    switch (obj->type) {
    case T_CONS: {
        risp_eobject *eobj = register_ephemeral_object(env, obj);
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
    }

    case T_STRING:
        printf("\"%.*s\"", (int)obj->d.str_len, obj->str_data);
        break;

    case T_SYMBOL:
    case T_KWSYMBOL:
        printf("%.*s", (int)obj->d.str_len, obj->str_data);
        break;

    case T_INT:
        printf("%ld", obj->d.integer);
        break;

    case T_FUNC:
        fputs("<func>", stderr);
        break;

    case T_NATIVE_FUNC:
        printf("<native_func@%p>", ((union {
                                        void *p;
                                        risp_native_func func;
                                    } *)&obj->d.native_func)
                                       ->p);
        break;

    case T_NATIVE_HANDLE:
        printf("<native_handle@%p>", obj->d.native_handle);
        break;
    }
}

i32 read_and_eval(lexer *lex, risp_env *env) {
    risp_error err;
    risp_error_init(&err);
    risp_object *sexp = read_exp(lex, &err, env);
    lex->rl_prompt = ">>> ";
    if (sexp == NULL) {
        if (err.has_error) {
            fprintf(stderr, "%s: %d: %d: %s\n", lex->in_name, err.line, err.column, err.message);
            return lex->repl ? 1 : -1;
        }
        return 0;
    }

    risp_object *result = eval_exp(env, sexp);

    risp_object *runtime_err = get_error(env);
    if (runtime_err != &Qnil) {
        fputs("Fatal error: ", stderr);
        repr_object(env, runtime_err);
        putchar('\n');
        clear_error(env);

        return lex->repl ? 1 : -1;
    }

    if (lex->repl) {
        repr_object(env, result);
        putchar('\n');
    }

    return 1;
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

void init_native_functions(risp_env *env) {
    register_native_function(env, "+", RISP_FUNC(plus));
    register_native_function(env, "function", RISP_FUNC(quote));
    register_native_function(env, "length", RISP_FUNC(length));
    register_native_function(env, "print", RISP_FUNC(print));
    register_native_function(env, "quote", RISP_FUNC(quote));
    register_native_function(env, "setq", RISP_FUNC(setq));
}
