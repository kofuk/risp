#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include "parse.h"
#include "primitive.h"
#include "rt.h"

#define FLAG_ALWAYS_GC (1)

/**
 * Value represents `nil' in the Risp world.
 */
risp_object Qnil = {
    .type = T_CONS,
    .size = sizeof(risp_object),
    .car = NULL,
    .cdr = NULL,
};

/**
 * Value represents `t' in the Risp world.
 */
risp_object Qt = {
    .type = T_CONS,
    .size = sizeof(risp_object),
    .car = NULL,
    .cdr = NULL,
};

/**
 * Copy `old_obj' to `free_ptr' and sets `forwarding' pointer.
 * The return value is copied size in byte.
 */
static inline usize copy_object(risp_object *free_ptr, risp_object *old_obj) {
    memcpy(free_ptr, old_obj, old_obj->size);
    old_obj->forwarding = free_ptr;
    return old_obj->size;
}

/**
 * Runs GC.
 * You must assume that all objects are invalid after running GC.
 * To keep track of the object through the GC, you can `register_ephemeral_object'.
 */
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
            if (scan_ptr->car != &Qnil) {
                if (scan_ptr->car->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->car);
                    scan_ptr->car = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->car = scan_ptr->car->forwarding;
                }
            }
            if (scan_ptr->cdr != &Qnil) {
                if (scan_ptr->cdr->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->cdr);
                    scan_ptr->cdr = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->cdr = scan_ptr->cdr->forwarding;
                }
            }
            break;

        case T_FUNC:
            if (scan_ptr->func.arglist != &Qnil) {
                if (scan_ptr->func.arglist->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->func.arglist);
                    scan_ptr->func.arglist = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->func.arglist = scan_ptr->func.arglist->forwarding;
                }
            }
            if (scan_ptr->func.body != &Qnil) {
                if (scan_ptr->func.body->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->func.body);
                    scan_ptr->func.body = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->func.body = scan_ptr->func.body->forwarding;
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

/**
 * Register `obj' as an ephemeral object and keep track of the object and avoid freed by subsequent GC.
 * `obj' of NULL or Qnil is permitted, and this function returns valid ephemeral object.
 *
 * Object returned by this function MUST be freed by passing it to `unregister_ephemeral_object' or
 * it causes memory leak in both C world and Risp world.
 */
risp_eobject *register_ephemeral_object(risp_env *env, risp_object *obj) {
    risp_eobject *eo = malloc(sizeof(risp_eobject));
    eo->o = obj;
    eo->next = env->ephemeral;
    env->ephemeral = eo;
    return eo;
}

/**
 * Unregisters ephemeral object.
 * After passing object to this function, values are not defended from GC.
 */
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

/**
 * Get error in Risp world.
 * After evaluating any Risp objects, you must check that error is `Qnil'.
 */
risp_object *get_error(risp_env *env) { return env->error; }

/**
 * Clears error state.
 */
void clear_error(risp_env *env) { env->error = &Qnil; }

/**
 * Rounds up the `size' to multiply of word size.
 */
static inline usize align_to_word(usize size) { return (size + sizeof(void *) - 1) & ~(sizeof(void *) - 1); }

/**
 * Ensure that specified `size' can be allocated from the heap.
 *
 * Note: This function can run GC.
 */
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

/**
 * Allocate string-like value from the heap.
 *
 * Note: This function can run GC.
 */
risp_object *alloc_str_like(risp_env *env, risp_type type, usize len) {
    usize str_offset = offsetof(risp_object, str_data);
    usize alloc_size;
    if (len <= sizeof(risp_object) - str_offset) {
        alloc_size = align_to_word(sizeof(risp_object));
    } else {
        alloc_size = align_to_word(str_offset + len);
    }

    ensure_allocatable(env, alloc_size);

    risp_object *r = (void *)((char *)env->heap + env->heap_len);
    r->type = type;
    r->size = alloc_size;
    r->forwarding = NULL;
    r->str_len = len;
    env->heap_len += alloc_size;
    return r;
}

/**
 * Allocates fixed-sized (usual) Risp object.
 *
 * Note: This function can run GC.
 */
risp_object *alloc_object(risp_env *env, risp_type type) {
    usize size = align_to_word(sizeof(risp_object));
    ensure_allocatable(env, size);

    risp_object *r = (void *)((char *)env->heap + env->heap_len);
    r->type = type;
    r->size = size;
    r->forwarding = NULL;
    env->heap_len += size;
    return r;
}

/**
 * Set error.
 */
void signal_error(risp_env *env, risp_object *err) { env->error = err; }

/**
 * Set error to `msg' converting to Risp object.
 * This function is a convenient wrapper for `signal_error'.
 *
 * Note: This function can run GC.
 */
void signal_error_s(risp_env *env, const char *msg) {
    size_t len = strlen(msg);
    risp_object *err = alloc_str_like(env, T_STRING, len);
    memcpy(err->str_data, msg, len);
    err->str_len = len;
    signal_error(env, err);
}

/**
 * Push a new variable table to stack.
 */
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

/**
 * Free all pushed variable table.
 */
void var_frame_free_all(risp_env *env) {
    for (risp_vars *vars = env->var_list; vars != NULL;) {
        risp_vars *prev = vars->prev;
        free(vars);
        vars = prev;
    }
}

/**
 * Initialize a new Risp execution environment.
 */
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

/**
 * Lookup variable by `symbol' and returnes cons containing the value.
 * `symbol' must be a interned symbol.
 */
static risp_object *lookup_variable_cons(risp_env *env, risp_object *symbol) {
    assert(symbol->type == T_SYMBOL);

    for (risp_vars *target = env->var_list; target != NULL; target = target->parent) {
        for (risp_object *vars = target->vars; vars != &Qnil; vars = vars->cdr) {
            assert(vars->type == T_CONS);

            risp_object *var = vars->car;
            assert(var->type == T_CONS);

            if (symbol == var->car) {
                return var;
            }
        }
    }

    return &Qnil;
}

/**
 * Lookup variable by `symbol'
 * `symbol' must be a interned symbol.
 */
risp_object *lookup_symbol(risp_env *env, risp_object *symbol) {
    risp_object *cons = lookup_variable_cons(env, symbol);
    if (cons == &Qnil) {
        return &Qnil;
    }
    return cons->cdr;
}

/**
 * Bind the `value' to `symbol' in variable table `vars'.
 *
 * Note: This function can run GC.
 */
static void make_variable(risp_env *env, risp_vars *vars, risp_object *symbol, risp_object *value) {
    assert(symbol->type == T_SYMBOL);

    for (risp_object *var = vars->vars; var != &Qnil; var = var->cdr) {
        assert(var->type == T_CONS);

        risp_object *var_cons = var->car;
        assert(var_cons->type == T_CONS);
        assert(var_cons->car->type == T_SYMBOL);

        if (symbol == var_cons->car) {
            var_cons->cdr = value;
            return;
        }
    }

    risp_eobject *esymbol = register_ephemeral_object(env, symbol);
    risp_eobject *evalue = register_ephemeral_object(env, value);

    risp_eobject *list_element = register_ephemeral_object(env, alloc_object(env, T_CONS));
    list_element->o->cdr = vars->vars;
    vars->vars = list_element->o;

    risp_object *cons = alloc_object(env, T_CONS);
    cons->car = esymbol->o;
    cons->cdr = evalue->o;

    list_element->o->car = cons;

    unregister_ephemeral_object(env, list_element);
    unregister_ephemeral_object(env, evalue);
    unregister_ephemeral_object(env, esymbol);
}

/**
 * Bind the `value' to `symbol' in the most-local variable table.
 *
 * Note: This function can run GC.
 */
void make_local_variable(risp_env *env, risp_object *symbol, risp_object *value) {
    make_variable(env, env->var_list, symbol, value);
}

/**
 * Bind the `value' to `symbol' as a global variable.
 *
 * Note: This function can run GC.
 */
void make_global_variable(risp_env *env, risp_object *symbol, risp_object *value) {
    risp_vars *vars = env->var_list;
    while (vars->prev) {
        vars = vars->prev;
    }

    make_variable(env, vars, symbol, value);
}

/**
 * Bind the `value' to `symbol' in most recently declared place in variable stack.
 * If no declaration found, it makes global binding.
 *
 * Note: This function can run GC.
 */
void scoped_set(risp_env *env, risp_object *symbol, risp_object *value) {
    risp_object *cons = lookup_variable_cons(env, symbol);
    if (cons == &Qnil) {
        make_global_variable(env, symbol, value);
        return;
    }
    cons->cdr = value;
}

/**
 * Intern or lookup the symbol named by `name'.
 *
 * Note: This function can run GC.
 */
risp_object *intern_symbol(risp_env *env, const char *name) {
    usize name_len = strlen(name);

    if (env->obarray != &Qnil) {
        for (risp_object *obj = env->obarray; obj != &Qnil; obj = obj->cdr) {
            assert(obj->type == T_CONS);

            risp_object *sym = obj->car;
            assert(sym->type == T_SYMBOL || sym->type == T_KWSYMBOL);

            if (sym->str_len == name_len) {
                if (!memcmp(sym->str_data, name, name_len)) {
                    return sym;
                }
            }
        }
    }

    risp_eobject *cons = register_ephemeral_object(env, alloc_object(env, T_CONS));
    cons->o->cdr = env->obarray;

    risp_type type = name[0] == ':' ? T_KWSYMBOL : T_SYMBOL;
    risp_object *sym = alloc_str_like(env, type, name_len);
    memcpy(sym->str_data, name, name_len);
    cons->o->car = sym;

    env->obarray = cons->o;

    unregister_ephemeral_object(env, cons);

    return sym;
}

/**
 * Evaluate anything and return the result.
 *
 * Note: This function can run GC.
 */
risp_object *eval_exp(risp_env *env, risp_object *exp, u32 caller_level) {
    if (exp == &Qnil) {
        return &Qnil;
    } else if (exp == &Qt) {
        return &Qt;
    }

    switch (exp->type) {
    case T_CONS: {
        if (exp->car->type != T_SYMBOL) {
            signal_error_s(env, "void function");
            return NULL;
        }

        risp_object *func = lookup_symbol(env, exp->car);
        if (func == NULL) {
            signal_error_s(env, "void function");
            return NULL;
        }

        if (func->type == T_NATIVE_FUNC) {
            return func->native_func(env, exp->cdr, caller_level);
        } else if (func->type == T_FUNC) {
            // TODO: call functions in lisp world
        } else {
            signal_error_s(env, "void function");
        }
        return NULL;
    }

    case T_STRING:
    case T_KWSYMBOL:
    case T_INT:
    case T_FUNC:
    case T_NATIVE_FUNC:
    case T_NATIVE_HANDLE:
        return exp;

    case T_SYMBOL: {
        risp_object *obj = lookup_symbol(env, exp);
        if (obj == &Qnil) {
            signal_error_s(env, "void variable");
            return NULL;
        }
        return obj;
    }
    }

    assert(false);
    return &Qnil;
}

static risp_object *read_exp(lexer *lex, risp_error *err, risp_env *env);

/**
 * Read whole expression assuming that LPAREN is already read.
 *
 * Note: This function can run GC.
 */
static risp_object *read_sexp_inner(lexer *lex, risp_error *err, risp_env *env) {
    risp_eobject *root = register_ephemeral_object(env, alloc_object(env, T_CONS));

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
    root->o->car = obj;

    tk = get_token(lex, err);
    if (tk == NULL) {
        goto lex_err;
    }
    if (tk->type == TK_RPAREN) {
        token_free(tk);
        risp_object *r = root->o;
        unregister_ephemeral_object(env, root);

        r->cdr = &Qnil;
        return r;
    } else if (tk->type == TK_DOT) {
        // this is cons.
        token_free(tk);
        risp_object *obj = read_exp(lex, err, env);
        if (err->has_error) {
            unregister_ephemeral_object(env, root);
            return NULL;
        }
        root->o->cdr = obj;

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

        risp_eobject *cons = register_ephemeral_object(env, alloc_object(env, T_CONS));
        cons->o->type = T_CONS;

        risp_object *obj = read_exp(lex, err, env);
        if (err->has_error) {
            unregister_ephemeral_object(env, cons);
            unregister_ephemeral_object(env, prev);
            unregister_ephemeral_object(env, root);
            return NULL;
        }

        cons->o->car = obj;

        prev->o->cdr = cons->o;
        unregister_ephemeral_object(env, prev);
        prev = cons;
    }

    prev->o->cdr = &Qnil;
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

/**
 * Read anything.
 *
 * Note: This function can run GC.
 */
static risp_object *read_exp(lexer *lex, risp_error *err, risp_env *env) {
    token *tk = get_token(lex, err);
    if (tk == NULL) {
        return NULL;
    }
#if HAVE_READLINE
    lex->rl_prompt = "... ";
#endif

    switch (tk->type) {
    case TK_LPAREN:
        token_free(tk);
        return read_sexp_inner(lex, err, env);

    case TK_QUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *equote = register_ephemeral_object(env, intern_symbol(env, "quote"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *quote = equote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, equote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = quote;
        cons->cdr = inner;
        return cons;
    }

    case TK_BACKQUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *ebackquote = register_ephemeral_object(env, intern_symbol(env, "backquote"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *backquote = ebackquote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, ebackquote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = backquote;
        cons->cdr = inner;
        return cons;
    }

    case TK_UNQUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *eunquote = register_ephemeral_object(env, intern_symbol(env, "unquote"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *unquote = eunquote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, eunquote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = unquote;
        cons->cdr = inner;
        return cons;
    }

    case TK_SPLICE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *esplice = register_ephemeral_object(env, intern_symbol(env, "splice"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *splice = esplice->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, esplice);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = splice;
        cons->cdr = inner;
        return cons;
    }

    case TK_FUNQUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *efunction = register_ephemeral_object(env, intern_symbol(env, "function"));
        risp_object *inner = read_exp(lex, err, env);
        risp_object *function = efunction->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, efunction);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = function;
        cons->cdr = inner;
        return cons;
    }

    case TK_INT: {
        risp_object *r = alloc_object(env, T_INT);
        r->integer = strtoll(tk->text, NULL, 0);
        token_free(tk);
        return r;
    }

    case TK_STRING: {
        size_t len = strlen(tk->text);
        risp_object *r = alloc_str_like(env, T_STRING, len);
        memcpy(r->str_data, tk->text, len);
        token_free(tk);
        return r;
    }

    case TK_KWSYMBOL: {
        risp_object *r = intern_symbol(env, tk->text);
        token_free(tk);
        return r;
    }

    case TK_SYMBOL: {
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
    }

    default:
        token_free(tk);

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

/**
 * Print representation of the list.
 */
static void repr_list(risp_env *env, risp_object *obj) {
    if (obj == &Qnil) {
        putchar(')');
    } else if (obj->type == T_CONS) {
        putchar(' ');
        repr_object(env, obj->car);
        repr_list(env, obj->cdr);
    } else {
        fputs(" . ", stdout);
        repr_object(env, obj);
        putchar(')');
    }
}

/**
 * Print representation of the object.
 */
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
        if (eobj->o->car == intern_symbol(env, "quote")) {
            putchar('\'');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->cdr);
        } else if (eobj->o->car == intern_symbol(env, "function")) {
            putchar('#');
            putchar('\'');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->cdr);
        } else if (eobj->o->car == intern_symbol(env, "unquote")) {
            putchar(',');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->cdr);
        } else if (eobj->o->car == intern_symbol(env, "splice")) {
            putchar(',');
            putchar('@');
            obj = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, obj->cdr);
        } else {
            putchar('(');
            repr_object(env, eobj->o->car);
            repr_list(env, obj->cdr);
        }
        break;
    }

    case T_STRING:
        printf("\"%.*s\"", (int)obj->str_len, obj->str_data);
        break;

    case T_SYMBOL:
    case T_KWSYMBOL:
        printf("%.*s", (int)obj->str_len, obj->str_data);
        break;

    case T_INT:
        printf("%ld", obj->integer);
        break;

    case T_FUNC:
        fputs("<func>", stderr);
        break;

    case T_NATIVE_FUNC:
        printf("<native_func@%p>", ((union {
                                        void *p;
                                        risp_native_func func;
                                    } *)&obj->native_func)
                                       ->p);
        break;

    case T_NATIVE_HANDLE:
        printf("<native_handle@%p>", obj->native_handle);
        break;
    }
}

/**
 * Read any Risp expression and evaluate.
 * If this is REPL mode, print returned value.
 *
 * Positive return value reports success state, and
 * non-positive value reports that the interpreter wants to exit by the negated value.
 *
 * Note: This function can run GC.
 */
i32 read_and_eval(lexer *lex, risp_env *env) {
    risp_error err;
    risp_error_init(&err);

#ifdef HAVE_READLINE
    char *prompt_orig = lex->rl_prompt;
#endif
    risp_object *sexp = read_exp(lex, &err, env);
#ifdef HAVE_READLINE
    lex->rl_prompt = prompt_orig;
#endif
    if (sexp == NULL) {
        if (err.has_error) {
            fprintf(stderr, "%s: %d: %d: %s\n", lex->in_name, err.line, err.column, err.message);
            return lex->repl ? 1 : -1;
        }
        return 0;
    }

    risp_object *result = eval_exp(env, sexp, 1);

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

/**
 * Register given native primitives to global variable table.
 *
 * Note: This function can run GC.
 */
static inline void register_native_function(risp_env *env, const char *name, risp_native_func func) {
    risp_object *func_var = alloc_object(env, T_NATIVE_FUNC);
    risp_object *sym = intern_symbol(env, name);

    func_var->native_func = func;

    make_global_variable(env, sym, func_var);
}

/**
 * Register all native primitives to global variable table.
 *
 * Note: This function can run GC.
 */
void init_native_functions(risp_env *env) {
    register_native_function(env, "+", RISP_FUNC(plus));
    register_native_function(env, "-", RISP_FUNC(minus));
    register_native_function(env, "*", RISP_FUNC(multiply));
    register_native_function(env, "/", RISP_FUNC(divide));
    register_native_function(env, "defun", RISP_FUNC(defun));
    register_native_function(env, "eq", RISP_FUNC(eq));
    register_native_function(env, "function", RISP_FUNC(quote));
    register_native_function(env, "intern", RISP_FUNC(intern));
    register_native_function(env, "length", RISP_FUNC(length));
    register_native_function(env, "make-symbol", RISP_FUNC(make_symbol));
    register_native_function(env, "print", RISP_FUNC(print));
    register_native_function(env, "quote", RISP_FUNC(quote));
    register_native_function(env, "setq", RISP_FUNC(setq));
}
