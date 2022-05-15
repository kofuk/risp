#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "parse.h"
#include "primitive.h"
#include "rt.h"

#define FLAG_ALWAYS_GC (1)

static risp_object internal_object_nil = {
    .type = T_SYMBOL,
    .size = sizeof(risp_object),
    .str_len = 0,
};

static risp_object internal_object_t = {
    .type = T_SYMBOL,
    .size = sizeof(risp_object),
    .str_len = 0,
};

/**
 * Value represents `nil' in the Risp world.
 */
risp_object *Qnil = &internal_object_nil;

/**
 * Value represents `t' in the Risp world.
 */
risp_object *Qt = &internal_object_t;

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
        if (eo->o == NULL || eo->o == Qnil || eo->o == Qt) {
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
        if (vars->vars == Qnil) {
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

    if (env->error != Qnil && env->error != Qt) {
        if (env->error->forwarding == NULL) {
            new_len += copy_object(free_ptr, env->error);
            env->error = free_ptr;
            free_ptr = (void *)((char *)new_heap + new_len);
        } else {
            env->error = env->error->forwarding;
        }
    }

    if (env->obarray != Qnil && env->obarray != Qt) {
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
            if (scan_ptr->car != Qnil && scan_ptr->car != Qt) {
                if (scan_ptr->car->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->car);
                    scan_ptr->car = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->car = scan_ptr->car->forwarding;
                }
            }
            if (scan_ptr->cdr != Qnil && scan_ptr->cdr != Qt) {
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
            if (scan_ptr->func.arglist != Qnil && scan_ptr->func.arglist != Qt) {
                if (scan_ptr->func.arglist->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->func.arglist);
                    scan_ptr->func.arglist = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->func.arglist = scan_ptr->func.arglist->forwarding;
                }
            }
            if (scan_ptr->func.body != Qnil && scan_ptr->func.body != Qt) {
                if (scan_ptr->func.body->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->func.body);
                    scan_ptr->func.body = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->func.body = scan_ptr->func.body->forwarding;
                }
            }
            break;

        case T_MACRO:
            if (scan_ptr->macro.arglist != Qnil && scan_ptr->macro.arglist != Qt) {
                if (scan_ptr->macro.arglist->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->macro.arglist);
                    scan_ptr->macro.arglist = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->macro.arglist = scan_ptr->macro.arglist->forwarding;
                }
            }
            if (scan_ptr->macro.body != Qnil && scan_ptr->macro.body != Qt) {
                if (scan_ptr->macro.body->forwarding == NULL) {
                    new_len += copy_object(free_ptr, scan_ptr->macro.body);
                    scan_ptr->macro.body = free_ptr;
                    free_ptr = (void *)((char *)new_heap + new_len);
                } else {
                    scan_ptr->macro.body = scan_ptr->macro.body->forwarding;
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
    if (registered == NULL) {
        return;
    }

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
void clear_error(risp_env *env) { env->error = Qnil; }

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
static void push_var_frame(risp_env *env, risp_vars *vars) { env->var_list = vars; }

/**
 * Prepare a new variable table.
 * Returned value must pass to `push_var_frame'.
 */
static risp_vars *make_var_frame_inner(risp_env *env) {
    risp_vars *vars = malloc(sizeof(risp_vars));
    vars->vars = Qnil;
    vars->parent = env->var_list;
    vars->prev = env->var_list;

    return vars;
}

/**
 * Create a new variable table which can only global variable.
 * Returned value must pass to `push_var_frame'.
 */
static risp_vars *make_var_frame_isolated(risp_env *env) {
    assert(env->var_list);

    risp_vars *parent_scope;
    for (parent_scope = env->var_list; parent_scope->parent; parent_scope = parent_scope->parent) {
    }

    risp_vars *vars = malloc(sizeof(risp_vars));
    vars->vars = Qnil;
    vars->parent = parent_scope;
    vars->prev = env->var_list;

    return vars;
}

/**
 * Free variable frame along with its contents.
 */
static void var_frame_free(risp_vars *vars) { free(vars); }

static void pop_var_frame(risp_env *env) {
    risp_vars *prev = env->var_list->prev;
    var_frame_free(env->var_list);
    env->var_list = prev;
}

/**
 * Free all pushed variable table.
 */
void var_frame_free_all(risp_env *env) {
    for (risp_vars *vars = env->var_list; vars != NULL;) {
        risp_vars *prev = vars->prev;
        var_frame_free(vars);
        vars = prev;
    }
}

static int file_getc(lexer *lex) { return fgetc(lex->infile); }

static int file_ungetc(int c, lexer *lex) { return ungetc(c, lex->infile); }

risp_object *load_module(risp_env *env, const char *mod_name) {
    usize len = strlen(mod_name);
    char path[7 + len + 6];
    memcpy(path, "../std/", 7);
    memcpy(path + 7, mod_name, len);
    memcpy(path + 7 + len, ".lisp", 5);
    path[7 + len + 5] = '\0';

    FILE *mod_file = fopen(path, "r");
    if (mod_file == NULL) {
        return Qnil;
    }

    lex_state state;
    lex_state_init(&state);

    lexer lex = {
        .getc = &file_getc,
        .ungetc = &file_ungetc,
        .repl = false,
        .infile = mod_file,
        .in_name = path,
        .state = &state,
    };

    for (;;) {
        i32 status = read_and_eval(&lex, env, false);
        if (get_error(env) != Qnil) {
            fclose(mod_file);
            token_free(lex.tk);
            return Qnil;
        }
        if (status <= 0) {
            break;
        }
    }

    fclose(mod_file);
    token_free(lex.tk);

    return Qt;
}

bool init_std_module(risp_env *env) { return load_module(env, "std") == Qt ? true : false; }

/**
 * Initialize a new Risp execution environment.
 */
void env_init(risp_env *env, int argc, char **argv) {
    env->heap_len = 0;
    env->heap_cap = 64 * 1024 * 1024;
    env->heap = malloc(env->heap_cap);
    env->var_list = NULL;
    env->ephemeral = NULL;
    env->obarray = Qnil;
    env->error = Qnil;
    env->flags = 0;

    if (getenv("RISP_ALWAYS_GC") != NULL) {
        env->flags |= FLAG_ALWAYS_GC;
    }

    push_var_frame(env, make_var_frame_inner(env));

    risp_eobject *args = register_ephemeral_object(env, Qnil);
    for (int i = argc - 1; i >= 0; --i) {
        usize len = strlen(argv[i]);
        risp_eobject *cons = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_object *arg = alloc_str_like(env, T_STRING, len);
        memcpy(arg->str_data, argv[i], len);
        cons->o->car = arg;
        cons->o->cdr = args->o;
        unregister_ephemeral_object(env, args);
        args = cons;
    }
    risp_object *args_sym = intern_symbol(env, "args");

    make_global_variable(env, args_sym, args->o);
    unregister_ephemeral_object(env, args);
}

/**
 * Lookup variable by `symbol' and returnes cons containing the value.
 * `symbol' must be a interned symbol.
 */
static risp_object *lookup_variable_cons(risp_env *env, risp_object *symbol) {
    assert(symbol->type == T_SYMBOL);

    for (risp_vars *target = env->var_list; target != NULL; target = target->parent) {
        for (risp_object *vars = target->vars; vars != Qnil; vars = vars->cdr) {
            assert(vars->type == T_CONS);

            risp_object *var = vars->car;
            assert(var->type == T_CONS);

            if (symbol == var->car) {
                return var;
            }
        }
    }

    return Qnil;
}

/**
 * Lookup variable by `symbol'
 * `symbol' must be a interned symbol.
 */
risp_object *lookup_symbol(risp_env *env, risp_object *symbol) {
    risp_object *cons = lookup_variable_cons(env, symbol);
    if (cons == Qnil) {
        return NULL;
    }
    return cons->cdr;
}

/**
 * Collect all visible lexical binding and return them as alist.
 *
 * Note: This function can run GC.
 */
risp_object *collect_lexical_variables(risp_env *env) {
    risp_eobject *prev = register_ephemeral_object(env, alloc_object(env, T_CONS));
    prev->o->car = Qt;
    prev->o->cdr = Qnil;

    for (risp_vars *vars = env->var_list; vars != NULL && vars->parent != NULL; vars = vars->parent) {
        for (risp_object *var = vars->vars; var != Qnil; var = var->cdr) {
            assert(var->type == T_CONS);

            risp_object *cons = alloc_object(env, T_CONS);
            cons->car = var->car;
            cons->cdr = prev->o;
            unregister_ephemeral_object(env, prev);
            prev = register_ephemeral_object(env, cons);
        }
    }

    risp_object *r = prev->o;
    unregister_ephemeral_object(env, prev);
    return r;
}

/**
 * Bind the `value' to `symbol' in variable table `vars'.
 *
 * Note: This function can run GC.
 */
static void make_variable(risp_env *env, risp_vars *vars, risp_object *symbol, risp_object *value, bool ignore_if_set) {
    assert(symbol->type == T_SYMBOL);

    for (risp_object *var = vars->vars; var != Qnil; var = var->cdr) {
        assert(var->type == T_CONS);

        risp_object *var_cons = var->car;
        assert(var_cons->type == T_CONS);
        assert(var_cons->car->type == T_SYMBOL);

        if (symbol == var_cons->car) {
            if (!ignore_if_set) {
                var_cons->cdr = value;
            }
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
void make_local_variable(risp_env *env, risp_object *symbol, risp_object *value, bool ignore_if_set) {
    make_variable(env, env->var_list, symbol, value, ignore_if_set);
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

    make_variable(env, vars, symbol, value, false);
}

/**
 * Bind the `value' to `symbol' in most recently declared place in variable stack.
 * If no declaration found, it makes global binding.
 *
 * Note: This function can run GC.
 */
void scoped_set(risp_env *env, risp_object *symbol, risp_object *value) {
    risp_object *cons = lookup_variable_cons(env, symbol);
    if (cons == Qnil) {
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

    if (env->obarray != Qnil) {
        for (risp_object *obj = env->obarray; obj != Qnil; obj = obj->cdr) {
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

static bool prepare_function_var_stack(risp_env *env, risp_object *arglist, risp_object *args, bool isolated) {
    risp_vars *vars;
    if (isolated) {
        vars = make_var_frame_isolated(env);
    } else {
        vars = make_var_frame_inner(env);
    }

    usize objs_cap = 1;
    usize objs_len = 0;
    risp_eobject **arg_objs = malloc(sizeof(risp_eobject *) * objs_cap);

    risp_eobject *arg_sym = register_ephemeral_object(env, arglist);
    risp_eobject *arg = register_ephemeral_object(env, args);

    risp_eobject *cur_arg_sym = register_ephemeral_object(env, arg_sym->o);

    for (;;) {
        if ((cur_arg_sym->o == Qnil || arg->o == Qnil)) {
            if (cur_arg_sym->o == arg->o) {
                // All arguments processed.
                break;
            } else {
                // Wrong argument count.
                signal_error_s(env, "Wrong argument count");
                goto failure;
            }
        }

        if (objs_len >= objs_cap) {
            objs_cap <<= 1;
            arg_objs = realloc(arg_objs, sizeof(risp_eobject *) * objs_cap);
        }

        risp_object *argval = eval_exp(env, arg->o->car);
        if (get_error(env) != Qnil) {
            goto failure;
        }
        risp_eobject *ea = register_ephemeral_object(env, argval);
        arg_objs[objs_len] = ea;
        ++objs_len;

        risp_object *next_arg = arg->o->cdr;
        risp_object *next_arg_sym = cur_arg_sym->o->cdr;

        unregister_ephemeral_object(env, arg);
        unregister_ephemeral_object(env, cur_arg_sym);

        arg = register_ephemeral_object(env, next_arg);
        cur_arg_sym = register_ephemeral_object(env, next_arg_sym);
    }

    unregister_ephemeral_object(env, cur_arg_sym);
    unregister_ephemeral_object(env, arg);

    cur_arg_sym = arg_sym;
    for (usize i = 0; i < objs_len; ++i) {
        make_variable(env, vars, cur_arg_sym->o->car, arg_objs[i]->o, false);
        unregister_ephemeral_object(env, arg_objs[i]);

        risp_object *next_arg_sym = cur_arg_sym->o->cdr;
        unregister_ephemeral_object(env, cur_arg_sym);
        cur_arg_sym = register_ephemeral_object(env, next_arg_sym);
    }
    free(arg_objs);

    unregister_ephemeral_object(env, cur_arg_sym);

    push_var_frame(env, vars);

    return true;

failure:
    unregister_ephemeral_object(env, cur_arg_sym);
    unregister_ephemeral_object(env, arg);
    unregister_ephemeral_object(env, arg_sym);
    for (usize i = 0; i < objs_len; ++i) {
        unregister_ephemeral_object(env, arg_objs[i]);
    }
    free(arg_objs);
    var_frame_free(vars);
    return false;
}

risp_object *call_risp_function(risp_env *env, risp_object *func, risp_object *args) {
    risp_eobject *efunc = register_ephemeral_object(env, func);
    risp_eobject *eargs = register_ephemeral_object(env, args);
    if (!prepare_function_var_stack(env, func->func.arglist, args, true)) {
        unregister_ephemeral_object(env, eargs);
        unregister_ephemeral_object(env, efunc);

        return NULL;
    }

    unregister_ephemeral_object(env, eargs);

    risp_eobject *body = register_ephemeral_object(env, efunc->o->func.body);
    unregister_ephemeral_object(env, efunc);

    risp_object *result = Qnil;

    while (body->o != Qnil) {
        result = eval_exp(env, body->o->car);
        if (get_error(env) != Qnil) {
            pop_var_frame(env);

            unregister_ephemeral_object(env, body);
            return NULL;
        }

        risp_object *next = body->o->cdr;
        unregister_ephemeral_object(env, body);
        body = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, body);

    pop_var_frame(env);

    return result;
}

/**
 * Execute closure and return the result.
 *
 * Note: This function can run GC.
 */
risp_object *call_risp_closure(risp_env *env, risp_object *func, risp_object *args) {
    if (func->type != T_CONS) {
        signal_error_s(env, "illegal closure");
        return NULL;
    }
    if (func->cdr == Qnil || func->cdr->type != T_CONS) {
        signal_error_s(env, "illegal closure (no environment supplied)");
        return NULL;
    }
    if (func->cdr->cdr == Qnil || func->cdr->cdr->type != T_CONS) {
        signal_error_s(env, "illegal closure (no arglist supplied)");
        return NULL;
    }

    risp_eobject *efunc = register_ephemeral_object(env, func);

    if (!prepare_function_var_stack(env, func->cdr->cdr->car, args, false)) {
        unregister_ephemeral_object(env, efunc);

        return NULL;
    }

    risp_eobject *lex_var = register_ephemeral_object(env, efunc->o->cdr->car);
    for (;;) {
        if (lex_var->o->type != T_CONS) {
            unregister_ephemeral_object(env, lex_var);
            unregister_ephemeral_object(env, efunc);
            pop_var_frame(env);
            signal_error_s(env, "Illegal lexical vars");
            return NULL;
        }

        risp_object *var = lex_var->o->car;
        if (var == Qt) {
            break;
        }

        if (var == Qnil || var->type != T_CONS) {
            unregister_ephemeral_object(env, lex_var);
            unregister_ephemeral_object(env, efunc);
            pop_var_frame(env);
            signal_error_s(env, "Illegal lexical vars (nil var found)");
            return NULL;
        }

        if (var->car->type != T_SYMBOL) {
            unregister_ephemeral_object(env, lex_var);
            unregister_ephemeral_object(env, efunc);
            pop_var_frame(env);
            signal_error_s(env, "Illegal lexical vars (symbol name is not symbol type)");
            return NULL;
        }

        make_local_variable(env, var->car, var->cdr, true);

        risp_object *next = lex_var->o->cdr;
        unregister_ephemeral_object(env, lex_var);
        lex_var = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, lex_var);

    risp_eobject *body = register_ephemeral_object(env, efunc->o->cdr->cdr->cdr);
    unregister_ephemeral_object(env, efunc);

    risp_object *result = Qnil;

    while (body->o != Qnil) {
        result = eval_exp(env, body->o->car);
        if (get_error(env) != Qnil) {
            pop_var_frame(env);

            unregister_ephemeral_object(env, body);
            return NULL;
        }

        risp_object *next = body->o->cdr;
        unregister_ephemeral_object(env, body);
        body = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, body);

    pop_var_frame(env);

    return result;
}

static risp_object *expand_macro_and_run(risp_env *env, risp_object *macro, risp_object *args) {
    push_var_frame(env, make_var_frame_isolated(env));

    risp_eobject *earglist = register_ephemeral_object(env, macro->macro.arglist);
    risp_eobject *eargs = register_ephemeral_object(env, args);
    risp_object *rest_sym = intern_symbol(env, "&rest");
    bool is_rest = false;
    for (;;) {
        if (earglist->o == Qnil) {
            unregister_ephemeral_object(env, earglist);
            unregister_ephemeral_object(env, eargs);
            if (eargs->o == Qnil) {
                break;
            } else {
                pop_var_frame(env);
                signal_error_s(env, "wrong number of arguments");
            }
        }

        if (earglist->o->type != T_CONS) {
            unregister_ephemeral_object(env, earglist);
            unregister_ephemeral_object(env, eargs);

            pop_var_frame(env);
            signal_error_s(env, "invalid arglist");
        }

        if (earglist->o->car == rest_sym) {
            // &rest
            is_rest = true;
            risp_object *next_al = earglist->o->cdr;
            unregister_ephemeral_object(env, earglist);
            earglist = register_ephemeral_object(env, next_al);
            continue;
        }

        if (is_rest) {
            make_local_variable(env, earglist->o->car, eargs->o, false);
        } else {
            if (eargs->o == Qnil) {
                unregister_ephemeral_object(env, earglist);
                unregister_ephemeral_object(env, eargs);
                pop_var_frame(env);
                signal_error_s(env, "wrong number of arguments");
            }

            if (eargs->o->type != T_CONS) {
                unregister_ephemeral_object(env, earglist);
                unregister_ephemeral_object(env, eargs);

                pop_var_frame(env);
                signal_error_s(env, "invalid arglist");
            }

            make_local_variable(env, earglist->o->car, eargs->o->car, false);
        }

        risp_object *next_al = earglist->o->cdr;
        risp_object *next_a = eargs->o->cdr;

        unregister_ephemeral_object(env, earglist);
        unregister_ephemeral_object(env, eargs);

        if (is_rest) {
            if (next_al != Qnil) {
                pop_var_frame(env);
                signal_error_s(env, "cannot have multiple rest argument");
                return NULL;
            }
            break;
        }

        earglist = register_ephemeral_object(env, next_al);
        eargs = register_ephemeral_object(env, next_a);
    }
    unregister_ephemeral_object(env, earglist);
    unregister_ephemeral_object(env, eargs);

    risp_object *exe = Qnil;

    risp_eobject *body = register_ephemeral_object(env, macro->macro.body);

    while (body->o != Qnil) {
        if (body->o == Qt || body->o->type != T_CONS) {
            unregister_ephemeral_object(env, body);
            pop_var_frame(env);
            signal_error_s(env, "invalid macro body");
            return NULL;
        }

        exe = eval_exp(env, body->o->car);
        if (get_error(env) != Qnil) {
            unregister_ephemeral_object(env, body);
            pop_var_frame(env);
            return NULL;
        }

        risp_object *next = body->o->cdr;
        unregister_ephemeral_object(env, body);
        body = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, body);

    pop_var_frame(env);

    risp_object *result = eval_exp(env, exe);
    if (get_error(env) != Qnil) {
        return NULL;
    }

    return result;
}

risp_object *run_with_local_vars(risp_env *env, risp_object *vars, risp_object *body) {
    risp_vars *var_frame = make_var_frame_inner(env);

    risp_eobject *cur_var = register_ephemeral_object(env, vars);
    risp_eobject *ebody = register_ephemeral_object(env, body);
    while (cur_var->o != Qnil) {
        risp_object *cur = cur_var->o->car;
        if (cur->type != T_CONS) {
            unregister_ephemeral_object(env, ebody);
            unregister_ephemeral_object(env, cur_var);
            push_var_frame(env, var_frame);
            pop_var_frame(env);
            signal_error_s(env, "Illegal var list");
            return NULL;
        }
        if (cur->car->type != T_SYMBOL) {
            unregister_ephemeral_object(env, ebody);
            unregister_ephemeral_object(env, cur_var);
            push_var_frame(env, var_frame);
            pop_var_frame(env);
            signal_error_s(env, "First element of var list must be a symbol");
            return NULL;
        }
        if (cur->cdr->type == T_CONS) {
            risp_eobject *sym = register_ephemeral_object(env, cur->car);
            risp_object *val = eval_exp(env, cur->cdr->car);
            if (get_error(env) != Qnil) {
                unregister_ephemeral_object(env, ebody);
                unregister_ephemeral_object(env, cur_var);
                push_var_frame(env, var_frame);
                pop_var_frame(env);
                return NULL;
            }
            make_variable(env, var_frame, sym->o, val, false);
            unregister_ephemeral_object(env, sym);
        } else if (cur->cdr == Qnil) {
            make_variable(env, var_frame, cur->car, Qnil, false);
        } else {
            unregister_ephemeral_object(env, ebody);
            unregister_ephemeral_object(env, cur_var);
            push_var_frame(env, var_frame);
            pop_var_frame(env);
            signal_error_s(env, "Illegal var list");
            return NULL;
        }

        risp_object *next = cur_var->o->cdr;
        unregister_ephemeral_object(env, cur_var);
        cur_var = register_ephemeral_object(env, next);
    }
    unregister_ephemeral_object(env, cur_var);

    push_var_frame(env, var_frame);

    risp_object *result = Qnil;

    while (ebody->o != Qnil) {
        result = eval_exp(env, ebody->o->car);
        if (get_error(env) != Qnil) {
            unregister_ephemeral_object(env, ebody);
            pop_var_frame(env);
            return NULL;
        }

        risp_object *next = ebody->o->cdr;
        unregister_ephemeral_object(env, ebody);
        ebody = register_ephemeral_object(env, next);
    }
    unregister_ephemeral_object(env, ebody);

    pop_var_frame(env);

    return result;
}

/**
 * Evaluate anything and return the result.
 *
 * Note: This function can run GC.
 */
risp_object *eval_exp(risp_env *env, risp_object *exp) {
    if (exp == Qnil) {
        return Qnil;
    } else if (exp == Qt) {
        return Qt;
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
            return func->native_func(env, exp->cdr);
        } else if (func->type == T_FUNC) {
            return call_risp_function(env, func, exp->cdr);
        } else if (func->type == T_MACRO) {
            return expand_macro_and_run(env, func, exp->cdr);
        } else {
            signal_error_s(env, "void function or macro");
        }
        return NULL;
    }

    case T_STRING:
    case T_KWSYMBOL:
    case T_INT:
    case T_FUNC:
    case T_MACRO:
    case T_NATIVE_FUNC:
    case T_NATIVE_HANDLE:
        return exp;

    case T_SYMBOL: {
        risp_object *obj = lookup_symbol(env, exp);
        if (obj == NULL) {
            signal_error_s(env, "void variable");
            return NULL;
        }
        return obj;
    }
    }

    assert(false);
    return Qnil;
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
        return Qnil;
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

        r->cdr = Qnil;
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
            token_free(tk);
            goto lex_err;
        }
        if (tk->type != TK_RPAREN) {
            unget_token(lex, tk);

            unregister_ephemeral_object(env, root);

            err->has_error = err;
            err->message = "RPAREN expected";

            return NULL;
        }

        token_free(tk);
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

    prev->o->cdr = Qnil;
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
        risp_eobject *inner = register_ephemeral_object(env, alloc_object(env, T_CONS));
        inner->o->car = read_exp(lex, err, env);
        inner->o->cdr = Qnil;
        risp_object *quote = equote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, equote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = quote;
        cons->cdr = inner->o;
        unregister_ephemeral_object(env, inner);
        return cons;
    }

    case TK_BACKQUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *ebackquote = register_ephemeral_object(env, intern_symbol(env, "backquote"));
        risp_eobject *inner = register_ephemeral_object(env, alloc_object(env, T_CONS));
        inner->o->car = read_exp(lex, err, env);
        inner->o->cdr = Qnil;
        risp_object *backquote = ebackquote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, ebackquote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = backquote;
        cons->cdr = inner->o;
        unregister_ephemeral_object(env, inner);
        return cons;
    }

    case TK_UNQUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *eunquote = register_ephemeral_object(env, intern_symbol(env, "unquote"));
        risp_eobject *inner = register_ephemeral_object(env, alloc_object(env, T_CONS));
        inner->o->car = read_exp(lex, err, env);
        inner->o->cdr = Qnil;
        risp_object *unquote = eunquote->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, eunquote);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = unquote;
        cons->cdr = inner->o;
        unregister_ephemeral_object(env, inner);
        return cons;
    }

    case TK_SPLICE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *esplice = register_ephemeral_object(env, intern_symbol(env, "splice"));
        risp_eobject *inner = register_ephemeral_object(env, alloc_object(env, T_CONS));
        inner->o->car = read_exp(lex, err, env);
        inner->o->cdr = Qnil;
        risp_object *splice = esplice->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, esplice);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = splice;
        cons->cdr = inner->o;
        unregister_ephemeral_object(env, inner);
        return cons;
    }

    case TK_FUNQUOTE: {
        token_free(tk);
        risp_eobject *econs = register_ephemeral_object(env, alloc_object(env, T_CONS));
        risp_eobject *efunction = register_ephemeral_object(env, intern_symbol(env, "function"));
        risp_eobject *inner = register_ephemeral_object(env, alloc_object(env, T_CONS));
        inner->o->car = read_exp(lex, err, env);
        inner->o->cdr = Qnil;
        risp_object *function = efunction->o;
        risp_object *cons = econs->o;
        unregister_ephemeral_object(env, efunction);
        unregister_ephemeral_object(env, econs);
        if (err->has_error) {
            return NULL;
        }
        cons->car = function;
        cons->cdr = inner->o;
        unregister_ephemeral_object(env, inner);
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
            return Qnil;
        } else if (!strcmp(tk->text, "t")) {
            token_free(tk);
            return Qt;
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

/**
 * Print representation of the list.
 *
 * Note: This function can run GC.
 */
static void repr_list(risp_env *env, risp_object *obj) {
    if (obj == Qnil) {
        putchar(')');
    } else if (obj->type == T_CONS && obj != Qt) {
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
 *
 * Note: This function can run GC.
 */
void repr_object(risp_env *env, risp_object *obj) {
    if (obj == Qnil) {
        fputs("nil", stdout);
        return;
    } else if (obj == Qt) {
        fputs("t", stdout);
        return;
    }

    switch (obj->type) {
    case T_CONS: {
        risp_eobject *eobj = register_ephemeral_object(env, obj);
        if (eobj->o->car == intern_symbol(env, "quote")) {
            if (obj->cdr->type == T_CONS && obj->cdr->cdr == Qnil) {
                putchar('\'');
                obj = eobj->o;
                unregister_ephemeral_object(env, eobj);
                repr_object(env, obj->cdr->car);
            } else {
                goto normal_obj;
            }
        } else if (eobj->o->car == intern_symbol(env, "function")) {
            if (obj->cdr->type == T_CONS && obj->cdr->cdr == Qnil) {
                putchar('#');
                putchar('\'');
                obj = eobj->o;
                unregister_ephemeral_object(env, eobj);
                repr_object(env, obj->cdr->car);
            } else {
                goto normal_obj;
            }
        } else if (eobj->o->car == intern_symbol(env, "backquote")) {
            if (obj->cdr->type == T_CONS && obj->cdr->cdr == Qnil) {
                putchar('`');
                obj = eobj->o;
                unregister_ephemeral_object(env, eobj);
                repr_object(env, obj->cdr->car);
            } else {
                goto normal_obj;
            }
        } else if (eobj->o->car == intern_symbol(env, "unquote")) {
            if (obj->cdr->type == T_CONS && obj->cdr->cdr == Qnil) {
                putchar(',');
                obj = eobj->o;
                unregister_ephemeral_object(env, eobj);
                repr_object(env, obj->cdr->car);
            } else {
                goto normal_obj;
            }
        } else if (eobj->o->car == intern_symbol(env, "splice")) {
            if (obj->cdr->type == T_CONS && obj->cdr->cdr == Qnil) {
                putchar(',');
                putchar('@');
                obj = eobj->o;
                unregister_ephemeral_object(env, eobj);
                repr_object(env, obj->cdr->car);
            } else {
                goto normal_obj;
            }
        } else {
        normal_obj:
            putchar('(');
            risp_object *inner = eobj->o;
            unregister_ephemeral_object(env, eobj);
            repr_object(env, inner->car);
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

    case T_MACRO:
        fputs("<macro>", stderr);
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
i32 read_and_eval(lexer *lex, risp_env *env, bool root) {
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

    risp_object *result = eval_exp(env, sexp);

    risp_object *runtime_err = get_error(env);
    if (runtime_err != Qnil) {
        if (root) {
            fputs("Fatal error: ", stderr);
            repr_object(env, runtime_err);
            putchar('\n');
            clear_error(env);
        }

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
    register_native_function(env, "=", RISP_FUNC(eq_num));
    register_native_function(env, ">", RISP_FUNC(gt));
    register_native_function(env, ">=", RISP_FUNC(ge));
    register_native_function(env, "<", RISP_FUNC(lt));
    register_native_function(env, "<=", RISP_FUNC(le));
    register_native_function(env, "and", RISP_FUNC(and));
    register_native_function(env, "backquote", RISP_FUNC(backquote));
    register_native_function(env, "car", RISP_FUNC(car));
    register_native_function(env, "cdr", RISP_FUNC(cdr));
    register_native_function(env, "concat", RISP_FUNC(concat));
    register_native_function(env, "cond", RISP_FUNC(cond));
    register_native_function(env, "consp", RISP_FUNC(consp));
    register_native_function(env, "defmacro", RISP_FUNC(defmacro));
    register_native_function(env, "defun", RISP_FUNC(defun));
    register_native_function(env, "eq", RISP_FUNC(eq));
    register_native_function(env, "error", RISP_FUNC(error));
    register_native_function(env, "funcall", RISP_FUNC(funcall));
    register_native_function(env, "function", RISP_FUNC(quote));
    register_native_function(env, "functionp", RISP_FUNC(functionp));
    register_native_function(env, "if", RISP_FUNC(if));
    register_native_function(env, "integerp", RISP_FUNC(integerp));
    register_native_function(env, "intern", RISP_FUNC(intern));
    register_native_function(env, "lambda", RISP_FUNC(lambda));
    register_native_function(env, "length", RISP_FUNC(length));
    register_native_function(env, "listp", RISP_FUNC(listp));
    register_native_function(env, "load", RISP_FUNC(load));
    register_native_function(env, "let", RISP_FUNC(let));
    register_native_function(env, "macrop", RISP_FUNC(macrop));
    register_native_function(env, "make-symbol", RISP_FUNC(make_symbol));
    register_native_function(env, "native-function-p", RISP_FUNC(native_function_p));
    register_native_function(env, "nth", RISP_FUNC(nth));
    register_native_function(env, "nthcdr", RISP_FUNC(nthcdr));
    register_native_function(env, "nthchar", RISP_FUNC(nthchar));
    register_native_function(env, "not", RISP_FUNC(not ));
    register_native_function(env, "or", RISP_FUNC(or));
    register_native_function(env, "print", RISP_FUNC(print));
    register_native_function(env, "progn", RISP_FUNC(progn));
    register_native_function(env, "quote", RISP_FUNC(quote));
    register_native_function(env, "setcar", RISP_FUNC(setcar));
    register_native_function(env, "setcdr", RISP_FUNC(setcdr));
    register_native_function(env, "setq", RISP_FUNC(setq));
    register_native_function(env, "string=", RISP_FUNC(stringeq));
    register_native_function(env, "stringp", RISP_FUNC(stringp));
    register_native_function(env, "symbolp", RISP_FUNC(symbolp));
    register_native_function(env, "while", RISP_FUNC(while));
}
