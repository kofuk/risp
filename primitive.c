#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "primitive.h"
#include "rt.h"

/**
 * Calculate length of the given `list'.
 * If the `list' is not a list, error is signaled and -1 is returned.
 */
static i64 list_length(risp_env *env, risp_object *list) {
    i64 result = 0;
    for (risp_object *arg = list; arg != &Qnil; arg = arg->cdr) {
        if (arg->type != T_CONS) {
            signal_error_s(env, "argument must be a list or string");
            return -1;
        }

        ++result;
    }
    return result;
}

static risp_object *handle_backquote_inner(risp_env *env, risp_object *arg, bool *spliced) {
    if (arg == &Qnil || arg == &Qt) {
        return arg;
    }

    if (arg->type == T_CONS) {
        if (arg->car->type == T_SYMBOL) {
            risp_eobject *earg = register_ephemeral_object(env, arg);
            risp_object *unquote = intern_symbol(env, "unquote");
            if (earg->o->car == unquote) {
                arg = earg->o;
                unregister_ephemeral_object(env, earg);
                if (list_length(env, arg->cdr) != 1) {
                    if (get_error(env) != &Qnil) {
                        signal_error_s(env, "1 argument required");
                    }
                    return NULL;
                }

                risp_object *result = eval_exp(env, arg->cdr->car);
                if (get_error(env) != &Qnil) {
                    return NULL;
                }
                return result;
            }

            risp_object *backquote = intern_symbol(env, "backquote");
            if (earg->o->car == backquote) {
                risp_object *r = earg->o;
                unregister_ephemeral_object(env, earg);
                return r;
            }

            risp_object *splice = intern_symbol(env, "splice");
            if (earg->o->car == splice) {
                arg = earg->o;
                unregister_ephemeral_object(env, earg);
                if (list_length(env, arg->cdr) != 1) {
                    if (get_error(env) != &Qnil) {
                        signal_error_s(env, "1 argument required");
                    }
                    return NULL;
                }

                risp_object *elements = eval_exp(env, arg->cdr->car);
                if (get_error(env) != &Qnil) {
                    return NULL;
                }

                if (spliced != NULL) {
                    *spliced = true;
                }

                return elements;
            }

            unregister_ephemeral_object(env, earg);
        }

        risp_object *result_head = alloc_object(env, T_CONS);
        risp_eobject *result = register_ephemeral_object(env, result_head);
        risp_eobject *result_prev = register_ephemeral_object(env, result_head);
        risp_eobject *cur = register_ephemeral_object(env, arg);
        while (cur->o != &Qnil) {
            bool spliced = false;
            risp_object *element = handle_backquote_inner(env, cur->o->car, &spliced);
            if (get_error(env) != &Qnil) {
                unregister_ephemeral_object(env, cur);
                unregister_ephemeral_object(env, result_prev);
                unregister_ephemeral_object(env, result);
                return NULL;
            }

            if (spliced) {
                result_prev->o->cdr = element;

                risp_object *next = element;
                while (next->type == T_CONS && next != &Qnil && next != &Qt && next->cdr != &Qnil) {
                    next = next->cdr;
                }

                unregister_ephemeral_object(env, result_prev);
                result_prev = register_ephemeral_object(env, next);
            } else {
                if (result_prev->o == &Qnil || result_prev->o == &Qt || result_prev->o->type != T_CONS) {
                    unregister_ephemeral_object(env, cur);
                    unregister_ephemeral_object(env, result_prev);
                    unregister_ephemeral_object(env, result);
                    signal_error_s(env, "invalid backquote element");
                    return NULL;
                }

                risp_eobject *eelement = register_ephemeral_object(env, element);

                risp_object *cons = alloc_object(env, T_CONS);
                cons->car = eelement->o;
                cons->cdr = &Qnil;
                result_prev->o->cdr = cons;
                unregister_ephemeral_object(env, result_prev);
                result_prev = register_ephemeral_object(env, cons);

                unregister_ephemeral_object(env, eelement);
            }

            if (cur->o->cdr->type != T_CONS || cur->o->cdr == &Qt) {
                break;
            }

            risp_object *next = cur->o->cdr;
            unregister_ephemeral_object(env, cur);
            cur = register_ephemeral_object(env, next);
        }
        unregister_ephemeral_object(env, cur);
        unregister_ephemeral_object(env, result_prev);

        risp_object *r = result->o->cdr;
        unregister_ephemeral_object(env, result);
        return r;
    }
    return arg;
}

DEFUN(backquote) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "1 argument required");
        }
        return NULL;
    }

    return handle_backquote_inner(env, args->car, NULL);
}

DEFUN(car) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "1 argument required");
        }
        return NULL;
    }

    risp_object *list = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (list == &Qnil || list == &Qt || list->type != T_CONS) {
        signal_error_s(env, "Argument is not cons");
        return NULL;
    }

    return list->car;
}

DEFUN(cdr) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "1 argument required");
        }
        return NULL;
    }

    risp_object *list = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (list == &Qnil || list == &Qt || list->type != T_CONS) {
        signal_error_s(env, "Argument is not cons");
        return NULL;
    }

    return list->cdr;
}

DEFUN(defmacro) {
    if (list_length(env, args) < 3) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "macro name and argument required");
        }
        return NULL;
    }

    risp_object *macro_sym = args->car;
    risp_object *macro_arg = args->cdr->car;
    risp_object *macro_body = args->cdr->cdr;

    if (macro_sym->type != T_SYMBOL) {
        signal_error_s(env, "macro name must be a symbol");
        return NULL;
    }

    for (risp_object *a = macro_arg; a != &Qnil; a = a->cdr) {
        if (a->car->type != T_SYMBOL) {
            signal_error_s(env, "macro must be a symbol");
            return NULL;
        }
    }

    risp_eobject *es = register_ephemeral_object(env, macro_sym);
    risp_eobject *ea = register_ephemeral_object(env, macro_arg);
    risp_eobject *eb = register_ephemeral_object(env, macro_body);

    risp_object *macro = alloc_object(env, T_MACRO);
    macro->macro.arglist = ea->o;
    macro->macro.body = eb->o;

    make_global_variable(env, es->o, macro);

    macro_sym = es->o;

    unregister_ephemeral_object(env, eb);
    unregister_ephemeral_object(env, ea);
    unregister_ephemeral_object(env, es);

    return macro_sym;
}

DEFUN(defun) {
    if (list_length(env, args) < 3) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "function name and argument required");
        }
        return NULL;
    }

    risp_object *func_sym = args->car;
    risp_object *func_arg = args->cdr->car;
    risp_object *func_body = args->cdr->cdr;

    if (func_sym->type != T_SYMBOL) {
        signal_error_s(env, "function name must be a symbol");
        return NULL;
    }

    for (risp_object *a = func_arg; a != &Qnil; a = a->cdr) {
        if (a->car->type != T_SYMBOL) {
            signal_error_s(env, "argument must be a symbol");
            return NULL;
        }
    }

    risp_eobject *es = register_ephemeral_object(env, func_sym);
    risp_eobject *ea = register_ephemeral_object(env, func_arg);
    risp_eobject *eb = register_ephemeral_object(env, func_body);

    risp_object *func = alloc_object(env, T_FUNC);
    func->func.arglist = ea->o;
    func->func.body = eb->o;

    make_global_variable(env, es->o, func);

    func_sym = es->o;

    unregister_ephemeral_object(env, eb);
    unregister_ephemeral_object(env, ea);
    unregister_ephemeral_object(env, es);

    return func_sym;
}

DEFUN(divide) {
    if (list_length(env, args) == 0) {
        signal_error_s(env, "1 or more arguments required");
        return NULL;
    } else if (get_error(env) != &Qnil) {
        return NULL;
    }

    i64 result = 0;

    risp_eobject *cur = register_ephemeral_object(env, args);
    bool first = true;

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        if (target->type != T_INT) {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "argument type must be int");
            return NULL;
        }

        if (first) {
            result = target->integer;
            first = false;
        } else {
            result /= target->integer;
        }

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->cdr);
    }

    unregister_ephemeral_object(env, cur);

    risp_object *r = alloc_object(env, T_INT);
    r->integer = result;

    return r;
}

DEFUN(dolist) {
    if (list_length(env, args) < 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "argument required");
        }
        return NULL;
    }

    risp_object *arg = args->car;
    if (list_length(env, arg) < 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "illegal argument");
        }
        return NULL;
    }

    risp_object *var = arg->car;
    if (var->type != T_SYMBOL) {
        signal_error_s(env, "argument var must be a symbol");
        return NULL;
    }
    risp_eobject *evar = register_ephemeral_object(env, var);
    risp_eobject *ebody = register_ephemeral_object(env, args->cdr);

    risp_object *list = eval_exp(env, arg->cdr->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, ebody);
        unregister_ephemeral_object(env, evar);
        return NULL;
    }

    list_length(env, list);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, ebody);
        unregister_ephemeral_object(env, evar);
        return NULL;
    }

    risp_eobject *result = register_ephemeral_object(env, arg->cdr->cdr);

    risp_object *body = ebody->o;
    unregister_ephemeral_object(env, ebody);

    var = evar->o;
    unregister_ephemeral_object(env, evar);

    run_dolist_body(env, var, list, body);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, result);
        return NULL;
    }

    risp_object *r = &Qnil;
    if (result->o != &Qnil) {
        r = eval_exp(env, result->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, result);
            return NULL;
        }
    }

    unregister_ephemeral_object(env, result);

    return r;
}

DEFUN(eq) {
    if (list_length(env, args) != 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "2 arguments expected");
        }
        return NULL;
    }

    risp_eobject *arg2 = register_ephemeral_object(env, args->cdr->car);

    risp_object *o1 = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, arg2);
        return NULL;
    }

    risp_eobject *eo1 = register_ephemeral_object(env, o1);

    risp_object *o2 = eval_exp(env, arg2->o);
    unregister_ephemeral_object(env, arg2);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, eo1);
        return NULL;
    }

    o1 = eo1->o;
    unregister_ephemeral_object(env, eo1);

    if (o1->type == T_INT && o2->type == T_INT) {
        return o1->integer == o2->integer ? &Qt : &Qnil;
    }

    return o1 == o2 ? &Qt : &Qnil;
}

DEFUN(funcall) {
    if (list_length(env, args) < 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "function name required");
        }
        return NULL;
    }

    risp_eobject *rest = register_ephemeral_object(env, args->cdr);
    risp_object *func = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    switch (func->type) {
    case T_SYMBOL: {
        risp_object *actual_func = lookup_symbol(env, func);
        if (actual_func == NULL) {
            unregister_ephemeral_object(env, rest);
            signal_error_s(env, "void variable");
            return NULL;
        }

        switch (actual_func->type) {
        case T_FUNC: {
            risp_object *args = rest->o;
            unregister_ephemeral_object(env, rest);
            return call_risp_function(env, actual_func, args);
        }
        case T_NATIVE_FUNC: {
            risp_object *args = rest->o;
            unregister_ephemeral_object(env, rest);
            return actual_func->native_func(env, args);
        }
        default:
            unregister_ephemeral_object(env, rest);
            signal_error_s(env, "object cannot be called");
            return NULL;
        }
    }

    case T_CONS: {
        risp_eobject *efunc = register_ephemeral_object(env, func);
        risp_object *closure = intern_symbol(env, "closure");
        if (efunc->o->car != closure) {
            unregister_ephemeral_object(env, efunc);
            unregister_ephemeral_object(env, rest);
            signal_error_s(env, "object cannot be called");
            return NULL;
        }

        func = efunc->o;
        risp_object *args = rest->o;
        unregister_ephemeral_object(env, efunc);
        unregister_ephemeral_object(env, rest);
        return call_risp_closure(env, func, args);
    }

    default:
        unregister_ephemeral_object(env, rest);
        signal_error_s(env, "object cannot be called");
        return NULL;
    }
}

DEFUN(if) {
    if (list_length(env, args) < 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "wrong number of arguments");
        }
        return NULL;
    }

    risp_object *cond = args->car;
    risp_eobject *then = register_ephemeral_object(env, args->cdr->car);
    risp_eobject *rest = register_ephemeral_object(env, args->cdr->cdr);

    risp_object *cond_val = eval_exp(env, cond);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, rest);
        unregister_ephemeral_object(env, then);
        return NULL;
    }

    if (cond_val != &Qnil) {
        unregister_ephemeral_object(env, rest);

        risp_object *exp = then->o;
        unregister_ephemeral_object(env, then);
        risp_object *r = eval_exp(env, exp);
        if (get_error(env) != &Qnil) {
            return NULL;
        }
        return r;
    }

    unregister_ephemeral_object(env, then);

    risp_object *result = &Qnil;
    while (rest->o != &Qnil) {
        result = eval_exp(env, rest->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, rest);
            return NULL;
        }

        risp_object *next = rest->o->cdr;
        unregister_ephemeral_object(env, rest);
        rest = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, rest);
    return result;
}

DEFUN(intern) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "just 1 argument expected");
        }
        return NULL;
    }

    risp_object *name_obj = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (name_obj->type != T_STRING) {
        signal_error_s(env, "Name must be a string");
        return NULL;
    }

    char name[name_obj->str_len + 1];
    name[name_obj->str_len] = '\0';
    memcpy(name, name_obj->str_data, name_obj->str_len);

    return intern_symbol(env, name);
}

DEFUN(lambda) {
    if (list_length(env, args) < 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "arglist requied");
        }
        return NULL;
    }

    risp_eobject *eclosure = register_ephemeral_object(env, intern_symbol(env, "closure"));
    risp_eobject *eargs = register_ephemeral_object(env, args->car);
    risp_eobject *ebody = register_ephemeral_object(env, args->cdr);
    risp_eobject *eenv = register_ephemeral_object(env, collect_lexical_variables(env));

    risp_eobject *result = register_ephemeral_object(env, alloc_object(env, T_CONS));
    result->o->car = eclosure->o;
    unregister_ephemeral_object(env, eclosure);
    risp_eobject *env_cons = register_ephemeral_object(env, alloc_object(env, T_CONS));
    result->o->cdr = env_cons->o;
    env_cons->o->car = eenv->o;
    risp_object *arg_cons = alloc_object(env, T_CONS);
    env_cons->o->cdr = arg_cons;
    arg_cons->car = eargs->o;
    arg_cons->cdr = ebody->o;

    risp_object *r = result->o;

    unregister_ephemeral_object(env, env_cons);
    unregister_ephemeral_object(env, result);
    unregister_ephemeral_object(env, eenv);
    unregister_ephemeral_object(env, ebody);
    unregister_ephemeral_object(env, eargs);
    unregister_ephemeral_object(env, eclosure);

    return r;
}

DEFUN(length) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "just 1 argument expected");
        }
        return NULL;
    }

    i64 result = 0;
    risp_object *target = eval_exp(env, args->car);

    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (target == &Qnil) {
        result = 0;
    } else if (target->type == T_CONS) {
        result = list_length(env, target);
    } else if (target->type == T_STRING) {
        result = target->str_len;
    }

    risp_object *r = alloc_object(env, T_INT);
    r->integer = result;

    return r;
}

DEFUN(let) {
    if (list_length(env, args) < 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "variable list required");
        }
        return NULL;
    }

    list_length(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    risp_object *result = run_with_local_vars(env, args->car, args->cdr);
    if (get_error(env) != &Qnil) {
        return NULL;
    }
    return result;
}

DEFUN(lt) {
    if (list_length(env, args) < 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "1 or more argument required");
        }
        return NULL;
    }

    risp_eobject *rest = register_ephemeral_object(env, args->cdr);

    risp_object *first = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, rest);
        return NULL;
    }
    if (first->type != T_INT) {
        unregister_ephemeral_object(env, rest);
        signal_error_s(env, "arguments must be integer");
        return NULL;
    }

    i64 last = first->integer;
    bool ok = true;
    while (rest->o != &Qnil) {
        risp_object *val = eval_exp(env, rest->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, rest);
            return NULL;
        }
        if (val->type != T_INT) {
            unregister_ephemeral_object(env, rest);
            signal_error_s(env, "arguments must be integer");
            return NULL;
        }

        if (!(last < val->integer)) {
            ok = false;
            break;
        }
        last = val->integer;

        risp_object *next = rest->o->cdr;
        unregister_ephemeral_object(env, rest);
        rest = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, rest);

    return ok ? &Qt : &Qnil;
}

DEFUN(make_symbol) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "just 1 argument expected");
        }
        return NULL;
    }

    risp_object *name_obj = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (name_obj->type != T_STRING) {
        signal_error_s(env, "Name must be a string");
        return NULL;
    }

    risp_eobject *name = register_ephemeral_object(env, name_obj);
    risp_object *sym = alloc_str_like(env, T_SYMBOL, name_obj->str_len);
    memcpy(sym->str_data, name->o->str_data, name->o->str_len);
    unregister_ephemeral_object(env, name);

    return sym;
}

DEFUN(minus) {
    i64 len = list_length(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }
    if (len == 1) {
        // negate mode
        risp_object *arg = eval_exp(env, args->car);

        if (get_error(env) != &Qnil) {
            return NULL;
        }

        if (arg->type != T_INT) {
            signal_error_s(env, "integer argument required");
            return NULL;
        }

        i64 val = arg->integer;

        risp_object *r = alloc_object(env, T_INT);
        r->integer = -val;

        return r;
    }

    // subtract mode

    i64 result = 0;

    risp_eobject *cur = register_ephemeral_object(env, args);
    bool first = true;

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        if (target->type != T_INT) {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "argument type must be int");
            return NULL;
        }

        if (first) {
            result = target->integer;
            first = false;
        } else {
            result -= target->integer;
        }

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->cdr);
    }

    unregister_ephemeral_object(env, cur);

    risp_object *r = alloc_object(env, T_INT);
    r->type = T_INT;
    r->integer = result;

    return r;
}

DEFUN(multiply) {
    list_length(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    i64 result = 1;

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        if (target->type != T_INT) {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "argument type must be int");
            return NULL;
        }

        result *= target->integer;

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->cdr);
    }

    unregister_ephemeral_object(env, cur);

    risp_object *r = alloc_object(env, T_INT);
    r->integer = result;

    return r;
}

DEFUN(not ) {
    if (list_length(env, args) != 1) {
        if (get_error(env) != &Qnil) {
            signal_error_s(env, "1 argument required");
        }
        return NULL;
    }

    risp_object *obj = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    return obj == &Qnil ? &Qt : &Qnil;
}

DEFUN(nth) {
    risp_object *nth_cdr = RISP_FUNC(nthcdr)(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }
    return nth_cdr->car;
}

DEFUN(nthcdr) {
    if (list_length(env, args) != 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "2 arguments required");
        }
        return NULL;
    }

    risp_eobject *list_exp = register_ephemeral_object(env, args->cdr->car);

    risp_object *index = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, list_exp);
        return NULL;
    }
    if (index->type != T_INT) {
        unregister_ephemeral_object(env, list_exp);
        signal_error_s(env, "index must be int");
        return NULL;
    }

    i64 n = index->integer;

    risp_object *list_arg = list_exp->o;
    unregister_ephemeral_object(env, list_exp);
    risp_object *list = eval_exp(env, list_arg);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    i64 len = list_length(env, list);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (n >= len) {
        signal_error_s(env, "Index out of range");
        return NULL;
    }

    risp_object *cur = list;
    for (i64 i = 0; i < n; ++i) {
        cur = cur->cdr;
    }

    return cur;
}

DEFUN(nthchar) {
    if (list_length(env, args) != 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "2 arguments required");
        }
        return NULL;
    }

    risp_eobject *str_exp = register_ephemeral_object(env, args->cdr->car);

    risp_object *index = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, str_exp);
        return NULL;
    }
    if (index->type != T_INT) {
        unregister_ephemeral_object(env, str_exp);
        signal_error_s(env, "index must be int");
        return NULL;
    }

    i64 n = index->integer;

    risp_object *str_arg = str_exp->o;
    unregister_ephemeral_object(env, str_exp);
    risp_object *str = eval_exp(env, str_arg);
    if (get_error(env) != &Qnil) {
        return NULL;
    }
    if (str->type != T_STRING) {
        signal_error_s(env, "argument must be string");
        return NULL;
    }
    if (n < 0) {
        signal_error_s(env, "Negative index is not permitted");
        return NULL;
    }

    if ((u64)n >= str->str_len) {
        signal_error_s(env, "Index out of range");
        return NULL;
    }

    u8 chr = str->str_data[n];
    risp_object *r = alloc_object(env, T_INT);
    r->integer = chr;
    return r;
}

DEFUN(plus) {
    list_length(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    risp_eobject *result = register_ephemeral_object(env, alloc_object(env, T_INT));
    result->o->integer = 0;

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, cur);
            unregister_ephemeral_object(env, result);
            return NULL;
        }

        if (target->type == T_INT) {
            result->o->integer += target->integer;
        } else {
            unregister_ephemeral_object(env, cur);
            unregister_ephemeral_object(env, result);

            signal_error_s(env, "argument type must be int");
            return NULL;
        }

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->cdr);
    }

    unregister_ephemeral_object(env, cur);
    risp_object *r = result->o;
    unregister_ephemeral_object(env, result);
    return r;
}

DEFUN(print) {
    list_length(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        if (target->type == T_INT) {
            printf("%ld\n", target->integer);
        } else if (target->type == T_STRING) {
            printf("%.*s\n", (int)target->str_len, target->str_data);
        } else {
            repr_object(env, target);
            putchar('\n');
        }

        risp_object *prev = cur->o;
        unregister_ephemeral_object(env, cur);
        cur = register_ephemeral_object(env, prev->cdr);
    }

    unregister_ephemeral_object(env, cur);

    return &Qnil;
}

DEFUN(progn) {
    list_length(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    risp_object *result = &Qnil;
    risp_eobject *body = register_ephemeral_object(env, args);
    while (body->o != &Qnil) {
        result = eval_exp(env, body->o->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, body);
            return NULL;
        }

        risp_object *next = body->o->cdr;
        unregister_ephemeral_object(env, body);
        body = register_ephemeral_object(env, next);
    }

    unregister_ephemeral_object(env, body);
    return result;
}

DEFUN(quote) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "1 argument required");
        }
        return NULL;
    }

    return args->car;
}

DEFUN(raise) {
    if (list_length(env, args) != 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "1 argument required");
        }
        return NULL;
    }
    signal_error(env, args->car);
    return NULL;
}

DEFUN(setcar) {
    if (list_length(env, args) != 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "2 arguments required");
        }
        return NULL;
    }

    risp_eobject *e_val = register_ephemeral_object(env, args->cdr->car);
    risp_object *cell = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, e_val);
        return NULL;
    }

    if (cell->type != T_CONS) {
        unregister_ephemeral_object(env, e_val);
        signal_error_s(env, "cell must be a cons cell");
        return NULL;
    }

    risp_eobject *ecell = register_ephemeral_object(env, cell);

    risp_object *val = e_val->o;
    unregister_ephemeral_object(env, e_val);

    val = eval_exp(env, val);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, ecell);
        return NULL;
    }

    ecell->o->car = val;
    unregister_ephemeral_object(env, ecell);

    return val;
}

DEFUN(setcdr) {
    if (list_length(env, args) != 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "2 arguments required");
        }
        return NULL;
    }

    risp_eobject *e_val = register_ephemeral_object(env, args->cdr->car);
    risp_object *cell = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, e_val);
        return NULL;
    }

    if (cell->type != T_CONS) {
        unregister_ephemeral_object(env, e_val);
        signal_error_s(env, "cell must be a cons cell");
        return NULL;
    }

    risp_eobject *ecell = register_ephemeral_object(env, cell);

    risp_object *val = e_val->o;
    unregister_ephemeral_object(env, e_val);

    val = eval_exp(env, val);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, ecell);
        return NULL;
    }

    ecell->o->cdr = val;
    unregister_ephemeral_object(env, ecell);

    return val;
}

DEFUN(setq) {
    i64 len = list_length(env, args);
    if (get_error(env) != &Qnil) {
        return NULL;
    }
    if (len % 2 != 0) {
        signal_error_s(env, "wrong argument count");
        return NULL;
    }

    risp_eobject *cur = register_ephemeral_object(env, args);
    risp_eobject *last = register_ephemeral_object(env, NULL);
    while (cur->o != &Qnil) {
        if (cur->o->car->type != T_SYMBOL) {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "variable must be symbol");
            return NULL;
        }
        risp_eobject *sym = register_ephemeral_object(env, cur->o->car);
        risp_object *val = eval_exp(env, cur->o->cdr->car);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, sym);
            unregister_ephemeral_object(env, last);
            unregister_ephemeral_object(env, cur);
            return NULL;
        }

        risp_eobject *e_val = register_ephemeral_object(env, val);
        scoped_set(env, sym->o, e_val->o);
        unregister_ephemeral_object(env, last);
        unregister_ephemeral_object(env, sym);

        last = e_val;

        risp_eobject *next = register_ephemeral_object(env, cur->o->cdr->cdr);
        unregister_ephemeral_object(env, cur);
        cur = next;
    }

    unregister_ephemeral_object(env, cur);
    risp_object *r = last->o;
    unregister_ephemeral_object(env, last);
    return r;
}

DEFUN(stringeq) {
    if (list_length(env, args) != 2) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "2 arguments expected");
        }
        return NULL;
    }

    risp_eobject *arg2 = register_ephemeral_object(env, args->cdr->car);

    risp_object *o1 = eval_exp(env, args->car);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, arg2);
        return NULL;
    }

    risp_eobject *eo1 = register_ephemeral_object(env, o1);

    risp_object *o2 = eval_exp(env, arg2->o);
    unregister_ephemeral_object(env, arg2);
    if (get_error(env) != &Qnil) {
        unregister_ephemeral_object(env, eo1);
        return NULL;
    }

    o1 = eo1->o;
    unregister_ephemeral_object(env, eo1);

    if (o1->type != T_STRING || o2->type != T_STRING) {
        signal_error_s(env, "Both arguments must be string");
        return NULL;
    }

    if (o1->str_len != o2->str_len) {
        return &Qnil;
    }

    return !memcmp(o1->str_data, o2->str_data, o1->str_len) ? &Qt : &Qnil;
}

DEFUN(while) {
    if (list_length(env, args) < 1) {
        if (get_error(env) == &Qnil) {
            signal_error_s(env, "condition required");
        }
        return NULL;
    }

    risp_eobject *cond = register_ephemeral_object(env, args->car);
    risp_eobject *body = register_ephemeral_object(env, args->cdr);

    for (;;) {
        risp_object *cond_result = eval_exp(env, cond->o);
        if (get_error(env) != &Qnil) {
            unregister_ephemeral_object(env, body);
            unregister_ephemeral_object(env, cond);
            return NULL;
        }

        if (cond_result == &Qnil) {
            break;
        }

        risp_eobject *cur_body = register_ephemeral_object(env, body->o);
        while (cur_body->o != &Qnil) {
            eval_exp(env, cur_body->o->car);
            if (get_error(env) != &Qnil) {
                unregister_ephemeral_object(env, cur_body);
                unregister_ephemeral_object(env, body);
                unregister_ephemeral_object(env, cond);
                return NULL;
            }

            risp_object *next = cur_body->o->cdr;
            unregister_ephemeral_object(env, cur_body);
            cur_body = register_ephemeral_object(env, next);
        }
        unregister_ephemeral_object(env, cur_body);
    }
    unregister_ephemeral_object(env, body);
    unregister_ephemeral_object(env, cond);

    return &Qnil;
}
