#include <string.h>

#include "primitive.h"
#include "rt.h"

static i64 list_length(risp_env *env, risp_object *list) {
    i64 result = 0;
    for (risp_object *arg = list; arg != &Qnil; arg = arg->d.cons.cdr) {
        ++result;

        risp_object *next = arg->d.cons.cdr;
        if (next != &Qnil && next->type != T_CONS) {
            signal_error_s(env, "argument must be a list or string");
            return -1;
        }
    }
    return result;
}

DEFUN(eq) {
    UNUSED(caller_level);

    if (list_length(env, args) != 2) {
        signal_error_s(env, "2 arguments expected");
        return NULL;
    }

    risp_eobject *arg2 = register_ephemeral_object(env, args->d.cons.cdr->d.cons.car);

    risp_object *o1 = eval_exp(env, args->d.cons.car);
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
        return o1->d.integer == o2->d.integer ? &Qt : &Qnil;
    }

    return o1 == o2 ? &Qt : &Qnil;
}

DEFUN(intern) {
    UNUSED(caller_level);

    if (list_length(env, args) != 1) {
        signal_error_s(env, "just 1 argument expected");
        return NULL;
    }

    risp_object *name_obj = eval_exp(env, args->d.cons.car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (name_obj->type != T_STRING) {
        signal_error_s(env, "Name must be a string");
        return NULL;
    }

    char name[name_obj->d.str_len + 1];
    name[name_obj->d.str_len] = '\0';
    memcpy(name, name_obj->str_data, name_obj->d.str_len);

    return intern_symbol(env, name);
}

DEFUN(length) {
    UNUSED(caller_level);

    if (list_length(env, args) != 1) {
        signal_error_s(env, "just 1 argument expected");
        return NULL;
    }

    i64 result = 0;
    risp_object *target = eval_exp(env, args->d.cons.car);

    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (target == &Qnil) {
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

DEFUN(make_symbol) {
    UNUSED(caller_level);

    if (list_length(env, args) != 1) {
        signal_error_s(env, "just 1 argument expected");
        return NULL;
    }

    risp_object *name_obj = eval_exp(env, args->d.cons.car);
    if (get_error(env) != &Qnil) {
        return NULL;
    }

    if (name_obj->type != T_STRING) {
        signal_error_s(env, "Name must be a string");
        return NULL;
    }

    risp_eobject *name = register_ephemeral_object(env, name_obj);
    risp_object *sym = alloc_str_like(env, name_obj->d.str_len);
    sym->type = T_SYMBOL;
    memcpy(sym->str_data, name->o->str_data, name->o->d.str_len);
    unregister_ephemeral_object(env, name);

    return sym;
}

DEFUN(plus) {
    UNUSED(caller_level);

    risp_eobject *result = register_ephemeral_object(env, alloc_object(env));
    result->o->type = T_INT;
    result->o->d.integer = 0;

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->d.cons.car);
        if (get_error(env) != &Qnil) {
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

DEFUN(print) {
    UNUSED(caller_level);

    risp_eobject *cur = register_ephemeral_object(env, args);

    while (cur->o != &Qnil) {
        risp_object *target = eval_exp(env, cur->o->d.cons.car);
        if (get_error(env) != &Qnil) {
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

    return &Qnil;
}

DEFUN(quote) {
    UNUSED(env);
    UNUSED(caller_level);
    return args;
}

DEFUN(setq) {
    UNUSED(caller_level);

    if (list_length(env, args) % 2 != 0) {
        signal_error_s(env, "wrong argument count");
        return NULL;
    }

    risp_eobject *cur = register_ephemeral_object(env, args);
    risp_eobject *last = register_ephemeral_object(env, NULL);
    while (cur->o != &Qnil) {
        if (cur->o->d.cons.car->type != T_SYMBOL) {
            unregister_ephemeral_object(env, cur);

            signal_error_s(env, "variable must be symbol");
            return NULL;
        }
        risp_eobject *sym = register_ephemeral_object(env, cur->o->d.cons.car);
        risp_object *val = eval_exp(env, cur->o->d.cons.cdr->d.cons.car);
        if (get_error(env) != &Qnil) {
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
