#ifndef RISP_RT_H
#define RISP_RT_H

#include "parse.h"
#include "risp.h"

typedef enum {
    T_CONS = 1,
    T_STRING,
    T_SYMBOL,
    T_KWSYMBOL,
    T_INT,
    T_FUNC,
    T_NATIVE_FUNC,
    T_NATIVE_HANDLE,
} risp_type;

typedef struct risp_object risp_object;
typedef struct risp_env risp_env;

typedef risp_object *(*risp_native_func)(risp_env *env, risp_object *args, u32 caller_level);

struct risp_object {
    risp_type type;
    usize size; // should be pow of sizeof(void *)
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
        void *native_handle;
    } d;
    u8 str_data[];
};

typedef struct risp_vars {
    struct risp_object *vars; // alist of symbol and its value.
    struct risp_vars *parent;
    struct risp_vars *prev;
} risp_vars;

extern risp_object Qnil;
extern risp_object Qt;

// risp_eobject is a wrapper for risp_object to avoid ephemeral objects to be freed and keep track
// of them.
typedef struct risp_eobject {
    risp_object *o;
    struct risp_eobject *next;
} risp_eobject;

struct risp_env {
    void *heap;
    usize heap_len;
    usize heap_cap;
    risp_vars *var_list; // last element of variable list
    risp_eobject *ephemeral;
    risp_object *obarray; // interned symbols
    risp_object *error;
    u32 flags;
};

risp_eobject *register_ephemeral_object(risp_env *env, risp_object *obj);
void unregister_ephemeral_object(risp_env *env, risp_eobject *registered);

void signal_error(risp_env *env, risp_object *err);
void signal_error_s(risp_env *env, const char *msg);
risp_object *get_error(risp_env *env);
void clear_error(risp_env *env);

risp_object *eval_exp(risp_env *env, risp_object *exp);

risp_object *alloc_object(risp_env *env);
risp_object *alloc_str_like(risp_env *env, usize len);

void make_local_variable(risp_env *env, risp_eobject *symbol, risp_eobject *value);
void make_global_variable(risp_env *env, risp_eobject *symbol, risp_eobject *value);
void scoped_set(risp_env *env, risp_eobject *symbol, risp_eobject *value);

void var_frame_free_all(risp_env *env);
void env_init(risp_env *env);
void init_native_functions(risp_env *env);
i32 read_and_eval(lexer *lex, risp_env *env);

#endif