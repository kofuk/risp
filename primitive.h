#ifndef PRIMITIVE_H
#define PRIMITIVE_H

#include "rt.h"

#define DEFUN(name) risp_object *RISP_DEFUN_##name(risp_env *env, risp_object *args, u32 caller_level)
#define RISP_FUNC(name) RISP_DEFUN_##name

DEFUN(divide);
DEFUN(eq);
DEFUN(intern);
DEFUN(length);
DEFUN(make_symbol);
DEFUN(minus);
DEFUN(multiply);
DEFUN(plus);
DEFUN(print);
DEFUN(quote);
DEFUN(setq);

#endif
