#ifndef PRIMITIVE_H
#define PRIMITIVE_H

#include "rt.h"

#define DEFUN(name) risp_object *RISP_DEFUN_##name(risp_env *env, risp_object *args)
#define RISP_FUNC(name) RISP_DEFUN_##name

DEFUN(backquote);
DEFUN(defun);
DEFUN(divide);
DEFUN(dolist);
DEFUN(eq);
DEFUN(funcall);
DEFUN(if);
DEFUN(intern);
DEFUN(lambda);
DEFUN(length);
DEFUN(lt);
DEFUN(make_symbol);
DEFUN(minus);
DEFUN(multiply);
DEFUN(plus);
DEFUN(print);
DEFUN(progn);
DEFUN(quote);
DEFUN(setq);
DEFUN(while);

#endif
