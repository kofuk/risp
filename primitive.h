#ifndef PRIMITIVE_H
#define PRIMITIVE_H

#include "rt.h"

#define DEFUN(name) risp_object *RISP_DEFUN_##name(risp_env *env, risp_object *args)
#define RISP_FUNC(name) RISP_DEFUN_##name

DEFUN(and);
DEFUN(backquote);
DEFUN(car);
DEFUN(cdr);
DEFUN(concat);
DEFUN(cond);
DEFUN(consp);
DEFUN(defmacro);
DEFUN(defun);
DEFUN(divide);
DEFUN(eq);
DEFUN(eq_num);
DEFUN(error);
DEFUN(funcall);
DEFUN(functionp);
DEFUN(ge);
DEFUN(gt);
DEFUN(if);
DEFUN(integerp);
DEFUN(intern);
DEFUN(lambda);
DEFUN(length);
DEFUN(let);
DEFUN(listp);
DEFUN(load);
DEFUN(le);
DEFUN(lt);
DEFUN(macrop);
DEFUN(make_symbol);
DEFUN(minus);
DEFUN(multiply);
DEFUN(native_function_p);
DEFUN(native_handle_p);
DEFUN(not );
DEFUN(nth);
DEFUN(nthcdr);
DEFUN(nthchar);
DEFUN(or);
DEFUN(plus);
DEFUN(print);
DEFUN(progn);
DEFUN(quote);
DEFUN(setcar);
DEFUN(setcdr);
DEFUN(setq);
DEFUN(stringeq);
DEFUN(stringp);
DEFUN(symbolp);
DEFUN(while);

#endif
