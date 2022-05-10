#ifndef PRIMITIVE_H
#define PRIMITIVE_H

#include "rt.h"

#define DEFUN(name) risp_object *RISP_DEFUN_##name(risp_env *env, risp_object *args)
#define RISP_FUNC(name) RISP_DEFUN_##name

DEFUN(backquote);
DEFUN(car);
DEFUN(cdr);
DEFUN(coin);
DEFUN(clock);
DEFUN(clock_free);
DEFUN(clock_print_diff);
DEFUN(defun);
DEFUN(divide);
DEFUN(dolist);
DEFUN(eq);
DEFUN(funcall);
DEFUN(if);
DEFUN(intern);
DEFUN(lambda);
DEFUN(length);
DEFUN(let);
DEFUN(lt);
DEFUN(make_symbol);
DEFUN(minus);
DEFUN(multiply);
DEFUN(not );
DEFUN(nth);
DEFUN(nthcdr);
DEFUN(nthchar);
DEFUN(plus);
DEFUN(print);
DEFUN(progn);
DEFUN(quote);
DEFUN(raise);
DEFUN(setcar);
DEFUN(setcdr);
DEFUN(setq);
DEFUN(stringeq);
DEFUN(while);

#endif
