(if (not (eq (length args) 5))
    (raise "4 arguments required"))

(setq begin (clock))

(setq sol (coin (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)))

(dolist (e sol)
  (print e))
(print (length sol))

(setq end (clock))

(clock-print-diff end begin)

(clock-free begin)
(clock-free end)
