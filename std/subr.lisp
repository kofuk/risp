(defmacro dolist (spec &rest body)
  (if (not (listp spec))
      (raise "spec must be a list"))
  (if (not (< 1 (length spec) 4))
      (raise "invalid spec (var list [result])"))
  (let ((result))
    (if (< 2 (length spec))
        (setq result (nth 2 spec)))
    `(let ((,(nth 0 spec)) (_list-head ,(nth 1 spec)))
       (while _list-head
         (setq ,(nth 0 spec) (car _list-head))
         ,@body
         (setq _list-head (cdr _list-head)))
       ,result)))
