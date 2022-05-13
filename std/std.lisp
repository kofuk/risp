(setq risp-version "1.0.0")

(defmacro dolist (spec &rest body)
  (if (not (listp spec))
      (error "spec must be a list"))
  (if (not (< 1 (length spec) 4))
      (error "invalid spec (var list [result])"))
  (let ((result))
    (if (< 2 (length spec))
        (setq result (nth 2 spec)))
    `(let ((,(car spec)) (_list-head ,(nth 1 spec)))
       (while _list-head
         (setq ,(car spec) (car _list-head))
         ,@body
         (setq _list-head (cdr _list-head)))
       ,result)))
