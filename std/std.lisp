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

(defun caar (cons)
  (car (car cons)))

(defun cdar (cons)
  (cdr (car cons)))

(defun cadar (cons)
  (car (cdr (car cons))))

(defmacro setf (&rest vals)
  (let ((result '(progn)))
    (let ((tail result))
      (while vals
        (if (< (length vals) 2)
            (error "vals must be multiple of 2"))
        (if (symbolp (car vals))
            (progn
              (setcdr tail `((setq ,(car vals) ,(nth 1 vals))))
              (setq tail (cdr tail))))
        (if (consp (car vals))
            (progn
              (setcdr tail
                      (cond
                        ((eq (caar vals) 'car)
                         `((setcar ,(cadar vals) ,(nth 1 vals))))
                        ((eq (caar vals) 'cdr)
                         `((setcdr ,(cadar vals) ,(nth 1 vals))))
                        ((eq (caar vals) 'car)
                         `((setcar ,(cadar vals) ,(nth 1 vals))))
                        ((eq (caar vals) 'nth)
                         `((setcar (nthcdr ,(cadar vals) ,(nth 1 (cdar vals))) ,(nth 1 vals))))
                        (t (error "Unsupported target of setf"))))
              (setq tail (cdr tail))))
        (if (consp (car vals))
            ())
        (setq vals (cdr (cdr vals)))))
    result))
