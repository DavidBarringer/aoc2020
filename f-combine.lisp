(defun *>> (target f1 &rest funcs)
  (cons (apply f1 target) (loop for f in funcs collect (apply f target))))

(defun >> (target f1 &rest funcs)
  (cons (funcall f1 target) (loop for f in funcs collect (funcall f target))))

(defun *<< (func t1 &rest targets)
  (cons (apply func t1) (loop for target in targets collect (apply func target))))

(defun << (func t1 &rest targets)
  (cons (funcall func t1) (loop for target in targets collect (funcall func target))))
