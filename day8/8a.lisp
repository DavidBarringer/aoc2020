(defstruct machine
  code
  (acc 0)
  (pos 0))

(defmacro make-hhgc (name code)
  `(setq name (make-machine :code ,code)))

(defun parse-input (l)
  (loop for s in l collect (cons (read-from-string (CAR (split " " s))) (mapcar 'parse-integer (CDR (split " " s))))))

(defun run-machine (name poslist)
  (cond ((null (find (machine-pos name) poslist))
          (setf poslist (cons (machine-pos name) poslist))
          (funcall (CAR (nth (machine-pos name) (machine-code name))) name (CDR (nth (machine-pos name) (machine-code name))))
          (run-machine name poslist))
        (t (machine-acc name))))

(defun nop (name in)
  (setf (machine-pos name) (+ 1 (machine-pos name))))

(defun acc (name in)
  (setf (machine-acc name) (+ (CAR in) (machine-acc name)))
  (setf (machine-pos name) (+ 1 (machine-pos name))))

(defun jmp (name in)
  (setf (machine-pos name) (+ (CAR in) (machine-pos name))))

(defun start ()
  (run-machine (make-hhgc (gensym) (parse-input (get-file "day8/day8.csv"))) nil))
