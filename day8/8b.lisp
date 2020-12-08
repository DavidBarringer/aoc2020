(defstruct machine
  code
  (acc 0)
  (pos 0))

(defmacro make-hhgc (name code)
  `(setq name (make-machine :code ,code)))

(defun parse-input (l)
  (loop for s in l collect (cons (read-from-string (CAR (split " " s))) (mapcar 'parse-integer (CDR (split " " s))))))

(defun run-machine (name poslist)
  (cond ((null (nth (machine-pos name) (machine-code name))) (machine-acc name))
        ((null (find (machine-pos name) poslist))
          (setf poslist (cons (machine-pos name) poslist))
          (funcall (CAR (nth (machine-pos name) (machine-code name))) name (CDR (nth (machine-pos name) (machine-code name))))
          (run-machine name poslist))
        (t nil)))

(defun nop (name in)
  (setf (machine-pos name) (+ 1 (machine-pos name))))

(defun acc (name in)
  (setf (machine-acc name) (+ (CAR in) (machine-acc name)))
  (setf (machine-pos name) (+ 1 (machine-pos name))))

(defun jmp (name in)
  (setf (machine-pos name) (+ (CAR in) (machine-pos name))))

(defun replace-jmp (code)
  (loop for i from 0 to (- (length code) 1) if (eq 'jmp (CAR (nth i code)))
    collect (nconc (subseq code 0 i) (list (cons 'nop (CDR (nth i code)))) (nthcdr (+ i 1) code))))

(defun replace-nop (code)
  (loop for i from 0 to (- (length code) 1) if (eq 'nop (CAR (nth i code)))
    collect (nconc (subseq code 0 i) (list (cons 'jmp (CDR (nth i code)))) (nthcdr (+ i 1) code))))

(defun start ()
  (let ((input (parse-input (get-file "day8/day8.csv"))))
    (CAR (remove nil (loop for c in (nconc (replace-jmp input) (replace-nop input)) collect (run-machine (make-hhgc (gensym) c) nil))))))
