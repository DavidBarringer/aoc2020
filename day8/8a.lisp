(defstruct machine
  code
  (acc 0)
  (pos 0))

(defun make-hhgc (code)
  (setq hhgc (make-machine :code code)))

(defun parse-input (l)
  (loop for s in l collect (cons (read-from-string (CAR (split " " s))) (mapcar 'parse-integer (CDR (split " " s))))))

(defun run-machine (poslist)
  (cond ((null (find (machine-pos hhgc) poslist))
          (setf poslist (cons (machine-pos hhgc) poslist))
          (funcall (CAR (nth (machine-pos hhgc) (machine-code hhgc))) (CDR (nth (machine-pos hhgc) (machine-code hhgc))))
          (run-machine poslist))
        (t (machine-acc hhgc))))

(defun nop (in)
  (setf (machine-pos hhgc) (+ 1 (machine-pos hhgc))))

(defun acc (in)
  (setf (machine-acc hhgc) (+ (CAR in) (machine-acc hhgc)))
  (setf (machine-pos hhgc) (+ 1 (machine-pos hhgc))))

(defun jmp (in)
  (setf (machine-pos hhgc) (+ (CAR in) (machine-pos hhgc))))

(defun start ()
  (make-hhgc (parse-input (get-file "day8/day8.csv")))
  (run-machine nil))
