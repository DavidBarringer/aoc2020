;;; Structure of a machine, accumulator and position are initialised to 0
(defstruct machine
  code
  (acc 0)
  (pos 0))

;;; Creates a machine with given name and code
(defmacro create-machine (name code)
  `(setq ,(eval name) (make-machine :code ,code)))

;;; Split on space, collect operations with their arguments
(defun parse-input (l)
  (loop for s in l collect (cons (read-from-string (CAR (split " " s))) (mapcar 'parse-integer (CDR (split " " s))))))

;;; Checks if position has already been seen. If it has return the accumulator of the machine
;;; If it hasn't, add it to list then call the function with arguments and run the next iteration of the machine
(defun run-machine (name poslist)
  (cond ((null (find (machine-pos name) poslist))
          (setf poslist (cons (machine-pos name) poslist))
          (funcall (CAR (nth (machine-pos name) (machine-code name))) name (CDR (nth (machine-pos name) (machine-code name))))
          (run-machine name poslist))
        (t (machine-acc name))))

;;; Function just increase position
(defun nop (name in)
  (setf (machine-pos (eval name)) (+ 1 (machine-pos (eval name)))))

;;; Add argument to accumulator of given machine
(defun acc (name in)
  (setf (machine-acc (eval name)) (+ (CAR in) (machine-acc (eval name))))
  (setf (machine-pos (eval name)) (+ 1 (machine-pos (eval name)))))

;;; Add argument to position of given machine
(defun jmp (name in)
  (setf (machine-pos (eval name)) (+ (CAR in) (machine-pos (eval name)))))

(defun start ()
  (run-machine (create-machine (gensym) (parse-input (get-file "day8/day8.csv"))) nil))
