(defstruct machine
  code
  (acc 0)
  (pos 0))

(defmacro create-machine (name code)
  `(setq ,(eval name) (make-machine :code ,code)))

(defun parse-input (l)
  (loop for s in l collect (cons (read-from-string (CAR (split " " s))) (mapcar 'parse-integer (CDR (split " " s))))))

;;; Same as part a, except it checks if the machine reaches the end
(defun run-machine (name poslist)
  (cond ((null (nth (machine-pos name) (machine-code name))) (machine-acc name))
        ((null (find (machine-pos name) poslist))
          (setf poslist (cons (machine-pos name) poslist))
          (funcall (CAR (nth (machine-pos name) (machine-code name))) name (CDR (nth (machine-pos name) (machine-code name))))
          (run-machine name poslist))
        (t nil)))

(defun nop (name in)
  (setf (machine-pos (eval name)) (+ 1 (machine-pos (eval name)))))

(defun acc (name in)
  (setf (machine-acc (eval name)) (+ (CAR in) (machine-acc (eval name))))
  (setf (machine-pos (eval name)) (+ 1 (machine-pos (eval name)))))

(defun jmp (name in)
  (setf (machine-pos (eval name)) (+ (CAR in) (machine-pos (eval name)))))

;;; Creates a list of codes, each replacing jmp with nop at a different part of the code
;;; Does this by going through each instruction, checking if the op is "jmp".
;;; If it is, then collect the ops before and after and make a new one in between
(defun replace-jmp (code)
  (loop for i from 0 to (- (length code) 1) if (eq 'jmp (CAR (nth i code)))
    collect (nconc (subseq code 0 i) (list (cons 'nop (CDR (nth i code)))) (nthcdr (+ i 1) code))))

;;; As above but swap nop and jmp
(defun replace-nop (code)
  (loop for i from 0 to (- (length code) 1) if (eq 'nop (CAR (nth i code)))
    collect (nconc (subseq code 0 i) (list (cons 'jmp (CDR (nth i code)))) (nthcdr (+ i 1) code))))

;;; Makes and runs a machine for each possible code, collecting each result. The non-nil result is the answer.
(defun start ()
  (let ((input (parse-input (get-file "day8/day8.csv"))))
    (CAR (remove nil (loop for c in (nconc (replace-jmp input) (replace-nop input)) collect (run-machine (create-machine 'hhgc c) nil))))))
