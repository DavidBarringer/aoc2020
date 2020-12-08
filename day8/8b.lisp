(defstruct machine
  code
  (acc 0)
  (pos 0))

(defun make-hhgc (code)
  (setq hhgc (make-machine :code code)))

(defun parse-input (l)
  (loop for s in l collect (cons (read-from-string (CAR (split " " s))) (mapcar 'parse-integer (CDR (split " " s))))))

(defun rebuild-machine (code)
  (make-hhgc code)
  (run-machine nil))

(defun run-machine (poslist)
  (cond ((null (nth (machine-pos hhgc) (machine-code hhgc))) (machine-acc hhgc))
        ((null (find (machine-pos hhgc) poslist))
          (setf poslist (cons (machine-pos hhgc) poslist))
          (funcall (CAR (nth (machine-pos hhgc) (machine-code hhgc))) (CDR (nth (machine-pos hhgc) (machine-code hhgc))))
          (run-machine poslist))
        (t nil)))

(defun nop (in)
  (setf (machine-pos hhgc) (+ 1 (machine-pos hhgc))))

(defun acc (in)
  (setf (machine-acc hhgc) (+ (CAR in) (machine-acc hhgc)))
  (setf (machine-pos hhgc) (+ 1 (machine-pos hhgc))))

(defun jmp (in)
  (setf (machine-pos hhgc) (+ (CAR in) (machine-pos hhgc))))

(defun replace-jmp (code)
  (loop for i from 0 to (- (length code) 1) if (eq 'jmp (CAR (nth i code)))
    collect (nconc (subseq code 0 i) (list (cons 'nop (CDR (nth i code)))) (nthcdr (+ i 1) code))))

(defun replace-nop (code)
  (loop for i from 0 to (- (length code) 1) if (eq 'nop (CAR (nth i code)))
    collect (nconc (subseq code 0 i) (list (cons 'jmp (CDR (nth i code)))) (nthcdr (+ i 1) code))))

(defun start ()
  (let ((input (parse-input (get-file "day8/day8.csv"))))
    (CAR (remove nil (loop for c in (nconc (replace-jmp input) (replace-nop input)) collect (rebuild-machine c))))))
