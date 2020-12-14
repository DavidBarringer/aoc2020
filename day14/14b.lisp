(defvar memory)

;;; Same as part a, but the memory address is the 36-bit-binary-char-list
(defun parse-input (l)
  (loop for m in (mapcar (lambda (s) (split "mem" s))
                         (CDR (split "mask" (remove #\[ (remove #\  (remove #\= (remove #\Newline l))))))) collect
    (list (concatenate 'list (CAR m)) (loop for c in (CDR m) for mem = (split "]" c) collect
                                        (cons (concatenate 'list (format nil "~36,'0B" (parse-integer (CAR mem))))
                                              (parse-integer (CADR mem)))))))

;;; Variates on part a by having res: a list of results.
;;; When X is called, a branch is made. The number of lists in res gets doubled, half having 1 added, the other having 0 added
(defun apply-mask (val mask res)
  (cond ((null val) res)
        ((eq #\X (CAR mask)) (apply-mask (CDR val) (CDR mask) (nconc (mapcar (lambda (s) (cons #\1 s)) res)
                                                                     (mapcar (lambda (s) (cons #\0 s)) res))))
        ((eq #\0 (CAR mask)) (apply-mask (CDR val) (CDR mask) (mapcar (lambda (s) (cons (CAR val) s)) res)))
        (t (apply-mask (CDR val) (CDR mask) (mapcar (lambda (s) (cons (CAR mask) s)) res)))))

;;; Similar to part a, but the addresses from apply-mask are converted to base 10 int before being called as hash key
(defun to-memory (l)
  (loop for grp in (CADR l) collect (mapcar (lambda (s) (setf (gethash (parse-integer (concatenate 'string s) :radix 2) memory) (CDR grp)))
                                            (apply-mask (CAR grp) (CAR l) '(())))))

(defun start ()
  (setq memory (make-hash-table))
  (mapcar 'to-memory (parse-input (get-file-string "day14/day14.csv")))
  (loop for v being each hash-value in memory sum v))
