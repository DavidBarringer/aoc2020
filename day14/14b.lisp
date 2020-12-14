(defun parse-input (l)
  (loop for m in (mapcar (lambda (s) (split "mem" s)) (CDR (split "mask" (remove #\[ (remove #\  (remove #\= (remove #\Newline l))))))) collect
    (list (concatenate 'list (CAR m)) (loop for c in (CDR m) for mem = (split "]" c) collect
                                        (cons (concatenate 'list (format nil "~36,'0B" (parse-integer (CAR mem)))) (parse-integer (CADR mem)))))))

(defun apply-mask (val mask res)
  (cond ((null val) res)
        ((eq #\X (CAR mask)) (apply-mask (CDR val) (CDR mask) (nconc (mapcar (lambda (s) (cons #\1 s)) res)
                                                                     (mapcar (lambda (s) (cons #\0 s)) res))))
        ((eq #\0 (CAR mask)) (apply-mask (CDR val) (CDR mask) (mapcar (lambda (s) (cons (CAR val) s)) res)))
        (t (apply-mask (CDR val) (CDR mask) (mapcar (lambda (s) (cons (CAR mask) s)) res)))))

(defun to-memory (l)
  (loop for grp in (CADR l) nconc (mapcar (lambda (l) (cons (parse-integer (concatenate 'string l) :radix 2) (CDR grp)))
                                            (apply-mask (CAR grp) (CAR l) '(())))))

(defun start ()
  (let ((mem (remove-duplicates (apply 'append (mapcar 'to-memory (parse-input (get-file-string "day14/day14.csv"))))
                                :test (lambda (x y) (= (CAR x) (CAR y))))))
    (apply '+ (mapcar 'CDR mem))))
