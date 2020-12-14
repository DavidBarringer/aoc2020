(defun parse-input (l)
  (loop for m in (mapcar (lambda (s) (split "mem" s)) (CDR (split "mask" (remove #\[ (remove #\  (remove #\= (remove #\Newline l))))))) collect
    (list (concatenate 'list (CAR m)) (loop for c in (CDR m) for mem = (split "]" c) collect
                                        (cons (parse-integer (CAR mem)) (concatenate 'list (format nil "~36,'0B" (parse-integer (CADR mem)))))))))

(defun apply-mask (val mask)
  (remove nil (cond ((null val) nil)
                    ((eq #\X (CAR mask)) (cons (CAR val) (apply-mask (CDR val) (CDR mask))))
                    (t (cons (CAR mask) (apply-mask (CDR val) (CDR mask)))))))

(defun to-memory (l)
  (loop for grp in (CADR l) collect (cons (CAR grp) (parse-integer (concatenate 'string (apply-mask (CDR grp) (CAR l))) :radix 2))))

(defun start ()
  (let ((mem (remove-duplicates (apply 'append (mapcar 'to-memory (parse-input (get-file-string "day14/day14.csv"))))
                                :test (lambda (x y) (= (CAR x) (CAR y))))))
    (apply '+ (mapcar 'CDR mem))))
