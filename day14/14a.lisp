(defvar memory)

;;; Takes in a string, and returns ((mask-chars) ((memory-address . (36-bit-binary-chars-value)) ...))
;;; How? First the "junk" is removed i.e. ("[" "=" " " "\n") then split on "mask"
;;; In each resulting list, we split on mem. It looks something like ("mask-string" (("address]value") ...)).
;;; Go through this list, turn the first element into a list of chars put it in a list with:
;;; Going through the ("address]value") list, split on "]", turn the address into an int and make it into a dotted pair with:
;;; Turn the value into an int, then format it as a binary string 36 chars long, padding whitespace with 0s, then put into char list
(defun parse-input (l)
  (loop for m in (mapcar (lambda (s) (split "mem" s))
                         (CDR (split "mask" (remove #\[ (remove #\  (remove #\= (remove #\Newline l))))))) collect
    (list (concatenate 'list (CAR m)) (loop for c in (CDR m) for mem = (split "]" c) collect
                                        (cons (parse-integer (CAR mem))
                                              (concatenate 'list (format nil "~36,'0B" (parse-integer (CADR mem)))))))))

;;; For each char in val, mask if mask-char is "X", take the val-char, else take the mask-char
(defun apply-mask (val mask)
  (remove nil (cond ((null val) nil)
                    ((eq #\X (CAR mask)) (cons (CAR val) (apply-mask (CDR val) (CDR mask))))
                    (t (cons (CAR mask) (apply-mask (CDR val) (CDR mask)))))))

;;; For each (memory . value) pair, put into hash table :key memory, :value base 10 number of the result of apply-mask
(defun to-memory (l)
  (loop for grp in (CADR l) collect (setf (gethash (CAR grp) memory)
                                          (parse-integer (concatenate 'string (apply-mask (CDR grp) (CAR l))) :radix 2))))

(defun start ()
  (setq memory (make-hash-table)) ;New hash table for memory
  (mapcar 'to-memory (parse-input (get-file-string "day14/day14.csv"))) ;Populate hash table
  (loop for v being each hash-value in memory sum v)) ;Sum values in hash table
