(defun parse-input (l)
  (let ((l1 (mapcar (lambda (s) (split "," s)) (mapcar (lambda (s) (substitute #\, #\  s)) (split ",," (format nil "~{~A,~}" l))))))
          (loop for x in l1 collect (mapcar (lambda (s) (split ":" s)) x))))

;;; This function works similar to part a, but applies the function from the check list on the value before verifying.
(defun check-valid (l check)
    (= 7 (loop for f1 in check count (loop for f2 in l if (string= (CAR f2) (CAR f1)) if (apply (CDR f1) (list (CADR f2))) collect t))))

;;; Returns true if given value appears in list
(defun ecl-check (val)
  (notevery #'null (mapcar (lambda (n) (string= n val)) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))

(defun byr-check (val)
  (AND (<= 1920 (parse-integer val)) (>= 2002 (parse-integer val))))

(defun iyr-check (val)
  (AND (<= 2010 (parse-integer val)) (>= 2020 (parse-integer val))))

(defun eyr-check (val)
  (AND (<= 2020 (parse-integer val)) (>= 2030 (parse-integer val))))

;;; Regex-free parsing of height strings, gets last 2 chars, makes the rest into a number
;;; Compares string with measurement and number with the ranges
;;; If these fail, return nil
(defun hgt-check (val)
  (let ((end-chars (subseq val (- (length val) 2)))
        (hgt (parse-integer (subseq val 0 (- (length val) 2)) :junk-allowed t)))
          (cond  ((AND (string= "in" end-chars) (<= 50 hgt) (>= 76 hgt)) t)
                  ((AND (string= "cm" end-chars) (<= 150 hgt) (>= 193 hgt)) t)
                  (t nil))))

;;; Regex-free parsing of haircolour, has a list of valid chars.
;;; If string starts with #, then checks for 6 matches of valid chars.
(defun hcl-check (val)
  (let ((valid-chars '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\a #\b #\c #\d #\e #\f)))
    (if (eq (char val 0) #\#)
      (= 6 (loop for i in (concatenate 'list val) if (member i valid-chars) count i)) nil)))

;;; If each character in string is a number and there are 9 of them, return t.
(defun pid-check (val)
  (= 9 (loop for c in (concatenate 'list val) for d = (digit-char-p c) count d)))

;;; Same as part a but with function names paired to each field
(defun start()
  (let ((check '(("ecl" . ecl-check) ("pid" . pid-check) ("eyr" . eyr-check) ("hcl" . hcl-check)
                                     ("byr" . byr-check) ("iyr" . iyr-check) ("hgt" . hgt-check)))) ; A pair of each list and their check function
          (loop for pp in (parse-input (get-file "day4/day4.csv")) if (check-valid pp check) count pp)))
