;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defvar *list* '("Harry Potter And The Half Blood Prince"
                 "Harry Potter And The Chamber Of Secrets"
                 "Healthy Eaters"
                 "Hardship Posting"
                 "Vogue Patterns"
                 "The Kirbhiz Pattern"
                 "Stromy Weather"))

(defvar *list-new* '("apple"
                     "pepsi"
                     "coca-cola"
                     "test"))
;; not mine
(defun all-positions (needle haystack)
  (loop
     for element in haystack 
     and position from 0
     when (eql element needle)
     collect position))

(defun sort-list (list)
  (sort list #'< :key #'car))

;; not mine
(defun substringp (needle haystack &key (test 'char=))
  (search (string needle)
          (string haystack)
          :test test))

(defun string-contains (string substring)
  (if (equal (substringp substring string) nil)
      nil
      t))
  
(defun string-does-not-contain (string substring)
  (if (equal (substringp substring string) nil)
      t
      nil))

(defun check-item (list search)
  (string-contains (car (last list)) search))

(defun check-not-item (list search)
  (string-does-not-contain (car (last list)) search))

(defmacro search-list (list search test)
  `(map 'list
        #'(lambda (x) (,test x ,search))
        ,list))

(defmacro get-search-results (list search test)
  `(map 'list
        #'(lambda (x) (nth x ,list))
        (all-positions t (search-list ,list ,search ,test))))

(defun flatten-results (a b)
  `(,@a ,@b))

(defun search-list-and-sort (list search)
  (flatten-results (sort-list (get-search-results list search check-item))
                   (sort-list (get-search-results list search check-not-item))))

(defun print-results (results)
  (map 'list
       #'(lambda (x) (pprint (car (last x))))
       results))

;; not mine
(defun levenshtein (a b)
  (let* ((la  (length a))
         (lb  (length b))
         (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
    
    (defun leven (x y)
      (cond
        ((zerop x) y)
        ((zerop y) x)
        ((aref rec x y) (aref rec x y))
        (t (setf (aref rec x y)
                 (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
                    (min (leven (1- x) y)
                         (leven x (1- y))
                         (leven (1- x) (1- y))))))))
    (leven la lb)))

(defmacro generate-pre-results (search items)
  `(map 'list #'(lambda (x)
                  (list (levenshtein ,search x) x))
        ,items))

(defun read-in (current &optional items)
  (let* ((input (read-char))
         (new (concatenate 'string current (string input))))
    (print-results (search-list-and-sort (generate-pre-results new items) new))
    (if (equal input #\Newline)
        current
        (read-in (concatenate 'string
                              current
                              (string input))))))

;(defun read-and-complete ()
  
