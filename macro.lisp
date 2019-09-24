;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defmacro -> (begin &rest items)
  (reduce #'(lambda (x y)
              (reverse (cons x y)))
          (mapcar #'reverse items)
          :initial-value begin))

(defmacro -<> (begin &rest items)
  (reduce #'(lambda (x y)
              (reverse (substitute x '<> y)))
          (mapcar #'reverse items)
          :initial-value begin))


(-> 1
    (+ 1)
    (* 2)
    (* 100)
    (/ 2))

(macroexpand '(-> 1
                  (+ 1)
                  (* 2)
                  (* 100)
                  (/ 2)))

(-<> 1
     (substitute 'new1 <> '(1 2 3 4))
     (substitute 'new2 2 <>))

(macroexpand '(-<> 1
                   (list 'new1 <> '(1 2 3 4))
                   (list 'new2 2 <>)))
