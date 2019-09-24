;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defun flatten (list)
  (if (some #'listp list)
      (flatten (reduce
                #'(lambda (a b)
                    (append (if (listp a) a (list a))
                            (if (listp b) b (list b))))
                list))
      list))
