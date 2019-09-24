;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defun fib (n &optional (np 0) (y 0) (x 1))
  (if (equal np n)
      x
      (fib n (+ np 1) x (+ y x))))
