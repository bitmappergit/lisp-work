;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defun fac (x &optional (y 1))
  (if (equal x 1)
      y
      (fac (- x 1) (* y x))))
