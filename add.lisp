;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defun sum-of-integers (end &optional (current 0) (value 0))
  (if (equal end current)
      (+ current value)
      (sum-of-integers end (1+ current) (+ value current))))

(sum-of-integers (coerce (read) 'number))

(reduce #'+ (loop for n below (read) collect n))
