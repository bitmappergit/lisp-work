;; copyright is attributed to mveety (matthew veety)

(defmacro defthread (sym (&key (name "anonymous thread") (place nil)) &body body)
  (if (eq place nil)
      `(defun ,sym ()
         (sb-thread:make-thread
          (lambda ()
            ,@body)
          :name ,name))
      `(defun ,sym ()
         (setf ,place (sb-thread:make-thread
                       (lambda ()
                         ,@body)
                       :name ,name)))))
