;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :turtle-geometry)

(defmacro add-event (&key code (priority 1))
  `(vector-push-extend (cons ,priority (lambda () ,code))
                       *destructive-changes*))

(defmacro defevent (name parameters &body body)
  "Creates a function that calls ADD-EVENT."
  `(defun ,name ,parameters
     (add-event :code (progn ,@body))))

(defun update-events ()
  (let* ((changes *destructive-changes*))

    (sort changes #'< :key #'car)
    (iter (for v in-vector changes)
          (funcall (cdr v)))

    ;; reinitialize the array removing all changes
    (setf (fill-pointer *destructive-changes*) 0))
  t)
