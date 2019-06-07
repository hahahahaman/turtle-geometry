(in-package :turtle-geometry)

(defclass program-manager (resource-manager)
  ())

(defun load-program (name resource &optional (manager *program-manager*))
  (let ((program (@ (resources manager) name)))
    (when program
      (gl:delete-program (id program))))
  (load-resource name resource manager))

(defun get-program (name &optional (manager *program-manager*))
  (get-resource name manager))

(defmethod clear-resources ((manager program-manager))
  (do-map (name resource (resources manager))
    (declare (ignore name))
    (gl:delete-program (id resource))))
