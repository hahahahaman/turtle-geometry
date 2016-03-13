(in-package #:turtle-geometry)

(defun make-turtle (&optional
                      (position (vec3f 0.0 0.0 0.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (color (vec4f 1.0 1.0 1.0 1.0)))
  (map (:position position)
       (:rotation rotation)
       (:color color)
       (:pen-down-p t)))

(defun add-turtle-data (array &optional (turtle *turtle*))
  (iter (for i in-vector (@ turtle :position))
    (vector-push-extend i array))
  (iter (for i in-vector (@ turtle :color))
    (vector-push-extend i array))
  array)