(in-package #:turtle-geometry)

(defun make-turtle (&key
                      (position (vec3f 0.0 0.0 0.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (color (vec4f 1.0 1.0 1.0 1.0)))
  "A turtle is an immutable map storing variables."
  (map (:position position)
       (:rotation rotation)
       (:color color)
       (:pen-down-p t)))

(defun add-turtle-data (array &optional (turtle *turtle*))
  "Adds the turtle's current position and color to a gl-dynamic-array."
  (iter (for i in-vector (@ turtle :position))
    (gl-dyn-push array i))
  (iter (for i in-vector (@ turtle :color))
    (gl-dyn-push array i))
  array)
