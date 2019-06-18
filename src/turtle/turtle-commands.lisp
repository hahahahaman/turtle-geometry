;;;; turtle-geometry.lisp

(in-package #:turtle-geometry)

;;; "turtle-geometry" goes here. Hacks and glory await!

(defun clear (&key (line-drawer *line-drawer*))
  ;; reset turtle
  ;; reset line-drawer
  ;; reset camera
  (clear-world *world*)
  (let ((array (make-instance 'gl-dynamic-array :array-type :float)))
    (with-slots (num-vertices draw-array) line-drawer
      (setf *turtle* nil
            *camera* (make-instance 'camera)
            num-vertices 0
            draw-array array))))

(defun clr (&key (line-drawer *line-drawer*))
  (clear :line-drawer line-drawer))

;; (defun pen-toggle ()
;;   (includef *turtle* :pen-down-p (not (@ *turtle* :pen-down-p))))

;; (defun pen-down ()
;;   (unless (@ *turtle* :pen-down-p)
;;     (pen-toggle)))

;; (defun pen-up ()
;;   (when (@ *turtle* :pen-down-p)
;;     (pen-toggle)))

;; (defun color (vec)
;;   (includef *turtle* :color (vec4f vec)))

;; (defun forward (distance)
;;   (let ((new-pos (vec3f (kit.glm:matrix*vec4
;;                          (kit.glm:matrix*
;;                           (kit.glm:translate (@ *turtle* :position))
;;                           (kit.glm:rotate (@ *turtle* :rotation)))
;;                          (vec4f 0.0 distance 0.0 1.0)))))

;;     ;; move turtle's position
;;     (includef *turtle* :position new-pos)

;;     ;; add new data to drawer if pen-down
;;     (when (@ *turtle* :pen-down-p)
;;       (incf (num-vertices *line-drawer*))
;;       (add-turtle-data (draw-array *line-drawer*)))))

;; (defun fd (d) (forward d))

;; (defun back (dist)
;;   (forward (- dist)))

;; (defun bk (d) (back d))

;; (defun rotate-turtle (vec)
;;   (includef *turtle* :rotation (vec3f+ (@ *turtle* :rotation) vec)))

;; (defun left (radians)
;;   "Rotate turtle around z-axis, which is going into the screen."
;;   (rotate-turtle (vec3f 0.0 0.0 radians)))

;; (defun lt (r) (left r))

;; (defun right (radians)
;;   "Same as (left radians) in opposite direction"
;;   (left (- radians)))

;; (defun rt (r) (right r))

;; (defun roll (radians)
;;   "Rotate around x-axis."
;;   (rotate-turtle (vec3f radians 0.0 0.0)))

;; (defun pitch (radians)
;;   "Rotate around y-axis."
;;   (rotate-turtle (vec3f 0.0 radians 0.0)))

;; (defun yaw (radians)
;;   "Rotate around z-axis."
;;   (rotate-turtle (vec3f 0.0 0.0 radians)))

