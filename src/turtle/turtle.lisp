(in-package #:turtle-geometry)

(defstruct orientation-component
  ((position (vec3f) :type vec3f)
   (rotation (vec3f) :type vec3f)))
(defstruct turtle-component
  ((color (vec4f) :type vec4f)
   (pen-down-p t :type boolean)))

;; keeps track of velocity and force in the forward direction
(defstruct newtonian-component
  ((velocity 0.0 :type single-float)
   (mass 0.0 :type single-float)
   (force 0.0 :type single-float)))

;; (defenum:defenum *turtle-message-types*
;;     ((+turtle-move+ :turtle-move)
;;      (+turtle-physics+ :turtle-physics)))

;; (defstruct turtle-message
;;   (type +turtle-move+ :type keyword)
;;   (fn (lambda ()) :type function))

(defcomponent turtle-message-component (message-list))

(defun make-turtle (&key
                      (position (vec3f 0.0 0.0 0.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (color (vec4f 1.0 1.0 1.0 1.0))
                      (pen-down-p t)
                      (world *world*))
  "Create a turtle entity. Return entity id."
  (let ((e (make-entity world))
        (turtle-component (make-instance 'turtle-component
                                         :position position
                                         :rotation rotation
                                         :color color
                                         :pen-down-p pen-down-p)))
    (add-component world e turtle-component)
    e))

(defun add-turtle-data (world id)
  "Adds the turtle's current position and color to a gl-dynamic-array."
  (iter (for i in-vector (@ turtle :position))
    (gl-dyn-push array i))
  (iter (for i in-vector (@ turtle :color))
    (gl-dyn-push array i))
  array)

(defsystem turtle-message-system (turtle-message-component))

(defmethod update-system ((world world) (system turtle-message-system) dt)
  (system-do-with-components (turtle-message-component)
      world system entity-id
    (with-slots (message-list) turtle-message-component

      ;; loop through all messages
      (iter (for message in message-list)
        (let ((message-type (type-of message)))
          (cond ((= message-type 'function)
                 (funcall message world entity-id))
                (t
                 (warn "Sent entity ~a an invalid message type ~a~%"
                       entity-id message-type)))))

      ;; clear all messages
      (setf message-list nil))))

(defsystem newtonian-system (orientation-component newtonian-component))

(defmethod update-system ((world world) (system newtonian-system) dt)
  (system-do-with-components ((ori orientation-component) (newt newtonian-component))
      world system id
    (with-slots (velocity mass force) newt
      (with-slots ((pos position) (rot rotation)) ori
        (let ((accel (if (zerop mass)
                         0.0
                         (/ force mass))))
          ;; semi-implicit euler integration
          (incf vel (* accel dt))
          (setf pos (vec3f+ pos
                            (kit.glm:matrix*vec3 (vec3f 0.0 (* vel dt) 0.0)
                                                 (kit.glm:rotate rot)))))))))
