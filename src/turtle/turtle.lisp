(in-package #:turtle-geometry)

(defstruct orientation-component
  (position (vec3f 0.0) :type vec3f)
  (rotation (vec3f 0.0) :type vec3f))
(defstruct turtle-component
  (color (vec4f 0.0) :type vec4f)
  (pen-down-p t :type boolean))

;; keeps track of velocity and force in the forward direction
(defstruct newtonian-component
  (velocity 0.0 :type real)
  (mass 0.0 :type real)
  (force 0.0 :type real))

;; (defenum:defenum *turtle-message-types*
;;     ((+turtle-move+ :turtle-move)
;;      (+turtle-physics+ :turtle-physics)))

;; (defstruct turtle-message
;;   (type +turtle-move+ :type keyword)
;;   (fn (lambda ()) :type function))

;; a message is a function which takes parameters WORLD and ENTITY-ID
(defstruct turtle-message-component
  (message-list (make-array 8 :fill-pointer 0)))

(defsystem turtle-message-system (turtle-message-component))

(defmethod update-system ((world world)
                          (system turtle-message-system)
                          dt)
  (system-do-with-components ((tmc turtle-message-component))
      world system entity-id
    (with-slots (message-list) tmc
      ;; loop through all messages
      (when message-list
        (run-thread
          (iter (for message in-vector message-list)
            (let ((message-type (type-of message)))
              (cond ((equal message-type 'function)
                     (funcall message world entity-id))
                    (t
                     (warn "Sent entity ~a an invalid message type ~a~%"
                           entity-id message-type))))))

        ;; clear all messages
        (setf (fill-pointer message-list) 0)))))

(defsystem newtonian-system (orientation-component
                             newtonian-component))

(defmethod update-system ((world world) (system newtonian-system) dt)
  (system-do-with-components ((ori orientation-component)
                              (newt newtonian-component))
      world system id
    (with-slots (velocity mass force) newt
      (with-slots ((pos position) (rot rotation)) ori
        (let ((accel (if (zerop mass)
                         0.0
                         (/ force mass))))
          ;; semi-implicit euler integration
          (incf velocity (* accel dt))
          (when (not (zerop velocity))
            (add-turtle-point :world world :turtle id)
            (setf pos (vec3f+ pos
                              (kit.glm:matrix*vec3
                               (vec3f 0.0 (* velocity dt) 0.0)
                               (kit.glm:rotate rot))))
            (add-turtle-point :world world :turtle id)))))))

(defsystem turtle-drawer-system (orientation-component
                                 turtle-component))

(defmethod update-system ((world world)
                          (system turtle-drawer-system) dt)
  (system-do-with-components ((ori orientation-component)
                              (turt turtle-component))
      world system id
    (with-slots ((pos position) (rot rotation)) ori
      (with-slots (color) turt
        (turtle-draw :position pos
                     :rotation rot
                     :color color)))))


(defun make-turtle (&key
                      (position (vec3f 0.0 0.0 0.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (color (vec4f 0.5 0.0 1.0 1.0))
                      (velocity 0.0)
                      (mass 1.0)
                      (force 0.0)
                      (pen-down-p t)
                      (world *world*))
  "Create a turtle entity. Return entity id."
  (let ((e (make-entity world))
        (ori (make-orientation-component
              :position position
              :rotation rotation))
        (turt (make-turtle-component
               :color color
               :pen-down-p pen-down-p))
        (mess (make-turtle-message-component))
        (newt (make-newtonian-component
               :velocity velocity
               :mass mass
               :force force)))
    (add-components world e ori turt mess newt)
    (setf *turtle* e)
    e))

(defun add-turtle-data (array &key (world *world*) (turtle *turtle*))
  "Adds the turtle's current position and color to a gl-dynamic-array."
  (let ((ori (get-component world turtle 'orientation-component))
        (turt (get-component world turtle 'turtle-component)))
    (iter (for i in-vector (orientation-component-position ori))
      (gl-dyn-push array i))
    (iter (for i in-vector (turtle-component-color turt))
      (gl-dyn-push array i)))
  array)

(defun add-turtle-point (&key (world *world*) (turtle *turtle*)
                           (line-drawer *line-drawer*))

  (with-slots (pen-down-p) (ec world turtle 'turtle-component)
    (when pen-down-p
      (incf (num-vertices line-drawer))
      (add-turtle-data (draw-array line-drawer)
                       :world world
                       :turtle turtle))))
