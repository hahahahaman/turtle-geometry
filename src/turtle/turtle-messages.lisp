;;;; turtle-geometry.lisp

(in-package #:turtle-geometry)

;;; "turtle-geometry" goes here. Hacks and glory await!

(defun clear (&key (line-drawer *line-drawer*))
  ;; reset entities
  ;; reset turtle
  ;; reset line-drawer
  ;; reset camera
  (clear-entities *world*)
  (let ((array (make-instance 'gl-dynamic-array :array-type :float)))
    (with-slots (num-vertices draw-array) line-drawer
      (setf *turtle* nil
            *camera* (make-instance 'camera)
            num-vertices 0
            draw-array array))))

(defun clr (&key (line-drawer *line-drawer*))
  (clear :line-drawer line-drawer))

(defun send-message (message &key (w *world*) (id *turtle*))
  (if (and (is-entity-p w id)
           (has-component-p w id 'turtle-message-component))
      (push message (turtle-message-component-message-list
                     (ec w id
                         'turtle-message-component)))
      (warn "Entity ~A doesn't have a message component. Entity's existence is ~A.~%"
            id
            (is-entity-p w id))))

(defmacro defmessage (name (&rest message-args) &body message-body)
  (let ((name-suffix (alexandria:symbolicate name "-MESSAGE")))
    `(progn
       (defun ,name-suffix (,@message-args)
         ,@message-body)
       (defun ,name (,@message-args)
         (send-message (,name-suffix ,@message-args))))))


(defun pen-toggle-message ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p (not pen-down-p)))))

(defun pen-down-message ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p t))))

(defun pen-up-message ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p nil))))

(defun color-message (color-vec)
  (lambda (w id)
    (with-slots (color) (ec w id 'turtle-component)
      (setf color color-vec))))

(defun pen-toggle ()
  (send-message (pen-toggle-message)))

(defun pen-down ()
  (send-message (pen-down-message)))

(defun pen-up ()
  (send-message (pen-up-message)))

(defun color (color-vec)
  (send-message (color-message color-vec)))

(defun forward-message (distance)
  (lambda (w id)
    (with-slots ((pos position) (rot rotation))
        (ec w id 'orientation-component)
      (with-slots (pen-down-p) (ec w id 'turtle-component)
        (let ((new-pos (vec3f (kit.glm:matrix*vec3
                               (vec3f 0.0 distance 0.0)
                               (kit.glm:matrix*
                                (kit.glm:translate pos)
                                (kit.glm:rotate rot))))))

          (when pen-down-p
            ;; add first point of the line
            (incf (num-vertices *line-drawer*))
            (add-turtle-data (draw-array *line-drawer*)
                             :w w
                             :id id))

          ;; move turtle's position
          (setf pos new-pos)


          (when pen-down-p
            ;; add second point
            (incf (num-vertices *line-drawer*))
            (add-turtle-data (draw-array *line-drawer*)
                             :w w
                             :id id)))))))

(defun forward (distance)
  (send-message (forward-message distance)))

(defun fd (d) (forward d))

(defun back (dist) (forward (- dist)))

(defun bk (d) (back d))

(defmessage turtle-rotate (vec)
  (lambda (w id)
    (with-slots ((rot rotation)) (ec w id 'orientation-component)
      (setf rot (vec3f+ rot vec)))))

(defun left (radians)
  "Rotate turtle around z-axis, which is going into the screen."
  (turtle-turtle (vec3f 0.0 0.0 radians)))

(defun lt (r) (left r))

(defun right (radians)
  "Same as (left radians) in opposite direction."
  (left (- radians)))

(defun rt (r) (right r))

(defun roll (radians)
  "Rotate around x-axis."
  (turtle-rotate (vec3f radians 0.0 0.0)))

(defun pitch (radians)
  "Rotate around y-axis."
  (turtle-rotate (vec3f 0.0 radians 0.0)))

(defun yaw (radians)
  "Rotate around z-axis."
  (turtle-rotate (vec3f 0.0 0.0 radians)))

