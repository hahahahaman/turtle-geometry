(in-package #:turtle-geometry)

(defsynonym (clear clr) (&key (line-drawer *line-drawer*))
  ;; reset entities
  ;; reset turtle
  ;; reset line-drawer
  ;; reset camera
  (clear-entities *world*)
  (let ((array (make-instance 'gl-dynamic-array :array-type :float)))
    (with-slots (num-vertices draw-array) line-drawer
      (setf *turtle* (make-turtle)
            *camera* (make-instance 'camera)
            num-vertices 0
            draw-array array))))

(defun send-message (message &key (world *world*)
                               (turtle *turtle*))
  (if (and (is-entity-p world turtle)
           (has-component-p world turtle 'turtle-message-component))
      (vector-push-extend message (turtle-message-component-message-list
                                   (ec world turtle
                                       'turtle-message-component)))
      (warn "Entity ~A doesn't have a message component. Entity's existence is ~A.~%"
            turtle
            (is-entity-p world turtle))))

(defmacro defmessage ((&rest names) (&rest message-args)
                      &body message-body)
  (let ((message-names (iter (for name in names)
                         (collect
                             (alexandria:symbolicate
                              name "-MESSAGE")))))
    `(progn
       (defsynonym (,@message-names) (,@message-args)
         ,@message-body)
       (defsynonym (,@names) (,@message-args &key (world *world*)
                                             (turtle *turtle*))
         (send-message (,(car message-names) ,@message-args)
                       :world world
                       :turtle turtle)))))

(defmessage (pen-toggle) ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p (not pen-down-p)))))

(defmessage (pen-down) ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p t))))

(defmessage (pen-up) ()
  (lambda (w id)
    (with-slots (pen-down-p) (ec w id 'turtle-component)
      (setf pen-down-p nil))))

(defmessage (color) (color-vec)
  (lambda (w id)
    (with-slots (color) (ec w id 'turtle-component)
      (setf color color-vec))))

(defmessage (forward fw) (distance)
  (lambda (w id)
    (with-slots ((pos position) (rot rotation))
        (ec w id 'orientation-component)
      (with-slots (pen-down-p) (ec w id 'turtle-component)
        (let ((new-pos (vec3f (kit.glm:matrix*vec3
                               (vec3f 0.0 distance 0.0)
                               (kit.glm:matrix*
                                (kit.glm:translate pos)
                                (kit.glm:rotate rot))))))

          ;; add first point of the line
          (add-turtle-point :world w :turtle id)

          ;; move turtle's position
          (setf pos new-pos)

          ;; second point
          (add-turtle-point :world w :turtle id))))))

(defmacro def-turtle-synonym ((&rest synonyms)
                              (&rest args)
                              fn-call)
  (let ((fn-extra-args (append fn-call
                               '(:world world :turtle turtle))))
    `(defsynonym (,@synonyms) (,@args
                               &key
                               (world *world*)
                               (turtle *turtle*))
       ,fn-extra-args)))

(def-turtle-synonym (back bk kcab) (distance)
                    (fw (- distance)))

(defmessage (turtle-rotate rot) (vec)
  (lambda (w id)
    (with-slots ((rot rotation)) (ec w id 'orientation-component)
      (setf rot (vec3f+ rot vec)))))

(def-turtle-synonym (left lt lf) (radians)
                    (turtle-rotate (vec3f 0.0 0.0 radians)))

(def-turtle-synonym (right rt) (radians)
                    (turtle-rotate (vec3f 0.0 0.0 (- radians))))

(def-turtle-synonym (roll) (radians)
                    (turtle-rotate (vec3f radians 0.0 0.0)))

(def-turtle-synonym (pitch) (radians)
                    (turtle-rotate (vec3f 0.0 radians 0.0)))

(def-turtle-synonym (yaw) (radians)
                    (turtle-rotate (vec3f 0.0 0.0 radians)))

(defmessage (velocity vel sp v) (velocity)
  (lambda (w id)
    (with-slots ((tvel velocity)) (ec w id 'newtonian-component)
      (setf tvel velocity))))

(defmessage (force f) (force)
  (lambda (w id)
    (with-slots ((tforce force)) (ec w id 'newtonian-component)
      (setf tforce force))))

(defmessage (add-force af) (force)
  (lambda (w id)
    (with-slots ((tforce force)) (ec w id 'newtonian-component)
      (incf tforce force))))

(defmessage (add-mass) (mass)
  (lambda (w id)
    (with-slots ((tmass mass)) (ec w id 'newtonian-component)
      (incf tmass mass))))
