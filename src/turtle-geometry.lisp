;;;; turtle-geometry.lisp

(in-package #:turtle-geometry)

;;; "turtle-geometry" goes here. Hacks and glory await!

(defun update-program-matrices ()
  (let ((matrix (kit.glm:perspective-matrix
                 (kit.glm:deg-to-rad (zoom *camera*))
                 (/ *width* *height*)
                 0.1
                 10000.0)))
    (set-program-matrices (get-program "turtle") :projection matrix)
    (set-program-matrices (get-program "line") :projection matrix)))

(defun make-init-camera ()
  (make-instance 'camera :position (vec3f 0.0 0.0 100.0)
                         :movement-speed 50.0
                         :mouse-sensitivity 0.1))

(defun handle-camera-input ()
  (when (key-pressed-p :scancode-lctrl)
    (when *cursor-callback-p*
      (let ((x-offset (cfloat (- *cursor-x* *last-x*)))
            (y-offset (cfloat (- *last-y* *cursor-y*))))
        (process-rotation-movement *camera* x-offset y-offset)))

    (when *scroll-callback-p*
      (process-scroll-movement *camera* (cfloat *scroll-y*))) 

    (when (key-pressed-p :scancode-w)
      (process-direction-movement *camera* +forward+ *dt*))
    (when (key-pressed-p :scancode-s)
      (process-direction-movement *camera* +backward+ *dt*))
    (when (key-pressed-p :scancode-a)
      (process-direction-movement *camera* +left+ *dt*))
    (when (key-pressed-p :scancode-d)
      (process-direction-movement *camera* +right+ *dt*)))

  (update-program-matrices))

(defun clear ()
  ;; reset turtle
  ;; reset line-drawer
  ;; reset camera
  (setf *turtle* (-> *turtle*
                     (with :position (vec3f 0.0 0.0 0.0))
                     (with :rotation (vec3f 0.0 0.0 0.0)))
        *camera* (make-init-camera)
        *line-drawer* (make-line-drawer (get-program "line"))))

(defun pen-toggle ()
  (includef *turtle* :pen-down-p (not (@ *turtle* :pen-down-p))))

(defun pen-down ()
  (unless (@ *turtle* :pen-down-p)
    (pen-toggle)))

(defun pen-up ()
  (when (@ *turtle* :pen-down-p)
    (pen-toggle)))

(defun color (vec)
  (includef *turtle* :color (vec4f vec)))

(defun forward (distance)
  (let ((new-pos (vec3f (kit.glm:matrix*vec4
                         (kit.glm:matrix*
                          (kit.glm:translate (@ *turtle* :position))
                          (kit.glm:rotate (@ *turtle* :rotation)))
                         (vec4f 0.0 (cfloat distance) 0.0 1.0)))))

    ;; move turtle's position
    (includef *turtle* :position new-pos)

    ;; add new data to drawer if pen-down
    (when (@ *turtle* :pen-down-p)
      (incf (num-vertices *line-drawer*))
      (add-turtle-data (draw-array *line-drawer*)))))

(defun back (dist)
  (forward (- dist)))

(defun rotate-turtle (vec)
  (includef *turtle* :rotation (vec3f+ (@ *turtle* :rotation) vec)))

(defun left (radians)
  ;; rotate turtle around z-axis
  (rotate-turtle (vec3f 0.0 0.0 (cfloat radians))))

(defun right (radians)
  ;; same as right in opposite direction
  (left (- radians)))
