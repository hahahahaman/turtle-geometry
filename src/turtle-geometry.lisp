;;;; turtle-geometry.lisp

(in-package #:turtle-geometry)

;;; "turtle-geometry" goes here. Hacks and glory await!

(defun update-program-matrices ()
  (set-program-matrices (get-program "turtle"))
  (set-program-matrices (get-program "line")))

(defun init-turtle-geometry ()
  (let ((line-program (make-program
                       (file-in-dir *shader-directory* "line.v.glsl")
                       (file-in-dir *shader-directory* "line.f.glsl")))
        (turtle-program (make-program
                         (file-in-dir *shader-directory* "basic.v.glsl")
                         (file-in-dir *shader-directory* "basic.f.glsl"))))

    (load-program "turtle" turtle-program)
    (load-program "line" line-program)

    (set-program-matrices turtle-program)
    (set-program-matrices line-program)

    (setf *turtle* (make-turtle)
          *turtle-drawer* (make-turtle-drawer turtle-program)
          *line-drawer* (make-line-drawer line-program)
          *camera* (make-init-camera))))

(defun make-init-camera ()
  (make-instance 'camera :position (vec3f 0.0 0.0 100.0)
                         :movement-speed 50.0
                         :mouse-sensitivity 0.1))

(defun handle-camera-input ()
  (when (key-pressed-p :left-control)
    (when *cursor-callback-p*
      (let ((x-offset (cfloat (- *cursor-x* *last-x*)))
            (y-offset (cfloat (- *last-y* *cursor-y*))))
        (process-rotation-movement *camera* x-offset y-offset)))

    (when *scroll-callback-p*
      (process-scroll-movement *camera* (cfloat *scroll-y*))) 

    (when (key-pressed-p :w)
      (process-direction-movement *camera* +forward+ *dt*))
    (when (key-pressed-p :s)
      (process-direction-movement *camera* +backward+ *dt*))
    (when (key-pressed-p :a)
      (process-direction-movement *camera* +left+ *dt*))
    (when (key-pressed-p :d)
      (process-direction-movement *camera* +right+ *dt*)))

  (update-program-matrices)

  (setf *last-x* *cursor-x*
        *last-y* *cursor-y*)
  (setf *cursor-callback-p* nil
        *scroll-callback-p* nil))

(defun clear ()
  ;; reset turtle
  ;; reset line-drawer
  ;; reset camera
  (let ((array (draw-array *line-drawer*)))
    (setf *turtle* (-> *turtle*
                       (with :position (vec3f 0.0 0.0 0.0))
                       (with :rotation (vec3f 0.0 0.0 0.0)))
          (fill-pointer array) 0
          (num-vertices *line-drawer*) 1
          *camera* (make-init-camera))
    (add-turtle-data array)))

(defun pen-toggle ())

(defun pen-down ())
(defun pen-up ())

(defun color (vec)
  (includef *turtle* :color vec))

(defun forward (distance)
  (let ((new-pos (vec3f (kit.glm:matrix*vec4
                         (kit.glm:matrix*
                          (kit.glm:translate (@ *turtle* :position))
                          (kit.glm:rotate (@ *turtle* :rotation)))
                         (vec4f distance 1.0)))))

    ;; move turtle's position
    (includef *turtle* :position new-pos)

    ;; add new data to drawer if pen-down
    (when (@ *turtle* :pen-down-p)
      (incf (num-vertices *line-drawer*))
      (add-turtle-data (draw-array *line-drawer*)))))

(defun rotate-turtle (vec)
  (includef *turtle* :rotation (vec3f+ (@ *turtle* :rotation) vec)))

(defun right (radians)
  ;; rotate turtle around z-axis
  (rotate-turtle (vec3f 0.0 0.0 radians)))

(defun left (radians)
  ;; same as right in opposite direction
  (right (- radians)))

(defun line ()
  (clear)
  (forward (vec3f 0.0 10.0 0.0)))
