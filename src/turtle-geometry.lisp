;;;; turtle-geometry.lisp

(in-package #:turtle-geometry)

;;; "turtle-geometry" goes here. Hacks and glory await!

(defclass turtle-drawer (err:drawer)
  ((vbo
    :accessor vbo
    :initarg :vbo)
   (draw-array
    :accessor draw-array
    :initarg :draw-array)
   (num-vertices
    :accessor num-vertices
    :initarg :num-vertices))
  (:default-initargs
   :vbo (gl:gen-buffer)
   :draw-array (make-array 0
                           :element-type 'single-float
                           :adjustable t
                           :fill-pointer 0)
   :num-vertices 0))

(defmethod initialize-instance :after ((drawer drawer) &key)
  (with-slots (vbo) drawer
    (trivial-garbage:finalize drawer (lambda () (gl:delete-buffers (vector vbo))))))

(defun turtle-draw (&optional (drawer *turtle-drawer*))
  (gl:line-width 1.0)
  (gl:enable :line-smooth)
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (with-slots (program vao vbo draw-array num-vertices) drawer
    (gl:use-program (id program))

    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)

    ;; position
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil (sizeof* :float 7) 0)

    ;; color
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 4 :float nil (sizeof* :float 7) (sizeof* :float 3))

    (with-sequence-to-gl-array (verts draw-array :float)
      (gl:buffer-data :array-buffer :dynamic-draw verts))

    (gl:draw-arrays :line-strip 0 num-vertices)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))

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

(defun make-turtle-drawer (program &optional (turtle *turtle*))
  (let ((array (make-array 0
                           :element-type 'single-float
                           :adjustable t
                           :fill-pointer 0)))

    (add-turtle-data array turtle)
    (make-instance 'turtle-drawer :program program
                                  :draw-array array
                                  :num-vertices 1)))

(defun make-init-camera ()
  (make-instance 'camera :position (vec3f 0.0 0.0 100.0)
                         :movement-speed 50.0
                         :mouse-sensitivity 0.1))

(defun init-turtle ()
  (let ((turtle-program (make-program
                         (file-in-dir *shader-directory* "turtle.v.glsl")
                         (file-in-dir *shader-directory* "turtle.f.glsl"))))
    (load-program "turtle" turtle-program)
    (set-program-matrices turtle-program)

    (setf *turtle* (make-turtle)
          *turtle-drawer* (make-turtle-drawer turtle-program)
          *camera* (make-init-camera))))

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

  (set-program-matrices (get-program "turtle"))

  (setf *last-x* *cursor-x*
        *last-y* *cursor-y*)
  (setf *cursor-callback-p* nil
        *scroll-callback-p* nil))

(defun clear ()
  ;; reset turtle
  ;; reset turtle-drawer
  ;; reset camera
  (let ((array (draw-array *turtle-drawer*)))
    (setf *turtle* (-> *turtle*
                       (with :position (vec3f 0.0 0.0 0.0))
                       (with :rotation (vec3f 0.0 0.0 0.0)))
          (fill-pointer array) 0
          (num-vertices *turtle-drawer*) 1
          *camera* (make-init-camera))
    (add-turtle-data array)))

(defun pen-toggle ())

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
      (incf (num-vertices *turtle-drawer*))
      (add-turtle-data (draw-array *turtle-drawer*)))))

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
