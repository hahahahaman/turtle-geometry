;;;; turtle-geometry.lisp

(in-package #:turtle-geometry)

;;; "turtle-geometry" goes here. Hacks and glory await!

(defclass turtle-drawer (err:drawer)
  ((vbo
    :accessor vbo
    :initarg :vbo))
  ((draw-array
    :accessor draw-array
    :initarg :draw-array))
  ((count
    :accessor count
    :initarg :count))
  (:default-initargs
   :vbo (car (gl:gen-buffers 1))
   :draw-array (make-array 0
                           :element-type 'single-float
                           :adjustable t
                           :fill-pointer 0)
   :count 0))

(defun turtle-draw ((&optional (drawer *turtle-drawer*)))
  (with-slots (program vao vbo draw-array) drawer
    (gl:use-program (id program))
    (gl:bind-buffer :array-buffer vbo)

    ;; position
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil (sizeof* :float 7) 0)

    ;; color
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 4 :float nil (sizeof* :float 7) (sizeof* :float 3))

    (with-sequence-to-gl-array (verts draw-array :float)
      (gl:buffer-data :array-buffer :dynamic-draw verts))

    (gl:draw-arrays :lines 0 0)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))

(defun make-turtle (&optional
                      (position (vec3f 0.0 0.0 0.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (color (vec4f 1.0 1.0 1.0 1.0)))
  (map (:position position)
       (:rotation rotation)
       (:color color)))

(defun init-turtle ()
  (let ((turtle-program (make-program
                         (file-in-dir *shader-directory* "turtle.v.glsl")
                         (file-in-dir *shader-directory* "turtle.f.glsl"))))

    (load-program "turtle" turtle-program)

    (setf *turtle* (make-turtle)
          *turtle-drawer* (make-instance 'turtle-drawer :program turtle-program))))

(defun forward (distance))

(defun right (radians))

(defun left (radians))

(defun clear ()
  (with-slots (draw-array count) *turtle-drawer*
    (setf draw-array (make-array 0
                                 :element-type 'single-float
                                 :adjustable t
                                 :fill-pointer 0)
          count 0
          *turtle* (make-turtle))))
