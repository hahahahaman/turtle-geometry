(in-package :turtle-geometry)

(defclass line-drawer (err:drawer)
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

(defmethod initialize-instance :after ((drawer line-drawer) &key)
  (with-slots (vbo) drawer
    (trivial-garbage:finalize drawer (lambda () (gl:delete-buffers (vector vbo))))))

(defun make-line-drawer (program &optional (turtle *turtle*))
  (let ((array (make-array 0
                           :element-type 'single-float
                           :adjustable t
                           :fill-pointer 0)))

    (add-turtle-data array turtle)
    (make-instance 'line-drawer :program program
                                :draw-array array
                                :num-vertices 1)))

(defun line-draw (&optional (drawer *line-drawer*))
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
