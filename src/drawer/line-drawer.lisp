(in-package :turtle-geometry)

(defclass line-drawer (drawer)
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
   :draw-array nil
   :num-vertices 0))

(defmethod initialize-instance :after ((drawer line-drawer) &key)
  (with-slots (vbo draw-array) drawer
    (trivial-garbage:finalize drawer (lambda () (gl:delete-buffers (vector vbo))))))



(defun make-line-drawer (program &optional (turtle *turtle*))
  (let ((array (make-instance 'gl-dynamic-array :array-type :float
                                                :capacity 10000
                                                :multiplier 10)))

    (make-instance 'line-drawer :program program
                                :draw-array array
                                :num-vertices 0)))

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

    (with-slots (gl-array array-size) draw-array
      ;; (print gl-array)
      (gl:buffer-data :array-buffer
                      :stream-draw
                      gl-array
                      :size (gl-dyn-array-byte-size draw-array)))

    (gl:draw-arrays :lines 0 num-vertices)

    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))
