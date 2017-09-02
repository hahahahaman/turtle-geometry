(in-package #:turtle-geometry)

;;; base class for things that draw

(defclass drawer ()
  ((program
    :type program
    :accessor program
    :initarg :program)
   (vao
    :type unsigned-byte
    :accessor vao
    :initarg :vao))
  (:default-initargs
   :program nil
   :vao (gl:gen-vertex-array)))

(defmethod intialize-instance :after ((drawer drawer) &key)
  (with-slots (vao) drawer
    (trivial-garbage:finalize drawer (lambda ()
                                       (gl:delete-vertex-arrays (vector vao))))))
