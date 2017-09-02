(in-package :turtle-geometry)

(defclass gl-dynamic-array ()
  ((gl-array
    :type gl:gl-array
    :reader gl-array)
   (array-capacity
    :reader array-capacity
    :type unsigned-int
    :initarg :capacity)
   (capacity-multiplier
    :type unsigned-int
    :initarg :multiplier)
   (array-size
    :reader array-size
    :type unsigned-int
    :initarg :size)
   (array-type
    :reader array-type
    :initarg :array-type))
  (:default-initargs
   :capacity 10
   :multiplier 2
   :size 0
   :array-type :float))

(defmethod initialize-instance :after ((array gl-dynamic-array) &key)
  (with-slots (gl-array array-capacity array-size array-type) array
    (setf gl-array (gl:alloc-gl-array array-type array-capacity)))

  (trivial-garbage:finalize array (lambda ()
                                    (gl:free-gl-array
                                     (gl-array array)))))

(defun gl-dyn-aref (dynamic-array index)
  (gl:glaref (gl-array dynamic-array) index))

(defun (setf gl-dyn-aref) (value dynamic-array index)
  (setf (gl:glaref (gl-array dynamic-array) index) value))

(defun gl-dyn-push (dynamic-array value)
  (with-slots (gl-array array-size array-capacity
               array-type capacity-multiplier) dynamic-array
    (cond ((= array-size array-capacity)
           (setf array-capacity (* capacity-multiplier array-capacity))
           (let ((new-array (gl:alloc-gl-array array-type array-capacity)))
             (dotimes (i array-size)
               (setf (gl:glaref new-array i) (gl:glaref gl-array i)))
             (gl:free-gl-array gl-array)
             (setf gl-array new-array))))

    (setf (gl:glaref gl-array array-size) value)
    (incf array-size)))

(defun gl-dyn-array-byte-size (dynamic-array)
  (* (array-size dynamic-array) (cffi:foreign-type-size (array-type dynamic-array))))
