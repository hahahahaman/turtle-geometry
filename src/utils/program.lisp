(in-package :turtle-geometry)
;;; gl program

(defclass program ()
  ((id
    :type %gl:uint
    :accessor id
    :initarg :id))
  (:default-initargs
   :id 0))

(defmethod initialize-instance ((program program) &key)
  (setf (id program) (gl:create-program))

  (trivial-garbage:finalize program (lambda () (gl:delete-program (id program)))))

(defmethod use ((program program))
  (gl:use-program (id program)))

(defun load-shader-file (filepath shader-type)
  "Creates a compiled shader object of SHADER-TYPE using the file FILEPATH, if
the shader did not compile an error is called."
  (let ((shader (gl:create-shader shader-type))
        (code (alexandria:read-file-into-string filepath))
        (status (cffi:foreign-alloc :int)))
    (gl:shader-source shader code)
    (gl:compile-shader shader)

    (%gl:get-shader-iv shader :compile-status status)

    ;; 1 means success
    (if (not (= (cffi:mem-ref status :int) 1))
        (progn
          (cffi:foreign-free status)
          (print (gl:get-shader-info-log shader))

          (gl:delete-shader shader)
          (setf shader 0)

          (error "Shader compile-time error, \"~a\"~% error code: ~a~%" filepath
                 (cffi:mem-ref status :int)))
        (progn
          (cffi:foreign-free status)
          shader))))

(defun program-compile (program vert-path frag-path &optional (geo-path nil))
  "Does not return any useful value, calls error if linking of program failed."
  (with-slots (id) program
    (let ((vert (load-shader-file vert-path :vertex-shader))
          (frag (load-shader-file frag-path :fragment-shader))
          (geo (if geo-path
                   (load-shader-file geo-path :geometry-shader)
                   nil)))

      (gl:attach-shader id vert)
      (gl:attach-shader id frag)
      (if geo (gl:attach-shader id geo))

      (gl:link-program id)

      ;; no longer need shaders after link
      (gl:detach-shader id vert)
      (gl:detach-shader id frag)
      (if geo (gl:detach-shader id geo))

      ;; free up shaders so no leak
      (gl:delete-shader vert)
      (gl:delete-shader frag)
      (if geo (gl:delete-shader geo))

      ;; error handling for program
      (let ((status (cffi:foreign-alloc :int)))
        (%gl:get-program-iv id :link-status status)
        ;; (format t "link-status: ~a~%"(cffi:mem-ref status :int))

        (when (not (= (cffi:mem-ref status :int) 1))
          (cffi:foreign-free status)

          (print (gl:get-program-info-log id))

          ;;program no longer needed
          ;;detaches all shaders
          ;;(gl:delete-program id)

          (error "Program link-time error: ~a~%" (gl:get-program-info-log id)))

        (cffi:foreign-free status)))))

(defun get-attrib (program name)
  "Return attrib-location NAME from PROGRAM if found, or else it calls an error."
  (let ((attrib (gl:get-attrib-location (id program) name)))
    (if (eql attrib -1)
        (error "Program attribute not found: ~a~%" name)
        attrib)))

(defun get-uniform (program name)
  "Return uniform-location NAME from PROGRAM if found, else call error."
  (declare (program program) (string name))
  (let ((uniform (gl:get-uniform-location (id program) name)))
    (if (eql uniform -1)
        (error "Program uniform not found: ~a~%" name)
        uniform)))

(defun make-program (vert-path frag-path &optional (geo-path nil))
  "Returns a new program instance."
  (let ((program (make-instance 'program)))
    (program-compile program vert-path frag-path geo-path)
    program))
