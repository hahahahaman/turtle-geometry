(in-package :turtle-geometry)

(defclass turtle-drawer (drawer)
  ())

(defmethod initialize-instance :after ((drawer turtle-drawer) &key)
  (with-slots (vao program) drawer
    (gl:use-program (id program))
    (gl:bind-vertex-array vao)
    (let ((vbo (gl:gen-buffer)))
      (gl:bind-buffer :array-buffer vbo)

      (with-sequence-to-gl-array (verts
                                  (vector 0.0 0.5 0.0
                                          -0.5 -0.5 0.5
                                          0.5 -0.5 0.5
                                          0.5 -0.5 -0.5
                                          -0.5 -0.5 -0.5
                                          -0.5 -0.5 0.5
                                          )
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil (sizeof* :float 3) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)
      (gl:delete-buffers (vector vbo)))))

(defun make-turtle-drawer (program)
  (make-instance 'turtle-drawer :program program))

(defun turtle-draw (&key
                      (position (vec3f 0.0 0.0 0.0))
                      (size (vec3f 1.0 1.0 1.0))
                      (color (vec4f 1.0 1.0 1.0 1.0))
                      (rotation (vec3f 0.0 0.0 0.0))
                      (rotation-center (vec3f 0.0 0.0 0.0))
                      (draw-center (vec3f 0.0 0.0 0.0))
                      (draw-mode :triangle-fan)
                      (drawer *turtle-drawer*))
  (with-slots (vao program) drawer
    (gl:use-program (id program))
    (let ((model (kit.glm:matrix*

                  ;; move into position
                  (kit.glm:translate position)

                  ;; move to draw center
                  (kit.glm:translate (vec3f-mul size (vec3f* draw-center -1.0)))

                  ;; move back from rotation center
                  (kit.glm:translate (vec3f-mul rotation-center size))

                  ;; perform rotation
                  (kit.glm:rotate rotation)

                  ;; move to rotation center
                  (kit.glm:translate (vec3f* (vec3f-mul rotation-center size)
                                             -1.0))

                  ;; scale first
                  (kit.glm:scale size))))
      (gl:uniform-matrix-4fv (get-uniform program "model") model nil))
    (gl:uniformfv (get-uniform program "color") color)
    (gl:bind-vertex-array vao)
    (gl:draw-arrays draw-mode 0 6)
    (gl:bind-vertex-array 0)))
