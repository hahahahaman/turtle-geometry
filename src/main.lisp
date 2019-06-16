(in-package #:turtle-geometry)

(defclass turtle-window (sdl2.kit:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defun init-turtle-geometry ()
  (init-managers)
  (setf *camera* (make-init-camera))
  (let ((line-program (make-program
                       (file-pathname *shader-directory* "line.v.glsl")
                       (file-pathname *shader-directory* "line.f.glsl")))
        (turtle-program (make-program
                         (file-pathname *shader-directory* "basic.v.glsl")
                         (file-pathname *shader-directory* "basic.f.glsl")))
        (perspective-matrix (kit.glm:perspective-matrix
                             (kit.glm:deg-to-rad (zoom *camera*))
                             (/ *width* *height*)
                             0.1
                             10000.0)))

    (setf *turtle-drawer* (make-turtle-drawer turtle-program)
          *line-drawer* (make-line-drawer line-program))

    (load-program "turtle" turtle-program)
    (load-program "line" line-program)

    (set-program-matrices turtle-program :projection perspective-matrix)
    (set-program-matrices line-program :projection perspective-matrix))

  ;; qua entity component system
  (clear-world *world*)
  (let ((message-sys (make-instance 'turtle-message-system))
        (newt-sys (make-instance 'newtonian-system)))
    (add-systems *world* message-sys newt-sys)))

(defmethod initialize-instance :after ((w turtle-window) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render w) t)
  (initialize-globals)
  (init-turtle-geometry)
  (gl:enable :line-smooth)
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (format t "Turtle window initialized.~%"))

(defrender render-objects 200.0
  ;; (gl:line-width 1.0)
  ;; (gl:enable :line-smooth)
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (line-draw)

  (let ((pos (@ *turtle* :position))
        (color (@ *turtle* :color))
        (rotation (@ *turtle* :rotation)))
    (turtle-draw :position pos
                 ;; :size (vec3f 2.0 2.0 2.0)
                 :color color
                 :rotation rotation
                 :draw-mode :triangle-fan)))

(defupdate update 200.0
  (update *world* *dt*))

(defun update-program-matrices ()
  (let ((matrix (kit.glm:perspective-matrix
                 (kit.glm:deg-to-rad (zoom *camera*))
                 (/ *width* *height*)
                 0.1
                 10000.0)))
    (set-program-matrices (get-program "turtle") :projection matrix)
    (set-program-matrices (get-program "line") :projection matrix)))

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

(defmethod render ((window turtle-window))
  (handle-camera-input)
  (render-objects)

  (update)

  ;; (update-events)
  (update-globals))

(defmethod keyboard-event ((window turtle-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    ;; (format t "state: ~A, ts: ~A, repeat-p: ~A, keysym: ~A~%" state ts repeat-p scancode)

    (if repeat-p
        (setf (getf *key-actions* scancode) :repeat)
        (setf (getf *key-actions* scancode) state))

    (cond ((eq state :keydown)
           (setf (getf *key-pressed* scancode) t))
          ((eq state :keyup)
           (setf (getf *key-pressed* scancode) nil))))


  ;; get camera to directly face the turtle
  (when (and (key-pressed-p :scancode-tab)
             (key-pressed-p :scancode-lshift))
    (let ((tpos (@ *turtle* :position)))
      (with-slots (position yaw pitch front) *camera*
        (setf (x-val position) (x-val tpos)
              (y-val position) (y-val tpos)))))

  (when (key-pressed-p :scancode-tab)
    (let ((tpos (@ *turtle* :position)))
      (with-slots (position yaw pitch front) *camera*
        (setf
         front (kit.glm:normalize (vec3f- tpos position))
         pitch (kit.glm:rad-to-deg (asin (y-val front)))
         yaw (* (signum (z-val front))
                (kit.glm:rad-to-deg
                 (realpart (acos (/ (x-val front)
                                    (cos (kit.glm:deg-to-rad
                                          pitch)))))))))))

  ;; camera info
  (when (key-pressed-p :scancode-c)
    (with-slots (yaw pitch front movement-speed) *camera*
      (format t "front-vec: ~A~%yaw: ~A~%pitch: ~A~%movement-speed: ~A~%"
              front yaw pitch movement-speed)))

  ;; movement-speed
  (when (key-pressed-p :scancode-z)
    (with-slots (movement-speed) *camera*
      (incf movement-speed -5.0)
      (format t "Decreasing camera speed to: ~A~%" movement-speed)))

  (when (key-pressed-p :scancode-x)
    (with-slots (movement-speed) *camera*
      (incf movement-speed 5.0)
      (format t "Increasing camera speed to: ~A~%" movement-speed)))

  (when (key-pressed-p :scancode-escape)
    (close-window window))
  (when (and (key-pressed-p :scancode-lalt)
             (key-action-p :scancode-r :keydown))
    (clear)))

(defmethod mousebutton-event ((window turtle-window) state ts b x y)
  ;; (format t "~A button: ~A at ~A, ~A~%" state b x y)

  (setf (getf *mouse-button-actions* b) state)
  (cond ((eq state :mousebuttondown)
         (setf (getf *mouse-button-pressed* b) t))
        ((eq state :mousebuttonup)
         (setf (getf *mouse-button-pressed* b) nil))))

(defmethod mousemotion-event ((window turtle-window) ts mask x y xr yr)
  (cond (*first-mouse* (setf *last-x* x
                             *last-y* y
                             *first-mouse* nil))
        (t
         (setf *cursor-x* x
               *cursor-y* y
               *cursor-callback-p* t)))
  ;; (with-slots (yaw pitch front) *camera*
  ;;   (format t "~A ~A ~A~%" front yaw pitch))
  )

(defmethod mousewheel-event ((window turtle-window) ts x y)
  ;; (format t "mousewheel: ~A ~A ~A ~A~%" x y *scroll-x* *scroll-y*)
  (setf *scroll-callback-p* t
        *scroll-x* x
        *scroll-y* y))

(defmethod textinput-event :after ((window turtle-window) ts text)
  ;; (when (string= "Q" (string-upcase text))
  ;;   (close-window window))
  )

(defun cleanup ()
  (format t "Bye!~%"))

(defmethod close-window ((w turtle-window))
  (cleanup)
  (call-next-method))

(defmethod window-event ((window turtle-window) (type (eql :resized)) ts data1 data2)
  ;; need to adjust the camera

  (format t "type: ~A, data1: ~A, data2: ~A ~%" type data1 data2)
  (setf *width* data1
        *height* data2)

  (gl:viewport 0 0 *width* *height*)
  )

(defmethod window-event :after ((window turtle-window) type ts data1 data2)
  ;; (format t "type: ~A, data1: ~A, data2: ~A ~%" type data1 data2)
  )

(defun run (&optional (w 800) (h 600))
  (setf *width* w
        *height* h)

  (sdl2.kit:start)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)

  (make-instance 'turtle-window
                 :title "Thinking about calculus."
                 :w *width*
                 :h *height*
                 :resizable t))
