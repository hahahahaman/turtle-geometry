(in-package #:turtle-geometry)

(defclass game-window (sdl2.kit:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defun init-turtle-geometry ()
  (init-managers)
  (setf *turtle* (make-turtle :color (vec4f 0.0 0.0 0.0 1.0))
        *camera* (make-init-camera))
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
    (set-program-matrices line-program :projection perspective-matrix)))


(defmethod initialize-instance :after ((w game-window) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render w) t)
  (initialize-globals)
  (init-turtle-geometry)
  (gl:enable :line-smooth)
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (format t "Game window initialized.~%"))

(defrender render-lines 200.0
  ;; (gl:line-width 1.0)
  (gl:enable :line-smooth)
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

(defupdate update 200.0)

(defmethod render ((window game-window))
  (handle-camera-input)
  (render-lines)

  (update)

  (update-events)
  (update-globals))

(defmethod mousewheel-event ((window game-window) ts x y)
  (setf *scroll-callback-p* t
        *scroll-x* x
        *scroll-y* y))

(defmethod keyboard-event ((window game-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    ;; (format t "state: ~A, ts: ~A, repeat-p: ~A, keysym: ~A~%" state ts repeat-p scancode)

    (if repeat-p
        (setf (getf *key-actions* scancode) :repeat)
        (setf (getf *key-actions* scancode) state))

    (cond ((eq state :keydown)
           (setf (getf *key-pressed* scancode) t))
          ((eq state :keyup)
           (setf (getf *key-pressed* scancode) nil))))


  (when (key-pressed-p :scancode-escape)
    (close-window window))
  (when (and (key-pressed-p :scancode-lalt)
             (key-action-p :scancode-r :keydown))
    (init-turtle-geometry)))

(defmethod mousebutton-event ((window game-window) state ts b x y)
  ;; (format t "~A button: ~A at ~A, ~A~%" state b x y)

  (setf (getf *mouse-button-actions* b) state)
  (cond ((eq state :mousebuttondown)
         (setf (getf *mouse-button-pressed* b) t))
        ((eq state :mousebuttonup)
         (setf (getf *mouse-button-pressed* b) nil))))

(defmethod mousemotion-event ((window game-window) ts mask x y xr yr)
  (cond (*first-mouse* (setf *last-x* x
                             *last-y* y
                             *first-mouse* nil))
        (t
         (setf *cursor-x* x
               *cursor-y* y
               *cursor-callback-p* t))))


(defmethod textinput-event :after ((window game-window) ts text)
  ;; (when (string= "Q" (string-upcase text))
  ;;   (close-window window))
  )

(defun cleanup ()
  (format t "Bye!~%"))

(defmethod close-window ((w game-window))
  (cleanup)
  (call-next-method))

(defmethod window-event ((window game-window) (type (eql :resized)) ts data1 data2)
  ;; need to adjust the camera

  ;; (format t "type: ~A, data1: ~A, data2: ~A ~%" type data1 data2)
  )

(defmethod window-event :after ((window game-window) type ts data1 data2)
  ;; (format t "type: ~A, data1: ~A, data2: ~A ~%" type data1 data2)
  )

(defun run (&optional (w 800) (h 600))
  (setf *width* w
        *height* h)

  (sdl2.kit:start)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)

  (make-instance 'game-window
                 :title "turtle-geometry"
                 :w *width*
                 :h *height*
                 :resizable t))
