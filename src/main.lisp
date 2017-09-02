(in-package #:turtle-geometry)

(defclass game-window (sdl2.kit:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defun init-turtle-geometry ()
  (init-managers)
  (setf *turtle* (make-turtle)
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
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (format t "Game window initialized.~%"))

(defun framelimit (window &optional (fps 60))
  "Issues SDL2:DELAY's to get desired FPS."
  (with-slots (one-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) one-frame-time))
          (time-per-frame (/ 1000.0 fps)))
      (when (< elapsed-time time-per-frame)
        (sdl2:delay (floor (- time-per-frame elapsed-time))))
      (setf one-frame-time (get-internal-real-time)))))


(defun display-fps (window)
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
        (format t "FPS: ~A~%" (float (/ frames seconds)))
        (setf frames 0)
        (setf start-time (get-internal-real-time))))))

(defrender render-lines 200.0
  ;; (gl:line-width 1.0)
  (gl:enable :line-smooth)
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 1.0 1.0 0.0 1.0)
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
  (with-slots (start-time frames) window
    (let* ((interval 1)
           (current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second))
           (fps (float (/ frames interval))))
      (incf frames)
      (when (and (> seconds interval)
                 (plusp fps))
        (format t "Framerate: ~,3f fps, ~,3f ms/frame~%" fps (/ 1000 fps))
        (setf frames 0
              start-time current-time))))
  (framelimit window)
  (call-next-method)
  ;;(handle-input)
  ;; (handle-camera-input)
  ;; (render-lines)

  ;; (gl:enable :line-smooth)
  ;; (gl:enable :blend :depth-test)
  ;; (gl:blend-func :src-alpha :one-minus-src-alpha)
  ;; (gl:clear-color 1.0 1.0 1.0 1.0)
  ;; (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; (line-draw)

  ;; (let ((pos (@ *turtle* :position))
  ;;       (color (@ *turtle* :color))
  ;;       (rotation (@ *turtle* :rotation)))
  ;;   (turtle-draw :position pos
  ;;                ;; :size (vec3f 2.0 2.0 2.0)
  ;;                :color color
  ;;                :rotation rotation
  ;;                :draw-mode :triangle-fan))

  ;; (update)

  ;; (update-files)
  ;; (update-events)
  ;; (update-globals)
  ;; (display-fps w)
  ;; (framelimit w 200)
  )

(defmethod mousewheel-event ((window game-window) ts x y)
  (setf *scroll-callback-p* t
        *scroll-x* x
        *scroll-y* y))

(defmethod keyboard-event ((window game-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (format t "state: ~A, ts: ~A, repeat-p: ~A, keysym: ~A~%" state ts repeat-p scancode)

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
    (init-turtle-geometry))
  (print *key-pressed*))

(defmethod mousebutton-event ((window game-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y)

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
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defun cleanup ())

(defmethod close-window ((w game-window))
  (cleanup)
  (call-next-method))

;; (defun handle-input ()
;;   (when (key-pressed-p :escape)
;;     (close-window))
;;   (handle-camera-input)
;;   (when (and (key-pressed-p :left-alt)
;;              (key-action-p :r :press)) 
;;     (add-event :code (initialize))))


#|
(defmacro run-macro (title
                     &key
                       init-code
                       input-code
                       render-code
                       update-code
                       cleanup-code)
  ;; In case the OS's default library search algorithm cannot find
  ;; the correct libraries, libraries will be provided
  ;; not sure how this works
  ;; (pushnew #P"./" *foreign-library-directories*
  ;;          :test #'equal)
  "Generic game loop code."

  ;; Graphics calls on OS X must occur in the main thread
  ;; (trivial-main-thread:with-body-in-main-thread ())
  ;; sbcl has issues with this, use ccl instead

  ;; block used to return out of function call
  `(run-thread
     (block nil
       (glfw:with-init-window (:title ,title
                               :width *width*
                               :height *height*
                               :opengl-forward-compat t
                               :opengl-profile :opengl-core-profile
                               :context-version-major 3
                               :context-version-minor 3
                               :decorated t
                               :resizable nil
                               ;;full screen mode
                               ;; :monitor (glfw:get-primary-monitor)
                               ;; :refresh-rate 60
                               )
         ;; (glfw:swap-interval 1)
         (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

         (unless (gl::features-present-p (>= :glsl-version 3.3))
           (return nil))

         ;; initialize
         (initialize-globals)
         ,init-code

         ;; glfw input
         (glfw:set-key-callback 'key-callback)
         (glfw:set-mouse-button-callback 'mouse-callback)
         (glfw:set-cursor-position-callback 'cursor-callback)
         (glfw:set-scroll-callback 'scroll-callback)
         ;; (glfw:set-input-mode :cursor :disabled) ;; hides cursor

         (iter (until (glfw:window-should-close-p))

               ;; running loop in seperate thread means swank still runs
               ;; (update-swank)

               (continuable
                (glfw:poll-events)

                ,input-code

                ,render-code
                ,update-code

                (update-files)
                (update-events)
                (glfw:swap-buffers)
                (update-globals)))

         ,cleanup-code)))
  `(run-thread
     (block nil
       (sdl2:with-init (:everything)
         (sdl2:gl-set-attr :context-major-version 3)
         (sdl2:gl-set-attr :context-minor-version 3)
         (initialize-globals)
         ,init-code

         (sdl2:with-window (win :flags '(:shown :opengl)
                                :title ,title
                                :w *width*
                                :h *height*)

           (sdl2:with-gl-context (gl-context win)
             (unless (gl::features-present-p (>= :glsl-version 3.3))
               (return nil))
             (sdl2:with-event-loop (:method :poll)
               (:idle ()
                      (continuable

                       ,input-code

                       ,render-code
                       ,update-code

                       (update-files)
                       (update-events)
                       (update-globals)))
               (:quit ()
                      ,cleanup-code
                      t))))))))
|#


(defclass testing-window (kit.sdl2:gl-window)
  ((start-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defmethod render :after ((window testing-window))
  (with-slots (start-time frames) window
    (let* ((interval 5)
           (current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second))
           (fps (float (/ frames interval))))
      (incf frames)
      (format t "hello~%")
      (when (and (> seconds interval)
                 (plusp fps))
        (format t "Framerate: ~,3f fps, ~,3f ms/frame~%" fps (/ 1000 fps))
        (setf frames 0
              start-time current-time)))))

(defmethod textinput-event :after ((window testing-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window testing-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (close-window window))))

(defun run ()
  (setf *width* 640
        *height* 480)
  #|
  (run-macro "turtle geometry"
  :init-code (initialize)
  :input-code (handle-input)
  :render-code (render)
  :update-code (update)
  :cleanup-code (cleanup))
  |#

  (sdl2.kit:start)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)

  (make-instance 'game-window
                 :title "turtle-geometry"
                 :w *width*
                 :h *height*
                 )
  ;; (make-instance 'kit.sdl2.test:cube-window)

  ;; (make-instance 'kit.sdl2.test:simple-window)
  ;; (make-instance 'testing-window)
  )
