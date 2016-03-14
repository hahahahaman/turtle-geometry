(in-package #:turtle-geometry)

(defun initialize ()
  (initialize-shaders *shader-directory*)
  (init-turtle-geometry))

(defun handle-input ()
  (when (key-pressed-p :escape)
    (close-window))
  (handle-camera-input)
  (when (and (key-pressed-p :left-control)
             (key-action-p :r :press)) 
    (add-event :code (initialize))))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)
      (timer-reset render-timer)

      ;; (gl:line-width 1.0)
      (gl:enable :line-smooth)
      (gl:enable :blend :depth-test)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (line-draw)

      (let ((pos (@ *turtle* :position))
            (color (@ *turtle* :color))
            (rotation (@ *turtle* :rotation)))
        (turtle-draw :position pos
                     :size (vec3f 2.0 2.0 2.0)
                     :color color
                     :rotation rotation
                     ;; :rotation (vec3f 0.0 0.0 (cfloat (glfw:get-time)))
                     :draw-mode :triangle-fan)))))

(let ((update-timer (make-timer :end (/ 1.0 120.0))))
  (defun update ()
    (timer-update update-timer)
    (iter (while (timer-ended-p update-timer))
      (timer-keep-overflow update-timer))))

(defun cleanup ())

(defun run ()
  (err-run "turtle geometry"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
