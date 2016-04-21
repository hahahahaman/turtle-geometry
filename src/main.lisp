(in-package #:turtle-geometry)

(defun initialize ()
  (initialize-shaders *shader-directory*)
  (init-turtle-geometry))

(defun handle-input ()
  (when (key-pressed-p :escape)
    (close-window))
  (handle-camera-input)
  (when (and (key-pressed-p :left-alt)
             (key-action-p :r :press)) 
    (add-event :code (initialize))))

(defrender render 200.0
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
                 ;; :size (vec3f 2.0 2.0 2.0)
                 :color color
                 :rotation rotation
                 :draw-mode :triangle-fan)))

(defupdate update 200.0)

(defun cleanup ())

(defun run ()
  (setf *width* 800
        *height* 600)
  (err-run "turtle geometry"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
