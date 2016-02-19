(in-package #:turtle-geometry)

(defun initialize ()
  (initialize-shaders *shader-directory*))

(defun handle-input ()
  (when (key-pressed-p :escape)
    (close-window)))
(defun render ())
(defun update ())
(defun cleanup ())

(defun run ()
  (err-run "turtle geometry"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
