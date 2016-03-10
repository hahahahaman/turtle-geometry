(in-package #:turtle-geometry)

(defun initialize ()
  (initialize-shaders *shader-directory*)
  (init-turtle))

(defun handle-input ()
  (when (key-pressed-p :escape)
    (close-window))
  (handle-camera-input))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)
      (turtle-draw)
      (timer-reset render-timer))))

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
