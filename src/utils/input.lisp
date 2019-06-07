(in-package #:turtle-geometry)

(defun key-action-p (key action)
  "Returns true if KEY is in *key-actions* and its state is EQ to ACTION."
  (let ((state (getf *key-actions* key)))
    (and (not (null state)) ;; if STATE is not null then key must have been found
         (eq action state))))

(defun key-pressed-p (key)
  (getf *key-pressed* key))

(defun mouse-button-action-p (button action)
  "Returns true if KEY is in *mouse-button-actions* and its state is EQ to ACTION."
  (let ((state (getf *mouse-button-actions* button)))
    (and (not (null state)) ;; if STATE is not null then button be active
         (eq action state))))

(defun mouse-button-pressed-p (button)
  (getf *mouse-button-pressed* button))
