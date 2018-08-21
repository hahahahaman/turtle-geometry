(in-package :turtle-geometry)

(defenum:defenum camera-movement
    ((+forward+ 0)
     +backward+
     +left+
     +right+
     +up+
     +down+))

(defclass camera ()
  ((position
    :type vec3f
    :initarg :position)
   (front
    :type vec3f
    :initarg :front)
   (up
    :type vec3f
    :initarg :up)
   (right
    :type vec3f
    :initarg :right)
   (world-up
    :type vec3f
    :initarg :world-up)
   (yaw
    :type single-float
    :initarg :yaw)
   (pitch
    :type single-float
    :initarg :pitch)
   (movement-speed
    :type single-float
    :initarg :movement-speed)
   (mouse-sensitivity
    :type single-float
    :initarg :mouse-sensitivity)
   (zoom
    :type single-float
    :reader zoom
    :initarg :zoom))
  (:default-initargs
   :position (vec3f 0.0 0.0 100.0)
   :front (vec3f 0.0 0.0 -1.0)
   :world-up (vec3f 0.0 1.0 0.0)
   :yaw -90.0
   :pitch 0.0
   :movement-speed 100.0
   :mouse-sensitivity 0.15
   :zoom 45.0))

(defmethod initialize-instance :after ((cam camera) &key)
  (update-camera-vectors cam))

(defmethod get-view-matrix ((cam camera))
  (with-slots (position front up) cam
    (kit.glm:look-at position (vec3f+ position front) up)))

(defmethod process-direction-movement ((cam camera) direction dt)
  (with-slots (movement-speed front right position up) cam
    (let ((velocity (cfloat (* movement-speed dt))))
      (cond ((eql direction +forward+)
             (setf position (vec3f+ position (vec3f* front velocity))))
            ((eql direction +backward+)
             (setf position (vec3f- position (vec3f* front velocity))))
            ((eql direction +left+)
             (setf position (vec3f- position (vec3f* right velocity))))
            ((eql direction +right+)
             (setf position (vec3f+ position (vec3f* right velocity))))
            ((eql direction +up+)
             (setf position (vec3f+ position (vec3f* up velocity))))
            ((eql direction +down)
             (setf position (vec3f- position (vec3f* up velocity))))))))

(defmethod process-rotation-movement ((cam camera) x y &optional (constrain-pitch t))
  (with-slots (mouse-sensitivity yaw pitch) cam
    (let ((x-offset (* x mouse-sensitivity))
          (y-offset (* y mouse-sensitivity)))
      (incf yaw x-offset)
      (incf pitch y-offset)

      (when constrain-pitch
        (cond ((> pitch 89.0)
               (setf pitch 89.0))
              ((< pitch -89.0)
               (setf pitch -89.0))))

      (update-camera-vectors cam))))

(defmethod process-scroll-movement ((cam camera) y)
  (with-slots (zoom mouse-sensitivity) cam
    (when (and (>= (- zoom y) 1)
               (<= (- zoom y) 45))
      (decf zoom y))))

(defmethod update-camera-vectors ((cam camera))
  (with-slots (yaw pitch right up front world-up) cam
    (let* ((yaw (kit.glm:deg-to-rad yaw))
           (pitch (kit.glm:deg-to-rad pitch))
           (new-front (vec3f (cfloat (* (cos yaw)
                                        (cos pitch)))
                             (cfloat (sin pitch))
                             (cfloat (* (sin yaw)
                                        (cos pitch))))))
      (setf front (kit.glm:normalize new-front)
            right (kit.glm:normalize (kit.glm:cross-product front world-up))
            up (kit.glm:normalize (kit.glm:cross-product right front))))))
