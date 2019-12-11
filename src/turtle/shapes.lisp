(in-package :turtle-geometry)

(defun square (side-length &key (rot-dim 2))
  (let ((rot-vec (vec3f 0.0)))
    (setf (aref rot-vec rot-dim) pi/2)
    (dotimes (x 4)
      (forward side-length)
      (turtle-rotate rot-vec))))

(defun ngon (sides radius)
  (let* ((center-angle (/ (* 2 pi) sides))
         (corner-angle (/ (- pi center-angle) 2))
         (initial-outer-angle (- pi corner-angle))
         (subsequent-outer-angle (- pi (* 2 corner-angle))))
    (fw radius)
    (rt initial-outer-angle)
    (fw )
    (dotimes (x sides)
      (pen-up)
      (fw radius)
      (rt center-angle))))

(defun arcr (radius radians)
  (let* ((perimeter (* radius radians))
         (degrees (round (/ (* radians 180) pi)))
         (side-length (/ perimeter degrees)))
    (dotimes (x degrees)
      (forward side-length)
      (right (/ radians degrees)))))

(defun arcl (radius radians)
  (let* ((perimeter (* radius radians))
         (degrees (round (/ (* radians 180) pi)))
         (side-length (/ perimeter degrees)))
    (dotimes (x degrees)
      (forward side-length)
      (left (/ radians degrees)))))

(defsynonym (squiggle squi) (l)
  (forward l)
  (right (/ pi 2))
  (forward l)
  (right (/ pi 2))
  (forward (* 0.5 l))
  (right (/ pi 2))
  (forward (* 0.5 l))
  (right (/ pi 2))
  (forward l)
  (right (/ pi 2))
  (forward (* 0.25 l))
  (right (/ pi 2))
  (forward (* 0.25 l))
  (right (/ pi 2))
  (forward (* 0.5 l)))

(defun thing (times fn &rest args)
  (dotimes (x times)
    (apply fn args)
    (right (/ pi 10))
    (forward 2)))

(defun petal (size)
  (let ((s size)
        (center-angle (/ pi 3)))
    (dotimes (x 2)
      (arcr s center-angle)
      (right (* center-angle 2)))))

(defun flower (size)
  (dotimes (x 6)
    (petal size)
    (right (/ pi 3))))

(defun ray (r)
  (dotimes (x 2)
    (arcl r (/ pi 2))
    (arcr r (/ pi 2))))

(defun sun (size)
  (dotimes (x 9)
    (ray size)
    (right (* (/ 8 9) pi))))

#| High level Comparison:
Coordinate Geometry using the Cartesian coordinate system uses two
numbers to represent a point. This system is interesting because it allows for a
study of geometric figures using equations which are plotted on graphs to
represent numerical relations. The system is a combination of algebra and
geometry and this relationship seems to be a fundamental insight.

Turtle geometry seems to be complement to this idea, but uses FORWARD and RIGHT
as its descriptors rather than X and Y.
|#

#| Intrinsic vs. Extrinsic:

An instrinsic property is one that depends only on the figure, not on its
relation to a frame of reference. 4 sides on a rectangle is a intrinsic prop,
while two vertical sides is an extrinsic prop, since vertical requires some
reference to say what direction is up. Turtle geometry prefers the intrinsic,
since objects, such as a rectangle, can be drawn in any orientation, while
Cartesian geometry one much change the points in order to align or rotate the
shape. |#

(defun circle ()
  (dotimes (x 360)
    (forward 10)
    (right (* 5 (/ pi 180)))))

#| Compare the circle in turtle geo to the Cartesian equation:

x^2 + y^2 = r^2

No matter what forward and right values are given it still remains a circle.
But with a modified equation:

x^2 + 2y^2 = r^2

it creates an ellipse.

The notion of the pointiness of something, is expressed as the ratio of center-angle
turned to distance traveled, is the intrinsic quantity that is called curvature.
|#

#| Local vs. Global:

The turtle also represents the circle more locally; it deals with the geometry
one part at a time, while, in contrast, the equation deals with the larg-scal
global coordinate system to define it. Defining a circle to be a set of points
equadistance from a fixec point is as global as using x^2 + y^2 = r^2. |#

#| Procedures vs. Equations:

Turtle geometry uses procedural mechanisms (like iterating) that are difficult
to capture in traditional algebraic formalism. And these descriptions in turtle
geometry are readily modified in many ways. |#

(defun poly (side center-angle)
  (dotimes (x 360)
    (forward side)
    (right center-angle)))

(defun newpoly (side center-angle)
  (dotimes (x 360)
    (forward side)
    (right center-angle)
    (forward side)
    (right (* center-angle 0.5))))

(defun newpoly1 (size center-angle)
  (dotimes (x 100)
    (square size)
    (right center-angle)
    (ray size)
    (right (* center-angle 1.6))))

(defun degree-poly (n)
  (dotimes (x 100)
    (newpoly (/ n 2) (kit.glm:deg-to-rad n))))

(defun random-poly (&optional (max 1000))
  (let ((n (random max)))
    (print n)
    (degree-poly n)))

(defun strange-poly ()
  (let* ((n 144)
         (center-angle (kit.glm:deg-to-rad n))
         (colors (vector (vec4f 1.0 0.0 0.0 1.0)
                         (vec4f 0.0 1.0 0.0 1.0)
                         (vec4f 0.0 0.0 1.0 1.0))))
    (dotimes (x 360)
      (color (aref colors (mod x 3)))
      (forward n)
      (right center-angle)
      (petal n)
      (right (* center-angle 2)))))

#| Recursive definitions of shapes show a form of sameness (symmetry?) in the
object. |#

(defun recursive-poly (side center-angle)
  (let ((end 360))
    (labels ((poly-iter (n)
               (unless (= n end)
                 (forward side)
                 (right center-angle)
                 (poly-iter (1+ n)))))
      (poly-iter 0))))


;; forms an outward spiral
(defun polyspi-side (side center-angle)
  (let ((end 50))
    (labels ((it (s a n)
               (unless (= n end)
                 (forward s)
                 (right a)
                 (it (+ s side) a (1+ n)))))
      (it side center-angle 0))))

;; forms an inward spiral
(defun polyspi-angle (side center-angle)
  (let ((end 50)
        (deg1 (/ pi 180)))
    (labels ((it (side center-angle n)
               (unless (= n end)
                 (forward side)
                 (right center-angle)
                 (it side (- center-angle deg1) (1+ n)))))
      (it side center-angle 0))))

;; (defun polyspi (side center-angle)
;;   (let ((end 100))
;;     (labels ((it (side center-angle n)
;;                (unless (= n end)
;;                  (forward side)
;;                  (right center-angle)
;;                  (it side center-angle (1+ n)))))
;;       (it side center-angle 0))))

(defun exponentials ()
  (let* ((scale-factor 0.01)
         (n 6)
         (e (iter (for i from 1 to n)
              (collect (make-turtle :position (vec3f i))
                result-type vector))))
    (iter (for j from 0 below (length e))
      (iter (for i from 1 to 5)
        (let ((turt (aref e j))
              (d (* (expt i (1+ j)) scale-factor)))
          (fw d :turtle turt)
          (color (random-color 1.0) :turtle turt))))))

(defmessage (timed-color) (interval times)
  (lambda (w id)
    (dotimes (x times)
      (color (vec4f (/ x times) 0.0 0.0 1.0))
      (lf pi/2)
      (sleep interval))
    (f 0) (v 0)))
