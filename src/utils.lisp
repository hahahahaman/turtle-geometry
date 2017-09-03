(in-package #:turtle-geometry)

(defun square (x)
  (* x x))
(defun cube (x)
  (* x x x))

(defun dist-mod (a b modulo)
  "Distance between A and B mod MODULO."
  (min (mod (- a b) modulo) (mod (- b a) modulo)))

;; fps
(let* ((max-samples 500)
       (samples (make-array max-samples
                            :element-type 'double-float
                            :initial-element (/ 1.0d0 +max-fps+)))
       (sample-index 0))
  (defun average-fps ()
    "=> FLOAT
Average time change over a few hundred frames."
    (setf (aref samples sample-index) *dt*
          sample-index (mod (1+ sample-index) max-samples))
    (/ max-samples (reduce #'+ samples))))

(defun limit-fps ()
  "Limits frame rate, preventing resource hogging."

  ;; if the frame completed quicker than the minimum frame length
  ;; then sleep for the necessary amount of time
  (sleep (max 0.0 (- #.(/ 1.0 +max-fps+)
                     (- (get-internal-real-time) *previous-time*)))))

;;;;;;;;;;;;;;;;;
;; handle globals
;;;;;;;;;;;;;;;;;

(defun update-dt ()
  "Updates time globals."
  (setf *dt* (/ (- (get-internal-real-time) *previous-time*) 1000.0d0)
        *previous-time* (get-internal-real-time))

  ;; prevent unruly time steps from breaking game
  (setf *dt* (max 0.0d0 (min 0.25d0 *dt*))))

(defun clear-actions ()
  "Clears the input actions from last frame."
  (setf *key-actions* nil
        *mouse-button-actions* nil
        *scroll-callback-p* nil
        *cursor-callback-p* nil
        *last-x* *cursor-x*
        *last-y* *cursor-y*))

(defun update-globals ()
  "A single function that encompasses global updates."
  (clear-actions)
  (update-dt)
  (limit-fps))

(defun initialize-globals ()
  ;; random seed
  (setf *random-state* (make-random-state t))

  ;; go through all globals setting them to original value
  (iter (for (var-symbol func) on *global-setfs* by #'cddr)
        (declare (ignore var-symbol))
        (funcall func)))

;;; type utils

(defun concat-vecs (&rest vecs)
  "Creates of single-float simple-array from lone values, lists, and/or vectors."
  (let* ((len 0) ;; keeps track of simple-array length
         (vec (apply #'concatenate ;; combine all sequences, lists and vectors
                     'vector ;; output a vector

                     ;; ensure all of the elements of VECS are of type sequence
                     (mapcar
                      (lambda (x)
                        (cond ((typep x 'sequence)
                               (incf len (length x))
                               x) ;; keep track of length
                              (t
                               (incf len 1)
                               (list (coerce x 'single-float)))))
                      vecs))))
    ;; finally output simple-array
    (coerce vec `(simple-array single-float (,len)))))

(defun sequence-to-gl-array (sequence type)
  "Creates a gl-array from a lisp sequence, with elements of type TYPE.
Remember to free gl-array afterwards."
  (let ((gl-arr (gl:alloc-gl-array type (length sequence))))
    (dotimes (i (length sequence)
                gl-arr)
      (setf (gl:glaref gl-arr i) (elt sequence i)))))

(defmacro with-sequence-to-gl-array ((var sequence type) &body body)
  `(let ((,var (sequence-to-gl-array ,sequence ,type)))
     ,@body
     (gl:free-gl-array ,var)))

(defun random-in-range (start end)
  "Random number between start and end, inclusive."
  (if (> start end)
      (random-in-range end start)
      (+ start
         (let* ((diff (- end start))
                (randy (random (+ diff
                                  ;; increase limit if numbers are integers
                                  ;; to include end
                                  (if (integerp diff) 1 0)))))
           (if (floatp randy)
               ;; round randy to diff in case it is super close to diff
               (if (<= (- diff randy) single-float-epsilon)
                   diff
                   randy)
               randy)))))

(declaim (ftype (function (real) single-float) cfloat))
(defun cfloat (n)
  "Coerce N to single-float. Just makes the function shorter."
  (declare (optimize (speed 3) (safety 0)))
  (coerce n 'single-float))

(defun sizeof (type)
  "Gives to foreign-type-size of TYPE. Used with cffi stuff, like cl-opengl."
  (cffi-sys:%foreign-type-size type))

(defun sizeof* (type multiple)
  "Multiply sizeof TYPE by MULTIPLE"
  (* (sizeof type) multiple))

;;; slots

(defmacro get-slot (object &rest nested-slot-names)
  "Returns a nested slot-value form."
  (iter (iter:with current = object)
        (for s in nested-slot-names)
        (setf current `(slot-value ,current ,s))
        (finally (return current))))

;;; file io

(defun read-sexp-from-file (filename)
  "Reads all sexp from file FILENAME, returning a list of all collected."
  (with-open-file (file filename :direction :input)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (iter (for sexp = (read file nil))
              (while sexp)
              (collect sexp))))))

;;; swank stuff

#|
(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an error. Remember
  to hit C (for continue) in slime or pick the restart so errors don't kill the
  app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))
|#

;; (defun update-swank ()
;;   "Called from within the main loop, this keep the lisp repl running."
;;   (ignore-errors
;;    (continuable
;;     (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
;;       (when connection
;;         (swank::handle-requests connection t))))))

;; copy instances

;; (defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
;;   (:documentation "Makes and returns a shallow copy of OBJECT.

;;   An uninitialized object of the same class as OBJECT is allocated by
;;   calling ALLOCATE-INSTANCE.  For all slots returned by
;;   CLASS-SLOTS, the returned object has the
;;   same slot values and slot-unbound status as OBJECT.

;;   REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
;;   (:method ((object standard-object) &rest initargs &key &allow-other-keys)
;;     (let* ((class (class-of object))
;;            (copy (allocate-instance class)))
;;       (dolist (slot-name (mapcar #'sb-mop:slot-definition-name
;;                                  (sb-mop:class-slots class)))
;;         (when (slot-boundp object slot-name)
;;           (setf (slot-value copy slot-name)
;;                 (slot-value object slot-name))))
;;       (apply #'reinitialize-instance copy initargs))))

;; Threading macro from clojure
(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form) ,@more)
          (if (listp form)
              `(,(car form) ,x ,@(cdr form))
              (list form x)))
      x))

;; list utils

(defun drop-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun add-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr n list))))

(defun set-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr (1+ n) list))))

(defun plist-set (place indicator value)
  (do ((plist place (cddr plist))
       (n 0 (+ 2 n)))
      ((null plist) (append place (list indicator value)))
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (return (append (subseq place 0 (1+ n))
                           (cons value (nthcdr (+ n 2) place))))))))

;;; fset functions

(defun get-map-keys (to-type map)
  (image (lambda (x) (car x)) (convert to-type map)))
(defun get-map-values (to-type map)
  (image (lambda (x) (cdr x)) (convert to-type map)))

;; destructive fset stuff
;; fset comes with these already
;; (defmacro with! (collection value1 &optional value2)
;;   `(setf ,collection (with ,collection ,value1 ,value2)))
;; (defmacro less! (collection value1 &optional value2)
;;   `(setf ,collection (less ,collection ,value1 ,value2)))

;;;
;;; Garbage Collection
;;;

(defun gc (&key full verbose)
  (trivial-garbage:gc :full full :verbose verbose))

;;;
;;; build file
;;;

;; TODO
;; (defun make-build-file (path entry &key system output))

;;;
;;; md5
;;;

(defun md5 (str)
  "=> CHECKSUM (STRING)
Returns the md5 checksum of STR."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (ironclad:ascii-string-to-byte-array str))))

(defun valid-checksum-p (checksum other-checksum)
  "=> BOOLEAN
Checks if two checksums are equal."
  (string= checksum other-checksum))

;;;
;;; some math
;;;
;; (defun radians->degrees (radians)
;;   (* (/ 180 pi) radians))

;; (defun degrees->radians (degrees)
;;   (* (/ pi 180) degrees))

(defun rotate-point-ccw (x y radians)
  (values
   (+ (* x (cos radians)) (- (* y (sin radians))))
   (+ (* x (sin radians)) (* y (cos radians)))))

(defun rect-in-rectangle-p (x y width height o-top o-left o-width o-height)
  "Nice to have for on the spot rectangle coliision."
  (declare (single-float x y width height o-top o-left o-width o-height)
           (optimize (speed 3)))
  (not (or
        (<= (+ o-top o-height) y)
        (<= (+ y height) o-top)
        (<= (+ x width) o-left)
        (<= (+ o-left o-width) x))))

(defun file-pathname (directory-path file)
  "Returns the pathname of FILE in DIRECTORY-PATH"
  (cl-fad:merge-pathnames-as-file (pathname directory-path) (pathname file)))

(defun init-managers ()
  (setf *program-manager* (make-instance 'program-manager)
        ;; *texture-manager* (make-instance 'texture-manager)
        ;; *font-manager* (make-instance 'font-manager)
        ))

(defun set-program-matrices (program
                             &key
                               (view (get-view-matrix *camera*))
                               (projection (kit.glm:perspective-matrix
                                            (kit.glm:deg-to-rad (zoom *camera*))
                                            (cfloat (/ *width* *height*))
                                            0.1 1000.0)))
  (gl:use-program (id program))
  (when view
    (gl:uniform-matrix-4fv (get-uniform program "view") view nil))
  (when projection
    (gl:uniform-matrix-4fv (get-uniform program "projection") projection nil)))

(defun set-program-perspective (program
                                &key
                                  (view (get-view-matrix *camera*)))
  (set-program-matrices program
                        :view view
                        :projection (kit.glm:perspective-matrix
                                     (kit.glm:deg-to-rad (zoom *camera*))
                                     (cfloat (/ *width* *height*))
                                     0.1 1000.0)))

(defun set-program-orthographic (program
                                 &key
                                   (view (get-view-matrix *camera*)))
  (set-program-matrices program :view view
                                :projection (kit.glm:ortho-matrix 0.0
                                                                  (cfloat *width*)
                                                                  0.0
                                                                  (cfloat *height*)
                                                                  -100.0
                                                                  100.0)))

#|
(defun initialize-shaders (shader-directory)
  (init-managers)

  (flet ((file-from-shader-dir (file)
           (file-pathname shader-directory file)))
    (let ((text-program (make-program (file-from-shader-dir "text.v.glsl")
                                      (file-from-shader-dir "text.f.glsl")))
          (cube-program (make-program (file-from-shader-dir "cube.v.glsl")
                                      (file-from-shader-dir "cube.f.glsl")))
          (rect-program (make-program (file-from-shader-dir "rect.v.glsl")
                                      (file-from-shader-dir "rect.f.glsl")))
          (sprite-program (make-program (file-from-shader-dir "sprite.v.glsl")
                                        (file-from-shader-dir "sprite.f.glsl"))))

      (setf *text-drawer* (make-instance 'text-drawer :program text-program)
            *cube-drawer* (make-instance 'cube-drawer :program cube-program)
            *rect-drawer* (make-instance 'rect-drawer :program rect-program)
            *sprite-drawer* (make-instance 'sprite-drawer :program sprite-program)
            *camera* (make-instance 'camera :position (vec3f 0.0 0.0 100.0)
                                            :movement-speed 50.0))

      (load-program "text" text-program)
      (load-program "cube" cube-program)
      (load-program "rect" rect-program)
      (load-program "sprite" sprite-program)

      (set-program-matrices cube-program)
      (set-program-matrices rect-program)
      (set-program-matrices sprite-program)
      (set-program-matrices text-program :view nil))))

(defun initialize-shaders-orthographic (shader-directory)
  (init-managers)

  (flet ((file-from-shader-dir (file)
           (file-pathname shader-directory file)))
    (let ((text-program (make-program (file-from-shader-dir "text.v.glsl")
                                      (file-from-shader-dir "text.f.glsl")))
          (cube-program (make-program (file-from-shader-dir "cube.v.glsl")
                                      (file-from-shader-dir "cube.f.glsl")))
          (rect-program (make-program (file-from-shader-dir "rect.v.glsl")
                                      (file-from-shader-dir "rect.f.glsl")))
          (sprite-program (make-program (file-from-shader-dir "sprite.v.glsl")
                                        (file-from-shader-dir "sprite.f.glsl"))))

      (setf *text-drawer* (make-instance 'text-drawer :program text-program)
            *cube-drawer* (make-instance 'cube-drawer :program cube-program)
            *rect-drawer* (make-instance 'rect-drawer :program rect-program)
            *sprite-drawer* (make-instance 'sprite-drawer :program sprite-program)
            *camera* (make-instance 'camera :position (vec3f 0.0 0.0 100.0)
                                            :movement-speed 50.0))

      (load-program "text" text-program)
      (load-program "cube" cube-program)
      (load-program "rect" rect-program)
      (load-program "sprite" sprite-program)

      (set-program-orthographic cube-program)
      (set-program-orthographic rect-program)
      (set-program-orthographic sprite-program)
      (set-program-orthographic text-program :view nil))))
|#

(defmacro defun-discrete-timer (name fps (&rest args) &body body)
  `(let ((timer (make-timer :end (/ 1.0 ,fps))))
     (defun ,name (,@args)
       (timer-update timer)
       (when (timer-ended-p timer)
         (timer-reset timer)
         ,@body))))

(defmacro defun-continuous-timer (name fps (&rest args) &body body)
  `(let ((timer (make-timer :end (/ 1.0 ,fps))))
     (defun ,name (,@args)
       (timer-update timer)
       (iter (while (timer-ended-p timer))
             (timer-keep-overflow timer)
             ,@body))))

(defmacro defrender (func-name fps &body body)
  `(let ((render-timer (make-timer :end (/ 1.0 ,fps))))
     (defun ,func-name ()
       (timer-update render-timer)
       (when (timer-ended-p render-timer)
         (timer-reset render-timer)
         ,@body))))

(defmacro defupdate (func-name fps &body body)
  `(let ((update-timer (make-timer :end (/ 1.0 ,fps))))
     (defun ,func-name ()
       (timer-update update-timer)
       (iter (while (timer-ended-p update-timer))
             (timer-keep-overflow update-timer)
             ,@body))))

(defmacro run-thread (&body body)
  `(bt:make-thread (lambda () ,@body)))

(defun debug-print (object)
  "Slime REPL rebinds *STANDARD-OUTPUT* while new threads do not.
LOAD-TIME-VALUE obtains the value of an object at read time, allowing for the
usage of the rebound *STANDARD-OUTPUT* stream."
  (print object (load-time-value *standard-output*)))
