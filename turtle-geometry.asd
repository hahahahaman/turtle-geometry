;;;; turtle-geometry.asd

(asdf:defsystem #:turtle-geometry
  :description "Logo in Common Lisp"
  :author "hahahahaman <hahahadude@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:iterate
               #:cl-fad
               #:cl-opengl
               #:fset
               #:glkit
               #:sdl2
               #:sdl2-ttf
               #:sdl2kit
               #:trivial-garbage
               #:trivial-main-thread
               #:defenum
               #:ironclad)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "globals")
               (:file "timer")
               (:file "vec")
               (:file "gl-dynamic-array")
               (:file "utils")
               (:file "event")
               (:file "input")
               (:file "program")
               (:file "resource-manager")
               (:file "program-manager")
               (:file "turtle")
               (:file "drawer")
               (:file "line-drawer")
               (:file "turtle-drawer")
               (:file "camera")
               (:file "turtle-geometry")
               (:file "shapes")
               (:file "main")))

(asdf:defsystem #:turtle-geometry.tests
  :description "turtle-geometry.tests"
  :author "hahahahaman <hahahadude@gmail.com>"
  :license "MIT"
  :depends-on (#:turtle-geometry
               #:simple-testing)
  :serial t
  :pathname "t/"
  :components ((:file "package")
               (:file "tests")))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :turtle-geometry))))
  (format t "~2&*************~@
                ** Loading **~@
                *************~%")
  (asdf:oos 'asdf:load-op :turtle-geometry.tests)
  (asdf:oos 'asdf:test-op :turtle-geometry.tests))
