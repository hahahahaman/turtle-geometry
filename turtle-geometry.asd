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
               #:glkit
               #:sdl2
               #:sdl2-ttf
               #:trivial-garbage
               #:trivial-main-thread
               #:defenum
               #:ironclad)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "globals")
               (:file "utils")
               (:file "turtle")
               (:file "line-drawer")
               (:file "turtle-drawer")
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
