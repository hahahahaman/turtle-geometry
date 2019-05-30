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

               ;; Utilities
               (:file "globals") ;; global variables
               (:file "timer") ;; timer class
               (:file "vec") ;; optimized vector class
               (:file "gl-dynamic-array") ;; dynamic array implementation for gl-arrays
               (:file "utils") ;; utility functions
               ;; (:file "event") ;; events are (priority . function) cons, can be destructive
               (:file "input") ;; functions to handle inputs values that are in global vars
               (:file "camera") ;; object that handles camera movement

               ;; GL
               (:file "program") ;; shader program
               (:file "resource-manager") ;; base class for managers
               (:file "program-manager") ;; handles all shader programs

               ;; Turtle geometry
               (:file "drawer") ;; base drawer class
               (:file "line-drawer") ;; store vertices and draws the lines
               (:file "turtle-drawer") ;; draws an object that represents the turtle
               (:file "turtle") ;; immutable map that stores drawing attributes
               (:file "turtle-commands") ;; defines the turtle operations
               (:file "shapes") ;; misc. functions that draw shapes
               (:file "main"))) ;; makes the window

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
