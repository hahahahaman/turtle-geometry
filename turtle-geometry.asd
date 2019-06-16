;;;; turtle-geometry.asd

(asdf:defsystem #:turtle-geometry
  :description "Logo in Common Lisp"
  :author "hahahahaman <hahahadude@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:iterate
               #:cl-fad ;; pathname library
               #:cl-opengl
               ;; #:fset
               #:glkit
               #:sdl2
               #:sdl2-ttf
               #:sdl2kit
               #:trivial-garbage
               #:trivial-main-thread
               #:defenum
               ;; #:ironclad
               #:qua ;; entity component system
               )
  :serial t
  :pathname "src/"
  :components ((:file "package")

               ;; Utilities
               (:module utils
                :components (
                             ;; global variables
                             (:file "globals")

                             ;; timer class
                             (:file "timer")

                             ;; optimized vector classese
                             (:file "vec")

                             ;; dynamic array implementation for gl-arrays
                             (:file "gl-dynamic-array")

                             ;; utility functions
                             (:file "utils")

                             ;; functions to handle inputs values that are in global vars
                             (:file "input")

                             ;; gl shader program
                             (:file "program")

                             ;; base class for managers
                             (:file "resource-manager")

                             ;; handles all shader programs
                             (:file "program-manager")))
               ;; (:file "event") ;; events are (priority . function) cons, can be destructive


               (:module camera
                :components (;; object that represents the camera
                             (:file "camera")))

               (:module drawer
                :components (;; base drawer class
                             (:file "drawer")

                             ;; store vertices and draws the lines
                             (:file "line-drawer")

                             ;; draws an object that represents the turtle
                             (:file "turtle-drawer")))

               ;; Turtle geometry
               (:module turtle
                :components (;; object that represents the turtle
                             (:file "turtle")

                             ;; defines the turtle operations
                             (:file "turtle-commands")

                             ;; misc. functions that draw shapes
                             (:file "shapes")))

               ;; makes the window
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
