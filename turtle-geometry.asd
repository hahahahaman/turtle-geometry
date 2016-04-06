;;;; turtle-geometry.asd

(asdf:defsystem #:turtle-geometry
  :description "Describe turtle-geometry here"
  :author "Name <your@email.com>"
  :license "Licenceless Rider"
  :depends-on (#:err)
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
  :description "Describe turtle-geometry.tests here"
  :author "Name <your@email.com>"
  :license "Licenceless Rider"
  :depends-on (#:err
               #:turtle-geometry
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
