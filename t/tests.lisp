;;;; tests.lisp

(in-package #:turtle-geometry.tests)

;;; "" goes here. Testing awaits!

(defun run-all-tests ())

;;; Hooking into ASDF
#|
To run:

(ql:quickload :turtle-geometry.tests)
(asdf:perform :test-op :turtle-geometry.tests)

|#
(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :turtle-geometry.tests))))
  (format t "~2&*******************~@
                ** Starting test **~@
                *******************~%~%")
  (handler-bind ((style-warning #'muffle-warning)) (run-all-tests))
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~%"))
