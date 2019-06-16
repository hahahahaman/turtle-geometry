(defpackage #:turtle-geometry
  (:use #:cl #:iterate #:kit.sdl2 #:qua)
  ;; (:shadowing-import-from :iterate #:iter #:while)
  ;; (:shadowing-import-from :fset
  ;;                         ;; Shadowed type/constructor names
  ;;                         #:set #:map
  ;;                         ;; Shadowed set operations
  ;;                         #:union #:intersection #:set-difference #:complement
  ;;                         ;; Shadowed sequence operations
  ;;                         #:first #:last #:subseq #:reverse #:sort #:stable-sort
  ;;                         #:reduce
  ;;                         #:find #:find-if #:find-if-not
  ;;                         #:count #:count-if #:count-if-not
  ;;                         #:position #:position-if #:position-if-not
  ;;                         #:remove #:remove-if #:remove-if-not
  ;;                         #:substitute #:substitute-if #:substitute-if-not
  ;;                         #:some #:every #:notany #:notevery
  ;;                         #:with)
  (:export #:run))
