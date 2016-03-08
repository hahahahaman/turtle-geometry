(in-package #:turtle-geometry)

(defglobal *project-directory* (asdf:system-source-directory :turtle-geometry))
(defglobal *shader-directory* (cl-fad:merge-pathnames-as-directory
                               *project-directory* (pathname "data/shaders/")))
(defglobal *texture-directory* (cl-fad:merge-pathnames-as-directory
                                *project-directory* (pathname "data/images/")))
(defglobal *font-directory* (cl-fad:merge-pathnames-as-directory
                             *project-directory* (pathname "data/fonts/")))

(defglobal *turtle* (empty-map))
(defglobal *turtle-drawer*)
