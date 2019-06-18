(in-package #:turtle-geometry)

;;; resource manager

(defgeneric load-resource (name resource manager)
  (:documentation "Loads RESOURCE into MANAGER. Can be retrieved with NAME."))
(defgeneric get-resource (name manager)
  (:documentation "Returns RESOURCE with key NAME, if it can be found, otherwise nil."))
(defgeneric clear-resources (manager)
  (:documentation "Cleans up all resources and empties RESOURCES."))

(defclass resource-manager ()
  ((resources
    :initarg :resources
    :accessor resources))
  (:default-initargs
   :resources (make-hash-table :test 'equal)))

(defmethod initialize-instance :after ((manager resource-manager) &key)
  t)

(defmethod get-resource (name (manager resource-manager))
  (gethash name (resources manager)))

(defmethod load-resource (name resource (manager resource-manager))
  (setf (gethash name (resources manager)) resource))

(defmethod clear-resources :after ((manager resource-manager))
  (clrhash (resources manager)))
