(in-package :mu-cl-resources)


(define-condition configuration-error (error)
  ((description :initarg :description :reader description))
  (:documentation "Indicates the system was configured incorrectly"))

(define-condition no-such-resource (error)
  ((description :initarg :description :reader description))
  (:documentation "Indicates the resource could not be found"))

(define-condition no-such-instance (error)
  ((type :initarg :type :reader target-type)
   (id :initarg :id :reader target-id)
   (resource :initarg :resource :reader resource))
  (:documentation "Indicates the resource could not be found"))

(define-condition no-such-link (error)
  ((resource :initarg :resource :reader resource)
   (path :initarg :path :reader path))
  (:documentation "Indicates the specified link does not exist
    for the supplied resource."))

(define-condition simple-described-condition (error)
  ((description :initarg :description :reader description))
  (:documentation "Indicates an exception which should mainly be
    handled by its type and a base description."))

(define-condition incorrect-accept-header (simple-described-condition)
  ()
  (:documentation "Indicates a necessary accept header was not found."))

(define-condition incorrect-content-type (simple-described-condition)
  ()
  (:documentation "Indicates a necessary content-type header was not found."))

(define-condition invalid-link-patch-body-format (simple-described-condition)
  ()
  (:documentation "Indicates the patch body for a link update was not correct."))

(define-condition no-type-in-data (error)
  ()
  (:documentation "Indicates no type property was found in the primary data"))

(define-condition id-in-data (error)
  ()
  (:documentation "Indicates an id property was found in the
    primary data whilst it was not expected."))

(define-condition no-id-in-data (error)
  ()
  (:documentation "Indicates no id property was found in the
    primary data whilst it was expected."))

(define-condition request-type-mismatch (error)
  ((path-defined-type :initarg :path-defined-type :reader path-defined-type)
   (content-defined-type :initarg :content-defined-type :reader content-defined-type))
  (:documentation "Indicates the type in the request does not match the type
    of the supplied content."))

(define-condition request-id-mismatch (error)
  ((path-defined-id :initarg :path-defined-id :reader path-defined-id)
   (content-defined-id :initarg :content-defined-id :reader content-defined-id))
  (:documentation "Indicates the id in the request does not match
    the id of the supplied content."))

