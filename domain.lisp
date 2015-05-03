(in-package :product-groups)

(defclass resource ()
  ((ld-class :initarg :ld-class :reader ld-class)
   (ld-properties :initarg :ld-properties :reader ld-properties)
   (ld-resource-base :initarg :ld-resource-base :reader ld-resource-base)))

(defparameter *resources* (make-hash-table)
  "contains all currently known resources")

(defun define-resource* (name &key ld-class ld-properties ld-resource-base)
  "defines a resource for which get and set requests exist"
  (let ((resource (make-instance 'resource
                                 :ld-class ld-class
                                 :ld-properties ld-properties
                                 :ld-resource-base ld-resource-base)))
    (setf (gethash name *resources*) resource)))

(defmacro define-resource (name options &key class properties resource-base)
  (declare (ignore options))
  `(define-resource* ',name
       :ld-class ,class
       :ld-properties ,properties
       :ld-resource-base ,resource-base))

(defgeneric support-creation (path resource)
  (:documentation "implements a POST request to create instances of RESOURCE.")
  (:method ((path string) (resource resource))
    (specify-call :method :post
                  :components (list path)
                  :callback (lambda (properties)
                              (declare (ignore properties))
                              (let ((json-input (jsown:parse (post-body))))
                                (insert *repository* ()
                                  (s+ 
                                   "GRAPH <http://mu.semte.ch/application/> { "
                                   "  <~A> a <~A>;~{~&~4t~{<~A>~,^/~} \"~A\"~,^;~}."
                                   "}")
                                  (clean-url (s+ (ld-resource-base resource)
                                                 (princ-to-string (uuid:make-v4-uuid))))
                                  (clean-url (ld-class resource))
                                  (loop for (property . path)
                                     in (ld-properties resource)
                                     append (list (mapcar #'clean-url path)
                                                  (jsown:filter json-input
                                                                "data"
                                                                (string-downcase
                                                                 (string property)))))))
                              (jsown:new-js ("success" :true))))))

(define-resource product-groups ()
  :class "http://veeakker.com/vocabulary/shop/ProductGroup"
  :properties '((:name . ("http://veeakker.com/vocabulary/shop/product-groups/name"))
                (:color . ("http://veeakker.com/vocabulary/shop/product-groups/color"))
                (:code . ("http://veeakker.com/vocabulary/shop/product-groups/code")))
  :resource-base "http://veeakker.com/api/product-groups/")

(support-creation "product-groups" (gethash 'product-groups *resources*))

;;;; LIST request

;;;; GET request

;;;; PUT request

;;;; POST request
;; create a new resource

;;;; DELETE request

