(in-package :mu-cl-resources)


;;;;;;;;;;;;;;;;;;;;;;;
;;;; parsing user input

(defgeneric interpret-json-value (slot value)
  (:documentation "Interprets the supplied json value <value>
   given that it should be used for the supplied slot.  Yields a
   value which can be used in a query.")
  (:method ((slot resource-slot) value)
    (interpret-json-value-by-type slot (resource-type slot) value)))

(defgeneric interpret-json-value-by-type (slot type value)
  (:documentation "Interprets the supplied json value <value>
   given that it should be used for the supplied slot.  The type
   of the slot is supplied is the second parameter to dispatch on.")
  (:method ((slot resource-slot) type value)
    (s-from-json value))
  (:method ((slot resource-slot) (type (eql :url)) value)
    (s-url value))
  (:method ((slot resource-slot) (type (eql :datetime)) value)
    (s-typed value (s-prefix "xsd:dateTime")))
  (:method ((slot resource-slot) (type (eql :date)) value)
    (s-typed value (s-prefix "xsd:date")))
  (:method ((slot resource-slot) (type (eql :g-year)) value)
    (s-typed value (s-prefix "xsd:gYear")))
  (:method ((slot resource-slot) (type (eql :geometry)) value)
    (s-typed value (s-prefix "geo:wktLiteral")))
  (:method ((slot resource-slot) (type (eql :language-string-set)) values)
    (apply #'s-values
           (loop for obj in values
              collect (s-str (jsown:val obj "content")
                             (jsown:val obj "language")))))
  (:method ((slot resource-slot) (type (eql :boolean)) value)
    (s-from-json value)))

(defun respond-no-content ()
  "Returns a 204 No Content response."
  (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)
  :no-content)

(defun respond-not-found (&optional jsown-object)
  "Returns a not-found response.  The supplied jsown-object is
   merged with the response if it is supplied.  This allows you
   to extend the response and tailor it to your needs."
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (merge-jsown-objects (jsown:new-js ("data" :null))
                       (or jsown-object (jsown:empty-object))))

(defun respond-not-acceptable (&optional jsown-object)
  "Returns a not-acceptable response.  The supplied jsown-object
   is merged with the response if it is supplied.  This allows
   you to extend the the response and tailor it to your needs."
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-acceptable+)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Not Acceptable")
                                     ("code" "406"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-forbidden (&optional jsown-object)
  "Returns a 403 Forbidden response.  The supplied jsown-object
   is merged with the response if it is supplied.  This allows
   you to extend the the response and tailor it to your needs."
  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "forbidden")
                                     ("code" "403"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-conflict (&optional jsown-object)
  "Returns a 409 Conflict response.  The supplied jsown-object
   is merged with the response if it is supplied.  This allows
   you to extend the the response and tailor it to your needs."
  (setf (hunchentoot:return-code*) hunchentoot:+http-conflict+)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Conflict")
                                     ("code" "409"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-unprocessable-entity (&optional jsown-object)
  "Returns a 422 Unprocessable Entity response.  The supplied
   jsown-object is merged with the response if it is supplied.
   This allows you to extend the response and tailor it to your
   needs."
  (setf (hunchentoot:return-code*) 422)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Unprocessable Entity")
                                     ("code" "422"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-server-error (&optional jsown-object)
  "Returns a 500 Server Error response.   The supplied
   jsown-object is merged with the response if it is supplied.
   This allows you to extend the response and tailor it to your
   needs."
  (setf (hunchentoot:return-code*) 500)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Server Error")
                                     ("code" "500"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-access-denied (&optional jsown-object)
  "Returns a 403 Access Denied response.  The supplied
   jsown-object is merged with the response if it is supplied.
   This allows you to extend the response and tailor it to your
   needs."
  (setf (hunchentoot:return-code*) 403)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Access Denied")
                                     ("code" "403"))))
                       (or jsown-object (jsown:empty-object))))

(defun verify-json-api-content-type ()
  "Throws an error if the Content Type is not the required
   application/vnd.api+json Accept header."
  ;; NOTE: I'm not convinced that the server is required to check this
  ;;       this constraint.  It is not explicited in the spec.
  (unless (search "application/vnd.api+json"
                  (hunchentoot:header-in* :content-type))
    (error 'incorrect-content-type
           :description "application/vnd.api+json not found in Content-Type header")))

(defun verify-json-api-request-accept-header ()
  "Returns a 406 Not Acceptable status from the request (and
   returns nil) if the Accept header did not include the
   correct application/vnd.api+json Accept header."
  (if (and *verify-accept-header*
           (not (search "application/vnd.api+json"
                      (hunchentoot:header-in* :accept))))
      (error 'incorrect-accept-header
             :description "application/vnd.api+json not found in Accept header")))

(defun verify-request-contains-type (obj)
  "Throws an error if the request does not contain a type."
  (unless (and (jsown:keyp obj "data")
               (jsown:keyp (jsown:val obj "data") "type"))
    (error 'no-type-in-data)))

(defun verify-request-contains-no-id (obj)
  "Throws an error if the request contains an id."
  (unless (and (jsown:keyp obj "data")
               (not (jsown:keyp (jsown:val obj "data") "id")))
    (error 'id-in-data)))

(defun verify-request-contains-id (obj)
  "Throws an error if the request does not contain an id."
  (unless (and (jsown:keyp obj "data")
               (jsown:keyp (jsown:val obj "data") "id"))
    (error 'no-id-in-data)))

(defun verify-request-type-matches-path (path obj)
  "Throws an error if the request type for path does not match
   the id specified as a type on obj."
  (let ((supplied-type (jsown:filter obj "data" "type"))
        (path-type (json-type (find-resource-by-path path))))
    (unless (string= supplied-type path-type)
      (error 'request-type-mismatch
             :content-defined-type supplied-type
             :path-defined-type path-type))))

(defun verify-request-id-matches-path (path-id obj)
  "Throws an error if the request id supplied in id does not
   match the id specified as an id on obj."
  (let ((supplied-id (jsown:filter obj "data" "id")))
    (unless (string= path-id supplied-id)
      (error 'request-id-mismatch
             :content-defined-id supplied-id
             :path-defined-id path-id))))

(defgeneric verify-link-patch-body-format (link obj)
  (:documentation "Throws an error if the supplied obj does not have a
    valid format for the supplied link object.")
  (:method ((link has-one-link) obj)
    (unless (jsown:keyp obj "data")
      (error 'invalid-link-patch-body-format
             :description "Top level key (data) missing."))
    (when (jsown:val obj "data")
      ;; only check if data is not null (we allow any falsy value)
      (let ((missing-keys (loop for k in '("id" "type")
                             unless (jsown:keyp (jsown:val obj "data") k)
                             collect k)))
        (when missing-keys
          (error 'invalid-link-patch-body-format
                 :description (format nil
                                      "Obligatory content (~{~A~,^, ~}) of data object was not found."
                                      missing-keys))))))
  (:method ((link has-many-link) obj)
    (unless (jsown:keyp obj "data")
      (error 'invalid-link-patch-body-format
             :description "Top level key (data) missing."))
    (when (jsown:val obj "data")
      ;; only perform these checks when data is not :null (or falsy in our interpretation).
      (loop for answer in (jsown:val obj "data")
         for missing-keys = (loop for k in '("id" "type")
                               unless (jsown:keyp answer k)
                               collect k)
         when missing-keys
         do
           (error 'invalid-link-patch-body-format
                  :description (format nil
                                       "Obligatory content (~{~A~,^, ~}) of one of the items in the data object was not found."
                                       missing-keys))))))
