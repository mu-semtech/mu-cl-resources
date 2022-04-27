(in-package :mu-cl-resources)


;;;;;;;;;;;;;;;;;;;;;;;
;;;; parsing user input

(defgeneric interpret-json-value (slot value)
  (:documentation "Interprets the supplied json value <value>
   given that it should be used for the supplied slot.  Yields a
   value which can be used in a query.")
  (:method ((slot resource-slot) value)
    (interpret-json-value-by-type slot (resource-type slot) value))
  (:method ((slot resource-slot) (value (eql :null)))
    :null))

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
  (:method ((slot resource-slot) (type (eql :time)) value)
    (s-typed value (s-prefix "xsd:time")))
  (:method ((slot resource-slot) (type (eql :date)) value)
    (s-typed value (s-prefix "xsd:date")))
  (:method ((slot resource-slot) (type (eql :g-year)) value)
    (s-typed value (s-prefix "xsd:gYear")))
  (:method ((slot resource-slot) (type (eql :geometry)) value)
    (s-typed value (s-prefix "geo:wktLiteral")))
  (:method ((slot resource-slot) (type (eql :integer)) value)
    (let ((simple-number (if (typep value 'integer)
                             value
                             (round value))))
      (s-typed (princ-to-string simple-number)
               (s-prefix "xsd:integer"))))
  (:method ((slot resource-slot) (type (eql :float)) value)
    (s-typed (princ-to-string (coerce value 'float))
             (s-prefix "xsd:float")))
  (:method ((slot resource-slot) (type (eql :number)) value)
    (let ((simple-number (if (typep value 'ratio)
                             (coerce value 'float)
                             value)))
      (s-typed (princ-to-string simple-number)
               (s-prefix "xsd:decimal"))))
  (:method ((slot resource-slot) (type (eql :string-set)) values)
    (apply #'s-values (mapcar #'s-str values)))
  (:method ((slot resource-slot) (type (eql :uri-set)) values)
    (apply #'s-values (mapcar #'s-url values)))
  (:method ((slot resource-slot) (type (eql :language-string)) value)
    (s-str (jsown:val value "content")
           (jsown:val value "language")))
  (:method ((slot resource-slot) (type (eql :language-string-set)) values)
    (apply #'s-values
           (loop for obj in values
              collect (s-str (jsown:val obj "content")
                             (jsown:val obj "language")))))
  (:method ((slot resource-slot) (type (eql :boolean)) value)
    (s-from-json value)))

(defgeneric interpret-json-string (slot string)
  (:documentation "Interprets a string as if it contained content
   placed in a json object, with respect to the given slot.  This means
   slots for which the content would be parsed in json will be parsed,
   whereas others will be treated as strings, and shifted through
   interpret-json-value.")
  (:method ((slot resource-slot) value)
    (let ((interpreted-value
            (cond
              ((find (resource-type slot)
                     '(:number :integer :float :boolean))
               ;; this will strip quotes on numbers if they exist.  it's
               ;; a code-path we should deprecate but it's used in the
               ;; wild due to earlier quirks.
               (let ((quote-stripped-value
                       (cl-ppcre:scan-to-strings "[^\"]+" value)))
                 (parse-jsown-primitive quote-stripped-value)))
              ((find (resource-type slot)
                     '(:string-set :uri-set :language-string :language-string-set))
               (jsown:parse value))
              (t value))))
      (interpret-json-value slot interpreted-value))))

(defun respond-no-content ()
  "Returns a 204 No Content response."
  (setf (webserver:return-code*) webserver:+http-no-content+)
  :no-content)

(defun respond-not-found (&optional jsown-object)
  "Returns a not-found response.  The supplied jsown-object is
   merged with the response if it is supplied.  This allows you
   to extend the response and tailor it to your needs."
  (setf (webserver:return-code*) webserver:+http-not-found+)
  (merge-jsown-objects (jsown:new-js ("data" :null))
                       (or jsown-object (jsown:empty-object))))

(defun respond-not-acceptable (&optional jsown-object)
  "Returns a not-acceptable response.  The supplied jsown-object
   is merged with the response if it is supplied.  This allows
   you to extend the the response and tailor it to your needs."
  (setf (webserver:return-code*) webserver:+http-not-acceptable+)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Not Acceptable")
                                     ("code" "406"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-forbidden (&optional jsown-object)
  "Returns a 403 Forbidden response.  The supplied jsown-object
   is merged with the response if it is supplied.  This allows
   you to extend the the response and tailor it to your needs."
  (setf (webserver:return-code*) webserver:+http-forbidden+)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "forbidden")
                                     ("code" "403"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-conflict (&optional jsown-object)
  "Returns a 409 Conflict response.  The supplied jsown-object
   is merged with the response if it is supplied.  This allows
   you to extend the the response and tailor it to your needs."
  (setf (webserver:return-code*) webserver:+http-conflict+)
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
  (setf (webserver:return-code*) 422)
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
  (setf (webserver:return-code*) 500)
  (merge-jsown-objects (jsown:new-js
                         ("errors" (jsown:new-js
                                     ("status" "Server Error")
                                     ("code" "500"))))
                       (or jsown-object (jsown:empty-object))))

(defun respond-general-server-error ()
  "Returns a general 500 server error without any extra information."
  (respond-server-error
   (jsown:new-js
     ("errors" (jsown:new-js
                 ("title" "Something went wrong while processing the request"))))))

(defun respond-access-denied (&optional jsown-object)
  "Returns a 403 Access Denied response.  The supplied
   jsown-object is merged with the response if it is supplied.
   This allows you to extend the response and tailor it to your
   needs."
  (setf (webserver:return-code*) 403)
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
  (unless (or (not *verify-content-type-header*)
             (search "application/vnd.api+json"
                     (webserver:header-in* :content-type)))
    (error 'incorrect-content-type
           :description "application/vnd.api+json not found in Content-Type header")))

(defun verify-json-api-request-accept-header ()
  "Returns a 406 Not Acceptable status from the request (and
   returns nil) if the Accept header did not include the
   correct application/vnd.api+json Accept header."
  (if (and *verify-accept-header*
           (not (search "application/vnd.api+json"
                      (webserver:header-in* :accept))))
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

(defun verify-request-required-properties (path obj)
  "Throws an error if the requested object does not contain
   the required properties."
  (let ((resource (find-resource-by-path path))
        (required-slots (remove-if-not #'required-p
                                       (ld-properties
                                        (find-resource-by-path path))))
        (attributes (if (jsown:keyp (jsown:val obj "data") "attributes")
                        (jsown:filter obj "data" "attributes")
                        (jsown:empty-object))))
    (let* ((supplied-required-slots
            (mapcar (lambda (attribute-name)
                      (resource-slot-by-json-key resource attribute-name))
                    (intersection (mapcar #'json-property-name required-slots)
                                  (jsown:keywords attributes)
                                  :test #'string=)))
           (missing-slots (set-difference required-slots supplied-required-slots))
           (required-nonvalue-slots (loop for slot in supplied-required-slots
                                       for json-name = (json-property-name slot)
                                       for json-value = (jsown:val attributes json-name)
                                       unless (slot-value-represents-triples-p slot json-value)
                                       collect slot))
           (breaking-slots (union missing-slots required-nonvalue-slots)))
      (when (or missing-slots required-nonvalue-slots)
        (error 'required-field-missing
               :missing-properties breaking-slots)))))

(defun verify-request-required-properties-not-removed (path obj)
  "Throws an error if the requested update would clear certain
   required properties."
  (let ((required-slots (remove-if-not #'required-p
                                       (ld-properties
                                        (find-resource-by-path path))))
        (attributes (if (jsown:keyp (jsown:val obj "data") "attributes")
                        (jsown:filter obj "data" "attributes")
                        (jsown:empty-object)))
        (resource (find-resource-by-path path)))
    (let ((supplied-required-slots
           (mapcar (lambda (attribute-name)
                     (resource-slot-by-json-key resource attribute-name))
                   (intersection (mapcar #'json-property-name required-slots)
                                 (jsown:keywords attributes)
                                 :test #'string=))))
      (let ((reset-slots (loop for slot in supplied-required-slots
                            for json-name = (json-property-name slot)
                            for json-value = (jsown:val attributes json-name)
                            unless (slot-value-represents-triples-p slot json-value)
                            collect slot)))
        (when reset-slots
          (error 'required-field-missing
                 :missing-properties reset-slots))))))

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
