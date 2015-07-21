(in-package :mu-cl-resources)

;;;;;;;;;;;;;;;;;;
;;;; configuration
(defparameter *camelcase-json-variables* nil
  "when non-nil, json variable names should be camelcased, rather than dasherized.")
(defparameter *verify-accept-header* nil
  "when non-nil, the application/vndi+json ACCEPT header is checked.")

;;;;;;;;;;;;;;;;
;;;; error codes

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


;;;;;;;;;;;;;;;;;;;;
;;;; Supporting code

(defun symbol-to-camelcase (content &key (cap-first nil))
  "builds a javascript variable from anything string-like"
  (format nil "~{~A~}"
          (let ((cap-next cap-first))
            (loop for char across (string-downcase (string content))
               if (char= char #\-)
               do (setf cap-next t)
               else collect (prog1
                                (if cap-next
                                    (char-upcase char)
                                    char)
                              (setf cap-next nil))))))

(defun merge-jsown-objects (a b)
  "Merges jsown objects a and b together.  Returns a new object
   which contains the merged contents."
  (let ((keys (union (jsown:keywords a) (jsown:keywords b) :test #'string=))
        (result (jsown:empty-object)))
    (loop for key in keys
       do (cond ((and (jsown:keyp a key)
                      (not (jsown:keyp b key)))
                 (setf (jsown:val result key)
                       (jsown:val a key)))
                ((and (not (jsown:keyp a key))
                      (jsown:keyp b key))
                 (setf (jsown:val result key)
                       (jsown:val b key)))
                (t (handler-case
                       (setf (jsown:val result key)
                             (merge-jsown-objects (jsown:val a key)
                                                  (jsown:val b key)))
                     (error () (setf (jsown:val result key)
                                     (jsown:val b key)))))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; query execution helpers

(defparameter *query-group* nil
  "Describes a group of queries to be executed in one go. The
   queries are executed when the query-group is ended.  This is a
   special variable which is set by with-grouped-queries")

(defmacro with-query-group (&body body)
  "Starts a new query-group.  The queries are executed when the
   group exits."
  `(let ((*query-group* (cons nil nil)))
     ,@body
     (let ((queries (apply #'s+ (reverse (butlast *query-group*)))))
       (fuseki:with-query-logging *error-output*
         (fuseki:query *repository* queries)))))

(defun sparql-query (content)
  "Executes a sparql query on the current repository, or pushes
   it on the set of queries to be executed in the query-group
   based on the current context.

   NOTE: see sparql-select, sparql-insert and sparql-delete for
         functions geared towards end-users."
  (if *query-group*
      (push content *query-group*)
      (fuseki:with-query-logging *error-output*
        (fuseki:query *repository* content))))

(defun sparql-select (variables body)
  "Executes a SPARQL SELECT query on the current graph.
   Takes with-query-group into account."
  (sparql-query
   (s-select variables
             (s-graph (s-url "http://mu.semte.ch/application/")
                      body))))

(defun sparql-insert (body)
  "Executes a SPARQL INSERT DATA query on the current graph.
   Takes with-query-group into account."
  (sparql-query
   (s-insert
    (s-graph (s-url "http://mu.semte.ch/application/")
             body))))

(defun sparql-delete (body)
  "Executes a SPARQL DELETE query on the current graph.
   Takes with-query-group into account."
  (sparql-query
   (s-delete (s-graph (s-url "http://mu.semte.ch/application/")
                      body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parsing query results

(defun from-sparql (object)
  "Converts the supplied sparql value specification into a lisp value."
  (let ((type (intern (string-upcase (jsown:val object "type"))
                      :keyword))
        (value (jsown:val object "value")))
    (import-value-from-sparql-result type value object)))

(defgeneric import-value-from-sparql-result (type value object)
  (:documentation "imports the value from 'object' given its 'value'
   and 'type' to dispatch on.")
  (:method ((type (eql :uri)) value object)
    (declare (ignore object))
    value)
  (:method ((type (eql :literal)) value object)
    (declare (ignore object))
    value)
  (:method ((type (eql :typed-literal)) value object)
    (import-typed-literal-value-from-sparql-result
     (jsown:val object "datatype")
     value
     object)))

(defparameter *typed-literal-importers* (make-hash-table :test 'equal :synchronized t)
  "contains all convertors for typed-literal values coming from the database.")

(defmacro define-typed-literal-importer (type (&rest variables) &body body)
  "defines a new typed literal importer.  should receive value, object
   as variables."
  `(setf (gethash ,type *typed-literal-importers*)
         (lambda (,@variables)
           ,@body)))

(defun import-typed-literal-value-from-sparql-result (type value object)
  "imports a typed-literal-value from a sparql result."
  (funcall (gethash type *typed-literal-importers*)
           value object))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#decimal"
    (value object)
  (declare (ignore object))
  (read-from-string value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#integer"
    (value object)
  (declare (ignore object))
  (parse-integer value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#boolean"
    (value object)
  (declare (ignore object))
  (if (or (string= value "1")
          (string= value "true"))
      :true :false))

(define-typed-literal-importer "http://mu.semte.ch/vocabularies/typed-literals/boolean"
    (value object)
  (declare (ignore object))
  (if (or (string= value "1")
          (string= value "true"))
      :true :false))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; defining resources

(defclass resource-slot ()
  ((json-key :initarg :json-key :reader json-key)
   (ld-property :initarg :ld-property :reader ld-property)
   (resource-type :initarg :resource-type :reader resource-type))
  (:documentation "Describes a single property of a resource."))

(defclass has-link ()
  ((resource-name :initarg :resource :reader resource-name)
   (ld-link :initarg :via :reader ld-link)
   (inverse :initarg :inverse :reader inverse-p :initform nil)
   (request-path :initarg :as :reader request-path))
  (:documentation "Describes a link to another resource.
   You should use one of its subclasses."))

(defclass has-many-link (has-link)
  ()
  (:documentation "Describes a has-many link to another resource"))

(defclass has-one-link (has-link)
  ()
  (:documentation "Describes a has-one link to another resource"))

(defclass resource ()
  ((ld-class :initarg :ld-class :reader ld-class)
   (ld-properties :initarg :ld-properties :reader ld-properties)
   (ld-resource-base :initarg :ld-resource-base :reader ld-resource-base)
   (json-type :initarg :json-type :reader json-type)
   (has-many-links :initarg :has-many :reader has-many-links)
   (has-one-links :initarg :has-one :reader has-one-links)
   (request-path :initarg :request-path :reader request-path)))

(defgeneric json-property-name (resource-slot)
  (:documentation "retrieves the name of the json property of the
   supplied resource-slot")
  (:method ((slot resource-slot))
    (if *camelcase-json-variables*
        (symbol-to-camelcase (json-key slot))
        (string-downcase (string (json-key slot))))))

(defgeneric sparql-variable-name (resource-slot)
  (:documentation "retrieves the name of the json property as it
   could be used in a sparql query")
  (:method ((slot resource-slot))
    (symbol-to-camelcase (json-key slot))))

(defgeneric ld-property-list (slot)
  (:documentation "yields the ld-property as a list from the
   resource-slot")
  (:method ((slot resource-slot))
    (list (ld-property slot))))

(defmethod json-key ((link has-link))
  (request-path link))

(defgeneric find-link-by-json-name (resource json-link)
  (:documentation "find a has-many link by resource and json-link of the link")
  (:method ((resource resource) json-link)
    (loop for link in (all-links resource)
       if (string= (json-key link) json-link)
       return link)))

(defgeneric all-links (resource)
  (:documentation "Retrieves all links for the supplied resource.
    Both the has-many-links and has-one-links.")
  (:method ((resource resource))
    (append (has-many-links resource) (has-one-links resource))))

(defgeneric resource-slot-by-json-key (resource key)
  (:documentation "Returns the slot which should be communicated
    with the json format through the use of the key attribute.")
  (:method ((resource resource) key)
    (loop for slot in (ld-properties resource)
       when (string= (json-property-name slot) key)
       return slot)))

(defparameter *resources* (make-hash-table)
  "contains all currently known resources")

(defun find-resource-by-name (symbol)
  "retrieves the resource with name symbol."
  (gethash symbol *resources*))

(defun find-resource-by-path (path)
  "finds a resource based on the supplied request path"
  (maphash (lambda (name resource)
             (declare (ignore name))
             (when (string= (request-path resource) path)
               (return-from find-resource-by-path resource)))
           *resources*)
  (error 'no-such-resource
         :description (format nil "Path: ~A" path)))

(defgeneric find-resource-link-by-path (resource path)
  (:documentation "Finds the link object corresponding to the specified
    resource and the specified path.")
  (:method ((resource resource) path)
    (let ((link (find path (all-links resource)
                      :test (lambda (path link)
                              (string= path (request-path link))))))
      (unless link
        (error 'no-such-link
               :resource resource
               :path path))
      link)))

(defun define-resource* (name &key ld-class ld-properties ld-resource-base has-many has-one on-path)
  "defines a resource for which get and set requests exist"
  (let* ((properties (loop for (key type prop) in ld-properties
                        collect (make-instance 'resource-slot
                                               :json-key key
                                               :resource-type type
                                               :ld-property prop)))
         (has-many-links (mapcar (alexandria:curry #'apply #'make-instance 'has-many-link :resource)
                                 has-many))
         (has-one-links (mapcar (alexandria:curry #'apply #'make-instance 'has-one-link :resource)
                                has-one))
         (resource (make-instance 'resource
                                  :ld-class ld-class
                                  :ld-properties properties
                                  :ld-resource-base ld-resource-base
                                  :has-many has-many-links
                                  :has-one has-one-links
                                  :json-type on-path ; (symbol-to-camelcase name :cap-first t)
                                  :request-path on-path)))
    (setf (gethash name *resources*) resource)))

(defmacro define-resource (name options &key class properties resource-base has-many has-one on-path)
  (declare (ignore options))
  `(define-resource* ',name
       :ld-class ,class
       :ld-properties ,properties
       :ld-resource-base ,resource-base
       :has-many ,has-many
       :has-one ,has-one
       :on-path ,on-path))

(defun property-paths-format-component (resource)
  (declare (ignore resource))
  "~{~&~4t~{~A~,^/~} ~A~,^;~}.")
(defun property-paths-content-component (resource json-input)
  (let ((attributes (jsown:filter json-input "data" "attributes")))
    (loop for slot
       in (ld-properties resource)
       if (jsown:keyp attributes (json-property-name slot))
       append (list (ld-property-list slot)
                    (interpret-json-value
                     slot
                     (jsown:val attributes (json-property-name slot)))))))

(defgeneric construct-resource-item-path (resource identifier)
  (:documentation "Constructs the path on which information can
   be fetched for a specific instance of a resource.")
  (:method ((resource resource) identifier)
    (format nil "/~A/~A"
            (request-path resource) identifier)))


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
    (s-url value)))


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

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; call implementation

(defgeneric create-call (resource)
  (:documentation "implementation of the POST request which
    handles the creation of a resource.")
  (:method ((resource-symbol symbol))
    (create-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (let ((json-input (jsown:parse (post-body)))
          (uuid (princ-to-string (uuid:make-v4-uuid))))
      (sparql-insert (format nil
                             (s+ "~A a ~A;"
                                 "~&~4tmu:uuid ~A;"
                                 (property-paths-format-component resource))
                             (s-url (format nil "~A~A"
                                            (raw-content (ld-resource-base resource))
                                            uuid))
                             (ld-class resource)
                             (s-str uuid)
                             (property-paths-content-component resource json-input)))
      (setf (hunchentoot:return-code*) hunchentoot:+http-created+)
      (setf (hunchentoot:header-out :location)
            (construct-resource-item-path resource uuid))
      (when (and (jsown:keyp json-input "data")
                 (jsown:keyp (jsown:val json-input "data") "relationships"))
        (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
           if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                          "data")
           do
             (update-resource-relation resource uuid relation
                                       (jsown:filter json-input
                                                     "data" "relationships" relation "data"))))
      (let ((show-content (show-call resource uuid)))
        ;; only need to set the id in attributes temporarily
        (setf (jsown:val (jsown:val (jsown:val show-content "data")
                                    "attributes")
                         "id")
              uuid)
        show-content))))

(defun find-resource-for-uuid (resource uuid)
  "Retrieves the resource hich specifies the supplied UUID in the database."
  (let ((result (sparql-select (s-var "s")
                               (format nil "?s mu:uuid ~A."
                                       (s-str uuid)))))
    (unless result
      (error 'no-such-instance
             :resource resource
             :id uuid
             :type (json-type resource)))
    (jsown:filter (first result) "s" "value")))

(defgeneric update-call (resource uuid)
  (:documentation "implementation of the PUT request which
    handles the updating of a resource.")
  (:method ((resource-symbol symbol) uuid)
    (update-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let* ((json-input (jsown:parse (post-body)))
           (attributes (jsown:filter json-input "data" "attributes"))
           (uri (s-url (find-resource-for-uuid resource uuid))))
      (with-query-group
        (sparql-delete
         (format nil "~{~&OPTIONAL{ ~A ~{~A~,^/~} ~A.}~}"
                 (loop for key in (jsown:keywords attributes)
                    for slot = (resource-slot-by-json-key resource key)
                    for i from 0
                    append (list uri
                                 (ld-property-list slot)
                                 (s-var (format nil "gensym~A" i))))))
        (sparql-insert
         (format nil "~A ~{~&~8t~{~A~,^/~} ~A~,^;~}."
                 uri
                 (loop for key in (jsown:keywords attributes)
                    for slot = (resource-slot-by-json-key resource key)
                    append (list (ld-property-list slot)
                                 (interpret-json-value slot
                                                       (jsown:val attributes key)))))))
      (when (and (jsown:keyp json-input "data")
                 (jsown:keyp (jsown:val json-input "data") "relationships"))
        (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
           if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                          "data")
           do
             (update-resource-relation resource uuid relation
                                       (jsown:filter json-input
                                                     "data" "relationships" relation "data")))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)))

(defgeneric update-resource-relation (resource uuid relation resource-specification)
  (:documentation "updates the specified relation with the given specification.")
  (:method ((resource resource) uuid (relation string) resource-specification)
    (update-resource-relation resource
                              uuid
                              (find-link-by-json-name resource relation)
                              resource-specification))
  (:method ((resource resource) uuid (link has-one-link) resource-specification)
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete (format nil "~A ~A ?s"
                                    resource-uri link-uri)))
           (inverse-delete-query (resource-uri link-uri)
             (sparql-delete (format nil "?s ~A ~A."
                                    link-uri resource-uri)))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql-insert (format nil "~A ~A ~A."
                                    resource-uri link-uri new-linked-uri))))
      (let ((linked-resource (find-resource-by-name (resource-name link)))
            (resource-uri (find-resource-for-uuid resource uuid)))
        (if resource-specification
            ;; update content
            (let* ((new-linked-uuid (jsown:val resource-specification "id"))
                   (new-linked-uri (find-resource-for-uuid linked-resource new-linked-uuid)))
              (if (inverse-p link)
                  (with-query-group
                    (inverse-delete-query (s-url resource-uri)
                                          (ld-link link))
                    (insert-query (s-url new-linked-uri)
                                  (ld-link link)
                                  (s-url resource-uri)))
                  (with-query-group
                    (delete-query (s-url resource-uri)
                                  (ld-link link))
                    (insert-query (s-url resource-uri)
                                  (ld-link link)
                                  (s-url new-linked-uri)))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-link link))))))
  (:method ((resource resource) uuid (link has-many-link) resource-specification)
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete (format nil "~A ~A ?s."
                                    resource-uri link-uri)))
           (insert-query (resource-uri link-uri new-linked-uris)
             (sparql-insert (format nil "~A ~A ~{~&~8t~A~,^, ~}."
                                    resource-uri link-uri new-linked-uris))))
      (let ((linked-resource (find-resource-by-name (resource-name link)))
            (resource-uri (find-resource-for-uuid resource uuid)))
        (if resource-specification
            ;; update content
            (let* ((new-linked-uuids (jsown:filter resource-specification map "id"))
                   (new-linked-resources (mapcar (alexandria:curry #'find-resource-for-uuid
                                                                   linked-resource)
                                                 new-linked-uuids)))
              (with-query-group
                (delete-query (s-url resource-uri)
                              (ld-link link))
                (insert-query (s-url resource-uri)
                              (ld-link link)
                              (mapcar #'s-url new-linked-resources))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-link link)))))))

(defgeneric list-call (resource)
  (:documentation "implementation of the GET request which
   handles listing the whole resource")
  (:method ((resource-symbol symbol))
    (list-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (let ((uuids (jsown:filter
                  (sparql-select "*"
                                 (format nil "?s mu:uuid ?uuid; a ~A."
                                         (ld-class resource)))
                  map "uuid" "value")))
      (jsown:new-js ("data" (loop for uuid in uuids
                               for shown = (handler-case
                                               (show-call resource uuid)
                                             (no-such-instance () nil))
                               when shown
                               collect (jsown:val shown "data")))))))

(defgeneric show-call (resource uuid)
  (:documentation "implementation of the GET request which
    handles the displaying of a single resource.")
  (:method ((resource-symbol symbol) uuid)
    (show-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let* ((resource-url
            ;; we search for a resource separately as searching it
            ;; in one query is redonculously slow.  in the order of
            ;; seconds for a single solution.
            (find-resource-for-uuid resource uuid))
           (solution (first
                      (sparql-select
                       "*"
                       (format nil
                               "~{~&OPTIONAL {~A ~{~A~,^/~} ~A.}~}"
                               (loop for slot in (ld-properties resource)
                                  append (list (s-url resource-url)
                                               (ld-property-list slot)
                                               (s-var (sparql-variable-name slot))))))))
           (attributes (jsown:empty-object)))
      (unless solution
        (error 'no-such-instance
               :resource resource
               :id uuid
               :type (json-type resource)))
      (loop for property in (ld-properties resource)
         for sparql-var = (sparql-variable-name property)
         for json-var = (json-property-name property)
         if (jsown:keyp solution sparql-var)
         do
           (setf (jsown:val attributes json-var)
                 (from-sparql (jsown:val solution sparql-var))))
      (let* ((resp-data (jsown:new-js
                          ("attributes" attributes)
                          ("id" uuid)
                          ("type" (json-type resource))
                          ("relationships" (jsown:empty-object)))))
        (loop for link in (all-links resource)
           do
             (setf (jsown:val (jsown:val resp-data "relationships") (json-key link))
                   (jsown:new-js ("links" (build-links-object resource uuid link)))))
        (jsown:new-js
          ("data" resp-data)
          ("links" (jsown:new-js ("self" (construct-resource-item-path resource uuid)))))))))

(defgeneric build-links-object (resource identifier link)
  (:documentation "Builds the json object which represents the link
    in a json object.")
  (:method ((resource resource) identifier (link has-link))
    (jsown:new-js ("self" (format nil "/~A/~A/links/~A"
                                  (request-path resource)
                                  identifier
                                  (request-path link)))
                  ("related" (format nil "/~A/~A/~A"
                                     (request-path resource)
                                     identifier
                                     (request-path link))))))

(defgeneric delete-call (resource uuid)
  (:documentation "implementation of the DELETE request which
   handles the deletion of a single resource")
  (:method ((resource-symbol symbol) uuid)
    (delete-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (sparql-delete
     (format nil (s+ "?s mu:uuid ~A;"
                     "   a ~A."
                     "~{~&OPTIONAL { ?s ~{~A~,^/~} ~A.}~}")
             (s-str uuid)
             (ld-class resource)
             (loop for slot in (ld-properties resource)
                append (list (ld-property-list slot)
                             (s-var (sparql-variable-name slot))))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)))

(defgeneric show-relation-call (resource id link)
  (:documentation "implementation of the GET request which handles
    the listing of a relation.")
  (:method ((resource-symbol symbol) id link)
    (show-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (let ((query-results
           (sparql-select (s-var "uuid")
                          (format nil (s+ "~A ~A ?resource. "
                                          "?resource mu:uuid ?uuid. ")
                                  (s-url (find-resource-for-uuid resource id))
                                  (let ((link-property (ld-link link)))
                                    (if (inverse-p link)
                                        (s-inv link-property)
                                        link-property)))))
          (linked-resource (find-resource-by-name (resource-name link))))
      (if query-results
          ;; one result or more
          (jsown:new-js
            ("data" (jsown:val (show-call linked-resource
                                          (jsown:filter (first query-results)
                                                        "uuid" "value"))
                               "data")
                    ;; (jsown:new-js
                    ;;   ("id" (jsown:filter (first query-results) "uuid" "value"))
                    ;;   ("type" (json-type (find-resource-by-name (resource-name link)))))
                    )
            ("links" (build-links-object resource id link)))
          (jsown:new-js
            ("data" :null)
            ("links" (build-links-object resource id link))))))
  (:method ((resource resource) id (link has-many-link))
    (let ((query-results
           (sparql-select (s-var "uuid")
                          (format nil
                                  (s+ "~A ~A ?resource. "
                                      "?resource mu:uuid ?uuid.")
                                  (s-url (find-resource-for-uuid resource id))
                                  (let ((link-property (ld-link link)))
                                    (if (inverse-p link)
                                        (s-inv link-property)
                                        link-property)))))
          (linked-resource (find-resource-by-name (resource-name link))))
      (jsown:new-js
        ("data" (loop for result in query-results
                   for uuid = (jsown:filter result "uuid" "value")
                   collect
                     (jsown:val (show-call linked-resource uuid) "data")
                     ;;   ("id" uuid)
                     ;;   ("self" (construct-resource-item-path linked-resource uuid))
                     ;;   ("type" (json-type linked-resource)))
                     ))
        ("links" (build-links-object resource id link))))))

(defgeneric patch-relation-call (resource id link)
  (:documentation "implementation of the PATCH request which
    handles the updating of a relation.")
  (:method ((resource-symbol symbol) id link)
    (patch-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete (format nil "~A ~A ?s."
                                    resource-uri link-uri)))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql-insert (format nil "~A ~A ~A."
                                    resource-uri link-uri new-linked-uri))))
      (let ((body (jsown:parse (post-body)))
            (linked-resource (find-resource-by-name (resource-name link)))
            (resource-uri (find-resource-for-uuid resource id))
            (link-path (if (inverse-p link) (s-inv (ld-link link)) (ld-link link))))
        (if (jsown:val body "data")
            ;; update content
            (let* ((new-linked-uuid (jsown:filter body "data" "id"))
                   (new-linked-uri (find-resource-for-uuid linked-resource new-linked-uuid)))
              (with-query-group
                (delete-query (s-url resource-uri) link-path)
                (insert-query (s-url resource-uri) link-path
                              (s-url new-linked-uri))))
            ;; delete content
            (delete-query (s-url resource-uri) link-path))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+))
  (:method ((resource resource) id (link has-many-link))
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete (format nil "~A ~A ?s."
                                    resource-uri link-uri)))
           (insert-query (resource-uri link-uri new-linked-uris)
             (sparql-insert (format nil "~A ~A ~{~&~8t~A~,^, ~}."
                                    resource-uri link-uri new-linked-uris))))
      (let ((body (jsown:parse (post-body)))
            (linked-resource (find-resource-by-name (resource-name link)))
            (resource-uri (find-resource-for-uuid resource id))
            (link-path (if (inverse-p link) (s-inv (ld-link link)) (ld-link link))))
        (if (jsown:val body "data")
            ;; update content
            (let* ((new-linked-uuids (jsown:filter body "data" map "id"))
                   (new-linked-resources (mapcar (alexandria:curry #'find-resource-for-uuid
                                                                   linked-resource)
                                                 new-linked-uuids)))
              (delete-query (s-url resource-uri) link-path)
              (insert-query (s-url resource-uri)
                            link-path
                            (mapcar #'s-url new-linked-resources)))
            ;; delete content
            (delete-query (s-url resource-uri)
                          link-path))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)))

(defgeneric delete-relation-call (resource id link)
  (:documentation "Performs a delete call on a relation, thereby
    removing a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let* ((linked-resource (find-resource-by-name (resource-name link)))
           (resources (mapcar
                       (alexandria:curry #'find-resource-for-uuid
                                         linked-resource)
                       (remove-if-not #'identity
                                      (jsown:filter (jsown:parse (post-body))
                                                    "data" map "id")))))
      (when resources
        (sparql-delete (format nil "~A ~A ~{~&~8t~A~,^, ~}"
                               (s-url (find-resource-for-uuid resource id))
                               (if (inverse-p link)
                                   (s-inv (ld-link link))
                                   (ld-link link))
                               (mapcar #'s-url resources)))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)))

(defgeneric add-relation-call (resource id link)
  (:documentation "Performs the addition call on a relation, thereby
    adding a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let* ((linked-resource (find-resource-by-name (resource-name link)))
           (resources (mapcar
                       (alexandria:curry #'find-resource-for-uuid
                                         linked-resource)
                       (remove-if-not #'identity
                                      (jsown:filter (jsown:parse (post-body))
                                                    "data" map "id")))))
      (when resources
        (sparql-insert (format nil "~A ~A ~{~&~8t~A~,^, ~}"
                               (s-url (find-resource-for-uuid resource id))
                               (if (inverse-p link)
                                   (s-inv (ld-link link))
                                   (ld-link link))
                               (mapcar #'s-url resources)))))
    (setf (hunchentoot:return-code*) hunchentoot:+http-no-content+)))

;;;;;;;;;;;;;;;;;;;
;;;; standard calls

(defcall :get (base-path)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (list-call (find-resource-by-path base-path)))
    (no-such-resource ()
      (respond-not-found))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))))

(defcall :get (base-path id)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (show-call (find-resource-by-path base-path) id))
    (no-such-resource ()
      (respond-not-found))
    (no-such-instance ()
      (respond-not-found))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))))

(defcall :post (base-path)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (verify-request-contains-type body)
          (verify-request-contains-no-id body)
          (verify-request-type-matches-path base-path body)
          (create-call (find-resource-by-path base-path)))
      (no-such-resource ()
        (respond-forbidden (jsown:new-js
                             ("errors" (jsown:new-js
                                         ("title" (format nil
                                                          "Resource for path (~A) not found"
                                                          base-path)))))))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Not allow to supply id in primary data."))))))
      (request-type-mismatch (condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "Supplied type (~A) did not match type for path (~A)."
                                        (content-defined-type condition)
                                        (path-defined-type condition)))))))))))

(defcall :patch (base-path id)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (verify-request-contains-type body)
          (verify-request-contains-id body)
          (verify-request-type-matches-path base-path body)
          (verify-request-id-matches-path id body)
          (update-call (find-resource-by-path base-path) id))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (request-type-mismatch (condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "Supplied type (~A) did not match type for path (~A)."
                                        (content-defined-type condition)
                                        (path-defined-type condition))))))))
      (request-id-mismatch (condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "id in data (~A) did ot match id in path (~A)."
                                        (content-defined-id condition)
                                        (path-defined-id condition)))))))))))

(defcall :delete (base-path id)
  (handler-case
      (delete-call (find-resource-by-path base-path) id)
    (no-such-resource ()
      (respond-forbidden (jsown:new-js
                           ("errors" (jsown:new-js
                                       ("title" (format nil
                                                        "Resource for path (~A) not found"
                                                        base-path)))))))))

;;;;;;;;;;;;;;;
;;;; link calls

(defun handle-relation-get-call (base-path id relation)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (let* ((resource (find-resource-by-path base-path))
               (link (find-resource-link-by-path resource relation)))
          (show-relation-call resource id link)))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))
    (no-such-resource ()
      (respond-not-found))
    (no-such-link (condition)
      (let ((message
             (format nil "Could not find link (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))))

(defcall :get (base-path id relation)
  (handle-relation-get-call base-path id relation))

(defcall :get (base-path id :links relation)
  (handle-relation-get-call base-path id relation))

(defcall :patch (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body)
            (patch-relation-call resource id link)))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource ()
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-such-instance (condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))

(defcall :post (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body) ; same as post body
            (add-relation-call resource id link)))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource ()
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-such-instance (condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))

(defcall :delete (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body) ; same as delete body
            (delete-relation-call resource id link)))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource ()
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-such-instance (condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))
