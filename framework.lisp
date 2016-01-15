(in-package :mu-cl-resources)

;;;;;;;;;;;;;;;;;;
;;;; configuration
(defparameter *camelcase-json-variables* nil
  "when non-nil, json variable names should be camelcased, rather than dasherized.")
(defparameter *verify-accept-header* nil
  "when non-nil, the application/vndi+json ACCEPT header is checked.")

(defparameter *application-graph* (s-url "http://mu.semte.ch/application")
  "standard graph for all sparql queries.")
(defparameter *default-page-size* 20
  "default amount of items in a single page of results.")

;;;;;;;;;;;;;;;;
;;;; error codes

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

(defun plist-remove-nil (plist)
  "Removes settings which are nil from <plist>."
  (loop for (key value) on plist by #'cddr
     if value
     append (list key value)))

(defun build-url (base-url request-params)
  "Constructs a simple url.  Request-params should contain
  lists of options.  Options which contain nil as their value
  are removed.
  eg: (build-url \"/taxonomies\" `((\"page[number]\" 42) (\"page[size]\" 3)))"
  (let ((parameters (plist-remove-nil request-params)))
    (if parameters
        (format nil "~A?~{~A=~A~,^&~}" base-url parameters)
        base-url)))

(defun alist-to-plist (alist)
  "Converts an alist to a plist"
  (loop for (k . v) in alist
     append (list k v)))

(define-setf-expander getfstr (place key &environment env)
  "see (setf getf) and val"
  (multiple-value-bind (*temps *vals *store-vars *setter *getter)
      (get-setf-expansion place env)
    (let ((value-v (gensym "value-v"))
          (key-v (gensym "key-v"))
          (result-v (gensym "result-v")))
      (values (list* key-v *temps) ;; key-v will be set to key
              (list* key *vals)    ;; <- because of this
              (list  value-v)      ;; contains the value to be set
              `(let ((,result-v (fn-update-getfstr ,*getter ,key-v ,value-v)))
                 (let ((,(first *store-vars) ,result-v))
                   ,*setter)
                 ,value-v)
              `(getfstr ,*getter ,key-v)))))

(defun getfstr (place key)
  "getf, but for strings."
  (loop for (k v) on place by #'cddr
     if (and (stringp k)
             (string= k key))
     return v))

(defun fn-update-getfstr (place key new-value)
  "updates getfstr in a functional way"
  (let ((keys (loop for k in place by #'cddr collect k)))
    (if (find key keys :test #'equal)
        (loop for (k v) on place by #'cddr
           if (equal k key)
           append (list k new-value)
           else
           append (list k v))
        (list* key new-value place))))

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

(defun sparql-select (variables body &rest args &key order-by limit offset)
  "Executes a SPARQL SELECT query on the current graph.
   Takes with-query-group into account."
  (declare (ignore order-by limit offset))
  (sparql-query
   (s-select variables args
             (s-graph *application-graph* body))))

(defun sparql-insert (body)
  "Executes a SPARQL INSERT DATA query on the current graph.
   Takes with-query-group into account."
  (sparql-query
   (s-insert
    (s-graph *application-graph* body))))

(defun sparql-insert-triples (triple-clauses)
  "Inserts a set of triples based on the provided triple-clauses.
   Provide a pattern containing triple patterns and variables as
   per 's-var."
  (let ((patterns
         (loop for triple-clause in triple-clauses
            for (subject predicate object) = triple-clause
            collect
              (if (s-inv-p predicate)
                  (format nil "~4t~A ~A ~A.~%"
                          object (s-inv predicate) subject)
                  (format nil "~4t~A ~A ~A.~%"
                          subject predicate object)))))
    (sparql-insert (apply #'concatenate 'string patterns))))

(defun sparql-delete (clauses &optional where)
  "Executes a SPARQL DELETE query on the current graph.
   Takes with-query-group into account."
  (let ((clauses (s-graph *application-graph* clauses))
        (where (when where
                 (s-graph *application-graph* where))))
    (sparql-query
     (s-delete clauses where))))

(defun sparql-delete-triples (triple-clauses)
  "Deletes a set of triples based on the provided triple-clauses.
   Provide a pattern containing triple patterns and variables as
   per 's-var."
  (let ((patterns
         (loop for triple-clause in triple-clauses
            for (subject predicate object) = triple-clause
            collect
              (if (s-inv-p predicate)
                  (format nil "~4t~A ~A ~A.~%"
                          object (s-inv predicate) subject)
                  (format nil "~4t~A ~A ~A.~%"
                          subject predicate object)))))
    (sparql-delete (apply #'concatenate 'string patterns)
                   (apply #'concatenate 'string patterns))))


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

(defparameter *typed-literal-importers* (make-hash-table :test 'equal #-abcl :synchronized #-abcl t)
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

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#string"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#float"
    (value object)
  (declare (ignore object))
  (read-from-string value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#integer"
    (value object)
  (declare (ignore object))
  (parse-integer value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#dateTime"
    (value object)
  (declare (ignore object))
  value)

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
   supplied slot")
  (:method ((slot resource-slot))
    (if *camelcase-json-variables*
        (symbol-to-camelcase (json-key slot))
        (string-downcase (string (json-key slot)))))
  (:method ((link has-link))
    (request-path link)))

(defgeneric sparql-variable-name (resource-slot)
  (:documentation "retrieves the name of the json property as it
   could be used in a sparql query")
  (:method ((slot resource-slot))
    (symbol-to-camelcase (json-key slot)))
  (:method ((link has-link))
    (symbol-to-camelcase (request-path link))))

(defgeneric ld-property-list (slot)
  (:documentation "yields the ld-property as a list from the
   resource-slot")
  (:method ((slot resource-slot))
    (list (ld-property slot)))
  (:method ((link has-link))
    (list (if (inverse-p link)
              (s-inv (ld-link link))
              (ld-link link)))))

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

(defgeneric resource-slot-p (resource &key json-key)
  (:documentation "Returns truethy if a resource could be found
    for the supplied definition.")
  (:method ((resource resource) &key json-key)
    (and (find json-key (ld-properties resource)
               :test #'equal
               :key #'json-property-name)
         t)))

(defparameter *resources* (make-hash-table)
  "contains all currently known resources")

(defun find-resource-by-name (symbol)
  "retrieves the resource with name symbol."
  (gethash symbol *resources*))

(defgeneric referred-resource (link)
  (:documentation "retrieves the resource which is referred to by a link")
  (:method ((link has-link))
    (let ((resource (find-resource-by-name (resource-name link))))
      (unless resource
        (error 'configuration-error
               :description (format nil "Could not find resource for link on path \"~A\".  Searched resource is ~A.  Common possibilities are that there's a (define-resource ~A ...) block missing or that the first argument of a :has-one or :has-many specification has a typo."
                                    (request-path link)
                                    (resource-name link) (resource-name link))))
      resource)))

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

(defgeneric find-resource-link-by-json-key (resource json-key)
  (:documentation "Finds the link object corresponding to the specified
    resource and the specified json key.")
  (:method ((resource resource) json-key)
    (find-resource-link-by-path resource json-key)))

(defun property-path-for-filter-components (resource components)
  "Constructs the SPARQL property path for a set of filter
   components.  Assumes the components end with an attribute
   specification.

   If the last component of the specification yields a resource,
   rather than a property, the search path will allow for all
   properties in that search path."
  (let ((current-resource resource) path-components
        (last-component-resource resource)) ; we use this to expand the search path at the end
    (loop for current-component in components
       for resting-components on components
       for last-component-p = (not (rest resting-components))
       do
         (if (resource-slot-p current-resource :json-key current-component)
             (let ((slot (resource-slot-by-json-key current-resource current-component)))
               (alexandria:appendf path-components (ld-property-list slot))
               (setf last-component-resource nil))
             (let ((link (find-resource-link-by-json-key current-resource current-component)))
               (alexandria:appendf path-components (ld-property-list link))
               (setf current-resource (referred-resource link))
               (setf last-component-resource current-resource))))
    (if last-component-resource
        `(,@path-components
          ,(format nil "(~{(~{~A~^/~})~^|~})"
                   (mapcar #'ld-property-list (ld-properties resource))))
        path-components)))

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
       collect (list (ld-property-list slot)
                     (interpret-json-value
                      slot
                      (jsown:val attributes (json-property-name slot)))))))

(defun attribute-properties-for-json-input (resource json-input)
  (let ((attributes (jsown:filter json-input "data" "attributes")))
    (loop for slot
       in (ld-properties resource)
       if (jsown:keyp attributes (json-property-name slot))
       collect
         (list (ld-property-list slot)
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
    (s-url value))
  (:method ((slot resource-slot) (type (eql :datetime)) value)
    (s-typed value (s-prefix "xsd:dateTime"))))

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

;;;;;;;;;;;;;;;;;;;;;
;;;; response support

(defun try-parse-number (entity)
  "Tries to parse the number, returns nil if no number could be found."
  (handler-case
      (parse-integer entity :junk-allowed t)
    (error () nil)))

(defun count-matches (identifier-variable query-body)
  "Returns the amount of matches for a particular response."
  (parse-integer
   (jsown:filter (first (sparql-select (format nil "((COUNT (DISTINCT ~A)) AS ?count)"
                                               identifier-variable)
                                       query-body))
                 "count" "value")))

(defun extract-pagination-info-from-request ()
  "Extracts the pagination info from the current request object."
  (let ((page-size (or (try-parse-number (hunchentoot:get-parameter "page[size]")) *default-page-size*))
        (page-number (or (try-parse-number (hunchentoot:get-parameter "page[number]")) 0)))
    (list page-size page-number)))

(defun paginate-uuids-for-sparql-body (&key sparql-body page-size page-number)
  (let ((limit page-size)
        (offset (* page-size page-number)))
    (jsown:filter (sparql-select (format nil "DISTINCT ~A" (s-var "uuid"))
                                 sparql-body
                                 :order-by (s-var "uuid")
                                 :limit limit
                                 :offset offset)
                  map "uuid" "value")))

(defun retrieve-data-for-uuids (resource uuids)
  "Retrieves the object description for all found uuids in the
   set of supplied uuids.
   If a uuid could not be found, it is not returned in the set of
   results."
  (loop for uuid in uuids
     for shown = (handler-case
                     (show-call resource uuid)
                   (no-such-instance () nil))
     when shown
     collect (jsown:val shown "data")))

(defun build-pagination-links (base-path &key page-number page-size total-count)
  "Builds a links object containing the necessary pagination
   links.  It bases itself on the base-path for the targetted
   request."
  (flet ((build-url (&key page-number)
           (let ((get-parameters (alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*))))
             (setf (getfstr get-parameters "page[number]")
                   (and (> page-number 0) page-number))
             (setf (getfstr get-parameters "page[size]")
                   (and (/= page-size *default-page-size*)
                        page-size))
             (build-url base-path get-parameters))))
    (let ((last-page (max 0 (1- (ceiling (/ total-count page-size))))))
      (let ((links (jsown:new-js
                     ("first" (build-url :page-number 0))
                     ("last" (build-url :page-number last-page)))))
        (unless (<= page-number 0)
          (setf (jsown:val links "prev")
                (build-url :page-number (1- page-number))))
        (unless (>= page-number last-page)
          (setf (jsown:val links "next")
                (build-url :page-number (1+ page-number))))
        links))))

(defun paginated-collection-response (&key resource sparql-body link-defaults)
  "Constructs the paginated response for a collection listing."
  (destructuring-bind (page-size page-number)
      (extract-pagination-info-from-request)
    (let ((uuid-count (count-matches (s-var "uuid") sparql-body))
          (uuids (paginate-uuids-for-sparql-body :sparql-body sparql-body
                                                 :page-size page-size
                                                 :page-number page-number)))
      (jsown:new-js ("data" (retrieve-data-for-uuids resource uuids))
                    ("links" (merge-jsown-objects
                              (build-pagination-links (hunchentoot:script-name*)
                                                      :total-count uuid-count
                                                      :page-size page-size
                                                      :page-number page-number)
                              link-defaults))))))

(defun sparql-pattern-filter-string (resource source-variable &key components search)
  "Constructs the sparql pattern for a filter constraint."
  (let ((search-var (s-genvar "search")))
   (cond ((string= "id" (car (last components)))
          (format nil "~A ~{~A~^/~}/mu:uuid ~A. ~&"
                  source-variable
                  (butlast (property-path-for-filter-components resource (butlast components)))
                  (s-str search)))
         (t
          (format nil "~A ~{~A~^/~} ~A FILTER CONTAINS(LCASE(~A), LCASE(~A)) ~&"
                  source-variable
                  (property-path-for-filter-components resource components)
                  search-var
                  search-var
                  (s-str search))))))

(defun extract-filters-from-request ()
  "Extracts the filters from the request.  The result is a list
   containing the :components and :search key.  The :components
   key includes a left-to-right specification of the strings
   between brackets.  The :search contains the content for that
   specification."
  (loop for (param . value) in (hunchentoot:get-parameters hunchentoot:*request*)
     if (string= "filter" (subseq param 0 (length "filter")))
     collect (list :components
                   (mapcar (lambda (str)
                             (subseq str 0 (1- (length str))))
                           (rest (cl-ppcre:split "\\[" param)))
                   :search
                   value)))

(defun filter-body-for-search (&key resource source-variable sparql-body)
  "Adds constraints to sparql-body so that it abides the filters
   which were posed by the user."
  (dolist (filter (extract-filters-from-request))
    (setf sparql-body
          (format nil "~A~&~t~A" sparql-body
                  (apply #'sparql-pattern-filter-string
                         resource source-variable filter))))
  sparql-body)


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; call implementation

(defgeneric create-call (resource)
  (:documentation "implementation of the POST request which
    handles the creation of a resource.")
  (:method ((resource-symbol symbol))
    (create-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (let* ((json-input (jsown:parse (post-body)))
           (uuid (mu-support:make-uuid)) 
           (resource-uri (s-url (format nil "~A~A"
                                       (raw-content (ld-resource-base resource))
                                       uuid))))
      (sparql-insert-triples
       `((,resource-uri ,(s-prefix "a") ,(ld-class resource))
         (,resource-uri ,(s-prefix "mu:uuid") ,(s-str uuid))
         ,@(loop for (predicates object)
              in (attribute-properties-for-json-input resource json-input)
              collect `(,resource-uri ,@predicates ,object))))
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
        (let ((delete-vars (loop for key in (jsown:keywords attributes)
                              for i from 0
                              collect (s-var (format nil "gensym~A" i)))))
          (sparql-delete-triples
           (loop for key in (jsown:keywords attributes)
              for slot = (resource-slot-by-json-key resource key)
              for s-var in delete-vars
              collect `(,uri ,@(ld-property-list slot) ,s-var))))
        (sparql-insert-triples
         (loop for key in (jsown:keywords attributes)
            for slot = (resource-slot-by-json-key resource key)
            for value = (interpret-json-value slot (jsown:val attributes key))
            for property-list = (ld-property-list slot)
            collect
              `(,uri ,@property-list ,value))))
      (when (and (jsown:keyp json-input "data")
                 (jsown:keyp (jsown:val json-input "data") "relationships"))
        (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
           if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                          "data")
           do
             (update-resource-relation resource uuid relation
                                       (jsown:filter json-input
                                                     "data" "relationships" relation "data")))))
    (respond-no-content)))

(defgeneric update-resource-relation (resource uuid relation resource-specification)
  (:documentation "updates the specified relation with the given specification.")
  (:method ((resource resource) uuid (relation string) resource-specification)
    (update-resource-relation resource
                              uuid
                              (find-link-by-json-name resource relation)
                              resource-specification))
  (:method ((resource resource) uuid (link has-one-link) resource-specification)
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql-insert-triples
              `((,resource-uri ,@link-uri ,new-linked-uri)))))
      (let ((linked-resource (referred-resource link))
            (resource-uri (find-resource-for-uuid resource uuid)))
        (if resource-specification
            ;; update content
            (let* ((new-linked-uuid (jsown:val resource-specification "id"))
                   (new-linked-uri (find-resource-for-uuid linked-resource new-linked-uuid)))
              (with-query-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (s-url new-linked-uri))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link))))))
  (:method ((resource resource) uuid (link has-many-link) resource-specification)
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uris)
             (sparql-insert-triples
              (loop for new-link-uri in new-linked-uris
                 collect
                   `(,resource-uri ,@link-uri ,new-link-uri)))))
      (let ((linked-resource (referred-resource link))
            (resource-uri (find-resource-for-uuid resource uuid)))
        (if resource-specification
            ;; update content
            (let* ((new-linked-uuids (jsown:filter resource-specification map "id"))
                   (new-linked-resources (mapcar (alexandria:curry #'find-resource-for-uuid
                                                                   linked-resource)
                                                 new-linked-uuids)))
              (with-query-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (mapcar #'s-url new-linked-resources))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link)))))))

(defgeneric list-call (resource)
  (:documentation "implementation of the GET request which
   handles listing the whole resource")
  (:method ((resource-symbol symbol))
    (list-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (paginated-collection-response
     :resource resource
     :sparql-body (filter-body-for-search
                   :sparql-body  (format nil "?s mu:uuid ?uuid; a ~A."
                                         (ld-class resource))
                   :source-variable (s-var "s")
                   :resource resource))))

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
    (let (relation-content)
      (loop for slot in (ld-properties resource)
         do (push (list (ld-property-list slot)
                        (s-var (sparql-variable-name slot)))
                  relation-content))
      (loop for link in (all-links resource)
         do (push (list (ld-property-list link)
                        (s-var (sparql-variable-name link)))
                  relation-content))
      (setf relation-content (reverse relation-content))
      (sparql-delete
       (apply #'concatenate 'string
              (loop for triple-clause
                 in
                   `((,(s-var "s") ,(s-prefix "mu:uuid") ,(s-str uuid))
                     (,(s-var "s") ,(s-prefix "a") ,(ld-class resource))
                     ,@(loop for (property-list value) in relation-content
                          collect `(,(s-var "s") ,@property-list ,value)))
                 for (subject predicate object) = triple-clause
                 collect (if (s-inv-p predicate)
                             (format nil "~4t~A ~A ~A.~%"
                                     object (s-inv predicate) subject)
                             (format nil "~4t~A ~A ~A.~%"
                                     subject predicate object))))
       (concatenate 'string
                    (format nil "~{~&~4t~{~A ~A ~A~}.~%~}"
                            `((,(s-var "s") ,(s-prefix "mu:uuid") ,(s-str uuid))
                              (,(s-var "s") ,(s-prefix "a") ,(ld-class resource))))
                    (format nil "~{~&~4tOPTIONAL {~{~A ~A ~A~}.}~%~}"
                            (loop for (property-list value) in relation-content
                               if (s-inv-p (first property-list))
                               collect `(,value ,(s-inv (first property-list)) ,(s-var "s"))
                               else
                               collect `(,(s-var "s") ,(first property-list) ,value))))))
    (respond-no-content)))

(defgeneric show-relation-call (resource id link)
  (:documentation "implementation of the GET request which handles
    the listing of a relation.")
  (:method ((resource-symbol symbol) id link)
    (show-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (let ((query-results
           (sparql-select (s-var "uuid")
                          (format nil (s+ "~A ~{~A~,^/~} ?resource. "
                                          "?resource mu:uuid ?uuid. ")
                                  (s-url (find-resource-for-uuid resource id))
                                  (ld-property-list link))))
          (linked-resource (referred-resource link)))
      (if query-results
          ;; one result or more
          (jsown:new-js
            ("data" (jsown:val (show-call linked-resource
                                          (jsown:filter (first query-results)
                                                        "uuid" "value"))
                               "data")
                    ;; (jsown:new-js
                    ;;   ("id" (jsown:filter (first query-results) "uuid" "value"))
                    ;;   ("type" (json-type (referred-resource link))))
                    )
            ("links" (build-links-object resource id link)))
          (jsown:new-js
            ("data" :null)
            ("links" (build-links-object resource id link))))))
  (:method ((resource resource) id (link has-many-link))
    (paginated-collection-response
     :resource (referred-resource link)
     :sparql-body (filter-body-for-search
                   :sparql-body (format nil
                                        (s+ "~A ~{~A~,^/~} ?resource. "
                                            "?resource mu:uuid ?uuid.")
                                        (s-url (find-resource-for-uuid resource id))
                                        (ld-property-list link))
                   :source-variable (s-var "resource")
                   :resource (referred-resource link))
     :link-defaults (build-links-object resource id link))))

(defgeneric patch-relation-call (resource id link)
  (:documentation "implementation of the PATCH request which
    handles the updating of a relation.")
  (:method ((resource-symbol symbol) id link)
    (patch-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql-insert-triples
              `((,resource-uri ,@link-uri ,new-linked-uri)))))
      (let ((body (jsown:parse (post-body)))
            (linked-resource (referred-resource link))
            (resource-uri (find-resource-for-uuid resource id))
            (link-path (ld-property-list link)))
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
    (respond-no-content))
  (:method ((resource resource) id (link has-many-link))
    (flet ((delete-query (resource-uri link-uri)
             (sparql-delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uris)
             (loop for new-uri in new-linked-uris
                collect `(,resource-uri ,@link-uri ,new-uri))))
      (let ((body (jsown:parse (post-body)))
            (linked-resource (referred-resource link))
            (resource-uri (find-resource-for-uuid resource id))
            (link-path (ld-property-list link)))
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
    (respond-no-content)))

(defgeneric delete-relation-call (resource id link)
  (:documentation "Performs a delete call on a relation, thereby
    removing a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let* ((linked-resource (referred-resource link))
           (resources (mapcar
                       (alexandria:curry #'find-resource-for-uuid
                                         linked-resource)
                       (remove-if-not #'identity
                                      (jsown:filter (jsown:parse (post-body))
                                                    "data" map "id")))))
      (when resources
        (sparql-delete-triples
         (loop for resource in resources
            collect
              `(,(s-url (find-resource-for-uuid resource id))
                 ,@(ld-property-list link)
                 ,resource)))))
    (respond-no-content)))

(defgeneric add-relation-call (resource id link)
  (:documentation "Performs the addition call on a relation, thereby
    adding a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let* ((linked-resource (referred-resource link))
           (resources (mapcar
                       (alexandria:curry #'find-resource-for-uuid
                                         linked-resource)
                       (remove-if-not #'identity
                                      (jsown:filter (jsown:parse (post-body))
                                                    "data" map "id")))))
      (when resources
        (let ((source-url (find-resource-for-uuid resource id))
              (properties (ld-property-list link)))
          (sparql-insert-triples
           (loop for resource in resources
              collect
                `(,(s-url source-url) ,@properties ,(s-url resource)))))))
    (respond-no-content)))

;;;;;;;;;;;;;;;;;;;
;;;; standard calls

(defcall :get (base-path)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (list-call (find-resource-by-path base-path)))
    (no-such-resource ()
      (respond-not-found))
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
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
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
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
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
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
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
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
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
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
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
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
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
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
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
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
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
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
