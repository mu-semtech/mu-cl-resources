(in-package :product-groups)

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

(defun from-sparql (object)
  "Converts the supplied sparql value specification into a lisp value."
  (break "Importing ~A" object)
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

(defclass resource ()
  ((ld-class :initarg :ld-class :reader ld-class)
   (ld-properties :initarg :ld-properties :reader ld-properties)
   (ld-resource-base :initarg :ld-resource-base :reader ld-resource-base)
   (json-type :initarg :json-type :reader json-type)))

(defparameter *resources* (make-hash-table)
  "contains all currently known resources")

(defun define-resource* (name &key ld-class ld-properties ld-resource-base)
  "defines a resource for which get and set requests exist"
  (let ((resource (make-instance 'resource
                                 :ld-class ld-class
                                 :ld-properties ld-properties
                                 :ld-resource-base ld-resource-base
                                 :json-type (symbol-to-camelcase name :cap-first t))))
    (setf (gethash name *resources*) resource)))

(defmacro define-resource (name options &key class properties resource-base)
  (declare (ignore options))
  `(define-resource* ',name
       :ld-class ,class
       :ld-properties ,properties
       :ld-resource-base ,resource-base))

(defun property-paths-format-component (resource)
  (declare (ignore resource))
  "~{~&~4t~{~A~,^/~} ~A~,^;~}.")
(defun property-paths-content-component (resource json-input)
  (loop for (property . path)
     in (ld-properties resource)
     append (list path
                  (s-str
                   (jsown:filter json-input
                                 "data"
                                 (string-downcase
                                  (string property)))))))

(defgeneric create-call (resource)
  (:documentation "implementation of the POST request which
    handles the creation of a resource.")
  (:method ((resource-symbol symbol))
    (create-call (gethash resource-symbol *resources*)))
  (:method ((resource resource))
    (let ((json-input (jsown:parse (post-body)))
          (uuid (princ-to-string (uuid:make-v4-uuid))))
      (insert *repository* ()
        (s+
         "GRAPH <http://mu.semte.ch/application/> { "
         "  ~A a ~A;"
         "     mu:number 1337.3;"
         "  ~&~4tmu:uuid ~A;"
         (property-paths-format-component resource)
         "}")
        (s-url (format nil "~A~A"
                       (raw-content (ld-resource-base resource))
                       uuid))
        (ld-class resource)
        (s-str uuid)
        (property-paths-content-component resource json-input))
      (show-call resource uuid))))

(defgeneric update-call (resource uuid)
  (:documentation "implementation of the PUT request which
    handles the updating of a resource.")
  (:method ((resource-symbol symbol) uuid)
    (update-call (gethash resource-symbol *resources*) uuid))
  (:method ((resource resource) (uuid string))
    ;; ideally, we'd be a lot more prudent with deleting content
    (let ((json-input (jsown:parse (post-body))))
      (fuseki:query
       *repository*
       (format nil
               (s+
                "DELETE WHERE {"
                "  GRAPH <http://mu.semte.ch/application/> { "
                "    ?s mu:uuid ~A; "
                "    ~{~&~8t~{~A~,^/~} ~A~,^;~}."
                "  }"
                "}")
               (s-str uuid)
               (loop for (property . path)
                  in (ld-properties resource)
                  for i from 0
                  append (list path (s-var (format nil "gensym~A" i))))))
      (insert *repository* ()
        (s+
         "GRAPH <http://mu.semte.ch/application/> { "
         "  ~A mu:uuid ~A; "
         "     mu:number 1337.12; "
         (property-paths-format-component resource)
         "}")
        (s-url (s+ (raw-content (ld-resource-base resource)) uuid))
        (s-str uuid)
        (property-paths-content-component resource json-input)))
    (jsown:new-js
      ("success" :true))))

(defgeneric list-call (resource)
  (:documentation "implementation of the GET request which
   handles listing the whole resource")
  (:method ((resource-symbol symbol))
    (list-call (gethash resource-symbol *resources*)))
  (:method ((resource resource))
    (let ((uuids (jsown:filter
                  (query *repository*
                         (format nil
                                 (s+ "SELECT * WHERE {"
                                     "  GRAPH <http://mu.semte.ch/application/> {"
                                     "    ?s mu:uuid ?uuid;"
                                     "       a ~A."
                                     "  }"
                                     "}")
                                 (ld-class resource)))
                  map "uuid" "value")))
      (jsown:new-js ("data" (loop for uuid in uuids
                                     collect (jsown:val (show-call resource uuid)
                                                        "data")))))))

(defgeneric show-call (resource uuid)
  (:documentation "implementation of the GET request which
    handles the displaying of a single resource.")
  (:method ((resource-symbol symbol) uuid)
    (show-call (gethash resource-symbol *resources*) uuid))
  (:method ((resource resource) (uuid string))
    (flet ((property-var-string (property-description)
             "returns a string for the json property"
             (string-downcase (string property-description))))
      (let* ((solutions
              (query *repository*
                     (format nil
                             (s+ "SELECT * WHERE {"
                                 "  GRAPH <http://mu.semte.ch/application/> {"
                                 "    ?s mu:uuid ~A; "
                                 "    ~{~&~8t~{~A~,^/~} ~A~,^;~}."
                                 "  }"
                                 "}")
                             (s-str uuid)
                             (loop for (property . path) in (ld-properties resource)
                                append (list path (s-var (property-var-string property)))))))
             (attributes (jsown:empty-object)))
        (dolist (var (mapcar (alexandria:compose #'property-var-string #'car)
                             (ld-properties resource)))
          (setf (jsown:val attributes (symbol-to-camelcase var))
                (jsown:filter (first solutions) var "value")))
        (jsown:new-js
          ("data" (jsown:new-js
                    ("attributes" attributes)
                    ("id" uuid)
                    ("type" (json-type resource)))))))))

(defgeneric delete-call (resource uuid)
  (:documentation "implementation of the DELETE request which
   handles the deletion of a single resource")
  (:method ((resource-symbol symbol) uuid)
    (delete-call (gethash resource-symbol *resources*) uuid))
  (:method ((resource resource) (uuid string))
    (query *repository*
           (format nil
                   (s+ "DELETE WHERE {"
                       "  GRAPH <http://mu.semte.ch/application/> {"
                       "    ?s mu:uuid ~A;"
                       "       a ~A;"
                       "       ~{~&~8t~{~A~,^/~} ~A~,^;~}."
                       "  }"
                       "}")
                   (s-str uuid)
                   (ld-class resource)
                   (loop for (property . path)
                      in (ld-properties resource)
                      append (list path
                                   (funcall (alexandria:compose
                                             #'s-var
                                             #'string-downcase
                                             #'string)
                                            property)))))))
 
