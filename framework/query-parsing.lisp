(in-package :mu-cl-resources)


(defgeneric from-sparql (object resource-type)
  (:documentation "Converts the supplied sparql value specification into a lisp value.")
  (:method (values (resource-type (eql :language-string-set)))
    (loop for value in values
       collect (from-sparql value :language-string)))
  (:method (values (resource-type (eql :uri-set)))
    (loop for value in values
       collect (from-sparql value :uri)))
  (:method (values (resource-type (eql :string-set)))
    (loop for value in values
       collect (from-sparql value :string)))
  (:method (object resource-type)
    (let ((type
            ;; in case keyword is a literal but we also have a datatype, we
            ;; want this to be a typed literal
            (if (jsown:keyp object "datatype")
                :typed-literal
                (intern (string-upcase (jsown:val object "type"))
                        :keyword)))
          (value (jsown:val object "value")))
      (import-value-from-sparql-result type resource-type value object))))

(defgeneric import-value-from-sparql-result (type resource-type value object)
  (:documentation "imports the value from 'object' given its 'value'
   and 'type' to dispatch on.")
  (:method ((type (eql :uri)) resource-type value object)
    (declare (ignore object resource-type))
    value)
  (:method ((type (eql :literal)) (resource-type (eql :language-string)) value object)
    (let ((language (or (and (jsown:keyp object "xml:lang")
                             (jsown:val object "xml:lang"))
                        *default-language-import-fallback*)))
      (jsown:new-js
        ("content" value)
        ("language" language))))
  (:method ((type (eql :literal)) resource-type value object)
    (declare (ignore resource-type object))
    value)
  (:method ((type (eql :typed-literal)) resource-type value object)
    (import-typed-literal-value-from-sparql-result
     (jsown:val object "datatype")
     value
     object)))

(defparameter *typed-literal-importers* (lhash:make-castable :test 'equal)
  "contains all converters for typed-literal values coming from the database.")

(defmacro define-typed-literal-importer (type (&rest variables) &body body)
  "defines a new typed literal importer.  should receive value, object
   as variables."
  `(setf (lhash:gethash ,type *typed-literal-importers*)
         (lambda (,@variables)
           ,@body)))

(defun import-typed-literal-value-from-sparql-result (type value object)
  "imports a typed-literal-value from a sparql result."
  (funcall (lhash:gethash type *typed-literal-importers*)
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

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#double"
    (value object)
  (declare (ignore object))
  (read-from-string value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#integer"
    (value object)
  (declare (ignore object))
  (parse-integer value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#int"
    (value object)
  (declare (ignore object))
  (parse-integer value))

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#dateTime"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#time"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#date"
    (value object)
  (declare (ignore object))
  value)

(define-typed-literal-importer "http://www.opengis.net/ont/geosparql#wktLiteral"
    (value object)
  (declare (ignore object))
  value)
;; virtuoso hack
(define-typed-literal-importer "http://www.openlinksw.com/schemas/virtrdf#Geometry"
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

(define-typed-literal-importer "http://www.w3.org/2001/XMLSchema#gYear"
    (value object)
  (declare (ignore object))
  (format nil "~A" (parse-integer (cl-ppcre:scan-to-strings "-?\\d+" value))))
