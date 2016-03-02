(in-package :mu-cl-resources)


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
