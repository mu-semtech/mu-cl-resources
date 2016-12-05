(in-package :mu-cl-resources)

(defun read-domain-json-file (file)
  "Imports contents from the json file specified by
   file."
  (funcall (alexandria:compose
            #'read-domain-json-string
            #'alexandria:read-file-into-string)
           file))

(defun read-domain-json-string (string)
  "Imports the json string as a domain file"
  (funcall (alexandria:compose
            #'import-domain-from-jsown
            #'jsown:parse)
           string))

(defun import-domain-from-jsown (js-domain)
  "Imports the domain from the jsown file"
  (dolist (resource js-domain)
    (import-jsown-domain-resource resource)))

(defun import-jsown-domain-resource (resource)
  (let ((relationships (mapcar #'import-jsown-domain-relationship
                               (jsown:val resource "relationships"))))
    (make-instance 'resource
                   :resource-name (intern (string-upcase (jsown:val resource "resource")))
                   :ld-class (read-uri-from-json (jsown:val resource "class"))
                   :ld-properties (mapcar #'import-jsown-domain-property
                                          (jsown:val resource "properties"))
                   :ld-resource-base (jsown:val resource "newResourceBase")
                   :has-many (remove-if-not (lambda (relationship) (typep relationship 'has-many-link))
                                            relationships)
                   :has-one (remove-if-not (lambda (relationship) (typep relationship 'has-one-link))
                                           relationships)
                   :json-type (jsown:val resource "path")
                   :request-path (jsown:val resource "path")
                   :authorization nil
                   :features (jsown:val resource "features"))))

(defun read-uri-from-json (value)
  "Reads a URI as specified in the JSON format.  Value
   is the parsed jsown variant, hence it can be either
   a string or a jsown object with keys \"type\" and
   \"value\"."
  (if (stringp value)
      (parse-simple-uri-reference value)
      (cond ((string= (jsown:val value "type") "prefix")
             (s-prefix value))
            ((string= (jsown:val value "type") "url")
             (s-url value))
            (t (error "Type of uri reference should be \"prefix\" or \"url\" but got \"~A\" instead."
                      (jsown:val value "type"))))))

(defun parse-simple-uri-reference (value)
  "Parses a simple uri reference.  This allows you to type
   something with :// in it which will be assumed to be a
   URL, and something without to be assumed to be a prefix."
  (if (search "://" value)
      (s-url value)
      (s-prefix value)))

(defun import-jsown-domain-relationship (jsown-relationship)
  "Imports a single belongs-to or has-many relationship from jsown."
  (let ((type (if (string= "singular" (jsown:val jsown-relationship "cardinality"))
                  'has-one-link
                  'has-many-link)))
    (make-instance type
                   :via (read-uri-from-json (jsown:val jsown-relationship "predicate"))
                   :as (jsown:val jsown-relationship "path")
                   :inverse (and (jsown:keyp jsown-relationship "inverse")
                               (jsown:val jsown-relationship "inverse"))
                   :resource (intern (string-upcase (jsown:val jsown-relationship "resource"))))))

(defun import-jsown-domain-property (jsown-property)
  "Imports a single domain property from the jsown format."
  (make-instance 'resource-slot
                 :json-key (intern (string-upcase (jsown:val jsown-property "path")))
                 :resource-type (intern (string-upcase (jsown:val jsown-property "type")) :keyword)
                 :ld-property (parse-simple-uri-reference (jsown:val jsown-property "predicate"))))
