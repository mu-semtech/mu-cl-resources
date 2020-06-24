(in-package :mu-cl-resources)

(defun read-domain-file (relative-path)
  "Reads the JSON file from a relative path."
  (let ((type (pathname-type relative-path))
        (pathname (asdf:system-relative-pathname
                   :mu-cl-resources
                   (s+ "configuration/" relative-path))))
    (cond ((or (string= type "js")
              (string= type "json"))
           (read-domain-json-file-from-path pathname))
          ((string= type "lisp")
           (load pathname)))))

(defun read-domain-json-file-from-path (file)
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

(defun map-jsown-object (object functor)
  "Maps the jsown object by looping over each of the keys, and
   calling functor with the key and the contents of the key as
   its arguments."
  (loop for key in (jsown:keywords object)
     for value = (jsown:val object key)
     collect (funcall functor key value)))

(defun import-domain-from-jsown (js-domain)
  "Imports the domain from the jsown file"
  (let ((version (jsown:val js-domain "version")))
    (cond
      ((string= version "0.1")
       (map-jsown-object (jsown:val js-domain "resources")
                         #'import-jsown-domain-resource))
      (t (error "Don't know version ~A of domain" version)))))

(defun import-jsown-domain-resource (resource-name resource-description)
  (let ((properties (jsown:val-safe resource-description "properties"))
        (relationships (map-jsown-object (jsown:val-safe resource-description "relationships")
                                         #'import-jsown-domain-relationship))
        (path (jsown:val resource-description "path"))
        (class (jsown:val resource-description "class"))
        (resource-base (jsown:val resource-description "newResourceBase"))
        (features (mapcar (lambda (feature)
                            (intern (string-upcase feature)))
                          (jsown:val-safe resource-description "features"))))
    (make-instance 'resource
                   :resource-name (intern (string-upcase resource-name))
                   :ld-class (read-uri-from-json class)
                   :ld-properties (map-jsown-object properties
                                                    #'import-jsown-domain-property)
                   :ld-resource-base resource-base
                   :has-many (remove-if-not (lambda (relationship) (typep relationship 'has-many-link))
                                            relationships)
                   :has-one (remove-if-not (lambda (relationship) (typep relationship 'has-one-link))
                                           relationships)
                   :json-type path
                   :request-path path
                   :authorization nil
                   :features features)))

(defun read-uri-from-json (value)
  "Reads a URI as specified in the JSON format.  Value
   is the parsed jsown variant, hence it can be either
   a string or a jsown object with keys \"type\" and
   \"value\"."
  (if (stringp value)
      (parse-simple-uri-reference value)
      (cond ((string= (jsown:val-safe value "type") "prefix")
             (s-prefix value))
            ((string= (jsown:val-safe value "type") "url")
             (s-url value))
            (t (error "Type of uri reference should be \"prefix\" or \"url\" but got \"~A\" instead."
                      (jsown:val-safe value "type"))))))

(defun parse-simple-uri-reference (value)
  "Parses a simple uri reference.  This allows you to type
   something with :// in it which will be assumed to be a
   URL, and something without to be assumed to be a prefix."
  (if (search "://" value)
      (s-url value)
      (s-prefix value)))

(defun import-jsown-domain-relationship (relationship-path jsown-relationship)
  "Imports a single belongs-to or has-many relationship from jsown."
  (let ((type (if (string= "one" (string-downcase (jsown:val jsown-relationship "cardinality")))
                  'has-one-link
                  'has-many-link)))
    (make-instance type
                   :via (read-uri-from-json (jsown:val jsown-relationship "predicate"))
                   :as relationship-path
                   :inverse (jsown:val-safe jsown-relationship "inverse")
                   :resource (intern (string-upcase (jsown:val jsown-relationship "resource"))))))

(defun import-jsown-domain-property (property-path jsown-property)
  "Imports a single domain property from the jsown format."
  (make-instance 'resource-slot
                 :json-key (intern (string-upcase property-path))
                 :resource-type (intern (string-upcase (jsown:val jsown-property "type")) :keyword)
                 :ld-property (parse-simple-uri-reference (jsown:val jsown-property "predicate"))))
