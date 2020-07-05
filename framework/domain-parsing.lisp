(in-package :mu-cl-resources)

;;; external interface

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


;;; top-level processing

(defun read-domain-json-file-from-path (file)
  "Imports contents from the json file specified by
   file."
  (funcall (alexandria:compose
            #'read-domain-json-string
            #'alexandria:read-file-into-string)
           file))

(defun read-domain-json-string (string)
  "Imports the json string as a domain file"
  (let ((parsed-content (jsown:parse string)))
    (import-prefixes-from-jsown parsed-content)
    (import-domain-from-jsown parsed-content)))


;;; support for prefixes

(defun import-prefixes-from-jsown (jsown-config)
  "Imports the domain from the jsown file"
  (let ((version (jsown:val jsown-config "version")))
    (cond
      ((string= version "0.1")
       (if (jsown:keyp jsown-config "prefixes")
           (map-jsown-object
            (jsown:val jsown-config "prefixes")
            (lambda (key value)
              (add-prefix key value)))
           (warn "Did not find \"prefixes\" key in json domain file")))
      (t (warn "Don't know version ~A for prefixes definition, skipping."
               version)))))


;;; importing of domain

(defun import-domain-from-jsown (js-domain)
  "Imports the domain from the jsown file"
  (let ((version (jsown:val js-domain "version")))
    (cond
      ((string= version "0.1")
       (if (jsown:keyp js-domain "paths")
           (map-jsown-object (jsown:val js-domain "paths")
                             #'import-jsown-domain-resource)
           (warn "Could not find path in domain specification.")))
      (t (warn "Don't know version ~A for resources definition, skipping."
               version)))))

(defun import-jsown-domain-resource (path resource-description)
  (let ((resource-name                  ; default to primary key
         (intern (string-upcase (or (jsown:val-safe resource-description "name")
                                    path))))
        (path                           ; default to primary key
         (or (jsown:val-safe resource-description "path")
             path))
        (properties (jsown:val-safe resource-description "attributes"))
        (relationships (map-jsown-object
                        (jsown:val-safe resource-description "relationships")
                        #'import-jsown-relationship))
        (class (jsown:val resource-description "class"))
        (resource-base (jsown:val resource-description "new-resource-base"))
        (features (mapcar (lambda (feature)
                            (intern (string-upcase feature)))
                          (jsown:val-safe resource-description "features"))))
    (define-resource* (intern (string-upcase resource-name) :mu-cl-resources)
        :ld-class (read-uri-from-json class)
        :ld-properties (map-jsown-object properties
                                         #'import-jsown-domain-property)
        :has-many (loop for (type . relationship) in relationships
                     when (eq type 'has-many)
                     collect relationship)
        :has-one (loop for (type . relationship) in relationships
                    when (eq type 'has-one)
                    collect relationship)
        :ld-resource-base (s-url resource-base)
        :on-path path
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

(defun import-jsown-relationship (relationship-path jsown-relationship)
  "Imports a single jsown relationship.

Emits a cons cell with the kind 'has-one or 'has-many and a list
defining the relationship."
  (let ((kind
         (cond ((string= "one" (string-downcase (jsown:val jsown-relationship "cardinality")))
                'has-one)
               ((string= "many" (string-downcase (jsown:val jsown-relationship "cardinality")))
                'has-many)
               (t (error "Did not recognize cardinality of relationships ~A, should be either \"one\" or \"many\"."
                         (jsown:val jsown-relationship "cardinality"))))))
    (cons kind
          (list
           (intern (string-upcase (jsown:val jsown-relationship "target")))
           :via (read-uri-from-json (jsown:val jsown-relationship "predicate"))
           :inverse (jsown:val-safe jsown-relationship "inverse")
           :as relationship-path))))

(defun import-jsown-domain-property (property-path jsown-property)
  "Imports a single domain property from the jsown format."
  (list
   (intern (string-upcase property-path) :keyword)
   (intern (string-upcase (jsown:val jsown-property "type")) :keyword)
   (parse-simple-uri-reference (jsown:val jsown-property "predicate"))))


;;; helpers

(defun map-jsown-object (object functor)
  "Maps the jsown object by looping over each of the keys, and
   calling functor with the key and the contents of the key as
   its arguments."
  (loop for key in (jsown:keywords object)
        for value = (jsown:val object key)
        collect (funcall functor key value)))
