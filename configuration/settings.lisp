(in-package :mu-cl-resources)

(defparameter *allow-xsd-in-uuids* nil
  "when non-nil, allow uuids to have the xsd:string type attached when reading.")

(defparameter *camelcase-json-variables* nil
  "when non-nil, json variable names should be camelcased, rather than dasherized.")
(defparameter *verify-accept-header* nil
  "when non-nil, the application/vndi+json ACCEPT header is checked.")

(defparameter sparql:*application-graph*
  (s-url (or (uiop:getenv "MU_APPLICATION_GRAPH")
             "http://mu.semte.ch/application"))
  "standard graph for all sparql queries.")

(defparameter sparql:*query-log-types* '(:default :update-group :update :query :ask)
  "If truethy, queries will be logged to *error-output*")

(defparameter *default-page-size*
  (handler-case
      (parse-integer (uiop:getenv "MU_DEFAULT_PAGE_SIZE"))
    (error () 20))
  "default amount of items in a single page of results.")

(defparameter *include-count-in-paginated-responses* nil
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")

(defparameter *supply-cache-headers-p* nil
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")

(defparameter *declare-resource-types-p*
  (let ((env (uiop:getenv "MU_DECLARE_RESOURCE_TYPES")))
    (and env (or (equal env "true") (equal env "TRUE") (equal env "True") (equal env T))))
  "when true, explicitly declare rdf:type for all resources in property and relation queries.")
