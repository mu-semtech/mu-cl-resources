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

(defparameter *default-page-size*
  (handler-case
      (parse-integer (uiop:getenv "MU_DEFAULT_PAGE_SIZE"))
    (error () 20))
  "default amount of items in a single page of results.")

(defparameter *supply-cache-headers-p* nil
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
