(in-package :mu-cl-resources)

(defparameter *allow-xsd-in-uuids* nil
  "when non-nil, allow uuids to have the xsd:string type attached when reading.")

(defparameter *camelcase-json-variables* nil
  "when non-nil, json variable names should be camelcased, rather than dasherized.")
(defparameter *verify-accept-header* nil
  "when non-nil, the application/vndi+json ACCEPT header is checked.")
(defparameter *verify-content-type-header* t
  "when non-nil, the application/vndi+json CONTENT-TYPE header is checked.")

(defparameter sparql:*application-graph*
  (s-url (or (uiop:getenv "MU_APPLICATION_GRAPH")
             "http://mu.semte.ch/application"))
  "standard graph for all sparql queries.")

(defparameter cl-fuseki::*query-log-stream* *standard-output*)

(defparameter sparql:*query-log-types* '(:default :update-group :update :query :ask)
  "If truethy, queries will be logged to *error-output*")

(defparameter dex:*default-read-timeout* 120
  "How long to wait for database response in seconds.")

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

(defparameter *default-language-import-fallback* "en"
  "EXPERIMENTAL: this variable defines the default string to be used when trying to
  import a language-typed string on a database that doesn't have the
  language-typed-string set.")

(defparameter *cache-model-properties-p* nil
  "Set this to t in order to cache query solutions")

(defparameter *cache-count-queries-p* nil
  "when non-nil, all count queries are cached like other resources")

(defparameter *max-group-sorted-properties* t
  "when non-nil, all properties to sort on will be fetched as a MAX
  operation.  This ensures that always the same property is sorted on
  should there be multiple values in the result.  This is
  configurable, as some triplestores seemingly don't handle this case
  correctly and return results which aren't correctly sorted.  Expect
  this option to be removed in the distant future.")

(defparameter *cache-clear-path* (uiop:getenv "CACHE_CLEAR_PATH")
  "when set, received delta's will be cleared on the supplied path.  a
  POST update is sent to the given path with the clear-keys header
  set.")

(defparameter *log-delta-clear-keys* nil
  "when non-nil, the clear-keys sent to the cache based on received
  delta messages will be logged in the console.")

(defparameter *max-optionals-per-query* nil
  "set this to a number to indicate the maximum amount of OPTIONAL
  statements to be used when an item is being fetched partially.  not
  consumed in all queries, use will be extended when necessary.  this is
  used as a maxmium amount of OPTIONALs, effective amount may be lower.")

(defparameter *include-at-least-one-non-optional* nil
  "when non-nil, we should aim to inject one known statement when
  constructing a query.  not consumed in all queries, use will be
  extended when necessary.")

(defparameter sparql:*experimental-no-application-graph-for-sudo-select-queries* nil
  "when non-nil no application graph will be set when sending out sudo
  queries, thus resulting in queries across the full database.")

(defparameter *soft-max-triples-in-property-construct* 250
  "Soft maximum on the amount of triples returned by construct for properties.")

(defparameter *soft-max-sources-in-property-construct* 50
  "Soft maximum on the amount of source items used in construct for properties.")

(defparameter *fetch-all-types-in-construct-queries* nil
  "When non-nil the CONSTRUCT queries will fetch all types rather than only
the possibly relevant types.  Fetching all types may result in a more
expensive query.

This setting should not have an impact on datasets which are fully correct.")
