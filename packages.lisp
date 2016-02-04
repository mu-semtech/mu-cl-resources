(defpackage :sparql
  (:use :cl :mu-support)
  (:shadow :delete :insert :query)
  (:export #:*application-graph*
           #:with-query-group
           #:insert
           #:select
           #:delete
           #:delete-triples
           #:insert-triples
           #:query))

(defpackage :mu-cl-resources
  (:use :cl :mu-support)
  (:export #:boot))
