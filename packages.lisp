(defpackage :sparql
  (:use :cl :mu-support)
  (:shadow :delete :insert)
  (:export #:with-query-group
           #:sparql-select
           #:sparql-insert
           #:sparql-insert-triples
           #:sparql-delete
           #:sparql-delete-triples
           #:insert
           #:select
           #:delete-triples
           #:delete
           #:insert-triples
           #:sparql-query
           #:*application-graph*))

(defpackage :mu-cl-resources
  (:use :cl)
  (:use :mu-support))
