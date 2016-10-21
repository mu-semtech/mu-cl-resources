(defpackage :sparql
  (:use :cl :mu-support)
  (:shadow :delete :insert :query)
  (:export #:*application-graph*
           #:*query-log-types*
           #:with-update-group
           #:insert
           #:select
           #:delete
           #:delete-triples
           #:insert-triples
           #:query
           #:ask))

(defpackage :mu-cl-resources
  (:use :cl :mu-support)
  (:export #:boot))
