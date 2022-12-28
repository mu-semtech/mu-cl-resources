(defpackage :sparql
  (:use :cl :mu-support)
  (:shadow :delete :insert :query)
  (:export #:*application-graph*
           #:*experimental-no-application-graph-for-sudo-select-queries*
           #:*query-log-types*
           #:with-update-group
           #:insert
           #:select
           #:delete
           #:delete-triples
           #:insert-triples
           #:update-triples
           #:query
           #:ask))

(defpackage :mu-cl-resources
  (:use :cl :mu-support)
  (:import-from #:alexandria
                #:flatten)
  (:export #:boot)
  (:local-nicknames (#:lhash #:org.shirakumo.luckless.hashtable)))
