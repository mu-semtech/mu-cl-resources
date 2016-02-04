(in-package :sparql)


(defparameter *query-group* nil
  "Describes a group of queries to be executed in one go. The
   queries are executed when the query-group is ended.  This is a
   special variable which is set by with-grouped-queries")

(defmacro with-query-group (&body body)
  "Starts a new query-group.  The queries are executed when the
   group exits."
  `(let ((*query-group* (cons nil nil)))
     ,@body
     (let ((queries (apply #'s+ (reverse (butlast *query-group*)))))
       (fuseki:with-query-logging *error-output*
         (fuseki:query *repository* queries)))))

(defun sparql-query (content)
  "Executes a sparql query on the current repository, or pushes
   it on the set of queries to be executed in the query-group
   based on the current context.

   NOTE: see sparql-select, sparql-insert and sparql-delete for
         functions geared towards end-users."
  (if *query-group*
      (push content *query-group*)
      (fuseki:with-query-logging *error-output*
        (fuseki:query *repository* content))))

(defun select (variables body &rest args &key order-by limit offset)
  "Executes a SPARQL SELECT query on the current graph.
   Takes with-query-group into account."
  (declare (ignore order-by limit offset))
  (sparql-query
   (s-select variables args
             (s-graph *application-graph* body))))

(defun sparql-select (&rest args)
  (apply #'select args))

(defun insert (body)
  "Executes a SPARQL INSERT DATA query on the current graph.
   Takes with-query-group into account."
  (sparql-query
   (s-insert
    (s-graph *application-graph* body))))

(defun sparql-insert (&rest args)
  (apply #'insert args))

(defun insert-triples (triple-clauses)
  "Inserts a set of triples based on the provided triple-clauses.
   Provide a pattern containing triple patterns and variables as
   per 's-var."
  (let ((patterns
         (loop for triple-clause in triple-clauses
            for (subject predicate object) = triple-clause
            collect
              (if (s-inv-p predicate)
                  (format nil "~4t~A ~A ~A.~%"
                          object (s-inv predicate) subject)
                  (format nil "~4t~A ~A ~A.~%"
                          subject predicate object)))))
    (sparql-insert (apply #'concatenate 'string patterns))))

(defun sparql-insert-triples (&rest args)
  (apply #'insert-triples args))

(defun delete (clauses &optional where)
  "Executes a SPARQL DELETE query on the current graph.
   Takes with-query-group into account."
  (let ((clauses (s-graph *application-graph* clauses))
        (where (when where
                 (s-graph *application-graph* where))))
    (sparql-query
     (s-delete clauses where))))

(defun sparql-delete (&rest args)
  (apply #'delete args))

(defun delete-triples (triple-clauses)
  "Deletes a set of triples based on the provided triple-clauses.
   Provide a pattern containing triple patterns and variables as
   per 's-var."
  (let ((patterns
         (loop for triple-clause in triple-clauses
            for (subject predicate object) = triple-clause
            collect
              (if (s-inv-p predicate)
                  (format nil "~4t~A ~A ~A.~%"
                          object (s-inv predicate) subject)
                  (format nil "~4t~A ~A ~A.~%"
                          subject predicate object)))))
    (sparql-delete (apply #'concatenate 'string patterns)
                   (apply #'concatenate 'string patterns))))

(defun sparql-delete-triples (&rest args)
  (apply #'delete-triples args))
