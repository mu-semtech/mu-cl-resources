(in-package :sparql)


(defparameter *query-update-group* nil
  "Describes a group of queries to be executed in one go. The
   queries are executed when the query-update-group is ended. 
   This is a special variable which is set by 
   with-grouped-queries.")

(defun query-log-stream (&optional (type :default))
  "Returns the stream to which errors should be logged."
  (when (find type *query-log-types*)
    *error-output*))

(defmacro with-update-group (&body body)
  "Starts a new query-group.  The queries are executed when the
   group exits."
  `(let ((*query-update-group* (cons nil nil)))
     ,@body
     (let ((queries (format nil "~{~A~^; ~%~}" (reverse (butlast *query-update-group*)))))
       (fuseki:with-query-logging (query-log-stream :update-group)
         (fuseki:update *repository* queries)))))

(defun update (content)
	"Executes a sparql update on the current repository, or pushes
   it on the set of queries to be executed in the query-group
   based on the current context.

   NOTE: see sparql:insert and sparql:delete for functions
         geared towards end-users."
	(if *query-update-group*
			(push content *query-update-group*)
			(fuseki:with-query-logging (query-log-stream :update)
				(fuseki:update *repository* content))))

(defun query (content)
  "Executes a sparql query on the current repository, or pushes
   it on the set of queries to be executed in the query-group
   based on the current context.

   NOTE: see sparql:select, sparql:insert and sparql:delete for
         functions geared towards end-users."
  (if *query-update-group*
      (push content *query-update-group*)
      (fuseki:with-query-logging (query-log-stream :query)
        (fuseki:query *repository* content))))

(defun ask (body)
  "Executes a SPARQL ASK query on the current graph."
  (fuseki:with-query-logging (query-log-stream :ask)
    (fuseki:ask *repository* (format nil "ASK WHERE { ~A }" body))))

(defun select (variables body &rest args &key order-by limit offset group-by)
  "Executes a SPARQL SELECT query on the current graph.
   Takes with-query-group into account."
  (declare (ignore order-by limit offset group-by))
  (query
   (s-select variables args
             (s-graph *application-graph* body))))

(defun insert (body)
  "Executes a SPARQL INSERT DATA query on the current graph.
   Takes with-query-group into account."
  (update
   (s-insert
    (s-graph *application-graph* body))))

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
    (insert (apply #'concatenate 'string patterns))))

(defun delete (clauses &optional where)
  "Executes a SPARQL DELETE query on the current graph.
   Takes with-query-group into account."
  (let ((clauses (s-graph *application-graph* clauses))
        (where (when where
                 (s-graph *application-graph* where))))
    (update
     (s-delete clauses where))))

(defun format-triple-pattern-clause (triple-pattern-clause)
  "Formats a triple pattern clause as specified in the clauses for
   delete-triples."
  (destructuring-bind (subject predicate object)
      triple-pattern-clause
    (if (s-inv-p predicate)
        (format nil "~A ~A ~A."
                object (s-inv predicate) subject)
        (format nil "~A ~A ~A."
                subject predicate object))))

(defun delete-triples (triple-clauses)
  "Deletes a set of triples based on the provided triple-clauses.
   Provide a pattern containing triple patterns and variables as
   per 's-var.
   If a triple pattern isn't available, the whole deletion will
   not stop working."
  (let ((patterns (mapcar #'format-triple-pattern-clause triple-clauses)))
    (delete (apply #'concatenate 'string patterns)
        (format nil "~{~4tOPTIONAL { ~A }~%~}" patterns))))
