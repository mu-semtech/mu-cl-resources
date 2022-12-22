(in-package :mu-cl-resources)

;;;;
;; support for 'included'
;;
;; Fetches content from the triplestore based on the current request

(defparameter *max-included-depth* 15
  "How many levels included is allowed.")

(defun make-tree (lists &key (test #'eql) (depth 0))
  "Constructs a tree from a set of lists, joining where possible.

The tree can have multiple nodes on each level.  Each level is therefore
a list of CONS cells of which the CAR is the key and the CDR is another
tree (and thus a list of CONS cells).  If there are no further elements
in the tree, it has NIL as CDR."
  (when (>= depth *max-included-depth*)
    (return-from make-tree))
  (setf lists (remove-if-not #'identity lists))
  (let (discovered-level)             ; list of (key . (options)) trees
    (loop for (first . rest) in lists
          for existing-tree = (find first discovered-level :key #'car :test test)
          if existing-tree
            do (push rest (cdr existing-tree))
          else
            do (push (cons first (list rest)) discovered-level))
    (loop for (key . options) in discovered-level
          if options
            collect (cons key (make-tree options :test test :depth (1+ depth)))
          else
            collect (cons key nil))))

(defun group-by (list &key (key #'identity) (test #'eql))
  "Groups a list of items based on key.  Yields a list of CONS cells in
which the CAR is the key and the CDR is the list of items matching KEY."
  (let (discovered-groups)
    (loop for item in list
          for group-key = (funcall key item)
          for discovered-group = (find group-key discovered-groups :key #'car :test test)
          if discovered-group
            do (push item (cdr discovered-group))
          else
            do (push (cons group-key (list item)) discovered-groups))
    discovered-groups))

(defclass sparql-construct-portion ()
  ()
  (:documentation "A portion of a SPARQL query"))

(defclass sparql-values-statement (sparql-construct-portion)
  ((variable :accessor values-statement-variable :initarg :variable)
   (values :accessor values-statement-values :initarg :values))
  (:documentation "Represents a variable having multiple values."))

(defclass sparql-union (sparql-construct-portion)
  ((options :accessor sparql-union-options :initarg :options))
  (:documentation "Represents a union statement in a SPARQL query."))

(defclass sparql-statements (sparql-construct-portion)
  ((statements :accessor sparql-statements :initarg :statements))
  (:documentation "A set of SPARQL statements, like UNION or VARIABLE."))

(defclass sparql-triple-match (sparql-construct-portion)
  ((subject :accessor sparql-subject :initarg :subject)
   (predicate :accessor sparql-predicate :initarg :predicate)
   (object :accessor sparql-object :initarg :object))
  (:documentation "A single s p o statement in a query."))

(defgeneric print-for-construct-where (object stream)
  (:documentation "Prints content for the WHERE part of a CONSTRUCT query.")
  (:method ((match sparql-triple-match) stream)
    (print-object match stream))
  (:method ((statements sparql-statements) stream)
    (print-object statements stream))
  (:method ((union sparql-union) stream)
    (print-object union stream))
  (:method ((statement sparql-values-statement) stream)
    (print-object statement stream)))

(defgeneric print-for-construct-template (object stream)
  (:documentation "Prints content for the template part of a CONSTRUCT query.")
  (:method ((match sparql-triple-match) stream)
    (print-object match stream))
  (:method ((statements sparql-statements) stream)
    ;; ignore the union bit but print the children
    (format stream "~{~A~,^ ~}" (mapcar (alexandria:rcurry #'print-for-construct-template stream)
                                        (sparql-statements statements))))
  (:method ((union sparql-union) stream)
    ;; ignore the union bit but print the children
    (format stream "~{~A~,^ ~}" (mapcar (alexandria:rcurry #'print-for-construct-template stream)
                                        (sparql-union-options union))))
  (:method ((statement sparql-values-statement) stream)
    ;; no output
    (format stream "")))

(defun print-union-query (union stream)
  (cl-fuseki::query-update-prefixes
   (format nil "CONSTRUCT {~%~A~%} WHERE {~%~A~%}"
           (print-for-construct-template union stream)
           (print-for-construct-where union stream))))

(defmethod print-object ((statement sparql-values-statement) stream)
  (format stream "~&VALUES ~A {~{~A~,^ ~}}."
          (values-statement-variable statement)
          (values-statement-values statement)))
(defmethod print-object ((union sparql-union) stream)
  (format stream "~{~&{ ~A }~,^~& UNION ~}"
          (sparql-union-options union)))
(defmethod print-object ((statements sparql-statements) stream)
  (format stream "~{~A~,^ ~}" (sparql-statements statements)))
(defmethod print-object ((match sparql-triple-match) stream)
  (format stream "~&~T~A ~A ~A.~%"
          (sparql-subject match)
          (sparql-predicate match)
          (sparql-object match)))

(defun construct-set-of-trees-for-included ()
  (break "making tree")
  (make-tree (extract-included-from-request) :test #'string=))
(defun group-item-specs-by-resource (item-specs)
  (group-by item-specs
            :key #'resource
            :test #'eq))
(defun all-resources-of-itemspecs (item-specs)
  "Yields all the resources for the supplied item-specs."
  (loop for item-spec in item-specs
        for resource = (resource item-spec)
        for result = (list resource)
          then (pushnew resource result)
        finally (return result)))
(defun group-resources-by-relationship-constraint (resources relation-json-key)
  "Groups all resources by their relationship constraint and returns
results as a list of '(relationship-constraint . resources)."
  ;; We should go through each of the resources and find the
  ;; relationship for the supplied json key.  Based on the predicate +
  ;; direction, and the resulting resource, these resources can be
  ;; joined.
  (group-by resources
            :key (lambda (resource) (find-link-by-json-name resource relation-json-key))
            :test (lambda (left-relation right-relation)
                    (or (eq left-relation right-relation)
                        (and (string= (resource-name left-relation) (resource-name right-relation))
                             (string= (princ-to-string (ld-link left-relation))
                                      (princ-to-string (ld-link right-relation)))
                             (eq (inverse-p left-relation) (inverse-p right-relation)))))))

(defmacro do-subtrees ((json-key-var subtrees-var) trees-var &body body)
  "Loops over each of the subtrees."
  `(loop for (,json-key-var . ,subtrees-var) in ,trees-var
         collect (progn ,@body)))
(defmacro append-subtrees-into-union ((json-key-var subtrees-var) trees-var &body body)
  "Like DO-SUBTREES but collects the results into a UNION."
  `(make-instance 'sparql-union
                  :options (flatten (do-subtrees (,json-key-var ,subtrees-var) ,trees-var ,@body))))
(defmacro with-all-resources-of-item-specs ((resources-var) item-specs-var &body body)
  "Executes BODY for each unique resource for ITEM-SPECS-VAR binding RESOURCES-VAR to the resource."
  `(let ((,resources-var (all-resources-of-itemspecs ,item-specs-var)))
     ,@body))
(defmacro do-resources-grouped-by-relationship-constraint ((relationship-var originating-resources-var target-resource-var) (relation-json-key-var source-resources-var) &body body)
  `(loop for (,relationship-var . ,originating-resources-var)
           in (group-resources-by-relationship-constraint ,source-resources-var ,relation-json-key-var)
         for ,target-resource-var = (find-resource-by-name (resource-name ,relationship-var))
         collect (progn ,@body)))
(defmacro with-item-specs-for-resources ((target-item-specs-var) (source-item-specs originating-resources) &body body)
  (let ((originating-resources-var (gensym "ORIGINATING-RESOURCES-"))
        (source-item-specs-var (gensym "SOURCE-ITEM-SPECS-")))
    `(let* ((,originating-resources-var ,originating-resources)
            (,source-item-specs-var ,source-item-specs)
            (,target-item-specs-var (remove-if-not (lambda (resource) (find resource ,originating-resources-var))
                                                   ,source-item-specs-var
                                                   :key #'resource)))
       ,@body)))

(defun make-values-statement-for-item-specs (item-specs &optional (variable (s-var "source")))
  "Constructs a SPARQL-VALUES construction for this set of item-specs."
  (make-instance 'sparql-values-statement
                 :variable variable
                 :values (mapcar (alexandria:compose #'s-url #'node-url) item-specs)))

(defun make-values-statement-for-uris (ld-classes variable)
  "Construct a VALUES statement to bind VARIABLE to each of LD-CLASSES."
  (make-instance 'sparql-values-statement
                 :variable variable
                 :values ld-classes))

(defun make-sparql-triple-match (subject predicate object)
  "Short form to create instance of 'SPARQL-TRIPLE-MATCH"
  (make-instance 'sparql-triple-match
                 :subject subject
                 :predicate predicate
                 :object object))

(defun make-relationship-constraints-statement (variable relationship &optional (target-var (s-genvar)))
  ;; construct a set of constraints for
  (let ((type-var (s-genvar "typeof"))
        (ld-subclasses (ld-subclasses (find-resource-by-name (resource-name relationship)))))
    (values
     (make-instance 'sparql-statements
                    :statements (list (make-sparql-triple-match variable
                                                                (format nil "~{~A~,^/~}"
                                                                        (mapcar #'s-url (expanded-ld-relation relationship)))
                                                                target-var)
                                      ;; make values statement for the classes
                                      (make-values-statement-for-uris ld-subclasses type-var)
                                      ;; use values statement for the type
                                      (make-sparql-triple-match target-var
                                                                (s-url "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                                                                type-var)))
     target-var)))

(defmacro with-resource-and-subtype-resources ((resources-var) source-resource-var &body body)
  `(let ((,resources-var (subclass-resources ,source-resource-var)))
     ,@body))

(defun augment-data-with-attached-info (item-specs)
  "Augments the current item-specs with extra information on which
   attached items to include in the relationships.
   Returns (values data-item-specs included-item-specs).
   data-item-specs: the current items of the main data portion.
   included-item-specs: items in the included portion of the
   response."
  (break "Augmenting data")
  ;; 1. get the core information
  (let (;; 1.1 construct a set of trees for the included key
        (included-tree (construct-set-of-trees-for-included))
        ;; ;; 1.2 group item-specs by their most specific type
        ;; (grouped-item-specs (group-item-specs-by-resource item-specs))
        )
    ;; 2. we walk over each of the plausible type definitions so we can
    ;; filter the relationships.  each differing result needs to be
    ;; split and follow a separate path.  note that the type definitions
    ;; for the first level are exactly known and are therefore a list of
    ;; 1 element.

    ;; Let's assume each sub-tree has its own property constraints
    (let ((output
            (append-subtrees-into-union (relation-json-key subtrees) included-tree
              (with-all-resources-of-item-specs (source-resources) item-specs
                (do-resources-grouped-by-relationship-constraint (relationship originating-resources target-resource) (relation-json-key source-resources)
                  (break "executing with resources grouped by relationship constraint")
                  (with-item-specs-for-resources (item-specs) (item-specs originating-resources)
                    (break "with item specs")
                    (let* ((originating-variable (s-var "source"))
                           (values-statement (make-values-statement-for-item-specs item-specs originating-variable))
                           (target-variable (s-genvar))
                           (results-statement (make-relationship-constraints-statement originating-variable relationship target-variable)))
                      (push values-statement (sparql-statements results-statement))
                      (expand-subtrees results-statement subtrees target-variable target-resource)
                      (break "Expanded subtree")
                      results-statement)))))))
      (format t "~&~%%== TREE OUTPUT ==~%~%~A~&" (print-union-query output nil))
      ;; In order to execute this, we need to convert it into only the
      ;; triple statements (no values clauses or unions) and a where
      ;; clause with everything.

      ;; Once we have that and we have the resulting data, we can start
      ;; constructing the included matches as was done before by
      ;; creating item-spec things with a wide range of properties.

      ;; A substantial improvement may be to fetch the UUID too so we
      ;; can construct a full item-spec.
      output)))

(defun expand-subtrees (results-statement subtrees variable target-resource)
  "Expands the subtrees into the given results-statement."

                      (break "Expanded subtree")
  (let ((new-union
          (append-subtrees-into-union (relation-json-key current-subtrees) subtrees
            (break "Processing key ~A for tree ~A => ~A" relation-json-key subtrees current-subtrees)
            (with-resource-and-subtype-resources (resources) target-resource
              (do-resources-grouped-by-relationship-constraint (relationship originating-resources target-resource) (relation-json-key resources)
                (let* ((target-variable (s-genvar))
                       (new-results-statement (make-relationship-constraints-statement variable relationship target-variable)))
                  (expand-subtrees new-results-statement current-subtrees target-variable target-resource)
                  new-results-statement))))))
    (alexandria:appendf (sparql-statements results-statement)
                        (list new-union))))





;; (defun augment-data-with-attached-info (item-specs)
;;   "Augments the current item-specs with extra information on which
;;    attached items to include in the relationships.
;;    Returns (values data-item-specs included-item-specs).
;;    data-item-specs: the current items of the main data portion.
;;    included-item-specs: items in the included portion of the
;;    response."
;;   (let ((included-items-store (make-included-items-store-from-list item-specs)))
;;     (dolist (included-spec (extract-included-from-request))
;;       (include-items-for-included included-items-store item-specs included-spec))
;;     (let ((items (included-items-store-list-items included-items-store)))
;;       (values (loop for item in items
;;                  if (find item item-specs)
;;                  collect item)
;;               (loop for item in items
;;                  unless (find item item-specs)
;;                  collect item)))))

;; (defun include-items-for-included (included-items-store item-specs included-spec)
;;   "Traverses the included-spec with the items in item-specs and ensures
;;    they're recursively included.  The item-specs also get to know which
;;    items have to be added."
;;   (declare (special *cache-store*))
;;   (let ((lparallel:*kernel* (lparallel:make-kernel
;;                              8 :bindings `((*standard-output* . ,*standard-output*)
;;                                            (*error-output* . ,*error-output*)
;;                                            (*resources* . ,*resources*)
;;                                            (*cache-store* . ,*cache-store*)
;;                                            (*included-items-store* . ,included-items-store)
;;                                            (hunchentoot:*catch-errors-p* . ,hunchentoot:*catch-errors-p*)
;;                                            (hunchentoot:*request* . ,hunchentoot:*request*)
;;                                            (hunchentoot:*reply* . ,hunchentoot:*reply*))))
;;         (lparallel:*debug-tasks-p* nil))
;;     (unwind-protect
;;          (lparallel:pmap 'list
;;                          (lambda (item)
;;                            (let (linked-items)
;;                              ;; fill in current path
;;                              (setf linked-items
;;                                    (union linked-items
;;                                           (include-items-for-single-included item (first included-spec))))
;;                              ;; traverse included-spec path
;;                              (when (rest included-spec)
;;                                (include-items-for-included included-items-store linked-items
;;                                                            (rest included-spec)))))
;;                          item-specs)
;;       (lparallel:end-kernel))))

;; (defun include-items-for-single-included (item-spec relation-string)
;;   (declare (special *included-items-store*))
;;   (let* ((included-items-store *included-items-store*)
;;          (resource (resource item-spec))
;;          (uuid (uuid item-spec))
;;          (relation (find-resource-link-by-json-key resource relation-string))
;;          (target-type (resource-name relation))
;;          (related-objects
;;           (loop for new-uuid
;;              in (jsown:filter
;;                  (sparql:select (s-distinct (s-var "target"))
;;                                 (format nil (s+ "?s mu:uuid ~A. "
;;                                                 "?s ~{~A/~}mu:uuid ?target. "
;;                                                 "~@[~A~] ")
;;                                         (s-str uuid)
;;                                         (ld-property-list relation)
;;                                         (authorization-query resource :show (s-var "s"))))
;;                  map "target" "value")
;;              collect (included-items-store-ensure included-items-store
;;                                                   (make-item-spec :uuid new-uuid
;;                                                                   :type target-type)))))
;;     (setf (gethash relation (related-items-table item-spec))
;;           related-objects)
;;     (cache-relation item-spec relation)
;;     (dolist (item-spec related-objects)
;;       (cache-object item-spec))
;;     related-objects))


(defun extract-included-from-request (&optional (include-parameter (cdr (assoc "include" (webserver:get-parameters*) :test #'string=))))
  "Extracts the filters from the request.  The result is a list
   containing the :components and :search key.  The :components
   key includes a left-to-right specification of the strings
   between brackets.  The :search contains the content for that
   specification."
  (and include-parameter
       (mapcar (alexandria:curry #'split-sequence:split-sequence #\.)
               (split-sequence:split-sequence #\, include-parameter))))

