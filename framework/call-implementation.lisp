(in-package :mu-cl-resources)

;;;;;;;;;;;;;;;;;;;;;;;;
;; authorization support
(defun session-uri ()
  "returns a session uri which can be used in a query directly."
  (if (find :docker *features*)
      (s-url (webserver:header-in* "mu-session-id"))
      (s-url "http://tst.mu.semte.ch/current-session")))

(defun build-authorization-query-string (token source &key (allow-target-inheritance t))
  "Constructs the query for authorizing a resource.
   Token and Source should be things which can be put in a query,
   either a variable, or a resource.
   @see: authorization-query, which is probably the method you'll
         want to use."
  (let ((token-var (s-genvar "tokenAssignment")))
    ;; Why don't you use a? in these queries, you ask?  Let me tell you a story
    ;; about a little kid named Virtuoso.  Virtuoso is a bit of a complex kid,
    ;; it likes to eat a lot of candy, but sometimes its brain twitches a bit.
    ;; Virtuoso can answer complex questions, but on some questions -- like most
    ;; of us -- it barfs.  It fails to answer them.  Turns out a? in combination
    ;; with a foo* portion in a path makes Virtuoso sad.  With this convoluted
    ;; piece of query, we're making Virtuoso a happy kid.  This should be
    ;; removed in the future, when https://github.com/openlink/virtuoso-opensource/issues/180
    ;; is solved.  -- 2016/03/24-15:43
    (format nil (s+ "~A session:account/^foaf:account/((a/auth:belongsToActorGroup*/auth:hasRight)|(auth:belongsToActorGroup*/auth:hasRight)) ~A. ~&"
                    "~A auth:hasToken ~A. ~&"
                    "~A auth:operatesOn/(~:[~;(^auth:belongsToArtifactGroup*/^a)|~](^auth:belongsToArtifactGroup*)) ~A. ")
            (session-uri) token-var
            token-var token
            token-var allow-target-inheritance source)))

(defgeneric authorization-query (object operation source)
  (:documentation "query which has to be matched to have authorization
   for <operation> on <object>.  <source> is the variable or url which
   is used in the query so far, to contain the resource to be validated.")
  (:method ((item-spec item-spec) operation source-variable)
    (authorization-query (resource item-spec) operation source-variable))
  (:method ((resource resource) operation source-variable)
    (alexandria:when-let ((token (authorization-token resource operation)))
      (build-authorization-query-string token source-variable)))
  (:method ((resource resource) (operation (eql :create)) source-variable)
    (declare (ignore source-variable))
    (alexandria:when-let ((token (authorization-token resource operation)))
      (build-authorization-query-string token (ld-class resource)
                                        :allow-target-inheritance nil))))

(defun check-access-rights-for-item-spec (item-spec token)
  (let ((resource-url (s-url (node-url item-spec)))
        (resource (resource item-spec))
        (uuid (uuid item-spec)))
   (alexandria:when-let ((query (authorization-query item-spec token resource-url)))
     (unless (sparql:ask query)
       (error 'access-denied
              :operation token
              :resource resource
              :id uuid
              :type (json-type resource))))))

(defun check-access-rights-for-resource (resource token)
  "Checks the access rights for <token> on <resource>.
   This has to be a class-based check.  Hence it only works for
   create at this point."
  (alexandria:when-let ((query (authorization-query resource token nil)))
    (unless (sparql:ask query)
      (error 'access-denied
             :operation token
             :resource resource
             :id nil
             :type (json-type resource)))))

(defun check-resource-base-existence (resource)
  "Checks whether an ld-resource-base is known for <resource>.  If
   it is not, no resources can be created."
  (unless (ld-resource-base resource)
    (error 'no-resource-base
           :description "Base string for resources was not found, is :resource-base set in the define-resource?")))


;;;;;;;;;;;;;;;;;;;;;;
;; call implementation

(defgeneric create-call (resource)
  (:documentation "implementation of the POST request which
    handles the creation of a resource.")
  (:method ((resource-symbol symbol))
    (create-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (check-access-rights-for-resource resource :create)
    (check-resource-base-existence resource)
    (with-cache-store
      (cache-clear-class resource)
      (let* ((jsown:*parsed-null-value* :null)
             (json-input (jsown:parse (post-body)))
             (uuid (mu-support:make-uuid)) 
             (resource-uri (format nil "~A~A"
                                   (raw-content (ld-resource-base resource))
                                   uuid))
             (item-spec (make-item-spec :uuid uuid
                                        :type (resource-name resource)
                                        :node-url resource-uri))
             (s-resource-uri (s-url resource-uri)))
        (with-surrounding-hook (:create (resource-name resource))
            (json-input item-spec)
          (sparql:insert-triples
           `((,s-resource-uri ,(s-prefix "a") ,(ld-class resource))
             (,s-resource-uri ,(s-prefix "mu:uuid") ,(s-str uuid))
             ,@(loop for (predicates object)
                  in (attribute-properties-for-json-input resource json-input)
                  unless (eq object :null)
                  collect `(,s-resource-uri ,@predicates ,object))))
          (setf (webserver:return-code*) webserver:+http-created+)
          (setf (webserver:header-out :location)
                (construct-resource-item-path item-spec))
          (when (and (jsown:keyp json-input "data")
                     (jsown:keyp (jsown:val json-input "data") "relationships"))
            (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
               if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                              "data")
               do
                 (update-resource-relation (make-item-spec :type (resource-name resource)
                                                           :uuid uuid
                                                           :node-url resource-uri)
                                           relation
                                           (jsown:filter json-input
                                                         "data" "relationships" relation "data"))))
          (jsown:new-js ("data" (retrieve-item item-spec))))))))

(defun find-resource-for-uuid-through-sparql (item-spec)
  "retrieves the resource url through a sparql query.
   @see: you probably want to use node-url instead."
  (let ((result (sparql:select (s-var "s")
                               (if *allow-xsd-in-uuids*
                                   (format nil (s+ "?s mu:uuid ?uuid. "
                                                   "FILTER(~A = str(?uuid))")
                                           (s-str (uuid item-spec)))
                                   (format nil "?s mu:uuid ~A. " (s-str (uuid item-spec)))))))
    (unless result
      (error 'no-such-instance
             :resource (resource item-spec)
             :id (uuid item-spec)
             :type (json-type (resource item-spec))))
    (jsown:filter (first result) "s" "value")))

(defparameter *uuid-uri-cache* (make-user-aware-hash-table :test #'equal)
  "Cache which connects UUIDs to URIs.")

(defun cached-uri-for-uuid (uuid)
  "Returns the cached uri for the supplied resource"
  (get-ua-hash uuid *uuid-uri-cache*))

(defun (setf cached-uri-for-uuid) (uri uuid)
  (setf (get-ua-hash uuid *uuid-uri-cache*) uri))

(defun clear-uri-cache-for-uuid (uuid)
  "Clears the URI cache for the supplied UUID"
  (rem-ua-hash uuid *uuid-uri-cache*))

(defun find-resource-for-uuid-through-cache-or-sparql (item-spec)
  "Retrieves the resource's URI from either the current cache, or
   by querying the SPRAQL endpoint."
  (or (cached-uri-for-uuid (uuid item-spec))
      (let ((uri (find-resource-for-uuid-through-sparql item-spec)))
        (setf (cached-uri-for-uuid (uuid item-spec)) uri)
        uri)))

(defgeneric node-url (item-spec)
  (:documentation "yields the node url for the supplied item-spec")
  (:method ((item-spec item-spec))
    (if (slot-boundp item-spec 'node-url)
        (slot-value item-spec 'node-url)
        (setf (slot-value item-spec 'node-url)
              (find-resource-for-uuid-through-cache-or-sparql item-spec)))))

(defgeneric update-call (resource uuid)
  (:documentation "implementation of the PUT request which
    handles the updating of a resource.")
  (:method ((resource-symbol symbol) uuid)
    (update-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let* ((item-spec (make-item-spec :type (resource-name resource) :uuid uuid))
           (jsown:*parsed-null-value* :null)
           (json-input (jsown:parse (post-body)))
           (attributes (if (jsown:keyp (jsown:val json-input "data") "attributes")
                           (jsown:filter json-input "data" "attributes")
                           (jsown:empty-object)))
           (uri (s-url (node-url item-spec))))
      (check-access-rights-for-item-spec item-spec :update)
      (with-surrounding-hook (:update (resource-name resource))
          (json-input item-spec)
        (with-cache-store
          (cache-clear-class resource)
          (cache-clear-object item-spec)
          (when (jsown:keywords attributes)
            (sparql:with-update-group
              (let ((delete-vars (loop for key in (jsown:keywords attributes)
                                    for i from 0
                                    collect (s-var (format nil "gensym~A" i)))))
                (sparql:delete-triples
                 (loop for key in (jsown:keywords attributes)
                    for slot = (resource-slot-by-json-key resource key)
                    for s-var in delete-vars
                    collect `(,uri ,@(ld-property-list slot) ,s-var))))
              (alexandria:when-let
                  ((triples-to-insert (loop for key in (jsown:keywords attributes)
                                         for slot = (resource-slot-by-json-key resource key)
                                         for json-value = (jsown:val attributes key)
                                         for value = (interpret-json-value slot json-value)
                                         for property-list = (ld-property-list slot)
                                         if (slot-value-represents-triples-p slot json-value)
                                         collect
                                           `(,uri ,@property-list ,value))))
                (sparql:insert-triples triples-to-insert))))
          (when (and (jsown:keyp json-input "data")
                     (jsown:keyp (jsown:val json-input "data") "relationships"))
            (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
               if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                              "data")
               do
                 (cache-clear-relation item-spec (find-link-by-json-name resource relation))
                 (update-resource-relation item-spec relation
                                           (jsown:filter json-input
                                                         "data" "relationships" relation "data"))))
          (respond-no-content))))))

(defgeneric update-resource-relation (item-spec relation resource-specification)
  (:documentation "updates the specified relation with the given specification.")
  (:method ((item-spec item-spec) (relation string) resource-specification)
    (update-resource-relation item-spec
                              (find-link-by-json-name (resource item-spec) relation)
                              resource-specification))
  (:method ((item-spec item-spec) (link has-one-link) resource-specification)
    (check-access-rights-for-item-spec item-spec :update)
    (reset-cache-for-resource-relation item-spec link)
    (flet ((delete-query (resource-uri link-uri)
             (sparql:delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql:insert-triples
              `((,resource-uri ,@link-uri ,new-linked-uri)))))
      (let ((linked-resource (referred-resource link))
            (resource-uri (node-url item-spec)))
        (if (and resource-specification
                 (not (eq resource-specification :null)))
            ;; update content
            (let* ((new-linked-uuid (jsown:val resource-specification "id"))
                   (new-linked-uri (node-url
                                    (make-item-spec :type (resource-name linked-resource)
                                                    :uuid new-linked-uuid))))
              (sparql:with-update-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (s-url new-linked-uri))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link))))))
  (:method ((item-spec item-spec) (link has-many-link) resource-specification)
    (check-access-rights-for-item-spec item-spec :update)
    (reset-cache-for-resource-relation item-spec link)
    (flet ((delete-query (resource-uri link-uri)
             (sparql:delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uris)
             (sparql:insert-triples
              (loop for new-link-uri in new-linked-uris
                 collect
                   `(,resource-uri ,@link-uri ,new-link-uri)))))
      (let ((linked-resource (referred-resource link))
            (resource-uri (node-url item-spec)))
        (if (and resource-specification
                 (not (eq resource-specification :null)))
            ;; update content
            (let* ((new-linked-uuids (jsown:filter resource-specification map "id"))
                   (new-linked-resources (loop for uuid in new-linked-uuids
                                            for spec = (make-item-spec :type linked-resource
                                                                       :uuid uuid)
                                            collect (node-url spec))))
              (sparql:with-update-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (mapcar #'s-url new-linked-resources))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link)))))))

(defun cache-list-call (resource)
  "Performs the caching of a list call.
   We've split this off so we can shortcut the case where a
   findMany call is received.  This results in caching at most
   the identifiers which are listed in that filter.
   These needn't be recalculated when any resource is returned,
   only when any of those resources is returned."
  (let ((id-filter (find "filter[id]" (webserver:get-parameters*) :test #'string= :key #'car)))
    (if id-filter
        (dolist (uuid (split-sequence:split-sequence #\, (cdr id-filter)))
          (cache-object (make-item-spec :uuid uuid :type (resource-name resource))))
        (cache-class resource))))

(defun self-for-list-call (resource)
  "Constructs the self url for the list call for <resource>."
  (build-url (request-path resource)
             (alist-to-plist (webserver:get-parameters*))))

(defgeneric list-call (resource)
  (:documentation "implementation of the GET request which
   handles listing the whole resource")
  (:method ((resource-symbol symbol))
    (list-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (with-surrounding-hook (:list (resource-name resource))
        (resource)
      (with-cache-store
        (cache-list-call resource)
        (paginated-collection-response
         :resource resource
         :self (self-for-list-call resource)
         :sparql-body (filter-body-for-search
                       :sparql-body  (format nil "?s mu:uuid ?uuid; a ~A. ~@[~A~]"
                                             (ld-class resource)
                                             (authorization-query resource :show (s-var "s")))
                       :source-variable (s-var "s")
                       :resource resource)
         :source-variable (s-var "s"))))))

(defgeneric show-call (resource uuid)
  (:documentation "implementation of the GET request which
    handles the displaying of a single resource.")
  (:method ((resource-symbol symbol) uuid)
    (show-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let ((item-spec (make-item-spec :uuid uuid :type (resource-name resource))))
      (check-access-rights-for-item-spec item-spec :show)
      (with-surrounding-hook (:show (resource-name resource))
          (item-spec)
        (with-cache-store
          (cache-object item-spec)
          (multiple-value-bind (data included-items)
              (retrieve-item item-spec)
            (if (eq data :null)
                (error 'no-such-instance
                       :resource resource
                       :id uuid
                       :type (json-type resource))
                (let ((response
                       (jsown:new-js
                         ("data" data)
                         ("links" (jsown:new-js
                                    ("self" (construct-resource-item-path item-spec)))))))
                  (when included-items
                    (setf (jsown:val response "included") included-items))
                  response))))))))

(defun sparse-fields-for-resource (item-spec)
  "Returns the sparse fieldsets for the supplied item-spec.
   Returns two values, the first being the attributes to return,
   the second being non-nil iff the fields for this item-spec
   were returned."
  (let ((spec (webserver:get-parameter
               (format nil "fields[~A]"
                       (json-type (resource item-spec))))))
    (values (and spec
                 (loop for key in (cl-ppcre:split "," spec)
                    collect
                      (or (resource-slot-by-json-key (resource item-spec) key)
                          (find-link-by-json-name (resource item-spec) key))))
            (and spec t))))

(defclass solution ()
  ((fields :initform (make-hash-table :test 'equal #-abcl :synchronized #-abcl t)
           :reader solution-fields))
  (:documentation "Represents a generic solution object, coming
   from various backends."))

(defun solution-value (solution property)
  "Returns the value for <property> of <solution>, in which <property>
   is the json type.  The second value is truethy iff the solution was
   retrieved from the triplestore at some point."
  (gethash property (solution-fields solution)))

(defun (setf solution-value) (value solution property)
  "Sets a found solution for the given property"
  (setf (gethash property (solution-fields solution)) value))

(defun solution-field-p (solution property)
  "returns non-nil if <property> has been fetched for <solution>."
  (second (multiple-value-list (solution-value solution property))))

(defgeneric complete-solution (solution item-spec)
  (:documentation "Completes <solution> for the settings requested in
    <item-spec>.")
  (:method ((solution solution) (item-spec item-spec))
    (multiple-value-bind (requested-fields sparse-fields-p)
        (sparse-fields-for-resource item-spec)
      (flet ((field-requested-p (field)
               (or (not sparse-fields-p)
                  (find field requested-fields))))
        (let* ((resource (resource item-spec))
               (resource-url
                ;; Fetch URL separately as that speeds up Virtuoso
                ;; querying.
                (node-url item-spec))
               (missing-properties (remove-if-not
                                    (lambda (slot)
                                      (and (field-requested-p slot)
                                         (not (solution-field-p solution (json-property-name slot)))))
                                    (ld-properties resource)))
               (missing-single-value-properties
                (remove-if-not #'single-value-slot-p missing-properties))
               (missing-multi-value-properties
                (remove-if-not #'multi-value-slot-p missing-properties))
               (query-solution
                ;; simple attributes
                (first
                 (sparql:select
                  "*"
                  (format nil
                          "~{~&~:[OPTIONAL {~A ~{~A~,^/~} ~A.}~;~A ~{~A~,^/~} ~A.~]~}"
                          (loop for slot in missing-single-value-properties
                             append (list (required-p slot)
                                          (s-url resource-url)
                                          (ld-property-list slot)
                                          (s-var (sparql-variable-name slot)))))))))
          ;; read simple attributes from sparql query
          (loop for slot in missing-single-value-properties
             for sparql-var = (sparql-variable-name slot)
             for json-var = (json-property-name slot)
             do
               (setf (solution-value solution json-var)
                     (and (jsown:keyp query-solution sparql-var)
                        (from-sparql (jsown:val query-solution sparql-var)
                                     (resource-type slot)))))
          ;; read extended variables through separate sparql query
          (loop for slot in missing-multi-value-properties
             for variable-name = (sparql-variable-name slot)
             for json-var = (json-property-name slot)
             do
               (let ((value (mapcar (lambda (query-solution)
                                      (jsown:val query-solution variable-name))
                                    (sparql:select "*"
                                                   (format nil "~A ~{~A~,^/~} ~A."
                                                           (s-url resource-url)
                                                           (ld-property-list slot)
                                                           (s-var variable-name))))))
                 (setf (solution-value solution json-var)
                       (from-sparql value (resource-type slot)))))
          solution)))))

(defparameter *cached-resources* (make-user-aware-hash-table :test 'equal)
  "Cache of solutions which were previously fetched or initialized.
   The resources might not be complete yet, and can be finished.
   The keys are the UUIDs the vaue is the cached resource.")

(defun ensure-solution (item-spec)
  "Ensures a solution exists for <item-spec> and returns it."
  (if *cache-model-properties-p*
      (or (get-ua-hash (uuid item-spec) *cached-resources*)
         (setf (get-ua-hash (uuid item-spec) *cached-resources*)
               (make-instance 'solution)))
      (make-instance 'solution)))

(defgeneric clear-solution (spec)
  (:documentation "Clears the solution from the given specification
   accepts both an item-spec as well as a uuid")
  (:method ((item-spec item-spec))
    (rem-ua-hash (uuid item-spec) *cached-resources*)
    (clear-uri-cache-for-uuid (uuid item-spec)))
  (:method ((uuid string))
    (rem-ua-hash uuid *cached-resources*)
    (clear-uri-cache-for-uuid uuid)))

(defun item-spec-to-jsown (item-spec)
  "Returns the jsown representation of the attributes and
   non-filled relationships of item-spec.  This is the default
   way of fetching the database contents of a single item."
  (handler-bind
      ((no-such-instance (lambda () :null)))
    (multiple-value-bind (requested-fields sparse-fields-p)
        (sparse-fields-for-resource item-spec)
      (flet ((field-requested-p (field)
               (or (not sparse-fields-p)
                  (find field requested-fields))))
        (let ((solution (ensure-solution item-spec))
              (resource (resource item-spec))
              (attributes (jsown:empty-object)))
          ;; ensure solution is complete
          (let ((unavailable-fields
                 (remove-if-not (lambda (slot)
                                  (and (field-requested-p slot)
                                     (not (solution-field-p solution (json-property-name slot)))))
                                (ld-properties (resource item-spec)))))
            (if unavailable-fields
                (complete-solution solution item-spec)
                (format t "Using cached solution for ~A" (uuid item-spec))))
          ;; read attributes from the solution
          (loop for property in (ld-properties resource)
             for sparql-var = (sparql-variable-name property)
             for json-var = (json-property-name property)
             if (and (field-requested-p property)
                   (solution-value solution json-var))
             do
               (setf (jsown:val attributes json-var)
                     (solution-value solution json-var)))
          ;; attach uri if feature is enabled (after variables were parsed)
          (when (find 'include-uri (features resource))
            (setf (jsown:val attributes "uri") (node-url item-spec)))
          ;; build response data object
          (let ((relationships-object (jsown:empty-object)))
            (loop for link in (all-links resource)
               if (field-requested-p link)
               do
                 (setf (jsown:val relationships-object (json-key link))
                       (build-relationships-object item-spec link)))
            (jsown:new-js
              ("attributes" attributes)
              ("id" (uuid item-spec))
              ("type" (json-type resource))
              ("relationships" relationships-object))))))))

(defun retrieve-item (item-spec)
  "Returns (values item-json included-items)
   item-json contains the description of the specified item with
     necessary links from <included>."
  (handler-bind
      ((no-such-instance (lambda () :null)))
    (multiple-value-bind (data-item-specs included-item-specs)
        (augment-data-with-attached-info
         (list item-spec))
      (values (item-spec-to-jsown (first data-item-specs))
              (mapcar #'item-spec-to-jsown included-item-specs)))))

(defgeneric build-relationships-object (item-spec link)
  (:documentation "Returns the content of one of the relationships based
   on the type of relation, and whether or not the relationship should
   be inlined.  Values to inline should be included directly.")
  ;; TODO: remove duplication
  (:method ((item-spec item-spec) (link has-one-link))
    (let ((links-object (build-links-object item-spec link)))
      (multiple-value-bind (included-items included-items-p)
          (related-items item-spec link)
        (if included-items-p
            (jsown:new-js ("links" links-object)
                          ("data" (if included-items
                                      (jsown-inline-item-spec (first included-items))
                                      :null)))
            (jsown:new-js ("links" links-object))))))
  (:method ((item-spec item-spec) (link has-link))
    (let ((links-object (build-links-object item-spec link)))
      (multiple-value-bind (included-items included-items-p)
          (related-items item-spec link)
        (if included-items-p
            (jsown:new-js ("links" links-object)
                          ("data" (mapcar #'jsown-inline-item-spec included-items)))
            (jsown:new-js ("links" links-object)))))))

(defgeneric jsown-inline-item-spec (item-spec)
  (:documentation "Yields the inline id/type to indicate a particular
   resource")
  (:method ((item-spec item-spec))
    (jsown:new-js ("type" (json-type (resource item-spec)))
                  ("id" (uuid item-spec)))))

  ;; TODO probably not used anymore
(defgeneric build-data-object-for-included-relation (link items)
  (:documentation "Builds the data object for an included relation.
   This object contains the references to the relationship.
   <items> should be a list of item-spec instances.")
  (:method ((link has-one-link) (items (eql nil)))
    :null)
  (:method ((link has-one-link) items)
    (jsown-inline-item-spec (first items)))
  (:method ((link has-many-link) items)
    (mapcar #'jsown-inline-item-spec items)))

(defgeneric build-links-object (item-spec link)
  (:documentation "Builds the json object which represents the link
    in a json object.")
  (:method ((item-spec item-spec) (link has-link))
    (let ((resource (resource item-spec))
          (uuid (uuid item-spec)))
      (jsown:new-js ("self" (build-url (format nil "/~A/~A/links/~A"
                                               (request-path resource)
                                               uuid
                                               (request-path link))
                                       (alist-to-plist (webserver:get-parameters*))))
                    ("related" (format nil "/~A/~A/~A"
                                       (request-path resource)
                                       uuid
                                       (request-path link)))))))

(defgeneric delete-call (resource uuid)
  (:documentation "implementation of the DELETE request which
   handles the deletion of a single resource")
  (:method ((resource-symbol symbol) uuid)
    (delete-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let ((item-spec (make-item-spec :type (resource-name resource)
                                     :uuid uuid)))
      (check-access-rights-for-item-spec item-spec :delete)
      (with-surrounding-hook (:delete (resource-name resource))
          (item-spec)
        (with-cache-store
          (cache-clear-class resource)
          (cache-clear-object item-spec)
          (let (relation-content)
            (loop for slot in (ld-properties resource)
               do (push (list (ld-property-list slot)
                              (s-var (sparql-variable-name slot)))
                        relation-content))
            (loop for link in (all-links resource)
               do (push (list (ld-property-list link)
                              (s-var (sparql-variable-name link)))
                        relation-content))
            (setf relation-content (reverse relation-content))
            (sparql:delete
                (apply #'concatenate 'string
                       (loop for triple-clause
                          in
                            `((,(s-var "s") ,(s-prefix "mu:uuid") ,(s-str uuid))
                              (,(s-var "s") ,(s-prefix "a") ,(ld-class resource))
                              ,@(loop for (property-list value) in relation-content
                                   collect `(,(s-var "s") ,@property-list ,value)))
                          for (subject predicate object) = triple-clause
                          collect (if (s-inv-p predicate)
                                      (format nil "~4t~A ~A ~A.~%"
                                              object (s-inv predicate) subject)
                                      (format nil "~4t~A ~A ~A.~%"
                                              subject predicate object))))
                (concatenate 'string
                             (format nil "~{~&~4t~{~A ~A ~A~}.~%~}"
                                     `((,(s-var "s") ,(s-prefix "mu:uuid") ,(s-str uuid))
                                       (,(s-var "s") ,(s-prefix "a") ,(ld-class resource))))
                             (format nil "~{~&~4tOPTIONAL {~{~A ~A ~A~}.}~%~}"
                                     (loop for (property-list value) in relation-content
                                        if (s-inv-p (first property-list))
                                        collect `(,value ,(s-inv (first property-list)) ,(s-var "s"))
                                        else
                                        collect `(,(s-var "s") ,(first property-list) ,value)))))))
        (respond-no-content)))))

(defgeneric show-relation-call (resource id link)
  (:documentation "implementation of the GET request which handles
    the listing of a relation.")
  (:method ((resource-symbol symbol) id link)
    (show-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (let ((item-spec (make-item-spec :type (resource-name resource) :uuid id)))
      (check-access-rights-for-item-spec item-spec :show)
      (with-surrounding-hook (:show-relation (resource-name resource))
          (item-spec link)
        (with-cache-store
          (cache-relation item-spec link)
          (let ((relation-item-spec (first (retrieve-relation-items item-spec link))))
            (when relation-item-spec
              (cache-object relation-item-spec))
            (jsown:new-js
              ("data" (if relation-item-spec
                          (retrieve-item relation-item-spec)
                          :null))
              ("links" (build-links-object item-spec link))))))))
  (:method ((resource resource) id (link has-many-link))
    (let* ((item-spec (make-item-spec :type (resource-name resource) :uuid id))
           (resource-url (s-url (node-url item-spec))))
      (with-surrounding-hook (:show-relation (resource-name resource))
          (item-spec link)
        (with-cache-store
          (cache-relation item-spec link)
          (let ((link-spec (make-item-spec :type (resource-name resource) :uuid id)))
            (multiple-value-bind (response data-item-specs)
                (paginated-collection-response
                 :resource (referred-resource link)
                 :sparql-body (filter-body-for-search
                               :sparql-body (format nil
                                                    (s+ "~A ~{~A~,^/~} ?resource. "
                                                        "?resource mu:uuid ?uuid. "
                                                        "~@[~A~] ")
                                                    resource-url
                                                    (ld-property-list link)
                                                    (authorization-query resource :show resource-url))
                               :source-variable (s-var "resource")
                               :resource (referred-resource link))
                 :source-variable (s-var "resource")
                 :link-spec link-spec
                 :link-defaults (build-links-object link-spec link))
            (if (find 'no-pagination-defaults (features resource))
                (dolist (spec data-item-specs)
                  (cache-object spec))
                (cache-class (referred-resource link)))
            response)))))))

(defgeneric retrieve-relation-items (item-spec link)
  (:documentation "retrieves the item descriptions of the items
    which are connected to <resource> <id> through link <link>.
    This yields the high-level description of the items, not
    their contents.
    Note that this method does not support pagination.")
  (:method ((item-spec item-spec) (link string))
    (retrieve-relation-items item-spec (find-link-by-json-name (resource item-spec) link)))
  (:method ((item-spec item-spec) (link has-one-link))
    (let* ((resource-url (s-url (node-url item-spec)))
           (query-results
            (first (sparql:select (s-var "uuid")
                                  (format nil (s+ "~A ~{~A~,^/~} ?resource. "
                                                  "?resource mu:uuid ?uuid. "
                                                  "~@[~A~] ")
                                          resource-url
                                          (ld-property-list link)
                                          (authorization-query item-spec :show resource-url)))))
           (linked-resource (resource-name (referred-resource link))))
      (and query-results
           (list
            (make-item-spec :type linked-resource
                            :uuid (jsown:filter query-results "uuid" "value"))))))
  (:method ((item-spec item-spec) (link has-many-link))
    (let* ((resource-url (s-url (node-url item-spec)))
           (query-results
            (sparql:select (s-var "uuid")
                           (format nil (s+ "~A ~{~A~,^/~} ?resource. "
                                           "?resource mu:uuid ?uuid. "
                                           "~@[~A~] ")
                                   resource-url
                                   (ld-property-list link)
                                   (authorization-query item-spec :show resource-url))))
           (linked-resource (resource-name (referred-resource link))))
      (loop for uuid in (jsown:filter query-results map "uuid" "value")
         collect
           (make-item-spec :type linked-resource :uuid uuid)))))

(defgeneric patch-relation-call (resource id link)
  (:documentation "implementation of the PATCH request which
    handles the updating of a relation.")
  (:method ((resource-symbol symbol) id link)
    (patch-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (flet ((delete-query (resource-uri link-uri)
             (sparql:delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql:insert-triples
              `((,resource-uri ,@link-uri ,new-linked-uri)))))
      (let ((item-spec (make-item-spec :type (resource-name resource)
                                       :uuid id)))
        (check-access-rights-for-item-spec item-spec :update)
        (with-cache-store
          (cache-clear-class resource)
          (cache-clear-relation item-spec link)
          (let ((body (jsown:parse (post-body)))
                (linked-resource (referred-resource link))
                (resource-uri (node-url item-spec))
                (link-path (ld-property-list link)))
            (with-surrounding-hook (:update-relation (resource-name resource))
                (item-spec link body)
              (if (jsown:val body "data")
                  ;; update content
                  (let* ((new-linked-uuid (jsown:filter body "data" "id"))
                         (new-linked-uri (node-url
                                          (make-item-spec :type (resource-name linked-resource)
                                                          :uuid new-linked-uuid))))
                    (sparql:with-update-group
                        (delete-query (s-url resource-uri) link-path)
                      (insert-query (s-url resource-uri) link-path
                                    (s-url new-linked-uri))))
                  ;; delete content
                  (delete-query (s-url resource-uri) link-path))
              (respond-no-content)))))))
  (:method ((resource resource) id (link has-many-link))
    (let ((item-spec (make-item-spec :type (resource-name resource) :uuid id)))
      (check-access-rights-for-item-spec item-spec :update)
      (with-cache-store
        (cache-clear-class resource)
        (cache-clear-relation item-spec link)
        (flet ((delete-query (resource-uri link-uri)
                 (sparql:delete-triples
                  `((,resource-uri ,@link-uri ,(s-var "s")))))
               (insert-query (resource-uri link-uri new-linked-uris)
                 (sparql:insert-triples
                  (loop for new-uri in new-linked-uris
                     collect `(,resource-uri ,@link-uri ,new-uri)))))
          (let ((body (jsown:parse (post-body)))
                (linked-resource (referred-resource link))
                (resource-uri (node-url item-spec))
                (link-path (ld-property-list link)))
            (with-surrounding-hook (:update-relation (resource-name resource))
                (item-spec link body)
              (if (jsown:val body "data")
                  ;; update content
                  (let* ((new-linked-uuids (jsown:filter body "data" map "id"))
                         (new-linked-resources (loop for uuid in new-linked-uuids
                                                  for spec = (make-item-spec :type linked-resource
                                                                             :uuid uuid)
                                                  collect
                                                    (node-url spec))))
                    (delete-query (s-url resource-uri) link-path)
                    (insert-query (s-url resource-uri)
                                  link-path
                                  (mapcar #'s-url new-linked-resources)))
                  ;; delete content
                  (delete-query (s-url resource-uri)
                                link-path))
              (respond-no-content))))))))

(defgeneric delete-relation-call (resource id link)
  (:documentation "Performs a delete call on a relation, thereby
    removing a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let ((item-spec (make-item-spec :type (resource-name resource)
                                     :uuid id))
          (body (jsown:parse (post-body))))
      (check-access-rights-for-item-spec item-spec :update)
      (with-cache-store
        (cache-clear-class resource)
        (cache-clear-relation item-spec link)
        (with-surrounding-hook (:delete-relation (resource-name resource))
            (item-spec body)
          (let* ((linked-resource (referred-resource link))
                 (resources (loop for uuid in
                                 (remove-if-not #'identity (jsown:filter body "data" map "id"))
                               for spec = (make-item-spec :type linked-resource
                                                          :uuid uuid)
                               collect (node-url spec))))
            (when resources
              (sparql:delete-triples
               (loop for resource in resources
                  collect
                    `(,(s-url (node-url item-spec))
                       ,@(ld-property-list link)
                       ,resource)))))
          (respond-no-content))))))

(defgeneric add-relation-call (resource id link)
  (:documentation "Performs the addition call on a relation, thereby
    adding a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let ((item-spec (make-item-spec :type (resource-name resource)
                                     :uuid id))
          (body (jsown:parse (post-body))))
      (check-access-rights-for-item-spec item-spec :update)
      (with-cache-store
        (cache-clear-class resource)
        (cache-clear-relation item-spec link)
        (with-surrounding-hook (:add-relation (resource-name resource))
            (resource item-spec body)
          (let* ((linked-resource (referred-resource link))
                 (resources (loop for uuid
                               in
                                 (remove-if-not #'identity (jsown:filter body "data" map "id"))
                               collect
                                 (node-url
                                  (make-item-spec :uuid uuid
                                                  :type (resource-name linked-resource))))))
            (when resources
              (let ((source-url (node-url item-spec))
                    (properties (ld-property-list link)))
                (sparql:insert-triples
                 (loop for resource in resources
                    collect
                      `(,(s-url source-url) ,@properties ,(s-url resource)))))))
          (respond-no-content))))))


;;;;
;; support for 'included'
;;
;; - Objects which are to be included follow the following structure:
;;   > (list :type 'catalog :id 56E6925A193F022772000001)
;; - relation-spec follows the following structure (books.author):
;;   > (list "books" "author")

(defun augment-data-with-attached-info (item-specs)
  "Augments the current item-specs with extra information on which
   attached items to include in the relationships.
   Returns (values data-item-specs included-item-specs).
   data-item-specs: the current items of the main data portion.
   included-item-specs: items in the included portion of the
   response."
  (let ((included-items-store (make-included-items-store-from-list item-specs)))
    (dolist (included-spec (extract-included-from-request))
      (include-items-for-included included-items-store item-specs included-spec))
    (let ((items (included-items-store-list-items included-items-store)))
      (values (loop for item in items
                 if (find item item-specs)
                 collect item)
              (loop for item in items
                 unless (find item item-specs)
                 collect item)))))

(defun include-items-for-included (included-items-store item-specs included-spec)
  "Traverses the included-spec with the items in item-specs and ensures
   they're recursively included.  The item-specs also get to know which
   items have to be added."
  (declare (special *cache-store*))
  (let ((lparallel:*kernel* (lparallel:make-kernel
                             8 :bindings `((*standard-output* . ,*standard-output*)
                                           (*error-output* . ,*error-output*)
                                           (*resources* . ,*resources*)
                                           (*cache-store* . ,*cache-store*)
                                           (*included-items-store* . ,included-items-store)
                                           (hunchentoot:*request* . ,hunchentoot:*request*)
                                           (hunchentoot:*reply* . ,hunchentoot:*reply*))))
        (lparallel:*debug-tasks-p* nil))
    (unwind-protect
         (lparallel:pmap 'list
                         (lambda (item)
                           (let (linked-items)
                             ;; fill in current path
                             (setf linked-items
                                   (union linked-items
                                          (include-items-for-single-included item (first included-spec))))
                             ;; traverse included-spec path
                             (when (rest included-spec)
                               (include-items-for-included included-items-store linked-items
                                                           (rest included-spec)))))
                         item-specs)
      (lparallel:end-kernel))))

(defun include-items-for-single-included (item-spec relation-string)
  (declare (special *included-items-store*))
  (let* ((included-items-store *included-items-store*)
         (resource (resource item-spec))
         (uuid (uuid item-spec))
         (relation (find-resource-link-by-json-key resource relation-string))
         (target-type (resource-name relation))
         (related-objects
          (loop for new-uuid
             in (jsown:filter
                 (sparql:select (s-distinct (s-var "target"))
                                (format nil (s+ "?s mu:uuid ~A. "
                                                "?s ~{~A/~}mu:uuid ?target. "
                                                "~@[~A~] ")
                                        (s-str uuid)
                                        (ld-property-list relation)
                                        (authorization-query resource :show (s-var "s"))))
                 map "target" "value")
             collect (included-items-store-ensure included-items-store
                                                  (make-item-spec :uuid new-uuid
                                                                  :type target-type)))))
    (setf (gethash relation (related-items-table item-spec))
          related-objects)
    (cache-relation item-spec relation)
    (dolist (item-spec related-objects)
      (cache-object item-spec))
    related-objects))


(defun extract-included-from-request ()
  "Extracts the filters from the request.  The result is a list
   containing the :components and :search key.  The :components
   key includes a left-to-right specification of the strings
   between brackets.  The :search contains the content for that
   specification."
  (let ((include-parameter
         (assoc "include" (webserver:get-parameters*) :test #'string=)))
    (and include-parameter
         (mapcar (alexandria:curry #'split-sequence:split-sequence #\.)
                 (split-sequence:split-sequence #\, (cdr include-parameter))))))
