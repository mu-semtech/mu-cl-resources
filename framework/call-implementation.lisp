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
          ;; INVARIANT01: other code assumes all relevant information
          ;; regarding classes and URIs is available in the triplestore
          ;; OR is available through a cache.  The following two lines
          ;; ensures this happens.  Because this is a create, we assume
          ;; no one talks about this same UUID and CLASS.
          (setf (classes-for-uri resource-uri) (ld-superclasses resource))
          (setf (cached-uri-for-uuid uuid) resource-uri)
          (setf (cached-uuid-for-uri resource-uri) uuid)
          (cache-clear-class resource)
          (sparql:insert-triples
           `((,s-resource-uri ,(s-prefix "mu:uuid") ,(s-str uuid))
             ;; types
             ,@(loop for ld-class in (flattened-ld-class-tree resource)
                     collect `(,s-resource-uri ,(s-prefix "a") ,ld-class))
             ;; predicates
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
                  (update-resource-relation item-spec
                                            resource
                                            relation
                                            (jsown:filter json-input
                                                          "data" "relationships" relation "data"))))
          (jsown:new-js ("data" (retrieve-item item-spec resource))))))))


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
          (when (jsown:keywords attributes)
            (sparql:with-update-group
              (let ((delete-vars (loop for key in (jsown:keywords attributes)
                                       collect (s-genvar key))))
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
                 (update-resource-relation item-spec
                                           (resource item-spec)
                                           relation
                                           (jsown:filter json-input
                                                         "data" "relationships" relation "data"))))
          ;; clear caches after updating the database
          ;; relations have been cleared in update-resource-relation
          (cache-clear-class (resource item-spec))
          (cache-clear-object item-spec)
          (respond-no-content))))))

(labels ((delete-query (resource-uri link-uri)
           (sparql:delete-triples
            `((,resource-uri ,@link-uri ,(s-var "s")))))
         (insert-query (resource-uri link-uri new-linked-uris)
           (sparql:insert-triples
            (loop for new-link-uri in new-linked-uris
                  collect
                  `(,resource-uri ,@link-uri ,new-link-uri))))
         (clear-inverse-has-one-relationships (new-item-spec link)
           "Remove inverses if they are defined on the target with a has-one relation to remove stale data"
           (let ((target-resource-super-tree (flattened-class-tree (resource new-item-spec))))
             (loop for inverse-link-spec in (inverse-links link)
                   for inverse-link = (getf inverse-link-spec :link)
                   when (typep inverse-link 'has-one-link)
                     do
                        (loop for target-super in target-resource-super-tree
                              when (find inverse-link (direct-has-one-links target-super))
                                do
                                   (delete-query (s-url (node-url new-item-spec)) (ld-property-list inverse-link)))))))
  (defgeneric update-resource-relation (item-spec source-resource relation resource-specification)
    (:documentation "updates the specified relation with the given specification.")
    (:method ((item-spec item-spec) source-resource (relation string) resource-specification)
      (update-resource-relation item-spec
                                source-resource
                                (find-link-by-json-name (resource item-spec) relation)
                                resource-specification))
    (:method ((item-spec item-spec) (source-resource resource) (link has-one-link) resource-specification)
      (check-access-rights-for-item-spec item-spec :update)
      (let ((linked-resource (referred-resource link))
            (resource-uri (node-url item-spec)))
        (if (and resource-specification
                 (not (eq resource-specification :null)))
            ;; update content
            (let* ((new-linked-uuid (jsown:val resource-specification "id"))
                   (new-linked-item-spec (make-item-spec :type (resource-name linked-resource)
                                                         :uuid new-linked-uuid))
                   (new-linked-uri (node-url new-linked-item-spec)))
              (sparql:with-update-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (clear-inverse-has-one-relationships new-linked-item-spec link)
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (list (s-url new-linked-uri)))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link))))
      ;; reset the cache after updating the triplestore
      (cache-clear-relation source-resource link))
    (:method ((item-spec item-spec) (source-resource resource) (link has-many-link) resource-specification)
      (check-access-rights-for-item-spec item-spec :update)
      (let ((linked-resource (referred-resource link))
            (resource-uri (node-url item-spec)))
        (if (and resource-specification
                 (not (eq resource-specification :null)))
            ;; update content
            (let* ((new-linked-uuids (jsown:filter resource-specification map "id"))
                   (new-linked-item-specs (loop for uuid in new-linked-uuids
                                                collect (make-item-spec :type (resource-name linked-resource)
                                                                        :uuid uuid))))
              (sparql:with-update-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (dolist (new-linked-item-spec new-linked-item-specs)
                  (clear-inverse-has-one-relationships new-linked-item-spec link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (mapcar (alexandria:compose #'s-url #'node-url) new-linked-item-specs))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link))))
      ;; reset the cache after updating the triplestore
      (cache-clear-relation source-resource link))))

(defun cache-list-call (resource)
  "Performs the caching of a list call.
   We've split this off so we can shortcut the case where a
   findMany call is received.  This results in caching at most
   the identifiers which are listed in that filter.
   These needn't be recalculated when any resource is returned,
   only when any of those resources is returned."
  (flet ((get-parameter-key (key)
           (rest (find key (webserver:get-parameters*) :test #'string= :key #'car))))
    (cond ((or (get-parameter-key "filter[id]")
               (get-parameter-key "filter[:id:]"))
           (dolist (uuid (split-sequence:split-sequence
                          #\,
                          (or (get-parameter-key "filter[id]")
                              (get-parameter-key "filter[:id:]"))))
             (cache-object (make-item-spec :uuid uuid :type (resource-name resource)))))
          ((get-parameter-key "filter[:uri:]")
           (cache-object (make-item-spec :node-url (get-parameter-key "filter[:uri:]"))))
          (t (cache-class resource)))))

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
                       :sparql-body  (format nil "?s mu:uuid ?uuid; a ?class. VALUES ?class {~{~A~^ ~}}. ~@[~A~]"
                                             (ld-subclasses resource)
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
              (retrieve-item item-spec resource)
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
  ;; TODO: cope with fields requested on superclasses of the resource's
  ;; type
  (let ((spec (webserver:get-parameter
               (format nil "fields[~A]"
                       (json-type (resource item-spec))))))
    (values (and spec
                 (loop for key in (cl-ppcre:split "," spec)
                       collect (handler-case
                                   (resource-slot-by-json-key (resource item-spec) key)
                                 (no-such-property (c)
                                   (declare (ignore c))
                                   (find-link-by-json-name (resource item-spec) key)))))
            (and spec t))))

(defclass solution ()
  ((fields :initform (lhash:make-castable :test 'equal)
           :reader solution-fields))
  (:documentation "Represents a generic solution object, coming
   from various backends."))

(defun solution-value (solution property)
  "Returns the value for <property> of <solution>, in which <property>
   is the json type.  The second value is truethy iff the solution was
   retrieved from the triplestore at some point."
  (lhash:gethash property (solution-fields solution)))

(defun (setf solution-value) (value solution property)
  "Sets a found solution for the given property"
  (setf (lhash:gethash property (solution-fields solution)) value))

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
                                    (ld-properties resource))))
          ;; We construct something like:
          ;;
          ;; CONSTRUCT {
          ;;   <http://example.com/people/24> ext:1 ?ext1.
          ;;   <http://example.com/people/24> ext:2 ?ext2.
          ;;   <http://example.com/people/24> ext:3 ?ext3.
          ;; } WHERE {
          ;;   <http://example.com/people/24> foaf:firstName ?ext1.
          ;;   <http://example.com/people/24> foaf:lastName ?ext2.
          ;;   <http://example.com/people/24> foaf:hasAccount ?ext3.
          ;; }
          ;;
          ;; This could yield triples:
          ;;
          ;; <http://example.com/people/24> ext:1 <http://mastodon.social/@madnificent>.
          ;; <http://example.com/people/24> ext:1 <http://mastodon.social/@aadversteden>.
          ;; <http://example.com/people/24> ext:2 "Aad"
          ;; <http://example.com/people/24> ext:3 "madnificent"
          ;; <http://example.com/people/24> ext:3 "Versteden"
          ;;
          ;; If we then discover single-value properties containing
          ;; multiple values, we might add this to the meta, or we could
          ;; render an error in the terminal whilst still accepting some
          ;; response.
          (let (query-where
                slot-predicate-combinations
                query-construct)
            (loop for slot in missing-properties
                  for index from 0
                  for var = (s-var (format nil "var~A" index))
                  for pred-string = (format nil "http://mu.semte.ch/ext/pred~A" index)
                  do
                     (push (cons slot pred-string) slot-predicate-combinations)
                     (push (format nil "~&~A ~{~A~,^/~} ~A.~%"
                                   (s-url resource-url)
                                   (ld-property-list slot)
                                   var)
                           query-where)
                     (push (format nil "~&~A ~A ~A.~%"
                                   (s-url resource-url)
                                   (s-url pred-string)
                                   var)
                           query-construct))
            (let* ((triples (sparql:query (format nil "CONSTRUCT { ~{~&  ~A~}~%} WHERE {~{~&{  ~A}~,^~%UNION~}~%}"
                                                  query-construct query-where)))
                   (triple-db (make-triple-db triples nil)))
              (loop for (slot . pred-string) in slot-predicate-combinations
                    for json-var = (json-property-name slot)
                    for subject-db = (subject-db-for-predicate triple-db pred-string)
                    for objects = (objects-for-subject subject-db resource-url)
                    do (if (and (single-value-slot-p slot)
                                (> (length objects) 1))
                           (format t "~&[WARNING] ~A has single-valued property ~A which contains more than one value in the triplestore: ~{~A ~}.~%"
                                   resource-url json-var objects))
                       (if (single-value-slot-p slot)
                           (setf (solution-value solution json-var)
                                 (when (first objects)
                                   (from-sparql (first objects) (resource-type slot))))
                           (setf (solution-value solution json-var)
                                 (when (first objects)
                                   (from-sparql objects (resource-type slot)))))))
            solution))))))

(defun ensure-out-allowed-groups ()
  "Ensures out allowed groups are set, possibly copying them over from the input headers."
  (unless (or (hunchentoot:header-out :mu-auth-allowed-groups)
              (not (hunchentoot:header-in* :mu-auth-allowed-groups)))
    (setf (hunchentoot:header-out :mu-auth-allowed-groups)
          (hunchentoot:header-in* :mu-auth-allowed-groups))))

(defun field-requested-p (item-spec field &optional (requested-fields nil requested-fields-p) (sparse-fields-p nil sparse-fields-p-supplied-p))
  "Checks if the supplied field was requested.

The requested fields and whether there are sparse fields at all may be
supplied through optional values, limiting computations."
  (destructuring-bind (requested-fields sparse-fields-p)
      (if (and requested-fields-p sparse-fields-p-supplied-p)
          (list requested-fields sparse-fields-p)
          (multiple-value-list (sparse-fields-for-resource item-spec)))
    (or (not sparse-fields-p)
        (find field requested-fields))))

(defun group-solutions-to-complete (item-specs solutions resources missing-slots)
  "Groups these arguments, with the same definition as COMPLETE-SOLUTIONS,
in a way through which they can be fetched.

This means we group these item-specs in such a way that they're all of
the same resource, and that the same slots are missing.  We may further
split up resources in order to make the fetching less bulky per query."
  (let* ((combinations (zip item-specs solutions resources missing-slots))
         (grouped (group-by combinations :key (lambda (combination) (cons (third combination)
                                                                          (fourth combination)))
                                         :test #'equal))
         (split-groups (loop for ((resource . slots) . combinations)
                               in grouped
                             append (split-list-when
                                     combinations
                                     ;; TODO: support a maximum amount of properties to be fetched in one go
                                     :test (lambda (items)
                                             (or (or (not *soft-max-sources-in-property-construct*)
                                                     (> (length items) *soft-max-sources-in-property-construct*))
                                                 (or (not *soft-max-triples-in-property-construct*)
                                                     (let ((property-count (length (fourth (first items)))))
                                                       (> (* (length items) property-count)
                                                          *soft-max-triples-in-property-construct*))))))))
         (top-groups
           (loop for combinations in split-groups
                 for item-specs = (mapcar #'first combinations)
                 for solutions = (mapcar #'second combinations)
                 for resource = (third (first combinations))
                 for slots = (fourth (first combinations))
                 collect (list item-specs solutions resource slots))))
    top-groups))

(defun complete-solutions (item-specs solutions resources requested-slots)
  "Completes the solutions for the set of zippable lists.

- ITEM-SPECS is a list of ITEM-SPEC instances to complete
- SOLUTIONS is a corresponding list of partial solutions which should be
  used to complete the results.
- RESOURCES is a corresponding list of resources for each item-spec
- REQUESTED-SLOTS is a corresponding list in which each item is the list
  of slots that should be made available. Note that some of these
  properties may already be available in the solution or that this list
  may be empty."
  (loop for (item-specs solutions resource missing-slots)
          in (group-solutions-to-complete item-specs solutions resources requested-slots)
        for resource-urls = (mapcar #'node-url item-specs)
        for subject-var = (s-var "source")
        when missing-slots
        do
           ;; We construct something like:
           ;;
           ;; CONSTRUCT {
           ;;   ?sourceUri ext:1 ?ext1.
           ;;   ?sourceUri ext:2 ?ext2.
           ;;   ?sourceUri ext:3 ?ext3.
           ;; } WHERE {
           ;;   VALUES ?sourceUri { <http://example.com/people/1337> <http://example.com/people/42> }
           ;;   ?sourceUri foaf:firstName ?ext1.
           ;;   ?sourceUri foaf:lastName ?ext2.
           ;;   ?sourceUri foaf:hasAccount ?ext3.
           ;; }
           ;;
           ;; This could yield triples:
           ;;
           ;; <http://example.com/people/24> ext:1 <http://mastodon.social/@madnificent>.
           ;; <http://example.com/people/24> ext:1 <http://mastodon.social/@aadversteden>.
           ;; <http://example.com/people/24> ext:2 "Aad"
           ;; <http://example.com/people/24> ext:3 "madnificent"
           ;; <http://example.com/people/24> ext:3 "Versteden"
           ;;
           ;; If we then discover single-value properties containing
           ;; multiple values, we might add this to the meta, or we could
           ;; render an error in the terminal whilst still accepting some
           ;; response.
           (let (query-where
                 slot-predicate-combinations
                 query-construct
                 (subject-values (format nil "~&VALUES ~A { ~{~A ~}}~%" subject-var (mapcar #'s-url resource-urls))))
             (loop for slot in missing-slots
                   for index from 0
                   for var = (s-var (format nil "var~A" index))
                   for pred-string = (format nil "http://mu.semte.ch/ext/pred~A" index)
                   do
                      (push (cons slot pred-string) slot-predicate-combinations)
                      (push (format nil "~&~A ~{~A~,^/~} ~A.~%"
                                    subject-var
                                    (ld-property-list slot)
                                    var)
                            query-where)
                      (push (format nil "~&~A ~A ~A.~%"
                                    subject-var
                                    (s-url pred-string)
                                    var)
                            query-construct))
             (let* ((triples (sparql:query (format nil "CONSTRUCT { ~{~&  ~A~}~%} WHERE {~A ~{~&{  ~A}~,^~%UNION~}~%}"
                                                   query-construct subject-values query-where)))
                    (triple-db (make-triple-db triples nil)))
               (loop for (slot . pred-string) in slot-predicate-combinations
                     for json-var = (json-property-name slot)
                     for subject-db = (subject-db-for-predicate triple-db pred-string)
                     do
                        (loop for item-spec in item-specs
                              for solution in solutions
                              for resource-url in resource-urls
                              for objects = (objects-for-subject subject-db resource-url)
                              do (if (and (single-value-slot-p slot)
                                          (> (length objects) 1))
                                     (format t "~&[WARNING] ~A has single-valued property ~A which contains more than one value in the triplestore: ~{~A ~}.~%"
                                             resource-url json-var objects))
                                 (if (single-value-slot-p slot)
                                     (setf (solution-value solution json-var)
                                           (when (first objects)
                                             (from-sparql (first objects) (resource-type slot))))
                                     (setf (solution-value solution json-var)
                                           (when (first objects)
                                             (from-sparql objects (resource-type slot)))))))))))

(defun item-specs-to-jsown (item-specs)
  "Converts a set of item-specs to jsown objects, fetching all missing data."
  (let* ((solutions (mapcar #'ensure-solution item-specs))
         (resources (mapcar #'resource item-specs))
         (requested-slots (loop for item-spec in item-specs
                                for resource in resources
                                for resource-slots = (ld-properties resource)
                                for (requested-fields sparse-fields-p)
                                  = (multiple-value-list (sparse-fields-for-resource item-spec))
                                collect (loop for field in resource-slots
                                              when (field-requested-p item-spec field
                                                                      requested-fields sparse-fields-p)
                                                collect field)))
         (requested-links (loop for item-spec in item-specs
                                for resource in resources
                                for resource-links = (all-links resource)
                                for (requested-fields sparse-fields-p)
                                  = (multiple-value-list (sparse-fields-for-resource item-spec))
                                collect (loop for resource-link in resource-links
                                              when (field-requested-p item-spec resource-link
                                                                      requested-fields sparse-fields-p)
                                                collect resource-link))))
    (complete-solutions item-specs solutions resources requested-slots)
    (ensure-out-allowed-groups)
    (loop for item-spec in item-specs
          for resource in resources
          for solution in solutions
          for requested-slots in requested-slots
          for requested-links in requested-links
          for attributes = (jsown:empty-object)
          for relationships-object = (jsown:empty-object)
          do
             ;; fill in attributes object
             (loop for requested-slot in requested-slots
                   for json-var = (json-property-name requested-slot)
                   when (solution-value solution json-var)
                     do (setf (jsown:val attributes json-var)
                              (solution-value solution json-var)))
             ;; attach uri if feature is enabled (after variables were parsed)
             (when (find 'include-uri (features resource))
               (setf (jsown:val attributes "uri") (node-url item-spec)))
             ;; fill in relationships object
             (loop for link in requested-links
                   for json-var = (json-key link)
                   do (setf (jsown:val relationships-object json-var)
                            (build-relationships-object item-spec link)))
             ;; collect the result
          collect (jsown:new-js
                    ("id" (uuid item-spec))
                    ("type" (json-type resource))
                    ("attributes" attributes)
                    ("relationships" relationships-object)))))

(defun item-spec-to-jsown (item-spec)
  "Returns the jsown representation of the attributes and
   non-filled relationships of item-spec.  This is the default
   way of fetching the database contents of a single item."
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
                               (ld-properties resource))))
          (if unavailable-fields
              (complete-solution solution item-spec)
              (progn
                ;; (format t "Using cached solution for ~A" (uuid item-spec))
                t)))
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
          ;; ensure we have a mu-auth-allowed-groups in case the full
          ;; response could be answered by a cache
          (ensure-out-allowed-groups)
          ;; construct response structure
          (jsown:new-js
            ("attributes" attributes)
            ("id" (uuid item-spec))
            ("type" (json-type resource))
            ("relationships" relationships-object)))))))

(defun retrieve-item (item-spec resource)
  "Returns (values item-json included-items)
   item-json contains the description of the specified item with
     necessary links from <included>."
  (handler-bind
      ((no-such-instance (lambda (err) (declare (ignore err)) :null)))
    (multiple-value-bind (data-item-specs included-item-specs)
        (augment-data-with-attached-info
         (list item-spec)
         resource)
      (values (item-spec-to-jsown (first data-item-specs))
              (item-specs-to-jsown included-item-specs)))))

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
    ;; TODO: eagerly remove cached classes
    (let ((item-spec (make-item-spec :type (resource-name resource)
                                     :uuid uuid)))
      (check-access-rights-for-item-spec item-spec :delete)
      (with-surrounding-hook (:delete (resource-name resource))
          (item-spec)
        (with-cache-store
          (flet ((cache-clear-for-delete-call ()
                   (cache-clear-class (resource item-spec))
                   (cache-clear-object item-spec)
                   ;; in our inheritance tree, find all defined relations
                   (let ((super-resources (flattened-class-tree (resource item-spec))))
                     ;; TODO: optimize this scenario, these can be precalculated.
                     (dolist (super-resource super-resources)
                       ;; we pick direct links because cache-clear-relation clears for all relevant subclasses
                       (dolist (link (all-direct-links super-resource))
                         (cache-clear-relation resource link :include-inverse-p nil)))
                     ;; from all resources, find all relations filter those who point to us or any of our superclasses
                     (loop for (source-resource . link)
                             in (all-defined-links-with-resource)
                           when (find (referred-resource link) super-resources)
                             do (cache-clear-relation source-resource link :include-inverse-p nil)))))
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
              (cache-clear-for-delete-call)
              (sparql:delete
                  (apply #'concatenate 'string
                         (loop for triple-clause
                                 in
                               `(;; uuid
                                 (,(s-var "s") ,(s-prefix "mu:uuid") ,(s-str uuid))
                                 ;; types
                                 ,@(loop for ld-class
                                           in (flattened-ld-class-tree (resource item-spec))
                                         collect `(,(s-var "s") ,(s-prefix "a") ,ld-class))
                                 ;; properties
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
                           `((,(s-var "s") ,(s-prefix "mu:uuid") ,(s-str uuid))))
                   (format nil "~{~&~4tOPTIONAL {~{~A ~A ~A~}.}~%~}"
                           (loop for (property-list value) in relation-content
                                 if (s-inv-p (first property-list))
                                   collect `(,value ,(s-inv (first property-list)) ,(s-var "s"))
                                 else
                                   collect `(,(s-var "s") ,(first property-list) ,value))))))))
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
          (cache-relation (resource item-spec) link)
          (let ((relation-item-spec (first (retrieve-relation-items item-spec link))))
            (when relation-item-spec
              (cache-object relation-item-spec))
            (jsown:new-js
              ("data" (if relation-item-spec
                          (retrieve-item relation-item-spec (referred-resource link))
                          :null))
              ("links" (build-links-object item-spec link))))))))
  (:method ((resource resource) id (link has-many-link))
    (let* ((item-spec (make-item-spec :type (resource-name resource) :uuid id))
           (resource-url (s-url (node-url item-spec))))
      (with-surrounding-hook (:show-relation (resource-name resource))
          (item-spec link)
        (with-cache-store
          (cache-relation (resource item-spec) link)
          (cache-class (referred-resource link))
          (let ((link-spec (make-item-spec :type (resource-name resource) :uuid id)))
            (multiple-value-bind (response data-item-specs)
                (paginated-collection-response
                 :resource (referred-resource link)
                 :sparql-body (filter-body-for-search
                               :sparql-body (format nil
                                                    (s+ "~A ~{~A~,^/~} ?resource. "
                                                        "?resource mu:uuid ?uuid; "
                                                        "          a ?class. "
                                                        "VALUES ?class {~{~A~^ ~}}."
                                                        "~@[~A~] ")
                                                    resource-url
                                                    (ld-property-list link)
                                                    (ld-subclasses (find-resource-by-name (resource-name link)))
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
          (cache-clear-relation (resource item-spec) link)
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
        (cache-clear-relation (resource item-spec) link)
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
                                                  for spec = (make-item-spec :type (resource-name linked-resource)
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
        (cache-clear-relation (resource item-spec) link)
        (with-surrounding-hook (:delete-relation (resource-name resource))
            (item-spec body)
          (let* ((linked-resource (referred-resource link))
                 (resources (loop for uuid in
                                 (remove-if-not #'identity (jsown:filter body "data" map "id"))
                               for spec = (make-item-spec :type (resource-name linked-resource)
                                                          :uuid uuid)
                               collect (node-url spec))))
            (when resources
              (sparql:delete-triples
               (loop for resource in resources
                  collect
                    `(,(s-url (node-url item-spec))
                       ,@(ld-property-list link)
                       ,(s-url resource))))))
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
        (cache-clear-relation (resource item-spec) link)
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

(defun handle-uri-class-changes (inserts deletes)
  "Handles creations and removals of class changes."
  ;; TODO: this could become smart and update the classes at runtime.
  ;; However, when triples are removed from one graph, they may still
  ;; exist in another graph.  We lack the knowledge about the specific
  ;; graphs, and therefore cannot store the types based on this.  Once
  ;; the graphs/access-rights are shared from the delta messages, we can
  ;; upgrade the type cache.  This type cache can then be used to update
  ;; the variables in place when needed.  This would make use of
  ;; (remove-cached-class-for-uri uri type)
  (flet ((filter-class-triples (list)
           (remove-if-not (lambda (triple)
                            ;; TODO: check this is effectively a URI
                            (string= (jsown:filter triple "predicate" "value")
                                     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
                          list)))
    (let ((inserted-types (filter-class-triples inserts))
          (deleted-types (filter-class-triples deletes)))
      (loop for triple in deleted-types
            for uri = (jsown:filter triple "subject" "value")
            do (clear-cached-classes-for-uri uri))
      (loop for triple in inserted-types
            for uri = (jsown:filter triple "subject" "value")
            do (clear-cached-classes-for-uri uri)))))

(defgeneric delta-call (body)
  (:documentation "Performs removal of data based on the received
    delta messages.")
  (:method (body)
    ;; TODO: consume and progress MU_AUTH_ALLOWED_GROUPS.  This could
    ;; be done automatically with the right changes in the DELTA
    ;; service.
    ;;
    ;; TODO: cope with MU_AUTH_SUDO specified through delta-service or
    ;; by corresponding sudo setting in MU_AUTH_ALLOWED_GROUPS.
    (let* ((inserts (loop for diff in body
                          append (jsown:val diff "inserts")))
           (deletes (loop for diff in body
                          append (jsown:val diff "deletes")))
           ;; note that something can fall through the cracks with this
           ;; calculation.  support from mu-authorization would help in
           ;; these cases.
           (effective-inserts (set-difference inserts deletes
                                              :test (lambda (a b)
                                                      (string= (jsown:to-json a)
                                                               (jsown:to-json b)))))
           (effective-deletes (set-difference deletes inserts
                                              :test (lambda (a b)
                                                      (string= (jsown:to-json a)
                                                               (jsown:to-json b)))))
           (triples (append effective-inserts effective-deletes))
           (types-per-uri (let ((uri-type-hash (lhash:make-castable :test 'equal)))
                            (dolist (triple triples)
                              (when (string= (jsown:filter triple "predicate" "value")
                                             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                                (let ((subject (jsown:filter triple "subject" "value"))
                                      (object (jsown:filter triple "object" "value")))
                                  (pushnew object (lhash:gethash subject uri-type-hash)
                                           :test #'string=))))
                            uri-type-hash)))
      (labels ((classes-for-uri-in-delta (uri)
                 (lhash:gethash uri types-per-uri))
               (resources-and-item-specs (jsown-term)
                 (if (string= (jsown:val jsown-term "type") "uri")
                     ;; it's a URI
                     (let* ((uri (jsown:val jsown-term "value"))
                            (resource-classes
                              ;; we determine the resource classes also
                              ;; based on the triples that are in the
                              ;; delta but might not be in the
                              ;; triplestore.  code that uses the
                              ;; item-specs to find the classes will not
                              ;; discover those based on the
                              ;; triplestore.  care should be taken to
                              ;; ensure cache clearing code has the
                              ;; right constraints.
                              (union (classes-for-uri-in-delta uri)
                                     (classes-for-uri uri)
                                     :test #'string=))
                            (resources (remove-if-not
                                        #'identity
                                        (mapcar (lambda (resource-class)
                                                  (handler-case
                                                      (find-resource-by-class-uri resource-class)
                                                    (no-such-instance (err)
                                                      (declare (ignore err))
                                                      nil)))
                                                resource-classes)))
                            (item-specs (mapcar (lambda (resource)
                                                  (make-item-spec :node-url uri
                                                                  :type (resource-name resource)))
                                                resources)))
                       (values resources item-specs))
                     (values nil nil)))
               (handle-object-clear (item-specs)
                 (dolist (item-spec item-specs)
                   (handler-case
                       (cache-clear-object item-spec)
                     (no-such-instance (e)
                       (declare (ignore e))
                       ;; this is a feasible case, skip it
                       )
                     (error (e)
                       (format t "AN ERROR OCCURRED PROCESSING A DELTA MESSAGE ~A" e)))))
               (handle-class-clear (resources)
                 (dolist (resource resources)
                   (cache-clear-class resource)))
               (handle-relation-clear (resources predicate inverse-p)
                 (loop for resource in resources
                       do (loop for relation in (all-links resource)
                                when (and (eq (inverse-p relation) inverse-p)
                                          (string= (full-uri (ld-link relation))
                                                   predicate))
                                  do
                                     (cache-clear-relation resource relation)))))
        (with-cache-store
          (loop for triple in triples
                for predicate = (jsown:filter triple "predicate" "value")
                do
                   (multiple-value-bind (subject-resources subject-item-specs)
                       (resources-and-item-specs (jsown:val triple "subject"))
                     (handle-object-clear subject-item-specs)
                     (handle-class-clear subject-resources)
                     (handle-relation-clear subject-resources predicate nil))
                   (multiple-value-bind (object-resources object-item-specs)
                       (resources-and-item-specs (jsown:val triple "object"))
                     (handle-object-clear object-item-specs)
                     (handle-class-clear object-resources)
                     (handle-relation-clear object-resources predicate t)))
          ;; it used to be that these had to be executed at the end
          ;; because we depended on cached URIs to clear what we knew of
          ;; the world, that is not fully the case anymore but it makes
          ;; most sense down here.
          (handle-uri-class-changes effective-inserts effective-deletes)))
      (let ((out-headers (cdr (assoc :clear-keys (hunchentoot:headers-out*)))))
        (when (and *cache-clear-path* out-headers)
          (when *log-delta-clear-keys*
            (format t "~&Sending clear keys: ~A~%" out-headers))
          (drakma:http-request *cache-clear-path*
                               :method :post
                               :additional-headers `(("clear-keys" . ,out-headers)))))
      (setf (hunchentoot:header-out :clear-keys) "null")
      (jsown:new-js ("message" "processed delta")))))
