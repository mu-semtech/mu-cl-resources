(in-package :mu-cl-resources)

;;;;;;;;;;;;;;;;;;;;;;
;; call implementation

(defgeneric create-call (resource)
  (:documentation "implementation of the POST request which
    handles the creation of a resource.")
  (:method ((resource-symbol symbol))
    (create-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (let* ((jsown:*parsed-null-value* :null)
           (json-input (jsown:parse (post-body)))
           (uuid (mu-support:make-uuid)) 
           (item-spec (make-item-spec :uuid uuid :type (resource-name resource)))
           (resource-uri (s-url (format nil "~A~A"
                                        (raw-content (ld-resource-base resource))
                                        uuid))))
      (sparql:insert-triples
       `((,resource-uri ,(s-prefix "a") ,(ld-class resource))
         (,resource-uri ,(s-prefix "mu:uuid") ,(s-str uuid))
         ,@(loop for (predicates object)
              in (attribute-properties-for-json-input resource json-input)
              unless (eq object :null)
              collect `(,resource-uri ,@predicates ,object))))
      (setf (hunchentoot:return-code*) hunchentoot:+http-created+)
      (setf (hunchentoot:header-out :location)
            (construct-resource-item-path item-spec))
      (when (and (jsown:keyp json-input "data")
                 (jsown:keyp (jsown:val json-input "data") "relationships"))
        (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
           if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                          "data")
           do
             (update-resource-relation (make-item-spec :type (resource-name resource)
                                                       :uuid uuid)
                                       relation
                                       (jsown:filter json-input
                                                     "data" "relationships" relation "data"))))
      (jsown:new-js ("data" (retrieve-item item-spec))))))


(defun find-resource-for-uuid (item-spec)
  "Retrieves the uri-resource which specifies the supplied item-spec."
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

(defgeneric update-call (resource uuid)
  (:documentation "implementation of the PUT request which
    handles the updating of a resource.")
  (:method ((resource-symbol symbol) uuid)
    (update-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let* ((item-spec (make-item-spec :type (resource-name resource) :uuid uuid))
           (jsown:*parsed-null-value* :null)
           (json-input (jsown:parse (post-body)))
           (attributes (jsown:filter json-input "data" "attributes"))
           (uri (s-url (find-resource-for-uuid item-spec))))
      (sparql:with-query-group
        (let ((delete-vars (loop for key in (jsown:keywords attributes)
                              for i from 0
                              collect (s-var (format nil "gensym~A" i)))))
          (sparql:delete-triples
           (loop for key in (jsown:keywords attributes)
              for slot = (resource-slot-by-json-key resource key)
              for s-var in delete-vars
              collect `(,uri ,@(ld-property-list slot) ,s-var))))
        (sparql:insert-triples
         (loop for key in (jsown:keywords attributes)
            for slot = (resource-slot-by-json-key resource key)
            for value = (if (eq (jsown:val attributes key) :null)
                            :null
                            (interpret-json-value slot (jsown:val attributes key)))
            for property-list = (ld-property-list slot)
            unless (eq value :null)
            collect
              `(,uri ,@property-list ,value))))
      (when (and (jsown:keyp json-input "data")
                 (jsown:keyp (jsown:val json-input "data") "relationships"))
        (loop for relation in (jsown:keywords (jsown:filter json-input "data" "relationships"))
           if (jsown:keyp (jsown:filter json-input "data" "relationships" relation)
                          "data")
           do
             (update-resource-relation item-spec relation
                                       (jsown:filter json-input
                                                     "data" "relationships" relation "data")))))
    (respond-no-content)))

(defgeneric update-resource-relation (item-spec relation resource-specification)
  (:documentation "updates the specified relation with the given specification.")
  (:method ((item-spec item-spec) (relation string) resource-specification)
    (update-resource-relation item-spec
                              (find-link-by-json-name (resource item-spec) relation)
                              resource-specification))
  (:method ((item-spec item-spec) (link has-one-link) resource-specification)
    (flet ((delete-query (resource-uri link-uri)
             (sparql:delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uri)
             (sparql:insert-triples
              `((,resource-uri ,@link-uri ,new-linked-uri)))))
      (let ((linked-resource (referred-resource link))
            (resource-uri (find-resource-for-uuid item-spec)))
        (if resource-specification
            ;; update content
            (let* ((new-linked-uuid (jsown:val resource-specification "id"))
                   (new-linked-uri (find-resource-for-uuid
                                    (make-item-spec :type (resource-name linked-resource)
                                                    :uuid new-linked-uuid))))
              (sparql:with-query-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (s-url new-linked-uri))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link))))))
  (:method ((item-spec item-spec) (link has-many-link) resource-specification)
    (flet ((delete-query (resource-uri link-uri)
             (sparql:delete-triples
              `((,resource-uri ,@link-uri ,(s-var "s")))))
           (insert-query (resource-uri link-uri new-linked-uris)
             (sparql:insert-triples
              (loop for new-link-uri in new-linked-uris
                 collect
                   `(,resource-uri ,@link-uri ,new-link-uri)))))
      (let ((linked-resource (referred-resource link))
            (resource-uri (find-resource-for-uuid item-spec)))
        (if resource-specification
            ;; update content
            (let* ((new-linked-uuids (jsown:filter resource-specification map "id"))
                   (new-linked-resources (loop for uuid in new-linked-uuids
                                            for spec = (make-item-spec :type linked-resource
                                                                       :uuid uuid)
                                            collect
                                              (find-resource-for-uuid spec))))
              (sparql:with-query-group
                (delete-query (s-url resource-uri)
                              (ld-property-list link))
                (insert-query (s-url resource-uri)
                              (ld-property-list link)
                              (mapcar #'s-url new-linked-resources))))
            ;; delete content
            (delete-query (s-url resource-uri)
                          (ld-property-list link)))))))

(defgeneric list-call (resource)
  (:documentation "implementation of the GET request which
   handles listing the whole resource")
  (:method ((resource-symbol symbol))
    (list-call (find-resource-by-name resource-symbol)))
  (:method ((resource resource))
    (paginated-collection-response
     :resource resource
     :sparql-body (filter-body-for-search
                   :sparql-body  (format nil "?s mu:uuid ?uuid; a ~A."
                                         (ld-class resource))
                   :source-variable (s-var "s")
                   :resource resource)
     :source-variable (s-var "s"))))

(defgeneric show-call (resource uuid)
  (:documentation "implementation of the GET request which
    handles the displaying of a single resource.")
  (:method ((resource-symbol symbol) uuid)
    (show-call (find-resource-by-name resource-symbol) uuid))
  (:method ((resource resource) (uuid string))
    (let ((item-spec (make-item-spec :uuid uuid :type (resource-name resource))))
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
              response))))))

(defun sparse-fields-for-resource (item-spec)
  "Returns the sparse fieldsets for the supplied item-spec.
   Returns two values, the first being the attributes to return,
   the second being non-nil iff the fields for this item-spec
   were returned."
  (let ((spec (hunchentoot:get-parameter
               (format nil "fields[~A]"
                       (json-type (resource item-spec))))))
    (values (and spec
                 (loop for key in (cl-ppcre:split "," spec)
                    collect
                      (or (resource-slot-by-json-key (resource item-spec) key)
                          (find-link-by-json-name (resource item-spec) key))))
            (and spec t))))

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
        (let* ((resource (resource item-spec))
               (uuid (uuid item-spec))
               (resource-url
                ;; we search for a resource separately as searching it
                ;; in one query is redonculously slow.  in the order of
                ;; seconds for a single solution.
                (find-resource-for-uuid item-spec))
               (solution
                ;; simple attributes
                (first
                 (sparql:select
                  "*"
                  (format nil
                          "~{~&OPTIONAL {~A ~{~A~,^/~} ~A.}~}"
                          (loop for slot in (ld-properties resource)
                             when (and (single-value-slot-p slot)
                                       (field-requested-p slot))
                             append (list (s-url resource-url)
                                          (ld-property-list slot)
                                          (s-var (sparql-variable-name slot))))))))
               (attributes (jsown:empty-object)))
          ;; read extended variables through separate sparql query
          (loop for slot in (ld-properties resource)
             for variable-name = (sparql-variable-name slot)
             if (and (not (single-value-slot-p slot))
                     (field-requested-p slot))
             do
               (setf (jsown:val solution variable-name)
                     (mapcar (lambda (solution) (jsown:val solution variable-name))
                             (sparql:select "*"
                                            (format nil "~A ~{~A~,^/~} ~A."
                                                    (s-url resource-url)
                                                    (ld-property-list slot)
                                                    (s-var variable-name))))))
          ;; read simple attributes from sparql query
          (loop for property in (ld-properties resource)
             for sparql-var = (sparql-variable-name property)
             for json-var = (json-property-name property)
             if (jsown:keyp solution sparql-var)
             do
               (setf (jsown:val attributes json-var)
                     (from-sparql (jsown:val solution sparql-var) (resource-type property))))
          ;; build response data object
          (let ((relationships-object (jsown:empty-object)))
            (loop for link in (all-links resource)
               if (field-requested-p link)
               do
                 (setf (jsown:val relationships-object (json-key link))
                       (build-relationships-object item-spec link)))
            (jsown:new-js
              ("attributes" attributes)
              ("id" uuid)
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
      (jsown:new-js ("self" (format nil "/~A/~A/links/~A"
                                    (request-path resource)
                                    uuid
                                    (request-path link)))
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
                               collect `(,(s-var "s") ,(first property-list) ,value))))))
    (respond-no-content)))

(defgeneric show-relation-call (resource id link)
  (:documentation "implementation of the GET request which handles
    the listing of a relation.")
  (:method ((resource-symbol symbol) id link)
    (show-relation-call (find-resource-by-name resource-symbol) id link))
  (:method ((resource resource) id (link has-one-link))
    (let ((item-spec (make-item-spec :type (resource-name resource) :uuid id)))
      (let ((relation-item-spec (first (retrieve-relation-items item-spec link))))
        (jsown:new-js
          ("data" (if relation-item-spec
                      (retrieve-item relation-item-spec)
                      :null))
          ("links" (build-links-object item-spec link))))))
  (:method ((resource resource) id (link has-many-link))
    (let ((item-spec (make-item-spec :type (resource-name resource) :uuid id)))
      (paginated-collection-response
       :resource (referred-resource link)
       :sparql-body (filter-body-for-search
                     :sparql-body (format nil
                                          (s+ "~A ~{~A~,^/~} ?resource. "
                                              "?resource mu:uuid ?uuid.")
                                          (s-url (find-resource-for-uuid item-spec))
                                          (ld-property-list link))
                     :source-variable (s-var "resource")
                     :resource (referred-resource link))
       :source-variable (s-var "resource")
       :link-defaults (build-links-object (make-item-spec :type (resource-name resource)
                                                          :uuid id)
                                          link)))))

(defgeneric retrieve-relation-items (item-spec link)
  (:documentation "retrieves the item descriptions of the items
    which are connected to <resource> <id> through link <link>.
    This yields the high-level description of the items, not
    their contents.
    Note that this method does not support pagination.")
  (:method ((item-spec item-spec) (link string))
    (retrieve-relation-items item-spec (find-link-by-json-name (resource item-spec) link)))
  (:method ((item-spec item-spec) (link has-one-link))
    (let ((query-results
           (first (sparql:select (s-var "uuid")
                                 (format nil (s+ "~A ~{~A~,^/~} ?resource. "
                                                 "?resource mu:uuid ?uuid. ")
                                         (s-url (find-resource-for-uuid item-spec))
                                         (ld-property-list link)))))
          (linked-resource (resource-name (referred-resource link))))
      (and query-results
           (list
            (make-item-spec :type linked-resource
                            :uuid (jsown:filter query-results "uuid" "value"))))))
  (:method ((item-spec item-spec) (link has-many-link))
    (let ((query-results
           (sparql:select (s-var "uuid")
                          (format nil (s+ "~A ~{~A~,^/~} ?resource. "
                                          "?resource mu:uuid ?uuid. ")
                                  (s-url (find-resource-for-uuid item-spec))
                                  (ld-property-list link))))
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
        (let ((body (jsown:parse (post-body)))
              (linked-resource (referred-resource link))
              (resource-uri (find-resource-for-uuid item-spec))
              (link-path (ld-property-list link)))
          (if (jsown:val body "data")
              ;; update content
              (let* ((new-linked-uuid (jsown:filter body "data" "id"))
                     (new-linked-uri (find-resource-for-uuid
                                      (make-item-spec :type (resource-name linked-resource)
                                                      :uuid new-linked-uuid))))
                (sparql:with-query-group
                  (delete-query (s-url resource-uri) link-path)
                  (insert-query (s-url resource-uri) link-path
                                (s-url new-linked-uri))))
              ;; delete content
              (delete-query (s-url resource-uri) link-path)))))
    (respond-no-content))
  (:method ((resource resource) id (link has-many-link))
    (let ((item-spec (make-item-spec :type (resource-name resource) :uuid id)))
      (flet ((delete-query (resource-uri link-uri)
               (sparql:delete-triples
                `((,resource-uri ,@link-uri ,(s-var "s")))))
             (insert-query (resource-uri link-uri new-linked-uris)
               (sparql:insert-triples
                (loop for new-uri in new-linked-uris
                   collect `(,resource-uri ,@link-uri ,new-uri)))))
        (let ((body (jsown:parse (post-body)))
              (linked-resource (referred-resource link))
              (resource-uri (find-resource-for-uuid item-spec))
              (link-path (ld-property-list link)))
          (if (jsown:val body "data")
              ;; update content
              (let* ((new-linked-uuids (jsown:filter body "data" map "id"))
                     (new-linked-resources (loop for uuid in new-linked-uuids
                                              for spec = (make-item-spec :type linked-resource
                                                                         :uuid uuid)
                                              collect
                                                (find-resource-for-uuid spec))))
                (delete-query (s-url resource-uri) link-path)
                (insert-query (s-url resource-uri)
                              link-path
                              (mapcar #'s-url new-linked-resources)))
              ;; delete content
              (delete-query (s-url resource-uri)
                            link-path)))))
    (respond-no-content)))

(defgeneric delete-relation-call (resource id link)
  (:documentation "Performs a delete call on a relation, thereby
    removing a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let* ((item-spec (make-item-spec :type (resource-name resource)
                                      :uuid id))
           (linked-resource (referred-resource link))
           (resources (loop for uuid in (remove-if-not #'identity
                                      (jsown:filter (jsown:parse (post-body))
                                                    "data" map "id"))
                         for spec = (make-item-spec :type linked-resource
                                                    :uuid uuid)
                         collect (find-resource-for-uuid spec))))
      (when resources
        (sparql:delete-triples
         (loop for resource in resources
            collect
              `(,(s-url (find-resource-for-uuid item-spec))
                 ,@(ld-property-list link)
                 ,resource)))))
    (respond-no-content)))

(defgeneric add-relation-call (resource id link)
  (:documentation "Performs the addition call on a relation, thereby
    adding a set of linked resources.")
  (:method ((resource resource) id (link has-many-link))
    (let* ((item-spec (make-item-spec :type (resource-name resource)
                                      :uuid id))
           (linked-resource (referred-resource link))
           (resources (loop for uuid
                         in (remove-if-not #'identity
                                           (jsown:filter (jsown:parse (post-body))
                                                         "data" map "id"))
                         collect
                           (find-resource-for-uuid
                            (make-item-spec :uuid uuid
                                            :type (resource-name linked-resource))))))
      (when resources
        (let ((source-url (find-resource-for-uuid item-spec))
              (properties (ld-property-list link)))
          (sparql:insert-triples
           (loop for resource in resources
              collect
                `(,(s-url source-url) ,@properties ,(s-url resource)))))))
    (respond-no-content)))


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
  (dolist (item item-specs)
    (let (linked-items)
      ;; fill in current path
      (setf linked-items
            (union linked-items
               (include-items-for-single-included included-items-store item
                                                  (first included-spec))))
      ;; traverse included-spec path
      (when (rest included-spec)
        (include-items-for-included included-items-store linked-items
                                    (rest included-spec))))))

(defun include-items-for-single-included (included-items-store item-spec relation-string)
  "Adds the items which are linked to item-spec by relation included-spec
   to included-items-store.  Returns the list of items which are linked
   through item-spec."
  (let* ((resource (resource item-spec))
         (uuid (uuid item-spec))
         (relation (find-resource-link-by-json-key resource relation-string))
         (target-type (resource-name relation))
         (related-objects
          (loop for new-uuid
             in (jsown:filter
                 (sparql:select (s-var "target")
                                (format nil (s+ "?s mu:uuid ~A. "
                                                "?s ~{~A/~}mu:uuid ?target. ")
                                        (s-str uuid)
                                        (ld-property-list relation)))
                 map "target" "value")
             collect (included-items-store-ensure included-items-store
                                                  (make-item-spec :uuid new-uuid
                                                                  :type target-type)))))
    (setf (gethash relation (related-items-table item-spec))
          related-objects)
    related-objects))

(defun extract-included-from-request ()
  "Extracts the filters from the request.  The result is a list
   containing the :components and :search key.  The :components
   key includes a left-to-right specification of the strings
   between brackets.  The :search contains the content for that
   specification."
  (let ((include-parameter
         (assoc "include" (hunchentoot:get-parameters*) :test #'string=)))
    (and include-parameter
         (mapcar (alexandria:curry #'split-sequence:split-sequence #\.)
                 (split-sequence:split-sequence #\, (cdr include-parameter))))))