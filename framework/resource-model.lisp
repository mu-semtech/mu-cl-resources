
(in-package :mu-cl-resources)


(defclass resource-slot ()
  ((json-key :initarg :json-key :reader json-key)
   (ld-property :initarg :ld-property :reader ld-property)
   (resource-type :initarg :resource-type :reader resource-type)
   (required-p :initarg :required-p :reader required-p))
  (:documentation "Describes a single property of a resource."))

(defclass has-link ()
  ((resource-name :initarg :resource :reader resource-name)
   (ld-link :initarg :via :reader ld-link)
   (inverse :initarg :inverse :reader inverse-p :initform nil)
   (request-path :initarg :as :reader request-path)
   (inverse-links :initform nil :accessor inverse-links))
  (:documentation "Describes a link to another resource.
   You should use one of its subclasses."))

(defclass has-many-link (has-link)
  ()
  (:documentation "Describes a has-many link to another resource"))

(defclass has-one-link (has-link)
  ()
  (:documentation "Describes a has-one link to another resource"))

(defclass resource ()
  ((superclass-names :initarg :superclass-names :reader superclass-names :initform nil)
   (ld-class :initarg :ld-class :reader ld-class)
   (ld-properties :initarg :ld-properties
                  :reader direct-ld-properties) ; used for uml
                                                ; generators etc
   (ld-properties-cache :documentation
                        "A cache for faster calculation of the
                        existing properties of a resource.")
   (ld-resource-base :initarg :ld-resource-base :reader ld-resource-base)
   (json-type :initarg :json-type :reader json-type)
   (has-many-links :initarg :has-many
                   :reader direct-has-many-links)
                                        ; used for uml generators etc
   (has-one-links :initarg :has-one
                  :reader direct-has-one-links)
                                        ; used for uml generators etc
   (request-path :initarg :request-path :reader request-path)
   (name :initarg :resource-name :reader resource-name)
   (features :initarg :features)
   (authorization :initarg :authorization :reader authorization)
   (query-count-cache :initform (make-user-aware-hash-table :test 'equal)
                      :accessor query-count-cache)
   (resource-subclass-cache :documentation
                            "A cache for faster calculation of a
                            resource's children")))

(defparameter *resources* (make-hash-table)
  "contains all currently known resources")

(defmethod initialize-instance :after ((resource resource) &key &allow-other-keys)
  (let ((name (resource-name resource)))
    (setf (gethash name *resources*) resource))
  ;; When links and inverse links are specified, we don't need to
  ;; cater for extra specific cases.  Reason is that inverse links
  ;; should be specified on the parent classes.  As we follow the
  ;; reified inheritance tree, this should have been applied to all
  ;; relevant classes, and their subclasses should take this into
  ;; account.

  ;; TODO: further testing is needed to ensure this set of inverse
  ;; properties cannot override certain properties due to subclasses
  ;; now existing, and thus incorrectly setting the property list.
  (dolist (link (all-links resource))
    (alexandria:when-let ((linked-resource (find-resource-by-name (resource-name link)))
                          (inverse-property-list (reverse
                                                  (mapcar (lambda (prop)
                                                            (format nil "~A" (s-inv prop)))
                                                          (ld-property-list link)))))
      inverse-property-list
      ;; find inverse relationship
      (dolist (inverse-link (all-links linked-resource))
        (when (equalp inverse-property-list
                      (mapcar (lambda (prop) (format nil "~A" prop))
                              (ld-property-list inverse-link)))
          ;; we have found an inverse relationship of ours
          (push `(:resource ,resource :link ,link)
                (inverse-links inverse-link))
          (push `(:resource ,linked-resource :link ,inverse-link)
                (inverse-links link)))))))

(defgeneric authorization-token (resource operation)
  (:documentation "Yields the authorization token which grants
   access for <operation> on <resource>.  If this yields nil, no
   token is needed.")
  (:method ((item-spec item-spec) key)
    (authorization-token (resource item-spec) key))
  (:method ((resource resource) key)
    (getf (authorization resource) key)))

(defgeneric authorization-p (object operation)
  (:documentation "Indicates that operation <operation> on <resource>
   needs to be authorised.")
  (:method (object relation)
    (and (authorization-token object relation) t)))

(defgeneric json-property-name (resource-slot)
  (:documentation "retrieves the name of the json property of the
   supplied slot")
  (:method ((slot resource-slot))
    (if *camelcase-json-variables*
        (symbol-to-camelcase (json-key slot))
        (string-downcase (string (json-key slot)))))
  (:method ((link has-link))
    (request-path link)))

(defgeneric single-value-slot-p (resource-slot)
  (:documentation "indicates whether or not a resource-slot's value can
   be constructed from a single value in the triple-store.")
  (:method ((slot resource-slot))
    ;; only the lang-string-set and :string-set have multiple values for now
    (not (find (resource-type slot)
             '(:language-string-set :string-set :uri-set)))))

(defun multi-value-slot-p (resource-slot)
  "inverse of single-value-slot-p."
  (not (single-value-slot-p resource-slot)))

(defun slot-value-represents-triples-p (resource-slot value)
  "non-nil if a <value> for <resource-slot> would need one one or
   more triples to be set in the triple-store to indicate that
   that content is present.  if <value> is the default value of a
   property, this would be nil.
   <value> should be the json representation of the value.
   eg: a lang-string-set with no options would yield nil here."
  (not (or (eq value :null)                        ; :null is always default
         (and (multi-value-slot-p resource-slot) ; an empty array is default when we have
              (eql value nil)))))                ; multiple values but :null is allowed too

(defgeneric sparql-variable-name (resource-slot)
  (:documentation "retrieves the name of the json property as it
   could be used in a sparql query")
  (:method ((slot resource-slot))
    (symbol-to-camelcase (json-key slot)))
  (:method ((link has-link))
    (symbol-to-camelcase (request-path link))))

(defgeneric ld-property-list (slot)
  (:documentation "yields the ld-property as a list from the
   resource-slot")
  (:method ((slot resource-slot))
    (list (ld-property slot)))
  (:method ((link has-link))
    (list (if (inverse-p link)
              (s-inv (ld-link link))
              (ld-link link)))))

(defgeneric expanded-ld-class (resource)
  (:documentation "Expanded version of the ld-class of the resource.")
  (:method ((resource resource))
    (full-uri (ld-class resource))))

(defgeneric expanded-ld-relation (relation)
  (:documentation "Expanded version of the ld-property-list of the relationship.")
  (:method ((relation has-link))
    (full-uri (ld-property-list relation))))

(defgeneric flattened-class-tree (resource)
  (:documentation "Yields an ordered list of all types which apply to
  this resource, most specific type first.

  This means we yield this resource, as well as all parents this resource has.

  The superclass-names are reversed from their original definition, as
  later names are assumed to be of less importance.")
  (:method ((resource resource))
    (cons resource
          (loop for resource-name in (reverse (superclass-names resource))
                append (flattened-class-tree
                        (find-resource-by-name resource-name))))))

(defgeneric features (resource)
  (:documentation "Features for the current resource, being the features specified at any level.")
  (:method ((resource resource))
    ;; TODO: this could be cleaner by checking if features was
    ;; explicitly set, and using the higest explicitly set features
    ;; even if empty.
    (let ((features (slot-value resource 'features)))
      (if features
          features
          (loop for resource in (flattened-class-tree resource)
                when (slot-value resource 'features)
                return it)))))

(defgeneric has-one-links (resource)
  (:documentation "Yields all has-one links (based on the inheritance
  tree) which link from this resource.")
  (:method ((resource resource))
    ;; TODO: filter out duplicate names?
    (loop for resource in (flattened-class-tree resource)
          append (slot-value resource 'has-one-links))))

(defgeneric has-many-links (resource)
  (:documentation "Yields all has-many links (based on the inheritance
  tree) which link from this resource.")
  (:method ((resource resource))
    ;; TODO: filter out duplicate names?
    (loop for resource in (flattened-class-tree resource)
          append (slot-value resource 'has-many-links))))

(defgeneric flattened-ld-class-tree (resource)
  (:documentation "Yields a list of all ld-class specifications for
  the supplied resource, most specific ld-class first.")
  (:method ((resource resource))
    (remove-if-not #'identity
                   (mapcar #'ld-class (flattened-class-tree resource)))))

(defgeneric ld-properties (resource)
  (:documentation "Yields the ld-properties which apply to this
  resource.  Either because they have been defined on this resource or
  because they are defined on one of its parents.

  MAY contain duplicates.")
  (:method ((resource resource))
    (if (slot-boundp resource 'ld-properties-cache)
        (slot-value resource 'ld-properties-cache)
        (setf (slot-value resource 'ld-properties-cache)
              ;; We choose not to remove duplicates here because we doubt this
              ;; will occur often.
              (loop for resource in (flattened-class-tree resource)
                    append (progn
                             (and (slot-boundp resource 'ld-properties)
                                  (slot-value resource 'ld-properties))))))))

(defun resource-distance (parent child)
  "Yields the distance between the child and parent resources, or nil
  if there is no such relation."
  (position parent (flattened-class-tree child)))

(defgeneric subclass-resources (resource)
  (:documentation "Yields an ordered list of subclasses of this resource, including itself.

   The most specific resource is listed first.")
  (:method ((resource resource))
    (if (slot-boundp resource 'resource-subclass-cache)
        (slot-value resource 'resource-subclass-cache)
        (setf (slot-value resource 'resource-subclass-cache)
              (mapcar #'cdr
                      (sort
                       (loop for maybe-subresource being the hash-values of *resources*
                             for distance = (resource-distance resource maybe-subresource)
                             when distance
                             collect (cons distance maybe-subresource))
                       #'< :key #'car))))))

(defgeneric ld-subclasses (resource)
  (:documentation "Yields a list of ld-classes for the current class
  and all of its subclasses.")
  (:method ((resource resource))
    (remove-if-not #'identity
                   (mapcar #'ld-class (subclass-resources resource)))))

(defmethod json-key ((link has-link))
  (request-path link))

(defgeneric find-link-by-json-name (resource json-link)
  (:documentation "find a has-many link by resource and json-link of the link")
  (:method ((resource resource) json-link)
    (loop for link in (all-links resource)
       if (string= (json-key link) json-link)
       return link)))

(defgeneric all-links (resource)
  (:documentation "Retrieves all links for the supplied resource.
    Both the has-many-links and has-one-links.")
  (:method ((resource resource))
    (append (has-many-links resource) (has-one-links resource))))

(defgeneric all-direct-links (resource)
  (:documentation "Retrieves all direct links for the supplied
    resource.  Both the has-many-links and has-one-links but only
    those defined on the supplied resource.")
  (:method ((resource resource))
    (append (direct-has-many-links resource) (direct-has-one-links resource))))

(defgeneric resource-slot-by-json-key (resource key)
  (:documentation "Returns the slot which should be communicated
    with the json format through the use of the key attribute.")
  (:method ((resource resource) key)
    (alexandria:if-let
        ((resource-slot (loop for slot in (ld-properties resource)
                              when (string= (json-property-name slot) key)
                              return slot)))
      resource-slot
      (error 'no-such-property :resource resource :path key))))

(defgeneric resource-slot-p (resource &key json-key)
  (:documentation "Returns truethy if a resource could be found
    for the supplied definition.")
  (:method ((resource resource) &key json-key)
    (and (find json-key (ld-properties resource)
               :test #'equal
               :key #'json-property-name)
         t)))

(defun find-resource-by-name (symbol)
  "retrieves the resource with name symbol."
  (gethash symbol *resources*))

(defgeneric referred-resource (link)
  (:documentation "retrieves the resource which is referred to by a link")
  (:method ((link has-link))
    (let ((resource (find-resource-by-name (resource-name link))))
      (unless resource
        (error 'configuration-error
               :description (format nil "Could not find resource for link on path \"~A\".  Searched resource is ~A.  Common possibilities are that there's a (define-resource ~A ...) block missing or that the first argument of a :has-one or :has-many specification has a typo."
                                    (request-path link)
                                    (resource-name link) (resource-name link))))
      resource)))

(defgeneric expanded-ld-link (link)
  (:documentation "Expanded version of the ld-link of the has-link")
  (:method ((link has-link))
    (full-uri (ld-link link))))

(defun find-resource-by-path (path)
  "finds a resource based on the supplied request path"
  (maphash (lambda (name resource)
             (declare (ignore name))
             (when (string= (request-path resource) path)
               (return-from find-resource-by-path resource)))
           *resources*)
  (error 'no-such-resource
         :description (format nil "Path: ~A" path)))

(defun find-resource-by-class-uri (uri)
  "finds a resource based on the supplied class uri"
  (maphash (lambda (name resource)
             (declare (ignore name))
             (when (string= (expanded-ld-class resource) uri)
               (return-from find-resource-by-class-uri resource)))
           *resources*)
  (error 'no-such-instance
         :description (format nil "Uri: ~A" uri)))

(defgeneric find-resource-link-by-path (resource path)
  (:documentation "Finds the link object corresponding to the specified
    resource and the specified path.")
  (:method ((resource resource) path)
    (let ((link (find path (all-links resource)
                      :test (lambda (path link)
                              (string= path (request-path link))))))
      (unless link
        (error 'no-such-link
               :resource resource
               :path path))
      link)))

(defgeneric find-resource-link-by-ld-link (resource ld-link)
  (:documentation "Finds the link object corresponding to the specified
    resource and the specified ld-link value.")
  (:method ((resource resource) ld-link)
    (let ((link (find ld-link (all-links resource)
                      :test (lambda (ld-link link)
                              (string= ld-link (expanded-ld-link link))))))
      (unless link
        (error 'no-such-link
               :resource resource))
      link)))

(defgeneric find-resource-link-by-json-key (resource json-key)
  (:documentation "Finds the link object corresponding to the specified
    resource and the specified json key.")
  (:method ((resource resource) json-key)
    (find-resource-link-by-path resource json-key)))

(defun slots-for-filter-components (resource components)
  "Returns the resource links (and slot) based on the given
   resource and filter-components."
  (flet ((is-property-p (resource component)
           (resource-slot-p resource :json-key component))
         (get-property (resource component)
           (resource-slot-by-json-key resource component))
         (get-link (resource component)
           (find-resource-link-by-json-key resource component)))
    (let ((current-resource resource))
      (loop
         for current-component in components
         collect
           (if (is-property-p current-resource current-component)
               (get-property current-resource current-component)
               (let ((link (get-link current-resource current-component)))
                 (setf current-resource (referred-resource link)) ; set resource of last link
                 link))))))

(defun sparql-pattern-for-filter-components (source-var resource components &optional wildcardp)
  "Constructs the SPARQL pattern for a set of filter components,
   starting from source-var.  If ending in a search statement should
   result in a wildcard search for properties of the element,
   wildcardp should be truethy.

   TODO: Splitting this method into a 'base' which detects the last
   element, a portion which follows constraints for the resource
   paths, and a part which contains the ending constraint for the
   specified (or all) properties, would make this function easier to
   read.  Consumers would likely know which base case to call, hence
   the latter two could receive a wrapper as an entry-point if that
   makes things easier to read in the consumer.

   If the last component of the specification yields a resource,
   rather than a property, the search path will allow for all
   properties in that search path.

   Returns (values sparql-pattern target-variable last-slot slots) in
   which the property-path is the description above, target-variable
   is a sparql variable containing the last most specific match,
   last-slot is the slot or has-link corresponding to the last
   component, and slots is the content from
   slots-for-filter-components as used by this function."
  (let* ((slots (slots-for-filter-components resource components))
         ;; REMOVE (path-components (alexandria:flatten (mapcar #'ld-property-list slots)))
         (last-slot (car (last slots)))
         (ends-in-link-p
          ;; we have a general search if the last element is a link,
          ;; or if no search components were supplied
          (or (typep last-slot 'has-link)
              (eq nil components)))
         (last-resource
          ;; this is relevant only if we end in a link.  it should
          ;; yield the last resource in the list in case we want to do
          ;; a search over all properties
          (and ends-in-link-p
               (if (eq components nil)
                   resource
                   (referred-resource last-slot))))
         (path-slots (if ends-in-link-p slots (butlast slots)))
         (target-variable (s-genvar "values")) ; Will always contain the values on which will be searched.
         ;; following is a loop calculation of which we need te have
         ;; the last-subject-var near the end of this computation.
         (last-subject-var source-var)
         (constraints-to-last-object
          (format nil "~{~A~,^ ~}"
                  (loop for slot in path-slots
                        for object-var = (s-genvar)
                        for type-var = (s-genvar "class")
                        for target-resource = (find-resource-by-name (resource-name slot))
                        collect (prog1
                                    (format nil "~A ~{~A~,^/~} ~A. ~A a ~A. VALUES ~A {~{~A~,^ ~}}. "
                                            last-subject-var (ld-property-list slot) object-var
                                            object-var type-var
                                            type-var (ld-subclasses target-resource))
                                  (setf last-subject-var object-var)))))
         ;; following is another logical flow building on the previous
         ;; which runs when the last element is a resourc and we need
         ;; to search broadly.
         (constraint-to-key (when last-resource
                              (format nil "~A ~{(~{~A~^/~})~^|~} ~A. "
                                      last-subject-var
                                      (mapcar #'ld-property-list
                                              (ld-properties last-resource))
                                      target-variable))))
    (cond ((and ends-in-link-p wildcardp)
           (values (format nil "~A ~A" constraints-to-last-object constraint-to-key)
                   target-variable
                   last-slot
                   slots))
          ((and ends-in-link-p (not wildcardp))
           (values constraints-to-last-object
                   last-subject-var
                   last-slot
                   slots))
          (t ; (not ends-in-link-p)
           (values (format nil "~A ~A ~{~A~^/~} ~A. "
                           constraints-to-last-object
                           last-subject-var (ld-property-list last-slot) target-variable)
                   target-variable
                   last-slot
                   slots)))))

(defun define-resource* (name superclass-names &key ld-class ld-properties ld-resource-base has-many has-one on-path authorization features)
  "defines a resource for which get and set requests exist"
  (let* ((properties (loop for (key type prop . options) in ld-properties
                        collect (make-instance 'resource-slot
                                               :json-key key
                                               :resource-type type
                                               :ld-property prop
                                               :required-p (find :required options))))
         (has-many-links (mapcar (alexandria:curry #'apply #'make-instance 'has-many-link :resource)
                                 has-many))
         (has-one-links (mapcar (alexandria:curry #'apply #'make-instance 'has-one-link :resource)
                                has-one)))
    (make-instance 'resource
                   :superclass-names superclass-names
                   :ld-class ld-class
                   :ld-properties properties
                   :ld-resource-base ld-resource-base
                   :has-many has-many-links
                   :has-one has-one-links
                   :json-type on-path ; (symbol-to-camelcase name :cap-first t)
                   :request-path on-path
                   :authorization authorization
                   :features features
                   :resource-name name)))

(defmacro define-resource (name superclass-names &key class properties resource-base has-many has-one on-path authorization features)
  `(define-resource* ',name ',superclass-names
       :ld-class ,class
       :ld-properties ,properties
       :ld-resource-base ,resource-base
       :has-many ,has-many
       :has-one ,has-one
       :on-path ,on-path
       :features ,features
       :authorization ,authorization))

(defun property-paths-format-component (resource)
  (declare (ignore resource))
  "~{~&~4t~{~A~,^/~} ~A~,^;~}.")
(defun property-paths-content-component (resource json-input)
  (let ((attributes (jsown:filter json-input "data" "attributes")))
    (loop for slot
       in (ld-properties resource)
       if (jsown:keyp attributes (json-property-name slot))
       collect (list (ld-property-list slot)
                     (interpret-json-value
                      slot
                      (jsown:val attributes (json-property-name slot)))))))

(defun attribute-properties-for-json-input (resource json-input)
  "Retrieves the values for the attributes of the supplied json
   input which will be filled in <resource> as an object.  For
   each found key, the property list is connected to the value
   which should be set."
  (let ((attributes
         (if (jsown:keyp (jsown:val json-input "data") "attributes")
             (jsown:filter json-input "data" "attributes")
             (jsown:empty-object))))
    (loop for slot
       in (ld-properties resource)
       for attribute-supplied-p =
         (jsown:keyp attributes (json-property-name slot))
       for primitive-value =
         (and attribute-supplied-p
            (jsown:val attributes (json-property-name slot)))
       if attribute-supplied-p
       collect
         (list (ld-property-list slot)
               (if (slot-value-represents-triples-p slot primitive-value)
                   (interpret-json-value
                    slot
                    (jsown:val attributes (json-property-name slot)))
                   :null)))))

(defgeneric construct-resource-item-path (item-spec)
  (:documentation "Constructs the path on which information can
   be fetched for a specific instance of a resource.")
  (:method ((item-spec item-spec))
    (format nil "/~A/~A"
            (request-path (resource item-spec))
            (uuid item-spec))))
