(in-package :mu-cl-resources)


(defun debug-execute-breakpoint (name content code)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (break "~A yielded: ~A ~&~A" name content code))

(defmacro debug-break (name &body block)
  "Executes the code in <block> and reports it as a break-point
   with name <name>."
  (let ((output-symbol (gensym (string-downcase (symbol-name name)))))
    `(progn (declaim (optimize (debug 3) (safety 3) (speed 0)))
        (let ((,output-symbol (progn ,@block)))
          (debug-execute-breakpoint ',name ,output-symbol ',block)
          ,output-symbol))))

(defmacro deprecated ((&key silent &allow-other-keys) &body body)
  "Indicates <body> is code with a deprecation.  The description
   can be used to describe how the deprecation works.  As nothing
   is done with the surrounding code, the options of description
   are yet to be determined."
  (declare (ignore silent))
  `(progn ,@body))

(defun symbol-to-camelcase (content &key (cap-first nil))
  "builds a javascript variable from anything string-like"
  (format nil "~{~A~}"
          (let ((cap-next cap-first))
            (loop for char across (string-downcase (string content))
               if (char= char #\-)
               do (setf cap-next t)
               else collect (prog1
                                (if cap-next
                                    (char-upcase char)
                                    char)
                              (setf cap-next nil))))))

(defun merge-jsown-objects (a &rest bs)
  "Merges n jsown objects.  Returns a new object which contains
   the merged contents."
  (flet
      ((merge-two-objects (a b)
         (let ((keys (union (jsown:keywords a) (jsown:keywords b) :test #'string=))
               (result (jsown:empty-object)))
           (loop for key in keys
              do (cond ((and (jsown:keyp a key)
                           (not (jsown:keyp b key)))
                        (setf (jsown:val result key)
                              (jsown:val a key)))
                       ((and (not (jsown:keyp a key))
                           (jsown:keyp b key))
                        (setf (jsown:val result key)
                              (jsown:val b key)))
                       (t (handler-case
                              (setf (jsown:val result key)
                                    (merge-jsown-objects (jsown:val a key)
                                                         (jsown:val b key)))
                            (error () (setf (jsown:val result key)
                                            (jsown:val b key)))))))
           result)))
    (if bs
        (apply #'merge-jsown-objects
               (merge-two-objects a (first bs))
               (rest bs))
        a)))

(defun plist-remove-nil (plist)
  "Removes settings which are nil from <plist>."
  (loop for (key value) on plist by #'cddr
     if value
     append (list key value)))

(defun build-url (base-url request-params)
  "Constructs a simple url.  Request-params should contain
  lists of options.  Options which contain nil as their value
  are removed.
  eg: (build-url \"/taxonomies\" `((\"page[number]\" 42) (\"page[size]\" 3)))"
  (let ((parameters (plist-remove-nil request-params)))
    (if parameters
        (format nil "~A?~{~A=~A~,^&~}" base-url parameters)
        base-url)))

(defun alist-to-plist (alist)
  "Converts an alist to a plist"
  (loop for (k . v) in alist
     append (list k v)))

(define-setf-expander getfstr (place key &environment env)
  "see (setf getf) and val"
  (multiple-value-bind (*temps *vals *store-vars *setter *getter)
      (get-setf-expansion place env)
    (let ((value-v (gensym "value-v"))
          (key-v (gensym "key-v"))
          (result-v (gensym "result-v")))
      (values (list* key-v *temps) ;; key-v will be set to key
              (list* key *vals)    ;; <- because of this
              (list  value-v)      ;; contains the value to be set
              `(let ((,result-v (fn-update-getfstr ,*getter ,key-v ,value-v)))
                 (let ((,(first *store-vars) ,result-v))
                   ,*setter)
                 ,value-v)
              `(getfstr ,*getter ,key-v)))))

(defun getfstr (place key)
  "getf, but for strings."
  (loop for (k v) on place by #'cddr
     if (and (stringp k)
             (string= k key))
     return v))

(defun fn-update-getfstr (place key new-value)
  "updates getfstr in a functional way"
  (let ((keys (loop for k in place by #'cddr collect k)))
    (if (find key keys :test #'equal)
        (loop for (k v) on place by #'cddr
           if (equal k key)
           append (list k new-value)
           else
           append (list k v))
        (list* key new-value place))))

;;;;
;; item specs

(defclass item-spec ()
  ((uuid :initarg :uuid)
   (type :accessor resource-name :initarg :type)
   (related-items :accessor related-items-table
                  :initform (make-hash-table :test 'equal #-abcl :synchronized #-abcl t)
                  :initarg :related-items)
   (node-url :initarg :node-url))
  (:documentation "Represents an item that should be loaded."))

(defun make-item-spec (&rest args &key uuid type node-url)
  "Creates a new item-spec instance."
  (declare (ignore uuid type node-url))
  (apply #'make-instance 'item-spec args))

(defun item-spec-hash-key (item-spec)
  "Creates a key which can be compared through #'equal."
  (list (resource-name item-spec) (uuid item-spec)))

(defgeneric resource (item-spec)
  (:documentation "Yields the specified resource for this item-spec.")
  (:method ((spec item-spec))
    (find-resource-by-name (resource-name spec))))

(defun item-spec-current-ld-classes (item-spec)
  "Yields the current LD classes the triplestore has on the given item-spec."
  (current-classes-for-uri (node-url item-spec)))

(defun current-classes-for-uri (uri)
  "Yields the classes which the database has for the current class's URL"
  ;; TODO: add caching for these classes, populate by caching during
  ;; creation, clear by delta messages and delete calls.
  (mapcar #'s-url
          (jsown:filter (sparql:select (s-var "class")
                                       (format nil "~A a ?class" (s-url uri)))
                        map "class" "value")))

(defgeneric most-specific-applicable-resource (item-spec)
  (:documentation "Yields the most specific resource based on the
  types of the supplied entity.  Assumes the resource could be found
  in the database.")
  (:method ((spec item-spec))
    ;; TODO: cache the types for the resources and clear that cache on
    ;; delta messages about the resource.
    (let ((specified-ld-classes (item-spec-current-ld-classes spec))
          (subresources (subclass-resources (resource spec))))
      ;; Subresources are ordered most broad to most specific.  As
      ;; such, we need to find the last subresource for which we have
      ;; the class in our set of specified-ld-classes.
      (loop for resource in (reverse subresources)
            for resource-class = (s-url (full-uri (ld-class resource)))
            if (find resource-class specified-ld-classes
                        :test (lambda (a b)
                                (string= (princ-to-string a) (princ-to-string b))))
            return resource))))

(defgeneric related-items (item-spec relation)
  (:documentation "Returns the related items for the given relation")
  (:method ((item-spec item-spec) relation)
    (gethash relation (related-items-table item-spec) nil)))


;;;;
;; included items store
(defstruct included-items-store
  (table (make-hash-table :test 'equal #-abcl :synchronized #-abcl t))
  (lock (bordeaux-threads:make-lock)))

(defun included-items-store-contains (store item-spec)
  "Returns item-spec iff <item-spec> is included in <store>.
   Returns nil otherwise"
  (gethash (item-spec-hash-key item-spec) (included-items-store-table store)))

(defgeneric included-items-store-ensure (store ensured-content)
  (:documentation "Ensures <item-spec> is contained in <store>.
   If an <item-spec> which matches the same item-spec-hash-key is
   already stored, then the one from the store is returned,
   otherwise, the new ensured-content is returned.")
  (:method ((store included-items-store) (item-spec item-spec))
    (bordeaux-threads:with-lock-held ((included-items-store-lock store))
      (let ((table (included-items-store-table store))
            (key (item-spec-hash-key item-spec)))
        (or (gethash key table)
            (setf (gethash key table) item-spec)))))
  (:method ((store included-items-store) (new-items included-items-store))
    (loop for item-spec in (included-items-store-list-items new-items)
       collect
         (included-items-store-ensure store item-spec))))

(defgeneric included-items-store-subtract (store subtracted-content)
  (:documentation "Subtracts <subtracted-content> from <store>.")
  (:method ((store included-items-store) (item-spec item-spec))
    (remhash (item-spec-hash-key item-spec)
             (included-items-store-table store)))
  (:method ((store included-items-store) (subtracted-store included-items-store))
    (bordeaux-threads:with-lock-held ((included-items-store-lock store))
      (mapcar (alexandria:curry #'included-items-store-subtract store)
              (included-items-store-list-items subtracted-store)))))

(defun included-items-store-list-items (store)
  "Retrieves all items in the included-items-store"
  (bordeaux-threads:with-lock-held ((included-items-store-lock store))
    (loop for item-spec being the hash-values of (included-items-store-table store)
       collect item-spec)))

(defun make-included-items-store-from-list (items-list)
  "Constructs a new included items store containing the list of
   items in <items-list>."
  (let ((store (make-included-items-store)))
    (mapcar (alexandria:curry #'included-items-store-ensure store)
            items-list)
    store))


;;;;;;;;;;;;;;;;;;;;;;;;
;; user aware hash-table
;;
;;
;;  This table supporst similar functions as a regular hash-tables.
;;  The naming becomes minimally different in order to differentiate
;;  with regular hash functions.  We have added a ua- prefix to each
;;  of the methods we currently support.  The logical idea behind
;;  these methods should be the same as the original ones.
;;
;;  | NAME             | SUPPORTED? | NAME                       |
;;  |------------------+------------+----------------------------|
;;  | clrhash          | no         |                            |
;;  | hash-table-p     | no         |                            |
;;  | remhash          | yes        | rem-ua-hash                |
;;  | gethash          | yes        | get-ua-hash                |
;;  | make-hash-table  | yes        | make-user-aware-hash-table |
;;  | sxhash           | no         |                            |
;;  | hash-table-count | no         |                            |
;;  | maphash          | no         |                            |


(defclass user-aware-hash-table ()
  ((hash-table :initarg :hash-table
               :documentation "top-level hash-table.  This contains
               the keys supplied by the user and will be populated by
               child hash-tables to contain the contents based on the
               current user's access rights."))
  (:documentation "Table which behaves differently based on the
  currently known mu-auth-allowed-groups and mu-auth-used-groups.

  The goal is to provide a high-level interface which makes the
  intended use of the hash-table clear, thereby pushing the logic of
  setting and clearing content into this component, rather than in the
  earlier logic."))

(defparameter *user-aware-hash-table-default-options*
  '(#-abcl :synchronized #-abcl t)
  "Default options for the user-aware-hash-table's internal tables.
  Sets synchronization to truethy when available in the
  implementation.  We don't expect this to be user-configurable, yet
  shadowing may come in handy at some point.")

(defun make-user-aware-hash-table (&rest options &key test size rehash-size rehash-threshold &allow-other-keys)
  "Constructs a new user-aware hash-table.
   Equivalent of make-hash-table."
  (declare (ignore test size rehash-size rehash-threshold)) ; only used for slime documentation
  (let ((all-table-options (append options *user-aware-hash-table-default-options*)))
    (make-instance 'user-aware-hash-table
                   :hash-table (apply #'make-hash-table all-table-options))))

(defun rem-ua-hash (key user-aware-hash-table)
  "Equivalent of remhash.
   Clears a value from the stored hash.  As we currently don't really
   interpret the mu-allowed-groups and mu-auth-used-groups, we remove
   the contents everywhere."
  (with-slots (hash-table) user-aware-hash-table
    (remhash key hash-table)))

(defun get-user-allowed-groups ()
  "Yields the allowed groups for the current user in the format
   expected by the user-aware-hash-table."
  (declare (special *user-allowed-groups*))
  (if (boundp '*user-allowed-groups*)
      *user-allowed-groups*
      (let ((out-headers (hunchentoot:headers-out*))
            (in-headers (hunchentoot:headers-in*)))
        (let ((allowed-groups-out-header (assoc :mu-auth-allowed-groups out-headers))
              (allowed-groups-in-header (assoc :mu-auth-allowed-groups in-headers)))
          (cond
            ;; if we received new mu-auth-allowed-groups from the database, return them
            (allowed-groups-out-header (cdr allowed-groups-out-header))
            ;; otherwise see if we have received the headers from the client
            (allowed-groups-in-header (cdr allowed-groups-in-header))
            ;; if nothing is available, yield nil as the key
            (t nil))))))

(defun get-ua-hash (key user-aware-hash-table &optional default)
  "Equivalent of gethash."
  ;; As we don't interpret the value yet, we simply have to check
  ;; whether there's a hash within key we're looking for.  We don't
  ;; parse the keys yet, greatly limiting the complexity of this
  ;; method.
  (let ((allowed-groups (get-user-allowed-groups)))
    (with-slots (hash-table) user-aware-hash-table
      (multiple-value-bind (nested-hash nested-hash-p)
          (gethash key hash-table)
        (if nested-hash-p
            (gethash allowed-groups nested-hash default)
            (values default nil))))))

(defun (setf get-ua-hash) (value key user-aware-hash-table &optional default)
  "Equivalent of (setf gethash)"
  ;; When trying to set the value, we first have to see whether there
  ;; is a hash-table available for our current groups.  If there is,
  ;; we can use it, if there isn't, we first have to create it.
  (let ((allowed-groups (get-user-allowed-groups)))
    ;; TODO: this should become mu-auth-used-groups when that becomes
    ;; supported by mu-authorization.

    ;; TODO: if the table is synchronized, we should set up a lock for
    ;; this operation.  as it stands, it may accidentally lose data.
    ;; This is likely not the worst in a practical case, but it
    ;; doesn't look nice.
    (with-slots (hash-table) user-aware-hash-table
      (multiple-value-bind (nested-hash nested-hash-p)
          (gethash key hash-table)
        (if nested-hash-p
            ;; if there's a hash, we can just set the value
            (setf (gethash allowed-groups nested-hash default) value)
            ;; if there's no hash, we need to set up a new hash and
            ;; configure that.
            (let ((new-hash-table
                   (make-hash-table :test 'equal #-abcl :synchronized #-abcl t)))
              (setf (gethash key hash-table)
                    new-hash-table)
              (setf (gethash allowed-groups new-hash-table default)
                    value)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; count queries cache
(defun make-count-cache-keys (link-spec)
  (list (and link-spec (resource-name link-spec))
        (and link-spec (uuid link-spec))
        (sorted-filters)))

(defun count-cache (resource link-spec)
  "Retrieves the amount of solutions from the cache"
  (let ((keys (make-count-cache-keys link-spec))
        (hash (query-count-cache resource)))
    (let ((cached-result (get-ua-hash keys hash)))
      (format t "Cache count for ~A ~A is ~A~%"
              (resource-name resource) keys
              cached-result)
      cached-result)))

(defun (setf count-cache) (value resource link-spec)
  "Sets the amount of solutions in the cache.  Returns the set
   amount."
  (let ((keys (make-count-cache-keys link-spec))
        (hash (query-count-cache resource)))
    (format t "Setting cached count for ~A ~A to ~A~%"
            (resource-name resource) keys
            value)
    (setf (get-ua-hash keys hash) value)))

(defun sorted-filters ()
  (sort-filters-for-caching (extract-filters-from-request)))

(defun sort-filters-for-caching (filters)
  (sort filters
        (lambda (x y)
          (labels ((c (filter) (apply #'s+ (getf filter :components)))
                   (s (filter) (write-to-string (getf filter :search))))
            (or (string-lessp (c x) (c y))
                (and (equal (c x) (c y))
                     (string-lessp (s x) (s y))))))))

(defun clear-cached-count-queries (resource)
  ;; TODO: this approach is overly aggressive.  If we understand the
  ;; cache keys, we could keep the cache around for calls which could
  ;; not be affected by the groups from which these calls appeared.
  (setf (query-count-cache resource)
        (make-user-aware-hash-table :test 'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; relevant relations store
(defstruct cache-store
  (cache-keys (make-hash-table :test 'equal #-abcl :synchronized #-abcl t))
  (clear-keys (make-hash-table :test 'equal #-abcl :synchronized #-abcl t))
  (cancel-cache nil))

(defun add-cache-key (&rest cache-key)
  "Adds a key to the set of cache keys"
  (declare (special *cache-store*))
  (let ((key (cache-key-to-jsown cache-key)))
    (setf (gethash (jsown:to-json key)
                   (cache-store-cache-keys *cache-store*))
          key)))

(defun add-clear-key (&rest clear-key)
  "Adds a key to the set of clear keys"
  (declare (special *cache-store*))
  (let ((key (cache-key-to-jsown clear-key)))
    (setf (gethash (jsown:to-json key)
                   (cache-store-clear-keys *cache-store*))
          key)))

(defun cache-key-to-jsown (cache-key)
  "Converts the supplied cache-key to json"
  (let ((obj (jsown:empty-object)))
    (loop for (key value) on cache-key by #'cddr
       do (setf (jsown:val obj (string-downcase (string key)))
                (format nil "~A" value)))
    obj))

(defun cache-class (resource)
  (add-cache-key :ld-resource (expanded-ld-class resource)))

(defun cache-object (item-spec)
  (add-cache-key :uri (node-url item-spec)))

(defun cache-relation (item-spec relation)
  (add-cache-key :uri (node-url item-spec)
                 :ld-relation (expanded-ld-relation relation))
  ;; for clearing of inverse relationships
  (add-cache-key :ld-resource (expanded-ld-class (resource item-spec))
                 :ld-relation (expanded-ld-relation relation)))

(defun cache-clear-class (resource)
  (clear-cached-count-queries resource)
  (add-clear-key :ld-resource (expanded-ld-class resource)))

(defun cache-clear-object (item-spec)
  (clear-solution item-spec)
  (add-clear-key :uri (node-url item-spec)))

(defun cache-clear-relation (item-spec relation)
  (add-clear-key :uri (node-url item-spec)
                 :ld-relation (expanded-ld-relation relation))
  ;; for clearing of inverse relationships
  (dolist (inverse-relation (inverse-links relation))
    (add-clear-key :ld-resource (expanded-ld-class (getf inverse-relation :resource))
                   :ld-relation (expanded-ld-relation (getf inverse-relation :link)))))



(defgeneric cache-on-class-list (resource)
  (:documentation "Adds the cache class to the current cache-store")
  (:method ((json-type string))
    (add-cache-key :ld-class
                   (expanded-ld-class (find-resource-by-path json-type)))))

(defgeneric cache-on-resource (resource)
  (:documentation "Caches on a specific resource, like an item-spec")
  (:method ((item-spec item-spec))
    (add-cache-key :uri (node-url item-spec))))

(defun cache-on-resource-relation (item-spec link)
  "Caches on the specified resource and its accompanying relationship."
  (cache-relation item-spec link)
  (cache-on-class-list (find-resource-by-name (resource-name link))))

(defun reset-cache-for-resource-relation (item-spec link)
  "Resets the cache for the specified resource and its
   accompanying relationship."
  (cache-clear-relation item-spec link)
  (cache-clear-class (find-resource-by-name (resource-name link))))

(defun cancel-cache ()
  "Cancel the cache.  Use this for resources which have access
   rights attached to them and which may not be shown to all
   users."
  (declare (special *cache-store*))
  (setf (cache-store-cancel-cache *cache-store*) t))

(defun cache-store-set-headers ()
  "Sets the cache headers on the response based on the current
   cache-store."
  (declare (special *cache-store*))
  (unless (or (cache-store-cancel-cache *cache-store*)
              (not *supply-cache-headers-p*))
    (alexandria:when-let
        ((cache-keys (loop for value being the hash-values of
                          (cache-store-cache-keys *cache-store*)
                        collect value)))
      (setf (webserver:header-out :cache-keys)
            (jsown:to-json cache-keys)))
    (alexandria:when-let
        ((clear-keys
          (loop for value being the hash-values of
               (cache-store-clear-keys *cache-store*)
             collect value)))
      (setf (webserver:header-out :clear-keys)
            (jsown:to-json clear-keys)))))

(defmacro with-cache-store (&body body)
  "Executes body with a context in which the cache-store is set.
   The resulting headers are set on the response just after body
   has been executed."
  `(let ((*cache-store* (make-cache-store)))
     (declare (special *cache-store*))
     (prog1 (progn ,@body)
       (cache-store-set-headers))))
