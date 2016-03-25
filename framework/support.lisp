(in-package :mu-cl-resources)


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

(defun merge-jsown-objects (a b)
  "Merges jsown objects a and b together.  Returns a new object
   which contains the merged contents."
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
    result))

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
  ((uuid :accessor uuid :initarg :uuid)
   (type :accessor resource-name :initarg :type)
   (related-items :accessor related-items-table
                  :initform (make-hash-table :test 'equal)
                  :initarg :related-items)
   (node-url))
  (:documentation "Represents an item that should be loaded."))

(defun make-item-spec (&key uuid type)
  "Creates a new item-spec instance."
  (make-instance 'item-spec :type type :uuid uuid))

(defun item-spec-hash-key (item-spec)
  "Creates a key which can be compared through #'equal."
  (list (resource-name item-spec) (uuid item-spec)))

(defmethod resource ((spec item-spec))
  (find-resource-by-name (resource-name spec)))

(defgeneric related-items (item-spec relation)
  (:documentation "Returns the related items for the given relation")
  (:method ((item-spec item-spec) relation)
    (gethash relation (related-items-table item-spec) nil)))


;;;;
;; included items store
(defstruct included-items-store
  (table (make-hash-table :test 'equal)))

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
    (let ((table (included-items-store-table store))
          (key (item-spec-hash-key item-spec)))
      (or (gethash key table)
          (setf (gethash key table) item-spec))))
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
    (mapcar (alexandria:curry #'included-items-store-subtract store)
            (included-items-store-list-items subtracted-store))))

(defun included-items-store-list-items (store)
  "Retrieves all items in the included-items-store"
  (loop for item-spec being the hash-values of (included-items-store-table store)
     collect item-spec))

(defun make-included-items-store-from-list (items-list)
  "Constructs a new included items store containing the list of
   items in <items-list>."
  (let ((store (make-included-items-store)))
    (mapcar (alexandria:curry #'included-items-store-ensure store)
            items-list)
    store))

