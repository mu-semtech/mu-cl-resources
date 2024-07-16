(in-package :mu-cl-resources)

;; INVARIANT01: when creating resources, the cache will be populated
;; with uuid -> uri and uri -> ld-classes.  If the cache does not
;; contain this information then it must be available in the
;; triplestore.  This invariant and the resulting partial queries become
;; less of an issue as the delta-notifier becomes smarter.

;; UUID URI cache
(defparameter *uuid-uri-cache* (make-user-aware-hash-table :test #'equal)
  "Cache which connects UUIDs to URIs.")

(defparameter *uri-uuid-cache* (make-user-aware-hash-table :test #'equal)
  "Cache which connects URIs to UUIDs.")

(defun cached-uri-for-uuid (uuid)
  "Returns the cached uri for the supplied resource"
  (get-ua-hash uuid *uuid-uri-cache*))

(defun (setf cached-uri-for-uuid) (uri uuid)
  (setf (get-ua-hash uri *uri-uuid-cache*) uuid)
  (setf (get-ua-hash uuid *uuid-uri-cache*) uri))

(defun clear-uri-cache-for-uuid (uuid)
  "Clears the URI cache for the supplied UUID"
  (let ((uri (cached-uri-for-uuid uuid)))
    (when uri
      (rem-ua-hash uri *uri-uuid-cache*)))
  (rem-ua-hash uuid *uuid-uri-cache*))

(defun cached-uuid-for-uri (uri)
  "Returns the cached uuid for the supplied uri"
  (get-ua-hash uri *uri-uuid-cache*))

(defun (setf cached-uuid-for-uri) (uuid uri)
  (setf (get-ua-hash uuid *uuid-uri-cache*) uri)
  (setf (get-ua-hash uri *uri-uuid-cache*) uuid))

(defun clear-uuid-cache-for-uri (uri)
  "Clears the URI cache for the supplied UUID"
  (let ((uuid (cached-uuid-for-uri uri)))
    (when uuid
      (rem-ua-hash uuid *uuid-uri-cache*)))
  (rem-ua-hash uri *uri-uuid-cache*))


(defun find-resource-for-uuid-through-cache-or-sparql (item-spec)
  "Retrieves the resource's URI from either the current cache, or
   by querying the SPRAQL endpoint."
  (or (cached-uri-for-uuid (uuid item-spec))
      (let ((uri (find-resource-for-uuid-through-sparql item-spec)))
        (setf (cached-uri-for-uuid (uuid item-spec)) uri)
        uri)))

(defun find-uuid-for-uri-through-cache-or-sparql (item-spec)
  "Retrieves the uuid from either the current cache, or by querying
   the SPRAQL endpoint."
  (or (cached-uuid-for-uri (node-url item-spec))
     (let ((uuid (find-uuid-for-uri-through-sparql item-spec)))
       (setf (cached-uuid-for-uri (node-url item-spec)) uuid)
       uuid)))

(defun find-resource-for-uuid-through-sparql (item-spec)
  "retrieves the resource url through a sparql query.
   @see: you probably want to use node-url instead."
  (let ((result
          (sparql:without-update-group
            (sparql:select (s-var "s")
                           (if *allow-xsd-in-uuids*
                               (format nil (s+ "?s mu:uuid ?uuid. "
                                               "FILTER(~A = str(?uuid))")
                                       (s-str (uuid item-spec)))
                               (format nil "?s mu:uuid ~A. " (s-str (uuid item-spec))))))))
    (unless result
      (error 'no-such-instance
             :id (uuid item-spec)))
    (jsown:filter (first result) "s" "value")))

(defun find-uuid-for-uri-through-sparql (item-spec)
  "retrieves the uuid for a uri through a sparql query.
   @see: you probably want to use uuid instead."
  (let ((result
          (sparql:without-update-group
            (sparql:select (s-var "uuid")
                           (format nil "~A mu:uuid ?uuid. " (s-url (node-url item-spec)))))))
    (unless result
      (error 'no-such-instance
             :uri (node-url item-spec)))
    (jsown:filter (first result) "uuid" "value")))


;; item specs
(defgeneric node-url (item-spec)
  (:documentation "yields the node url for the supplied item-spec")
  (:method ((item-spec item-spec))
    (if (slot-boundp item-spec 'node-url)
        (slot-value item-spec 'node-url)
        (setf (slot-value item-spec 'node-url)
              (find-resource-for-uuid-through-cache-or-sparql item-spec)))))

(defgeneric uuid (item-spec)
  (:documentation "yields the node uuid for the supplied item-spec")
  (:method ((item-spec item-spec))
    (if (slot-boundp item-spec 'uuid)
        (slot-value item-spec 'uuid)
        (setf (slot-value item-spec 'uuid)
              (find-uuid-for-uri-through-cache-or-sparql item-spec)))))


;; Cached resources
(defparameter *cached-resources* (make-user-aware-hash-table :test 'equal)
  "Cache of solutions which were previously fetched or initialized.
   The resources might not be complete yet, and can be finished.
   The keys are the UUIDs the vaue is the cached resource.")

(defun cache-model-properties-p (item-spec)
  "Yields a truethy result iff the model properties for the supplied
   item-spec should be cached."
  (or *cache-model-properties-p*
     (find 'cache-model-properties (features (resource item-spec)))))

(defun ensure-solution (item-spec)
  "Ensures a solution exists for <item-spec> and returns it."
  (if (cache-model-properties-p item-spec)
      (or (get-ua-hash (uuid item-spec) *cached-resources*)
         (setf (get-ua-hash (uuid item-spec) *cached-resources*)
               (make-instance 'solution)))
      (make-instance 'solution)))

(defgeneric clear-solution (spec)
  (:documentation "Clears the solution from the given specification
   accepts both an item-spec as well as a uuid")
  (:method ((uuid string))
    (clear-solution (make-item-spec :uuid uuid)))
  (:method ((item-spec item-spec))
    ;; In each of these cases, if the item-spec could not be found, it
    ;; does not exist in our cache and it does not exist in the story
    ;; anymore.  The best we can do is ignore the clear.
    (handler-case
        (rem-ua-hash (uuid item-spec) *cached-resources*)
      (no-such-instance () nil))
    (handler-case
        (clear-uri-cache-for-uuid (uuid item-spec))
      (no-such-instance () nil))
    (let (retried-p)
      (handler-case
          (clear-cached-count-queries (resource item-spec))
        ;; try the restart once, then skip on failures
        (resource-type-not-found-for-item-spec (e)
          (unless retried-p
            (setf retried-p t)
            ;; (format t "~&Could not find resource-type for ~A when clearing cached count, fetching and retrying~%"
            ;;         item-spec)
            (handler-case
                (invoke-restart 'retry-after-clearing-ld-classes)
              (error (re)
                nil
                ;; (format t "~&Failed to invoke retry-after-clearing-ld-classes in clear-solution because it does not exist for ~A when erroring as ~A then ~A~%" item-spec e re)
                ))))))))

(defvar *uri-classes-cache* (lhash:make-castable :test 'equal)
  "Contains a mapping from URI strings to the URI classes belonging to the URI.")

(defun classes-for-uri-p (uri)
  "Yields truethy iff there are classes for the given URI"
  (second (multiple-value-list (lhash:gethash uri *uri-classes-cache*))))

(defun classes-for-uri (uri)
  "Finds all classes for a given uri"
  (multiple-value-bind (uris present-p)
      (lhash:gethash uri *uri-classes-cache*)
    (if present-p
        uris
        (setf (lhash:gethash uri *uri-classes-cache*)
              (jsown:filter
               (sparql:without-update-group
                 (sparql:select (s-distinct (s-var "target"))
                                (format nil "~A a ?target."
                                        (s-url uri))
                                :no-graph t))
               map "target" "value")))))

(defun (setf classes-for-uri) (value uri)
  (setf (lhash:gethash uri *uri-classes-cache*) value))

(defun add-cached-class-for-uri (uri class)
  "Adds a cached class for a the given uri.  Handy for delta
  messages."
  (if (classes-for-uri-p uri)
      (pushnew class (lhash:gethash uri *uri-classes-cache*) :test #'string=)
      (setf (classes-for-uri uri) (list class))))

(defun remove-cached-class-for-uri (uri class)
  "Removes a cached class for the given URI.  Handy for delta
  messages."
  (setf (lhash:gethash uri *uri-classes-cache*)
        (delete class (lhash:gethash uri *uri-classes-cache*) :test #'string=)))

(defun clear-cached-classes-for-uri (uri)
  "Removes all cached clasess for a given uri"
  (lhash:remhash uri *uri-classes-cache*))
