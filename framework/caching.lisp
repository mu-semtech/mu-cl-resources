(in-package :mu-cl-resources)

;; UUID URI cache
(defparameter *uuid-uri-cache* (make-user-aware-hash-table :test #'equal)
  "Cache which connects UUIDs to URIs.")

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

(defparameter *uri-uuid-cache* (make-user-aware-hash-table :test #'equal)
  "Cache which connects URIs to UUIDs.")

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

(defun find-uuid-for-uri-through-sparql (item-spec)
  "retrieves the uuid for a uri through a sparql query.
   @see: you probably want to use uuid instead."
  (let ((result (sparql:select (s-var "uuid")
                               (format nil "~A mu:uuid ?uuid. " (s-url (node-url item-spec))))))
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
  (:method ((item-spec item-spec))

    (rem-ua-hash (uuid item-spec) *cached-resources*)
    (clear-uri-cache-for-uuid (uuid item-spec)))
  (:method ((uuid string))
    (rem-ua-hash uuid *cached-resources*)
    (clear-uri-cache-for-uuid uuid)))


;; Cache for classes belonging to URI, currently scoped to a single
;; request and used by .mu/delta.
(defmacro with-request-uri-cache (&body body)
  "Executes body with a cache for find-classes-for-uri"
  `(let ((*request-uri-cache* (make-hash-table :test 'equal)))
     (declare (special *request-uri-cache*))
     ,@body body))

(defun find-classes-for-uri (uri)
  "Finds all classes for a given uri"
  ;; TODO: this should be cached
  (declare (special *request-uri-cache*))
  (flet ((find-uri-classes-from-db (uri)
           (jsown:filter
            (sparql:select (s-distinct (s-var "target"))
                           (format nil "~A a ?target."
                                   (s-url uri)))
            map "target" "value")))
    (if *request-uri-cache*
        (multiple-value-bind (value present-p)
            (gethash (princ-to-string (s-url uri)) *request-uri-cache*)
          (if present-p
              value
              (setf (gethash (princ-to-string (s-url uri)) *request-uri-cache*)
                    (find-uri-classes-from-db uri))))
        (find-uri-classes-from-db uri))))

