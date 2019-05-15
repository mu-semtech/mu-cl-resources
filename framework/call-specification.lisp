(in-package :mu-cl-resources)

(setf jsown:*parsed-null-value* :null)

(defun response-for-access-denied-condition (condition)
  (respond-access-denied
   (jsown:new-js
     ("errors" (jsown:new-js
                 ("title" (format nil "Access for ~A operation on ~A ~@[with id ~A ~]was denied."
                                  (operation condition)
                                  (json-type (resource condition))
                                  (target-id condition))))))))

(defcall :get (base-path)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (list-call (find-resource-by-path base-path)))
    (no-such-resource ()
      (respond-not-found))
    (access-denied (condition)
      (response-for-access-denied-condition condition))
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))))

(defcall :get (base-path id)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (show-call (find-resource-by-path base-path) id))
    (no-such-resource ()
      (respond-not-found))
    (access-denied (condition)
      (response-for-access-denied-condition condition))
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (no-such-instance ()
      (respond-not-found))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))))

(defcall :post (base-path)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (verify-request-contains-type body)
          (verify-request-contains-no-id body)
          (verify-request-type-matches-path base-path body)
          (verify-request-required-properties base-path body)
          (create-call (find-resource-by-path base-path)))
      (no-such-resource ()
        (respond-forbidden (jsown:new-js
                             ("errors" (jsown:new-js
                                         ("title" (format nil
                                                          "Resource for path (~A) not found"
                                                          base-path)))))))
      (access-denied (condition)
        (response-for-access-denied-condition condition))
      (required-field-missing (condition)
        (let ((missing-properties (missing-properties condition)))
          (respond-unprocessable-entity
           (jsown:new-js
             ("errors" (jsown:new-js
                         ("title" (format nil "Following fields are missing or have no value: ~{~A~,^, ~}"
                                          (mapcar #'json-property-name
                                                  missing-properties)))))))))
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Not allow to supply id in primary data."))))))
      (request-type-mismatch (condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "Supplied type (~A) did not match type for path (~A)."
                                        (content-defined-type condition)
                                        (path-defined-type condition)))))))))))

(defcall :patch (base-path id)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (verify-request-contains-type body)
          (verify-request-contains-id body)
          (verify-request-type-matches-path base-path body)
          (verify-request-id-matches-path id body)
          (verify-request-required-properties-not-removed base-path body)
          (update-call (find-resource-by-path base-path) id))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (required-field-missing (condition)
        (let ((missing-properties (missing-properties condition)))
          (respond-unprocessable-entity
           (jsown:new-js
             ("errors" (jsown:new-js
                         ("title" (format nil "Required fields would be removed: ~{~A~,^, ~}"
                                          (mapcar #'json-property-name
                                                  missing-properties)))))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (request-type-mismatch (condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "Supplied type (~A) did not match type for path (~A)."
                                        (content-defined-type condition)
                                        (path-defined-type condition))))))))
      (request-id-mismatch (condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "id in data (~A) did not match id in path (~A)."
                                        (content-defined-id condition)
                                        (path-defined-id condition)))))))))))

(defcall :delete (base-path id)
  (handler-case
      (delete-call (find-resource-by-path base-path) id)
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (access-denied (condition)
      (response-for-access-denied-condition condition))
    (no-such-resource ()
      (respond-forbidden (jsown:new-js
                           ("errors" (jsown:new-js
                                       ("title" (format nil
                                                        "Resource for path (~A) not found"
                                                        base-path)))))))))

;;;;;;;;;;;;;;;
;;;; link calls

(defun handle-relation-get-call (base-path id relation)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (let* ((resource (find-resource-by-path base-path))
               (link (find-resource-link-by-path resource relation)))
          (show-relation-call resource id link)))
    (incorrect-accept-header (condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))
    (configuration-error (condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (no-such-resource ()
      (respond-not-found))
    (no-such-link (condition)
      (let ((message
             (format nil "Could not find link (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))))

(defcall :get (base-path id relation)
  (handle-relation-get-call base-path id relation))

(defcall :get (base-path id :links relation)
  (handle-relation-get-call base-path id relation))

(defcall :patch (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body)
            (patch-relation-call resource id link)))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource ()
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-such-instance (condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))

(defcall :post (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body) ; same as post body
            (add-relation-call resource id link)))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource ()
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-such-instance (condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))

(defcall :delete (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body) ; same as delete body
            (delete-relation-call resource id link)))
      (incorrect-accept-header (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-content-type (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data ()
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource ()
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-such-instance (condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))

(defcall :post (:.mu :delta)
  ;; This call is positioned at the end of the stack, as calls at the
  ;; end are checked first.  We need to ensure we run first-hand as
  ;; other calls might overlap in future versions of the jsonapi spec.
  (let* ((raw-body (let ((p (post-body))) (format t "~&Post body: ~A~%" p) p))
         (body (let ((p (jsown:parse raw-body))) (format t "~&Parsed body: ~A~%" p) p)))
    ;; our goal should be to clear everything that might be impacted
    ;; by the changes here.  in the future we could intelligently
    ;; merge.

    ;; we can execute the cache clearing locally, read the
    ;; cache_clear_keys from the header and push the necessary changes
    ;; to the mu-cache.
    (delta-call body)))
