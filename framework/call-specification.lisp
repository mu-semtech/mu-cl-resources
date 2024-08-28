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
        (with-single-itemspec-classes-retry
          (list-call (find-resource-by-path base-path))))
    (no-such-resource (e)
      (trivial-backtrace:print-backtrace e)
      (respond-not-found))
    (access-denied (condition)
      (trivial-backtrace:print-backtrace condition)
      (response-for-access-denied-condition condition))
    (no-such-link (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js ("title" "Request invalid"))))))
    (no-such-property (condition)
      (trivial-backtrace:print-backtrace condition)
      (let ((message
             (format nil "Could not find property (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))
    (cl-fuseki:sesame-exception (exception)
      (trivial-backtrace:print-backtrace exception)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Could not execute SPARQL query.")))))))
    (configuration-error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (incorrect-accept-header (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                           ("title" (description condition)))))))
    (error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-general-server-error))))

(defcall :get (base-path id)
  (handler-case
      (progn
        (verify-json-api-request-accept-header)
        (with-single-itemspec-classes-retry
          (show-call (find-resource-by-path base-path) id)))
    (no-such-resource (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-not-found))
    (access-denied (condition)
      (trivial-backtrace:print-backtrace condition)
      (response-for-access-denied-condition condition))
    (cl-fuseki:sesame-exception (exception)
      (trivial-backtrace:print-backtrace exception)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Could not execute SPARQL query.")))))))
    (configuration-error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (no-such-instance (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-not-found))
    (incorrect-accept-header (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))
    (no-such-property (condition)
      (trivial-backtrace:print-backtrace condition)
      (let ((message
             (format nil "Could not find property (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))
    (error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-general-server-error))))

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
          (with-single-itemspec-classes-retry
            (create-call (find-resource-by-path base-path))))
      (no-such-resource (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-forbidden (jsown:new-js
                             ("errors" (jsown:new-js
                                         ("title" (format nil
                                                          "Resource for path (~A) not found"
                                                          base-path)))))))
      (access-denied (condition)
        (trivial-backtrace:print-backtrace condition)
        (response-for-access-denied-condition condition))
      (required-field-missing (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((missing-properties (missing-properties condition)))
          (respond-unprocessable-entity
           (jsown:new-js
             ("errors" (jsown:new-js
                         ("title" (format nil "Following fields are missing or have no value: ~{~A~,^, ~}"
                                          (mapcar #'json-property-name
                                                  missing-properties)))))))))
      (configuration-error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-accept-header (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (incorrect-content-type (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (id-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Not allow to supply id in primary data."))))))
      (cl-fuseki:sesame-exception (exception)
        (trivial-backtrace:print-backtrace exception)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Could not execute SPARQL query.")))))))
      (request-type-mismatch (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "Supplied type (~A) did not match type for path (~A)."
                                        (content-defined-type condition)
                                        (path-defined-type condition))))))))
      (no-such-property (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((message
               (format nil "Could not find property (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-general-server-error)))))

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
          (with-single-itemspec-classes-retry
            (update-call (find-resource-by-path base-path) id)))
      (incorrect-accept-header (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (trivial-backtrace:print-backtrace condition)
        (response-for-access-denied-condition condition))
      (cl-fuseki:sesame-exception (exception)
        (trivial-backtrace:print-backtrace exception)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Could not execute SPARQL query.")))))))
      (configuration-error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (required-field-missing (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((missing-properties (missing-properties condition)))
          (respond-unprocessable-entity
           (jsown:new-js
             ("errors" (jsown:new-js
                         ("title" (format nil "Required fields would be removed: ~{~A~,^, ~}"
                                          (mapcar #'json-property-name
                                                  missing-properties)))))))))
      (incorrect-content-type (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (request-type-mismatch (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "Supplied type (~A) did not match type for path (~A)."
                                        (content-defined-type condition)
                                        (path-defined-type condition))))))))
      (request-id-mismatch (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (format nil "id in data (~A) did not match id in path (~A)."
                                        (content-defined-id condition)
                                        (path-defined-id condition))))))))
      (no-such-property (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((message
                (format nil "Could not find property (~A) on resource (~A)."
                        (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-general-server-error)))))

(defcall :delete (base-path id)
  (handler-case
      (with-single-itemspec-classes-retry
        (delete-call (find-resource-by-path base-path) id))
    (configuration-error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (access-denied (condition)
      (trivial-backtrace:print-backtrace condition)
      (response-for-access-denied-condition condition))
    (cl-fuseki:sesame-exception (exception)
      (trivial-backtrace:print-backtrace exception)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Could not execute SPARQL query.")))))))
    (no-such-resource (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-forbidden (jsown:new-js
                           ("errors" (jsown:new-js
                                       ("title" (format nil
                                                        "Resource for path (~A) not found"
                                                        base-path)))))))
    (error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-general-server-error))))

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
      (trivial-backtrace:print-backtrace condition)
      (respond-not-acceptable (jsown:new-js
                                ("errors" (jsown:new-js
                                            ("title" (description condition)))))))
    (configuration-error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Server configuration issue: " (description condition))))))))
    (no-such-resource (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-not-found))
    (no-such-property (condition)
      (trivial-backtrace:print-backtrace condition)
      (let ((message
             (format nil "Could not find property (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))
    (cl-fuseki:sesame-exception (exception)
      (trivial-backtrace:print-backtrace exception)
      (respond-server-error
       (jsown:new-js
         ("errors" (jsown:new-js
                     ("title" (s+ "Could not execute SPARQL query.")))))))
    (no-such-link (condition)
      (trivial-backtrace:print-backtrace condition)
      (let ((message
             (format nil "Could not find link (~A) on resource (~A)."
                     (path condition) (json-type (resource condition)))))
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" message)))))))
    (error (condition)
      (trivial-backtrace:print-backtrace condition)
      (respond-general-server-error))))

(defcall :get (base-path id relation)
  (with-single-itemspec-classes-retry
    (handle-relation-get-call base-path id relation)))

(defcall :get (base-path id :links relation)
  (with-single-itemspec-classes-retry
    (handle-relation-get-call base-path id relation)))

(defcall :patch (base-path id :links relation)
  (let ((body (jsown:parse (post-body))))
    (handler-case
        (progn
          (verify-json-api-request-accept-header)
          (verify-json-api-content-type)
          (let* ((resource (find-resource-by-path base-path))
                 (link (find-resource-link-by-path resource relation)))
            (verify-link-patch-body-format link body)
            (with-single-itemspec-classes-retry
              (patch-relation-call resource id link))))
      (incorrect-accept-header (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (trivial-backtrace:print-backtrace condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-content-type (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-found))
      (no-such-property (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((message
               (format nil "Could not find property (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (no-such-link (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (cl-fuseki:sesame-exception (exception)
        (trivial-backtrace:print-backtrace exception)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Could not execute SPARQL query.")))))))
      (no-such-instance (condition)
        (trivial-backtrace:print-backtrace condition)
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
            (with-single-itemspec-classes-retry
              (add-relation-call resource id link))))
      (incorrect-accept-header (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (trivial-backtrace:print-backtrace condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-content-type (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-found))
      (no-such-property (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((message
               (format nil "Could not find property (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (no-such-link (condition)
        (trivial-backtrace:print-backtrace condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (cl-fuseki:sesame-exception (exception)
        (trivial-backtrace:print-backtrace exception)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Could not execute SPARQL query.")))))))
      (no-such-instance (condition)
        (trivial-backtrace:print-backtrace condition)
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
            (with-single-itemspec-classes-retry
              (delete-relation-call resource id link))))
      (incorrect-accept-header (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (access-denied (condition)
        (trivial-backtrace:print-backtrace condition)
        (response-for-access-denied-condition condition))
      (configuration-error (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Server configuration issue: " (description condition))))))))
      (incorrect-content-type (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (no-type-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "No type found in primary data."))))))
      (no-id-in-data (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-conflict (jsown:new-js
                            ("errors" (jsown:new-js
                                        ("title" "Must supply id in primary data."))))))
      (no-such-resource (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-found))
      (no-such-link (condition)
        (let ((message
               (format nil "Could not find link (~A) on resource (~A)."
                       (path condition) (json-type (resource condition)))))
          (respond-not-acceptable (jsown:new-js
                                    ("errors" (jsown:new-js
                                                ("title" message)))))))
      (invalid-link-patch-body-format (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable (jsown:new-js
                                  ("errors" (jsown:new-js
                                              ("title" (description condition)))))))
      (cl-fuseki:sesame-exception (exception)
        (trivial-backtrace:print-backtrace exception)
        (respond-server-error
         (jsown:new-js
           ("errors" (jsown:new-js
                       ("title" (s+ "Could not execute SPARQL query.")))))))
      (no-such-instance (condition)
        (trivial-backtrace:print-backtrace condition)
        (respond-not-acceptable
         (jsown:new-js ("errors"
                        (jsown:new-js
                          ("title" (format nil "No resource found for supplied type (~A) and id (~A)"
                                           (target-type condition)
                                           (target-id condition)))))))))))

(defcall :post (:.mu :delta)
  ;; TODO: error out when database doesn't respond here too?

  ;; This call is positioned at the end of the stack, as calls at the
  ;; end are checked first.  We need to ensure we run first-hand as
  ;; other calls might overlap in future versions of the jsonapi spec.
  (let* ((raw-body (post-body))
         (body (jsown:parse raw-body)))
    ;; our goal should be to clear everything that might be impacted
    ;; by the changes here.  in the future we could intelligently
    ;; merge.

    ;; we can execute the cache clearing locally, read the
    ;; cache_clear_keys from the header and push the necessary changes
    ;; to the mu-cache.
    (with-single-itemspec-classes-retry
      (delta-call body))))
