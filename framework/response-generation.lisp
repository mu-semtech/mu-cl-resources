(in-package :mu-cl-resources)

(defun try-parse-number (entity)
  "Tries to parse the number, returns nil if no number could be found."
  (handler-case
      (parse-integer entity :junk-allowed t)
    (error () nil)))

(defun cache-count-queries-p (resource)
  "Returns a truethy value iff the count queries should be cached for
   resource."
  (or *cache-count-queries-p*
     (find 'cache-count-queries (features resource))))

(defun count-matches (identifier-variable query-body resource link-spec)
  "Returns the amount of matches for a particular response."
  (flet ((sparql-count ()
           (parse-integer
            (jsown:filter (first (sparql:select (format nil "((COUNT (DISTINCT ~A)) AS ?count)"
                                                        identifier-variable)
                                                query-body))
                          "count" "value"))))
    (if (cache-count-queries-p resource)
        (or (count-cache resource link-spec)
           (setf (count-cache resource link-spec) (sparql-count)))
        (sparql-count))))

(defun extract-pagination-info-from-request ()
  "Extracts the pagination info from the current request object.
   The first value is a list containing the page size and page
   number.
   The second value indicates whether the page size and page
   number supplied by the end-user."
  (let ((page-size (or (try-parse-number (webserver:get-parameter "page[size]")) *default-page-size*))
        (page-number (or (try-parse-number (webserver:get-parameter "page[number]")) 0)))
    (values (list page-size page-number)
            (list (and (webserver:get-parameter "page[size]") t)
                  (and (webserver:get-parameter "page[number]") t)))))

(defun extract-order-info-from-request (resource)
  "Extracts the order info from the current request object."
  (alexandria:when-let ((sort (webserver:get-parameter "sort")))
    (loop for sort-string in (cl-ppcre:split "," sort)
       collect
         (let* ((descending-p (char= (aref sort-string 0) #\-))
                (attribute-name (if descending-p
                                    (subseq sort-string 1)
                                    sort-string))
                (components (split-sequence:split-sequence #\. attribute-name)))
           (list :order (if descending-p :descending :ascending)
                 :name attribute-name
                 :property-path (property-path-for-filter-components resource components))))))

(defun paginate-uuids-for-sparql-body (&key sparql-body page-size page-number order-info source-variable)
  "Returns the paginated uuids for the supplied sparql body and
   side constraints."
  (let ((order-variables (loop for info in order-info
                            for name = (getf info :name)
                            collect
                              (s-genvar name))))
    ;; looking at the implementation, much of it is split
    ;; between having to sort (order-info) and not having
    ;; to sort.  some logic is shared.
    (let ((sparql-variables (cond
                              ((and order-info *max-group-sorted-properties*)
                               (format nil "DISTINCT ~A~{ ~{(MAX(~A) AS ~A)~}~}"
                                       (s-var "uuid") (mapcar (lambda (a) (list a a)) order-variables)))
                              ((and order-info (not *max-group-sorted-properties*))
                               (format nil "DISTINCT ~A~{ ~A~}"
                                       (s-var "uuid") order-variables))
                              (t
                               (format nil "DISTINCT ~A" (s-var "uuid")))))
          (sparql-body (if order-info
                           (format nil "~A~%~{OPTIONAL {~{~A ~{~A~^/~} ~A~}.}~%~}"
                                   sparql-body
                                   (loop for info in order-info
                                      for variable in order-variables
                                      collect
                                        (list source-variable (getf info :property-path) variable)))
                           sparql-body))
          (order-by (if order-info
                        (format nil "~{~A(~A) ~}"
                                (loop for info in order-info
                                   for variable in order-variables
                                   append
                                     (list (if (eql (getf info :order)
                                                    :ascending)
                                               "ASC" "DESC")
                                           variable)))))
          (group-by (s-var "uuid"))
          (limit page-size)
          (offset (if (and page-size page-number) (* page-size page-number) 0)))
      (jsown:filter (sparql:select sparql-variables sparql-body
                                   :order-by order-by
                                   :group-by group-by
                                   :limit limit
                                   :offset offset)
                    map "uuid" "value"))))

(defun build-pagination-links (base-path &key page-number page-size total-count)
  "Builds a links object containing the necessary pagination
   links.  It bases itself on the base-path for the targeted
   request."
  (flet ((build-url (&key page-number)
           (let ((get-parameters (alist-to-plist (webserver:get-parameters*))))
             (setf (getfstr get-parameters "page[number]")
                   (and (> page-number 0) page-number))
             (setf (getfstr get-parameters "page[size]")
                   (and (/= page-size *default-page-size*)
                        page-size))
             (build-url base-path get-parameters))))
    (let ((last-page (max 0 (1- (ceiling (/ total-count page-size))))))
      (let ((links (jsown:new-js
                     ("first" (build-url :page-number 0))
                     ("last" (build-url :page-number last-page)))))
        (unless (<= page-number 0)
          (setf (jsown:val links "prev")
                (build-url :page-number (1- page-number))))
        (unless (>= page-number last-page)
          (setf (jsown:val links "next")
                (build-url :page-number (1+ page-number))))
        links))))

(defun include-count-feature-p (resource)
  "returns non-nil if the count of  available items should be
   included in meta response of paginated collections"
  (or *include-count-in-paginated-responses*
     (find 'include-count (features resource))))

(defun paginated-collection-response (&key resource sparql-body link-spec link-defaults source-variable self)
  "Constructs the paginated response for a collection listing."
  (destructuring-bind ((page-size page-number) (page-size-p page-number-p))
      (multiple-value-list (extract-pagination-info-from-request))
    (let ((order-info (extract-order-info-from-request resource))
          (use-pagination (or page-size-p page-number-p
                              (not (find 'no-pagination-defaults
                                       (features resource))))))
      (let ((uuid-count (count-matches (s-var "uuid") sparql-body resource link-spec))
            (uuids (paginate-uuids-for-sparql-body :sparql-body sparql-body
                                                   :page-size (and use-pagination page-size)
                                                   :page-number (and use-pagination page-number)
                                                   :source-variable source-variable
                                                   :order-info order-info))
            (resource-type (resource-name resource)))
        (multiple-value-bind (data-item-specs included-item-specs)
            (augment-data-with-attached-info
             (loop for uuid in uuids
                   collect (make-item-spec :uuid uuid :type resource-type)))
          (let ((response
                 (jsown:new-js ("data" (mapcar #'item-spec-to-jsown data-item-specs))
                               ("links" (merge-jsown-objects
                                         (build-pagination-links (webserver:script-name*)
                                                                 :total-count uuid-count
                                                                 :page-size page-size
                                                                 :page-number page-number)
                                         link-defaults)))))
            (when self
              (setf (jsown:val (jsown:val response "links") "self") self))
            (when (include-count-feature-p resource)
              (setf (jsown:val response "meta")
                    (jsown:new-js ("count" uuid-count))))
            (when included-item-specs
              (setf (jsown:val response "included")
                    (mapcar #'item-spec-to-jsown included-item-specs)))
            (values response data-item-specs included-item-specs)))))))

(defun sparql-pattern-filter-string (resource source-variable &key components search)
  "Constructs the sparql pattern for a filter constraint."
  (let ((search-var (s-genvar "search"))
        (last-component (car (last components))))
    (flet ((smart-filter-p (pattern)
             "Returns non-nil if the supplied match is the operation specified
              in the last filter component."
             (eql 0 (search pattern last-component)))
           (comparison-filter (pattern comparator)
             (let ((new-components (append (butlast components)
                                           (list (subseq last-component (length pattern))))))
               (multiple-value-bind (pattern target-variable last-slot slots)
                   (sparql-pattern-for-filter-components source-variable resource new-components nil)
                 (declare (ignore slots))
                 (format nil "~A FILTER (~A ~A ~A)~&"
                         pattern
                         target-variable
                         comparator
                         (interpret-json-string last-slot search))))))
      (cond
        ;; search for ids
        ((and (or (deprecated (:silent "Use [:id:] instead.")
                    (string= "id" last-component))
                  (string= ":id:" last-component)))
         (let ((search-components (mapcar #'s-str (split-sequence:split-sequence #\, search))))
           (multiple-value-bind (sparql-pattern target-variable last-slot slots)
               (sparql-pattern-for-filter-components source-variable resource (butlast components) nil)
             (declare (ignore last-slot slots))
             (format nil "~A ~A mu:uuid ~A. VALUES ~A {~{~A~^ ~}} ~&"
                     sparql-pattern
                     target-variable search-var
                     search-var search-components))))
        ;; search for url
        ((string= ":uri:" last-component)
         (if (> (length components) 1)
             (multiple-value-bind (sparql-pattern target-variable last-slot slots)
                 (sparql-pattern-for-filter-components source-variable resource (butlast components) nil)
               (declare (ignore last-slot slots))
               (format nil "~A VALUES ~A { ~A } ~&"
                       sparql-pattern
                       target-variable (s-url search)))
             (format nil "VALUES ~A { ~A } ~&"
                     source-variable (s-url search))))
        ;; exact search
        ((smart-filter-p ":exact:")
         (let ((last-property (subseq last-component (length ":exact:"))))
           (multiple-value-bind (sparql-pattern target-variable last-slot slots)
               (sparql-pattern-for-filter-components source-variable resource (butlast components) nil)
             (declare (ignore slots))
             (let* ((last-resource (if last-slot (referred-resource last-slot) resource))
                    (property-slot (resource-slot-by-json-key last-resource last-property)))
               (format nil "~A ~A ~{~A~^/~} ~A.~&"
                       sparql-pattern
                       target-variable (ld-property-list property-slot) (interpret-json-string property-slot search))))))
        ;; comparison searches
        ((smart-filter-p ":gt:") (comparison-filter ":gt:" ">"))
        ((smart-filter-p ":lt:") (comparison-filter ":lt:" "<"))
        ((smart-filter-p ":gte:") (comparison-filter ":gte:" ">="))
        ((smart-filter-p ":lte:") (comparison-filter ":lte:" "<="))
        ;; has-no
        ((smart-filter-p ":has-no:")
         (let ((new-components (append (butlast components)
                                       (list (subseq last-component (length ":has-no:"))))))
           (format nil "FILTER( NOT EXISTS {~&~T~T~A~&~T} )~&"
                   (sparql-pattern-for-filter-components source-variable resource new-components nil))))
        ;; has
        ((smart-filter-p ":has:")
         (let ((new-components (append (butlast components)
                                       (list (subseq last-component (length ":has:"))))))
           (format nil "FILTER( EXISTS {~&~T~T~A~&~T} )~&"
                   (sparql-pattern-for-filter-components source-variable resource new-components nil))))
        ;; not
        ((smart-filter-p ":not:")
         (let ((last-property (subseq last-component (length ":not:"))))
           (multiple-value-bind (sparql-pattern target-variable last-slot slots)
               (sparql-pattern-for-filter-components source-variable resource (butlast components) nil)
             (declare (ignore slots))
             (let* ((last-resource (if last-slot (referred-resource last-slot) resource))
                    (property-slot (resource-slot-by-json-key last-resource last-property)))
               (format nil "FILTER( NOT EXISTS { ~A ~A ~A ~A. } )"
                       sparql-pattern
                       target-variable (ld-property-list property-slot) (interpret-json-string property-slot search))))))
        ;; paths
        ;; Example: /people?[:paths:friend.firstName:address.street]=yolo
        ((smart-filter-p ":paths:")
         (let ((options (loop for section in (cddr (split-sequence:split-sequence #\: last-component))
                              collect (concatenate 'list
                                                   (butlast components)
                                                   (split-sequence:split-sequence #\. section)))))
           ;; Once we have these options, we have to union them in the
           ;; same way we do the "standard semi-fuzzy search"
           (format nil "~{~&{ ~A }~,^~%UNION~%~}"
                   (loop for components in options
                         for (sparql-pattern target-variable last-slot slots)
                           = (multiple-value-list
                              (sparql-pattern-for-filter-components source-variable resource components t))
                         collect
                         (format nil "~A FILTER CONTAINS(LCASE(str(~A)), LCASE(~A)) ~&"
                                 sparql-pattern
                                 target-variable (s-str search))))))
        ;; standard semi-fuzzy search
        ;; PPFFC: TARGET VALUE=VALUE
        (t
         (multiple-value-bind (sparql-pattern target-variable last-slot slots)
             (sparql-pattern-for-filter-components source-variable resource components t)
           (declare (ignore last-slot slots))
           (format nil "~A FILTER CONTAINS(LCASE(str(~A)), LCASE(~A)) ~&"
                   sparql-pattern
                   target-variable (s-str search))))))))

(defun extract-filters-from-request ()
  "Extracts the filters from the request.  The result is a list
   containing the :components and :search key.  The :components
   key includes a left-to-right specification of the strings
   between brackets.  The :search contains the content for that
   specification."
  (loop for (param . value) in (webserver:get-parameters*)
     if (eql (search "filter" param :test #'char=) 0)
     collect (list :components
                   (mapcar (lambda (str)
                             (subseq str 0 (1- (length str))))
                           (rest (cl-ppcre:split "\\[" param)))
                   :search
                   value)))

(defun filter-body-for-search (&key resource source-variable sparql-body)
  "Adds constraints to sparql-body so that it abides the filters
   which were posed by the user."
  (dolist (filter (extract-filters-from-request))
    (setf sparql-body
          (format nil "~A~&~t~A" sparql-body
                  (apply #'sparql-pattern-filter-string
                         resource source-variable filter))))
  sparql-body)
