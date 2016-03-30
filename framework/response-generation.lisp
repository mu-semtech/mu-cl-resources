(in-package :mu-cl-resources)

(defun try-parse-number (entity)
  "Tries to parse the number, returns nil if no number could be found."
  (handler-case
      (parse-integer entity :junk-allowed t)
    (error () nil)))

(defun count-matches (identifier-variable query-body)
  "Returns the amount of matches for a particular response."
  (parse-integer
   (jsown:filter (first (sparql:select (format nil "((COUNT (DISTINCT ~A)) AS ?count)"
                                               identifier-variable)
                                       query-body))
                 "count" "value")))

(defun extract-pagination-info-from-request ()
  "Extracts the pagination info from the current request object."
  (let ((page-size (or (try-parse-number (hunchentoot:get-parameter "page[size]")) *default-page-size*))
        (page-number (or (try-parse-number (hunchentoot:get-parameter "page[number]")) 0)))
    (list page-size page-number)))

(defun extract-order-info-from-request (resource)
  "Extracts the order info from the current request object."
  (alexandria:when-let ((sort (hunchentoot:get-parameter "sort")))
    (loop for sort-string in (cl-ppcre:split "," sort)
       collect
         (let* ((descending-p (char= (aref sort-string 0) #\-))
                (attribute-name (if descending-p
                                    (subseq sort-string 1)
                                    sort-string)))
           (list :order (if descending-p :descending :ascending)
                 :name attribute-name
                 :property-path (ld-property-list (resource-slot-by-json-key resource
                                                                             attribute-name)))))))

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
    (let ((sparql-variables (if order-info
                                (format nil "DISTINCT ~{~A~^, ~}"
                                        (cons (s-var "uuid") order-variables))
                                (format nil "DISTINCT ~A" (s-var "uuid"))))
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
          (offset (* page-size page-number)))
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
           (let ((get-parameters (alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*))))
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

(defun paginated-collection-response (&key resource sparql-body link-defaults source-variable)
  "Constructs the paginated response for a collection listing."
  (destructuring-bind (page-size page-number)
      (extract-pagination-info-from-request)
    (let ((order-info (extract-order-info-from-request resource)))
      (let ((uuid-count (count-matches (s-var "uuid") sparql-body))
            (uuids (paginate-uuids-for-sparql-body :sparql-body sparql-body
                                                   :page-size page-size
                                                   :page-number page-number
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
                                         (build-pagination-links (hunchentoot:script-name*)
                                                                 :total-count uuid-count
                                                                 :page-size page-size
                                                                 :page-number page-number)
                                         link-defaults)))))
            (when included-item-specs
              (setf (jsown:val response "included")
                    (mapcar #'item-spec-to-jsown included-item-specs)))
            response))))))

(defun sparql-pattern-filter-string (resource source-variable &key components search)
  "Constructs the sparql pattern for a filter constraint."
  (let ((search-var (s-genvar "search")))
    (cond ((and (string= "id" (car (last components)))
                (find #\, search :test #'char=))
           (let ((search-components (mapcar #'s-str (split-sequence:split-sequence #\, search))))
             (format nil "~A ~{~A/~}mu:uuid ~A FILTER ( ~A IN (~{~A~^, ~}) ) ~&"
                     source-variable
                     (butlast (property-path-for-filter-components resource (butlast components)))
                     search-var
                     search-var
                     search-components)))
          ((string= "id" (car (last components)))
           (format nil "~A ~{~A/~}mu:uuid ~A. ~&"
                   source-variable
                   (butlast (property-path-for-filter-components resource (butlast components)))
                   (s-str search)))
          (t
           (format nil "~A ~{~A~^/~} ~A FILTER CONTAINS(LCASE(str(~A)), LCASE(~A)) ~&"
                   source-variable
                   (property-path-for-filter-components resource components)
                   search-var
                   search-var
                   (s-str search))))))

(defun extract-filters-from-request ()
  "Extracts the filters from the request.  The result is a list
   containing the :components and :search key.  The :components
   key includes a left-to-right specification of the strings
   between brackets.  The :search contains the content for that
   specification."
  (loop for (param . value) in (hunchentoot:get-parameters hunchentoot:*request*)
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
