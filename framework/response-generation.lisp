(in-package :mu-cl-resources)

(defun try-parse-number (entity)
  "Tries to parse the number, returns nil if no number could be found."
  (handler-case
      (parse-integer entity :junk-allowed t)
    (error () nil)))

(defun count-matches (identifier-variable query-body)
  "Returns the amount of matches for a particular response."
  (parse-integer
   (jsown:filter (first (sparql-select (format nil "((COUNT (DISTINCT ~A)) AS ?count)"
                                               identifier-variable)
                                       query-body))
                 "count" "value")))

(defun extract-pagination-info-from-request ()
  "Extracts the pagination info from the current request object."
  (let ((page-size (or (try-parse-number (hunchentoot:get-parameter "page[size]")) *default-page-size*))
        (page-number (or (try-parse-number (hunchentoot:get-parameter "page[number]")) 0)))
    (list page-size page-number)))

(defun paginate-uuids-for-sparql-body (&key sparql-body page-size page-number)
  (let ((limit page-size)
        (offset (* page-size page-number)))
    (jsown:filter (sparql-select (format nil "DISTINCT ~A" (s-var "uuid"))
                                 sparql-body
                                 :order-by (s-var "uuid")
                                 :limit limit
                                 :offset offset)
                  map "uuid" "value")))

(defun retrieve-data-for-uuids (resource uuids)
  "Retrieves the object description for all found uuids in the
   set of supplied uuids.
   If a uuid could not be found, it is not returned in the set of
   results."
  (loop for uuid in uuids
     for shown = (handler-case
                     (show-call resource uuid)
                   (no-such-instance () nil))
     when shown
     collect (jsown:val shown "data")))

(defun build-pagination-links (base-path &key page-number page-size total-count)
  "Builds a links object containing the necessary pagination
   links.  It bases itself on the base-path for the targetted
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

(defun paginated-collection-response (&key resource sparql-body link-defaults)
  "Constructs the paginated response for a collection listing."
  (destructuring-bind (page-size page-number)
      (extract-pagination-info-from-request)
    (let ((uuid-count (count-matches (s-var "uuid") sparql-body))
          (uuids (paginate-uuids-for-sparql-body :sparql-body sparql-body
                                                 :page-size page-size
                                                 :page-number page-number)))
      (jsown:new-js ("data" (retrieve-data-for-uuids resource uuids))
                    ("links" (merge-jsown-objects
                              (build-pagination-links (hunchentoot:script-name*)
                                                      :total-count uuid-count
                                                      :page-size page-size
                                                      :page-number page-number)
                              link-defaults))))))

(defun sparql-pattern-filter-string (resource source-variable &key components search)
  "Constructs the sparql pattern for a filter constraint."
  (let ((search-var (s-genvar "search")))
   (cond ((string= "id" (car (last components)))
          (format nil "~A ~{~A~^/~}/mu:uuid ~A. ~&"
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
     if (string= "filter" (subseq param 0 (length "filter")))
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
