(in-package :mu-cl-resources)

;;;;; product groups

;; Examples resources

;; (define-resource taxonomy ()
;;   :class (s-prefix "mt:Taxonomy")
;;   :properties `((:name :string ,(s-prefix "mt:name"))
;;                 (:description :string ,(s-prefix "dc:description")))
;;   :resource-base (s-url "http://mapping-tool.sem.tenforce.com/taxonomies/")
;;   :has-many `((topic :via ,(s-prefix "mt:taxonomyTopic")
;;                      :as "topics"))
;;   :on-path "taxonomies")

;; (define-resource topic ()
;;   :class (s-prefix "mt:CursoryTopic")
;;   :properties `((:name :string ,(s-prefix "mt:name"))
;;                 (:description :string ,(s-prefix "dc:description")))
;;   :resource-base (s-url "http://mapping-tool.sem.tenforce.com/topics/")
;;   :has-many `((topic :via ,(s-prefix "mt:topic")
;;                      :as "topics")
;;               (mapping :via ,(s-prefix "mt:mapping")
;;                        :as "mappings"))
;;   :has-one `((taxonomy :via ,(s-prefix "mt:taxonomyTopic")
;;                        :inverse t
;;                        :as "taxonomy"))
;;   :on-path "topics")

;; (define-resource mapping ()
;;   :class (s-prefix "mt:Mapping")
;;   :has-many `((topic :via ,(s-prefix "mt:maps")
;;                      :as "topics"))
;;   :resource-base (s-url "http://mapping-tool.sem.tenforce.com/mappings/")
;;   :on-path "mappings")
              
;; (define-resource page ()
;;   :class (s-url "http://mu.semte.ch/vocabulary/cms/Page")
;;   :resource-base (s-url "http://mu.semte.ch/cms/resources/pages/")
;;   :properties `((:title :string ,(s-prefix "dcterms:title"))
;;                 (:content :string ,(s-prefix "cms:pageContent")))
;;   :on-path "pages")

;; (around (:show page) (&rest args)
;;   (break "This is page showing with ~A" args)
;;   (let ((response (yield)))
;;     (break "The response should be ~A" (jsown:to-json response))
;;     (jsown:new-js ("ok" t))))

;; (after (:show page) (&rest args)
;;   (declare (ignore args))
;;   (jsown:new-js ("ok" :false)))


(read-domain-file "domain.json") ;; no domain.lisp, then load domain.json

(run-validations)
