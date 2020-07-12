(in-package :mu-cl-resources)

;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.

;; Describe your resources here

;; The general structure could be described like this:
;;
;; (define-resource <name-used-in-this-file> ()
;;   :class <class-of-resource-in-triplestore>
;;   :properties `((<json-property-name-one> <type-one> ,<triplestore-relation-one>)
;;                 (<json-property-name-two> <type-two> ,<triplestore-relation-two>>))
;;   :has-many `((<name-of-an-object> :via ,<triplestore-relation-to-objects>
;;                                    :as "<json-relation-property>")
;;               (<name-of-an-object> :via ,<triplestore-relation-from-objects>
;;                                    :inverse t ; follow relation in other direction
;;                                    :as "<json-relation-property>"))
;;   :has-one `((<name-of-an-object :via ,<triplestore-relation-to-object>
;;                                  :as "<json-relation-property>")
;;              (<name-of-an-object :via ,<triplestore-relation-from-object>
;;                                  :as "<json-relation-property>"))
;;   :resource-base (s-url "<string-to-which-uuid-will-be-appended-for-uri-of-new-items-in-triplestore>")
;;   :on-path "<url-path-on-which-this-resource-is-available>")


;; An example setup with a catalog, dataset:
(define-resource catalog ()
  :class (s-prefix "dcat:Catalog")
  :properties `((:title :string ,(s-prefix "dct:title")))
  :has-many `((dataset :via ,(s-prefix "dcat:dataset")
                       :as "datasets"))
  :resource-base (s-url "http://webcat.tmp.semte.ch/catalogs/")
  :on-path "catalogs")

(define-resource dataset ()
  :class (s-prefix "dcat:Dataset")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:description :string ,(s-prefix "dct:description")))
  :has-one `((catalog :via ,(s-prefix "dcat:dataset")
                      :inverse t
                      :as "catalog"))
  :has-many `((theme :via ,(s-prefix "dcat:theme")
                     :as "themes"))
  :resource-base (s-url "http://webcat.tmp.semte.ch/datasets/")
  :on-path "datasets")

(define-resource distribution ()
  :class (s-prefix "dcat:Distribution")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:access-url :url ,(s-prefix "dcat:accessURL")))
  :resource-base (s-url "http://webcat.tmp.semte.ch/distributions/")
  :on-path "distributions")

(define-resource theme ()
  :class (s-prefix "dcat:Theme")
  :properties `((:pref-label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((dataset :via ,(s-prefix "dcat:theme")
                       :inverse t
                       :as "datasets"))
  :resource-base (s-url "http://webcat.tmp.semte.ch/themes/")
  :on-path "themes")
