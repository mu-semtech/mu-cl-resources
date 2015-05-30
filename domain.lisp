(in-package :product-groups)

;;;;; product groups

;;;; define the resource
(define-resource product-group ()
  :class (s-url "http://veeakker.com/vocabulary/shop/ProductGroup")
  :properties `((:name :string ,(s-prefix "productGroup:name"))
                ;; (:color :url ,(s-prefix "productGroup:color"))
                ;; (:code :number ,(s-prefix "productGroup:code"))
                (:sort-order :number ,(s-prefix "mu:sortOrder")))
  :resource-base (s-url "http://veeakker.com/api/product-groups/")
  :has-many `((product :via ,(s-url "http://veeakker.com/vocabulary/shop/hasProduct")
                       :as "products"))
  :on-path "product-groups")

(define-resource product ()
  :class (s-url "http://veeakker.com/vocabulary/shop/Product")
  :properties `((:name :string ,(s-prefix "product:name"))
                (:price :number ,(s-prefix "product:price"))
                (:fat :number ,(s-prefix "product:fat"))
                (:joule :number ,(s-prefix "product:joule"))
                (:allergenics :string ,(s-prefix "product:allergenics")))
  :resource-base (s-url "http://veeakker.com/api/products/")
  :has-one `((product-group :via ,(s-url "http://veeakker.com/vocabulary/shop/hasProduct")
                            :inverse t
                            :as "product-group")
             (producer :via ,(s-prefix "product:producer")
                       :as "producer"))
  :on-path "products")

(define-resource producer ()
  :class (s-url "http://veeakker.com/vocabulary/shop/Producer")
  :properties `((:name :string ,(s-prefix "producer:name"))
                (:description :string ,(s-prefix "dc:description"))
                (:website :string ,(s-prefix "producer:website"))
                (:long-description :string ,(s-prefix "producer:longDescription")))
  :resource-base (s-url "http://veeakker.com/api/producers/")
  :on-path "producers")
