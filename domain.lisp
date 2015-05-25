(in-package :product-groups)

;;;;; product groups

;;;; define the resource
(define-resource product-group ()
  :class (s-url "http://veeakker.com/vocabulary/shop/ProductGroup")
  :properties `((:name :string ,(s-prefix "productGroup:name"))
                (:color :url ,(s-prefix "productGroup:color"))
                (:code :number ,(s-prefix "productGroup:code")))
  :resource-base (s-url "http://veeakker.com/api/product-groups/")
  :has-many `((product :via ,(s-url "http://veeakker.com/vocabulary/shop/hasProduct")
                       :as "products"))
  :on-path "product-groups")

(define-resource product ()
  :class (s-url "http://veeakker.com/vocabulary/shop/Product")
  :properties `((:name :string ,(s-prefix "product:name"))
                (:price :number ,(s-prefix "product:price")))
  :resource-base (s-url "http://veeakker.com/api/products/")
  :has-one `((product-group :via ,(s-url "http://veeakker.com/vocabulary/shop/hasProduct")
                            :inverse t
                            :as "product-group"))
  :on-path "products")
