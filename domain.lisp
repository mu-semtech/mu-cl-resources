(in-package :product-groups)

;;;; define the resource
(define-resource product-group ()
  :class (s-url "http://veeakker.com/vocabulary/shop/ProductGroup")
  :properties `((:name :string ,(s-prefix "productGroup:name"))
                (:color :url ,(s-prefix "productGroup:color"))
                (:code :number ,(s-prefix "productGroup:code")))
  :resource-base (s-url "http://veeakker.com/api/product-groups/"))


;;;; LIST request
(defcall :get (:product-groups)
  (list-call 'product-group))

;;;; GET request
(defcall :get (:product-groups uuid)
  (show-call 'product-group uuid))

;;;; PUT request
(defcall :put (:product-groups uuid)
  (update-call 'product-group uuid))

;;;; POST request
;; create a new resource
(defcall :post (:product-groups)
  (create-call 'product-group))

;;;; DELETE request
(defcall :delete (:product-groups uuid)
  (delete-call 'product-group uuid))
