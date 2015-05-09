(asdf:defsystem :product-groups
  :name "product groups"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Microservice supporting the creation and retrieval of product groups"
  :serial t
  :depends-on (mu-support)
  :components ((:file "packages")
               (:file "repository")
               (:file "framework")
               (:file "support")
               (:file "domain")
               (:file "product-groups")))
