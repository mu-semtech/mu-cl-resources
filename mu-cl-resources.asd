(asdf:defsystem :mu-cl-resources
  :name "mu-cl-resources"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Configurable microservice for managing the creation and retrieval of generic resources with a backed triplestore."
  :serial t
  :depends-on (mu-support)
  :components ((:file "packages")
               (:file "repository")
               (:file "framework/framework")
               (:file "support")
               (:file "domain")))
