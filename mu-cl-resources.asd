(asdf:defsystem :mu-cl-resources
  :name "mu-cl-resources"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.2.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Configurable microservice for managing the creation and retrieval of generic resources with a backed triplestore."
  :serial t
  :depends-on (mu-support)
  :components ((:file "packages")
               (:file "configuration/repository")
               (:file "configuration/settings")
               (:file "framework/error-codes")
               (:file "framework/support")
               (:file "framework/query-execution")
               (:file "framework/query-parsing")
               (:file "framework/resource-model")
               (:file "framework/request-parsing")
               (:file "framework/response-generation")
               (:file "framework/call-implementation")
               (:file "framework/call-specification")
               (:file "configuration/domain")))
