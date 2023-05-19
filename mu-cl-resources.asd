(asdf:defsystem :mu-cl-resources
  :name "mu-cl-resources"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.22.2"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Configurable microservice for managing the creation and retrieval of generic resources with a backed triplestore."
  :serial t
  :depends-on (mu-support split-sequence lparallel trivial-backtrace trivial-package-local-nicknames luckless)
  :components ((:file "packages")
               (:file "configuration/repository")
               (:file "configuration/settings")
               (:file "framework/callbacks")
               (:file "framework/error-codes")
               (:file "framework/support")
               (:file "framework/caching")
               (:file "framework/query-execution")
               (:file "framework/query-parsing")
               (:file "framework/resource-model")
               (:file "framework/domain-parsing")
               (:file "framework/request-parsing")
               (:file "framework/response-generation")
               (:file "framework/included-items")
               (:file "framework/call-implementation")
               (:file "framework/call-specification")
               (:file "framework/resource-validations")
               (:file "configuration/domain")))
