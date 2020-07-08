(in-package :mu-cl-resources)

(defun run-validations ()
  "Runs validations on the currently defined resources.  Emits
  warningsn about inconsistent state.

  Current validations:
  - no-slash-in-on-path"

  (if (find-if-not #'identity
                   (list (validate-no-slash-in-on-path)))
      (format t "~&FOUND VALIDATION ISSUES WHEN VALIDATING RESOURCES.  SEE ABOVE.~%")
      (format t "~&All validations positive.~%")))

(defun validate-no-slash-in-on-path ()
  "Validates no / is found in the on-path or emits a warning.

   Yields a non-nil value if the validation succeeded"
  (not
   (find-if #'identity
            (loop
               for resource being the hash-values of *resources*
               for found-slash = (find #\/ (request-path resource) :test #'char=)
               if found-slash
               do (format t "~&Resource ~A has a / in its :on-path ~A~%"
                          (resource-name resource) (request-path resource))
               collect found-slash))))
