(in-package :mu-cl-resources)

(defun run-validations ()
  "Runs validations on the currently defined resources.  Emits
  warnings about inconsistent state.

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
            (map-castable (lambda (key resource)
                            (declare (ignore key))
                            (let ((found-slash (find #\/ (request-path resource) :test #'char=)))
                              (when found-slash
                                (format t "~&Resource ~A has a / in its :on-path ~A~%"
                                        (resource-name resource) (request-path resource)))
                              found-slash))
                          *resources*))))
