(in-package :mu-cl-resources)

(defparameter *hooks* (make-hash-table :test 'equal #-abcl :synchronized #-abcl t)
  "Contains all implemented hooks")

(defun run-hook (specification arguments)
  "Runs the hook specified by <specification> and <arguments>,
   both of which are expected to be lists."
  (alexandria:when-let ((method (gethash specification *hooks*)))
    (values (apply method arguments) t)))

(defun define-hook* (specification method)
  "Specifies that the hook for <specification> is defined by
   <method>."
  (setf (gethash specification *hooks*) method))

(defmacro define-hook ((&rest specification) (&rest arguments) &body body)
  "Defines a hook for <specification>.  The method accepts the
   arguments specified in <arguments> and executes the code in
   <body> when the hook is executed."
  `(define-hook* '(,@specification) (lambda ,arguments ,@body)))

(defmacro with-surrounding-hook ((&rest specification) (&rest arguments) &body body)
  "Surrounds <body> with hook execution for <specification>.
   Two hooks are defined, one which runs before the execution of
   body, being `(:before ,@specification).  It is called with the
   values of arguments.  The second hook `(:after ,@specification)
   is called with the values of <arguments>, augmented with the
   content of <body>.  The result of <body> contains the returned
   content.
   Each argument in specification is evaluated twice.  Just before
   running the before-hook and just before running the after hook.
   Arguments is evaluated twice, once just before calling the
   before-hook and once right before calling the after-hook."
  (let ((result-var (gensym "surrounding-hook-result")))
    `(progn (run-hook (list :before ,@specification)
                  (list ,@arguments))
        (let ((,result-var (progn ,@body)))
          (run-hook (list :after ,@specification)
                    (list ,@arguments ,result-var))
          ,result-var))))

(defmacro before ((&rest specification) (&rest variables) &body body)
  "Implements a before macro.  This makes code run before a
   specification.  <variables> contains the lambda list for the
   callback, which is specified in body."
  `(define-hook (:before ,@specification) ,variables ,@body))

(defmacro after ((&rest specification) (&rest variables) &body body)
  "Implements an after macro.  This makes code run after a
   specification.  <variables> contains the lambda list for the
   callback, which is specified in body."
  `(define-hook (:after ,@specification) ,variables ,@body))
