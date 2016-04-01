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

(defparameter *yield* nil
  "Contains the callback function to call, if such function is
   defined.")

(defun yield ()
  "Yields to the callback method, if such method is defined in
   the current context."
  (if *yield* (funcall *yield*) nil))

(defmacro with-around-hook ((&rest specification) (&rest arguments) &body body)
  "Runs an around hook for <specification>.  The method calls
   the hook with the arguments in <arguments>.  It calculates
   the arguments before running the hook.  <body> is ran when
   the hook calls the (yield) method."
  (let ((hook-sym (gensym "around-hook")))
    `(let ((,hook-sym (gethash (,@specification) *hooks*)))
       (if ,hook-sym
           (let ((*yield* (lambda () ,@body)))
             (apply ,hook-sym (,@arguments)))
           (progn ,@body)))))

(defmacro with-surrounding-hook ((&rest specification) (&rest arguments) &body body)
  "Surrounds <body> with hook execution for <specification>.
   Three hooks are defined, one which runs before the execution of
   body, being `(:before ,@specification).  It is called with the
   values of arguments.  The second hook `(:after ,@specification)
   is called with the values of <arguments>, augmented with the
   content of <body>.  The result of <body> contains the returned
   content.  The last hook is `(:around ,@specification).  This
   hook is ran after the :before hook and before the :after hook.
   The implementation of the :around hook needs to make a call to
   the call-next-method method in order to execute the body of
   this hook.  The hook code is ran in the environment provided by
   the call-next-method hook.  This allows for overriding special
   variables.
   Each argument in specification is evaluated three times.  Just
   before running the before-hook, just before running the around
   hook, and just before running the after hook.
   Each argument in arguments is evaluated three times too.  Just
   before running the before-hook, just before running the around
   hook, and just before running the after hook."
  (let ((result-var (gensym "surrounding-hook-result"))
        (new-result-var (gensym "surrounding-hook-new-result"))
        (hook-p (gensym "hook-p")))
    `(progn (run-hook (list :before ,@specification)
                  (list ,@arguments))
        (let ((,result-var
               (with-around-hook
                   (list :around ,@specification)
                   (list ,@arguments)
                 ,@body)))
          (multiple-value-bind (,new-result-var ,hook-p)
              (run-hook (list :after ,@specification)
                        (list ,@arguments ,result-var))
            (if ,hook-p ,new-result-var ,result-var))))))

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

(defmacro around ((&rest specification) (&rest variables) &body body)
  "Implements an around hook.  This makes code run around a
   specification.  <variables> contains the lambda list for the
   callback, which is specified in <body>.  You must call #'yield
   to ensure the parent code is ran.  Not doing so causes
   undefined behaviour."
  `(define-hook (:around ,@specification) ,variables ,@body))
