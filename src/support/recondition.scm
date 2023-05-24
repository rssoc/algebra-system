;;; recondition.scm -- Bend the semantics of Scheme to our favor.
;;;
;;; This is what really makes the system feel like an algebra
;;; system. You can find similar tricks in GJS's scmutils, but I've
;;; gone a bit further to scope them to the arithmetic operators we
;;; defined.



(define (stack-frame/subproblem-ref stack-frame n)
  (if (< n 0)
      stack-frame
      (stack-frame/subproblem-ref
       (stack-frame/next-subproblem stack-frame)
       (-1+ n))))

(define (stack-frame/expression stack-frame)
  (call-with-values
      (lambda ()
        (stack-frame/debugging-info stack-frame))
    (lambda (expression environment sub-expression)
      (unsyntax expression))))

(define (stack-frame/environment stack-frame)
  (call-with-values
      (lambda ()
        (stack-frame/debugging-info stack-frame))
    (lambda (expression environment sub-expression)
      environment)))

(define (stack-frame/expression&environment stack-frame)
  (let ((expression  (stack-frame/expression stack-frame))
        (environment (stack-frame/environment stack-frame)))
    (values
     expression
     environment
     (not (or (debugging-info/undefined-expression?
               expression)
              (debugging-info/undefined-environment?
               environment))))))



(define (continuation/peek-outside continuation level)
  (let ((outside-stack-frame
         (stack-frame/subproblem-ref
          (continuation->stack-frame continuation)
          level)))
    (stack-frame/expression&environment
     outside-stack-frame)))

(define (doing-algebra? continuation)
  (call-with-values
      (lambda ()
        (continuation/peek-outside continuation 1))
    (lambda (outer-expression outer-environment defined?)
      (and defined?
           (list? outer-expression)
           (symbol? (car outer-expression))
           (procedure? (environment-safe-lookup outer-environment (car outer-expression)))
           (op/extensible? (eval (car outer-expression) outer-environment))))))



(bind-default-condition-handler
 (list condition-type:unbound-variable)
 (lambda (condition)
   (let ((variable-name (access-condition condition 'location)))
     (let ((k (condition/continuation condition)))
       (when (doing-algebra? k)
         (use-value variable-name))))))
