;;; operators.scm



(define condition-type:no-applicable-methods
  (make-condition-type
   'no-applicable-methods condition-type:error
   '(operator operands)
   (lambda (condition port)
     (write-string "No applicable methods for " port)
     (write (access-condition condition 'operator) port)
     (write-string " with these arguments: " port)
     (write (access-condition condition 'operands) port)
     (write-string "." port))))

(define error:no-applicable-methods
  (condition-signaller
   condition-type:no-applicable-methods
   '(operator operands)
   standard-error-handler))



(define *pattern-operators*
  (make-hash-table))

(define (pattern-operator-new! operator knowledge-base)
  (hash-table-set!
   *pattern-operators*
   operator (cons '() knowledge-base)))

(define (pattern-operator-add! operator rule #!optional overriding?)
  (hash-table-ref
   *pattern-operators* operator
   (lambda ()
     (error "Not an extensible operator: " operator))
   (lambda (knowledge-base)
     (set-cdr!
      knowledge-base
      (if overriding?
          (append (list rule) (cdr knowledge-base))
          (append (cdr knowledge-base) (list rule)))))))



(define (make-extensible-rewriting-op name . ground-rules)
  (let ((the-rules ground-rules))
    (define (the-operator . operands)
      (let ((expression (cons name operands)))
        (bind-condition-handler
         (list condition-type:inapplicable-object)
         (lambda (condition)
           (use-value (lambda _ (cons name _))))
         (lambda ()
           (enter-rewrite-system
            expression ground-rules
            (lambda (rewriten-expression)
              rewriten-expression)
            (lambda ()
              expression))))))
    (pattern-operator-new! the-operator the-rules)
    the-operator))

(define (make-extensible-arithmetic-op op-name op-numeric)
  (make-extensible-rewriting-op
   op-name
   (rule (op-name ?a)
         (and (number? ?a)
              (op-numeric ?a)))
   (rule (op-name ?a ?b)
         (and (number? ?a) (number? ?b)
              (op-numeric ?a ?b)))
   ;; (rule (op-name lhs... ?a ?b rhs...)
   ;;       (and (number? ?a) (number? ?b)
   ;;            (op-name lhs... (op-numeric ?a ?b) rhs...)))
   ))
