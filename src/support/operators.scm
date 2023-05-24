;;; operators.scm -- Extensible rewriting and arithmetic operators.
;;;
;;; This is a neat idea that I had to gloss over in the lecture. Our
;;; intent is to replace MIT-Scheme's arithmetic system with one
;;; that's extensible at runtime. It's basically a riff on GJS's idea
;;; of sticky notes. Rewriting ops have an internal store of rules (a
;;; rule-base) to reference for rewriting, so what we've done here is
;;; expose the internal store to the outside world for mutation. This
;;; lets us mess with the rule-base of an op in whichever way we
;;; please after the fact. This also disgusts your average Haskeller,
;;; which is a good thing.


;; NOTE: weak-hash-tables are the best thing since sliced bread,
;;       everybody should have them.
(define *op-storage*
  (make-key-weak-eq-hash-table))

(define (op/register! op #!optional rule-base)
  (when (default-object? rule-base)
    (set! rule-base (make-rule-base)))
  (hash-table-set!
   *op-storage*
   op rule-base))

(define (op/extend! op rule)
  (hash-table-ref
   *op-storage* op
   (lambda ()
     (error "OP/EXTEND! -- Operator is not an extensible operator: " op))
   (lambda (rule-base)
     (rule-base/extend! rule rule-base))))



(define (make-extensible-rewriting-op name . rules)
  (let ((the-rules (apply make-rule-base rules)))
    (define (the-operator . operands)
      (let ((expression (cons name operands)))
        (enter-rewrite-system
         expression the-rules
         (lambda (rewriten-expression)
           rewriten-expression)
         (lambda ()
           expression))))
    (op/register! the-operator the-rules)
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
   (rule (op-name ... ?a ?b ...)
         (and (number? ?a) (number? ?b)
              (op-name ... (op-numeric ?a ?b) ...)))))

(define-syntax define-arithmetic-op
  (syntax-rules ()
    ((_ op property ...)
     (begin
       (define op
         (let ((op-numeric op))
           (make-extensible-arithmetic-op 'op op)))
       (op/extend! op (property 'op))
       ...))))


