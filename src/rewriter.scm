;;; rewriter.scm -- Support for the rewriting of terms according to a
;;; rule-base.
;;;
;;; The matching language by example:
;;;
;;; Consider: (compile-rule (op ?a 0) ?a)




(define (map-tree procedure xs)
  (cond ((null? xs) '())
        ((list? xs)
         (cons
          (map-tree procedure (car xs))
          (map-tree procedure (cdr xs))))
        (else
         (procedure xs))))

(define (butlast xs)
  (reverse (cdr (reverse xs))))



(define (pattern-variable? symbol)
  (eq? (string-ref (symbol-name symbol) 0) #\?))

(define (compile-lhs lhs)
  (list 'QUASIQUOTE
        (map-tree
         (lambda (symbol)
           (if (pattern-variable? symbol)
               (list '? symbol)
               (list 'UNQUOTE symbol)))
         lhs)))

(define (compile-rhs rhs)
  (list 'QUASIQUOTE
        (map-tree
         (lambda (symbol)
           (cond ((pattern-variable? symbol)
                  (list '? symbol))
                 ((member symbol '(if and or))
                  symbol)
                 (else
                  (list 'UNQUOTE symbol))))
         (last rhs))))

(define-syntax rule
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((lhs (compile-lhs (cadr expr)))
           (rhs (caddr expr)))
       (let ((receiver
              `(lambda ,(simple-matcher-pattern->names lhs)
                 ,rhs)))
         `(lambda (data)
            (apply-simple-matcher
             (make-simple-matcher
              ,lhs)
             data eq?
             (lambda (values)
               (apply ,receiver values)))))))))



(define (try-rule data rule win lose)
  (let ((result (rule data)))
    (if result
        (win result)
        (lose))))

(define (try-rules data the-rules win lose)
  (let per-rule ((the-rules the-rules))
    (if (null? the-rules)
        (lose)
        (try-rule
         data (car the-rules)
         win
         (lambda ()
           (per-rule (cdr the-rules)))))))

(define (enter-rewrite-system
         expression the-rules
         win lose)
  (let ((simplified-subterms
         (if (list? expression)
             (map (lambda (subterm)
                    (enter-rewrite-system
                     subterm the-rules
                     (lambda (simplified-term)
                       simplified-term)
                     (lambda ()
                       subterm)))
                  expression)
             expression)))
    (try-rules
     simplified-subterms the-rules
     win lose)))





(define-syntax define-property
  (syntax-rules (rule)
    ;; NOTE: Instead of creating the list of rules at compile-time, we
    ;; have to map over a list of rule constructors (meaning we create
    ;; our list of rules at run-time), to work around the
    ;; ellipsis-depth of op. Admittedly, the implementation would be
    ;; slightly cleaner given unhygienic syntax-rules.
    ((_ property-name
        (rule (op sub-terms ...) rhs)
        ...)
     (define property-name
       (lambda (the-op)
         (map
          (lambda (a-rule)
            (a-rule the-op))
          (list
           (lambda (op)
             (rule
              (op sub-terms ...)
              rhs))
           ...)))))
    ((_ property-name (parameters ...)
        (rule (op sub-terms ...) rhs)
        ...)
     (define (property-name parameters ...)
       (lambda (the-op)
         (map
          (lambda (a-rule)
            (a-rule the-op))
          (list
           (lambda (op)
             (rule
              (op sub-terms ...)
              rhs))
           ...)))))))
