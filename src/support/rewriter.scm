;;; rewriter.scm -- Support for the matching and rewriting of terms.
;;;
;;; The match and rewrite language by example:
;;;
;;; Rewrite rules are procedures that either "rewrite" their input, or
;;; return #f if the rule is inapplicable. Rewrite rules are
;;; applicable when the left-hand side matches on the input, and the
;;; right-hand side does not return false. They are constructed using
;;; the `rule' macro.
;;;
;;; Consider: (rule (foo ... ?a 0 ...)
;;;                 (and (number? ?a)
;;;                      (foo ... ?a ...)))
;;;
;;; - `foo' and 0 are evaluated and their result is matched literally
;;;   against the input expression.
;;;
;;; - `?a' matches against anything, and binds the result to `?a' in
;;;    the right-hand side.
;;;
;;; - `...' and `...' both match zero or more things, and binds the
;;;    match to the right-hand side. This is also an example of an
;;;    anonymous match, where the matches are bound to the ellipses on
;;;    the right-hand side left-to-right. We could have also named
;;;    them `left...' and `right...'  respectively.
;;;
;;; The right-hand side is then evaluated in an environment where the
;;; bindings made by the matcher are present. The result of this
;;; evaluation is what the rewrite rule returns.



(define (make-rule-base . rules)
  (apply list rules))

(define (rule-base/extend! rule rule-base)
  (push! rule rule-base))



(define (try-rule data rule win lose)
  (bind-condition-handler
   (list condition-type:inapplicable-object)
   (lambda (condition)
     (let ((operator (access-condition condition 'datum))
           (operands (access-condition condition 'operands)))
       (use-value (lambda _ (cons operator operands)))))
   (lambda ()
     (let ((result (rule data)))
       (if result
           (win result)
           (lose))))))

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



(define (pattern/variable? obj)
  (and (symbol? obj)
       (eq? (string-ref (symbol-name obj) 0)
            #\?)))

(define (pattern/ellipsis? obj)
  (and (symbol? obj)
       (equal? (string-take-right (symbol-name obj) 3)
               "...")))

(define (pattern/anonymous? obj)
  (and (symbol? obj)
       (member (symbol-name obj) '("..." "?"))
       #t))

(define (pattern/deanonymize pattern)
  (let ((count 0))
    (map-tree
     (lambda (symbol)
       (if (pattern/anonymous? symbol)
           (begin0
            (intern
             (string-append
              (number->string count) "_"
              (symbol-name symbol)))
            (set! count (1+ count)))
           symbol))
     pattern)))



(define (rule/compile-lhs lhs)
  (list
   'QUASIQUOTE
   (map-tree
    (lambda (symbol)
      (cond ((pattern/variable? symbol)
             (list '? symbol))
            ((pattern/ellipsis? symbol)
             (list '?? symbol))
            (else
             (list 'UNQUOTE symbol))))
    (pattern/deanonymize lhs))))

(eval
 '(define $the-environment
    (spar-classifier->runtime
     (delay
       (spar-and
        (spar-subform)
        (spar-match-null)
        (spar-push-value the-environment-item
                         spar-arg:ctx)))))
 (->environment '(runtime syntax mit)))

(define (rule/compile-rhs rhs bindings)
  `(LAMBDA (_)
    (APPLY
     (LAMBDA ,bindings
      (EVAL
       ,(list
         'QUASIQUOTE
         (map-tree
          (lambda (symbol)
            (cond ((member symbol '(if and or))
                   symbol)
                  ((pattern/ellipsis? symbol)
                   (list 'UNQUOTE-SPLICING (list 'QUOTE symbol)))
                  (else symbol)))
          (pattern/deanonymize rhs)))
       (THE-ENVIRONMENT)))
     _)))

(define (rule/compile lhs rhs)
  (let* ((lhs (rule/compile-lhs lhs))
         (rhs (rule/compile-rhs rhs
               (simple-matcher-pattern->names lhs))))
    (values lhs rhs)))

(define-syntax rule
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((lhs (cadr expr))
           (rhs (caddr expr))
           (er-data (rename 'data))
           (er-values (rename 'values)))
       (call-with-values
           (lambda () (rule/compile lhs rhs))
         (lambda (lhs rhs)
           `(lambda (,er-data)
              (apply-simple-matcher
               (make-simple-matcher ,lhs)
               ,er-data eq?
               ,rhs))))))))


