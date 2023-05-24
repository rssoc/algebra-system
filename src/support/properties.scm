;;; properties.scm -- Support for defining algebraic properties.
;;;
;;; Properties are really just templates for rewrite rules that are
;;; instantiated when asserted. Optionally, they may be
;;; parameterized. This will probably be extended in the future to
;;; support building more abstract properties.



(define-syntax define-property
  ;; NOTE: Instead of creating the list of rules at compile-time, we
  ;;       have to map over a list of rule constructors (meaning we
  ;;       create our list of rules at run-time), to work around the
  ;;       ellipsis-depth of op. Admittedly, the implementation
  ;;       would be significantly shorter when given unhygienic
  ;;       syntax-rules.
  (syntax-rules (rule)
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
           ...)))))

;;; NOTE: Everything above is actually special case of this
;;; one. Unfortunately I can't trivially reflect this fact.
    ((_ property-name (parameters ...)
        properties ...)
     (define (property-name parameters ...)
       (lambda (the-op)
         (map
          (lambda (a-rule)
            (a-rule the-op))
          (list properties ...)))))))
