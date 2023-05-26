;;; algebra.scm -- Installs the basic algebra package.
;;;
;;; Your usual commutativity, associativity, identity, and
;;; distributive laws on +, -, *, and /.



(define (backwards? a b)
  (and (not (number? a))
       (number? b)))



(define-property is-commutative
  (rule (op ... ?b ?a ...)
        (and (backwards? ?b ?a)
             (op ... ?a ?b ...))))

(define-property is-associative
  (rule (op a... (op b... c...))
        (op a... b... c...)))

(define-property distributes-over-left (op₂)
  (rule (op₁ ... ?a (op₂ xs...))
        (cons op₂ (map (lambda (x) (op₁ ... ?a x))
                       xs...))))

(define-property distributes-over-right (op₂)
  (rule (op₁ (op₂ xs...) ?a ...)
        (cons op₂ (map (lambda (x) (op₁ x ?a ...))
                       xs...))))

(define-property distributes-over (op₂)
  (distributes-over-left op₂)
  (distributes-over-right op₂))

(define-property has-left-identity (i)
  (rule (op)
        i)
  (rule (op ?a)
        ?a)
  (rule (op i a...)
        (op a...)))

(define-property has-right-identity (i)
  (rule (op)
        i)
  (rule (op ?a)
        ?a)
  (rule (op a... i)
        (op a...)))

(define-property has-identity (i)
  (has-left-identity i)
  (has-right-identity i))

(define-property can-combine-like-terms
  (rule (op (?op ... ?x) middle... (?op ... ?x) outer...)
        (op (?op (op ... ...) ?x) middle... outer...)))



(define-arithmetic-op +
  is-commutative
  is-associative
  can-combine-like-terms
  (has-identity 0))

(define-arithmetic-op -
  is-associative
  (has-identity 0))

(define-arithmetic-op *
  is-commutative
  is-associative
  (has-identity 1)
  (distributes-over '+)
  (distributes-over '-)
  (distributes-over '/))

(define-arithmetic-op /
  is-associative
  (has-right-identity 1)
  (distributes-over-right '+)
  (distributes-over-right '-)
  (distributes-over-right '*))
