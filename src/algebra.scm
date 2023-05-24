;;; algebra.scm -- Installs the basic algebra package.
;;;
;;; Your usual commutativity, associativity, identity, and
;;; distributive laws on +, -, *, and /.



(define-property is-commutatitve
  (rule (op ?b ?a)
        (op ?a ?b)))

(define-property is-associative
  (rule (op ?a (op ?b ?c))
        (op ?a ?b ?c)))

(define-property distributes-over (op₂)
  (rule (op₁ ?a (op₂ ?b ?c))
        (op₂ (op₁ ?a ?b)
             (op₁ ?a ?c))))

(define-property has-left-identity (i)
  (rule (op ?a)
        ?a)
  (rule (op i ?a)
        ?a))

(define-property has-right-identity (i)
  (rule (op ?a)
        ?a)
  (rule (op ?a i)
        ?a))

(define-property has-identity (i)
  (has-left-identity i)
  (has-right-identity i))



(define-arithmetic-op +
  (has-identity 0)
  is-commutative
  is-associate)

(define-arithmetic-op -
  (has-identity 0)
  is-commutative
  is-associate)

(define-arithmetic-op *
  (distributes-over '+)
  (distributes-over '-)
  (distributes-over '/)
  (has-identity 1)
  is-commutative
  is-associate)

(define-arithmetic-op /
  (distributes-over '+)
  (distributes-over '-)
  (distributes-over '*)
  (has-left-identity 1)
  is-commutative
  is-associate)
