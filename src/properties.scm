;;; properties.scm -- Define some basic algebraic properties that an
;;; operation can have.



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

(define-property has-identity (i)
  (rule (op ?a)
        ?a)
  (rule (op i ?a)
        ?a)
  (rule (op ?a i)
        ?a))



;; (assert-properties! '+
;;  is-commutative
;;  is-associative
;;  (distributes-over '*)
;;  (has-identity 0))
