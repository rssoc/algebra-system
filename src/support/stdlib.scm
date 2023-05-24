;;; stdlib.scm -- That "stdlib" file that every Scheme project has.
;;;
;;; Scheme is famously under-specified, the SRFI's don't help much
;;; either. This file provides some miscellaneous functions to help
;;; round things off.



(define (push! obj lst)
  (let ((x  (car lst))
        (xs (cdr lst)))
    (set-car! lst obj)
    (set-cdr! lst (cons x xs))))

(define (pop! lst)
  (let ((x  (car lst))
        (xs (cdr lst)))
    (set-car! lst (car xs))
    (set-cdr! lst (cdr xs))
    x))



(define (map-tree procedure xs)
  (cond ((null? xs) '())
        ((list? xs)
         (xcons
          (map-tree procedure (cdr xs))
          (map-tree procedure (car xs))))
        (else
         (procedure xs))))

(define (filter-tree predicate xs)
  (cond ((null? xs) '())
        ((list? xs)
         (xcons
          (filter-tree predicate (cdr xs))
          (filter-tree predicate (car xs))))
        (else
         (if (predicate xs) (list xs) '()))))

(define (butlast xs)
  (reverse (cdr (reverse xs))))



(define (string-take-right string nchars)
  (let ((n (string-length string)))
    (let ((start (fx- n nchars)))
      (if (> 0 start)
          ""
          (substring string (fx- n nchars) n)))))
