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



(define ((depth-first handler) xs)
  (let ((step (depth-first handler)))
    (if (list? xs)
        (handler (map step xs))
        (handler xs))))

(define ((breath-first handler) xs)
  (let ((step (breath-first handler)))
    (if (list? xs)
        (map step (handler xs))
        (handler xs))))

(define ((walk how #!optional combine) handler xs)
  (let ((results '()))
    (define (compose f g)
      (lambda (x) (f (g x))))
    (define (record-result! x)
      (set! results (cons x results))
      x)
    (if (default-object? combine)
        ((how handler) xs)
        (begin
          ((how (lambda (x)
                  (record-result! (handler x))
                  x))
           xs)
          (combine results)))))

(define (collect-if predicate xs)
  ((walk depth-first concatenate)
   (lambda (x) (if (predicate x) (list x) '()))
   xs))

(define (only-on predicate handler xs)
  ((walk depth-first)
   (lambda (x) (if (predicate x) (handler x) x))
   xs))



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
