; (define compose 
;   (lambda (f g)
;     (lambda args
;       (f (g args)))))

(define (compose f g)
  (define (the-composition . args)
    (f (g args)))
  the-composition)

(define result (
  (compose (lambda (x) (list 'foo x))
	  (lambda (x) (list 'bar x)))
 'z))

(display result)
