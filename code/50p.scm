(define (compose f g)
  (lambda args 
    (f (apply g args))))

(define result ((compose (lambda (x) (list 'foo x))
	  (lambda (x) (list 'bar x)))
 'z))

(display result)
