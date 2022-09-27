(define compose 
  (lambda (f g)
    (lambda args
      (f (g args)))))

; (define compose 
;   (lambda (f g)
;     (lambda args
;       (f (g args)))))

; (define compose 
;   (lambda (f g)
;     (lambda args
;       (f (apply g args)))))

; (define (compose f g)
;   (lambda args 
;     (f (apply g args))))

(define result (
  (compose (lambda (x) (list 'foo x))
	  (lambda (x) (list 'bar x)))
 'z))

(display result)
