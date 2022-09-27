(define (compose f g)
	(define (the-composition . args)
		(f (apply g args)))
	the-composition)

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)