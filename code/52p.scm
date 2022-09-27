(define (compose f g)
	(define (the-composition . args)
		(f (apply g args)))
	the-composition)

(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (identity x) x)

; (display (((iterate 3) square) 5))
(display (iterate 3))
(display "\n")
(display ((iterate 3) square))
(display "\n")
(display (((iterate 3) square) 5))
(display "\n")

; (((iterate 3) square) 5)
; (display (square 4))