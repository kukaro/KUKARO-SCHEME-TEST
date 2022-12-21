(define (abs x)
    (cond ((< x 0) (- x))
      ((= x 0) x)
      ((> x 0) x)))

(display (abs -10))
(display "\n")
(display (abs 20))
(display "\n")