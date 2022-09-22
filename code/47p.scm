(display (+ 1 2))
(display (* 3 2))

(define (compose f g )
  (lambda args
    (f (apply g args))))

