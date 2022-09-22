(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (identity x) x)

(display (((iterate 3) square) 5))
