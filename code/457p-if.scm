(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial(- n 1)))))

(display (factorial 6))
(display "\n")
(display (factorial 40))
(display "\n")