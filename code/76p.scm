(define (gas-law-volume pressure temperature amount)
    (/ (* amount gas-constant temperature) pressure))
; 여기서 gas-constant가 없음

(define gas-constant 8.3144621)

(define (sphere-radius volume)
    (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(display (string "pi:" pi "\n"))
(display (sphere-radius 10))
(newline)