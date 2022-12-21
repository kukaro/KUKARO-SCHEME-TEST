(load "./wrapper/unit.scm")
(load "./compose/combine.scm")

(define (gas-law-volume pressure temperature amount)
    (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)

(define (sphere-radius volume)
    (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(define fahrenheit-to-celsius
    (make-unit-conversion 
        (lambda (f) (* 5/9 (- f 32)))
        (lambda (c) (+ (* c 9/5) 32))))

(define celsius-to-kelvin
    (let ((zero-celsius 273.15))
        (make-unit-conversion 
            (lambda (c) (+ c zero-celsius))
            (lambda (k) (- k zero-celsius)))))

(define inch-to-meter
    (let ((meters-per-inch .0254))
        (make-unit-conversion 
            (lambda (inches) (* inches meters-per-inch))
            (lambda (meters) (/ meters meters-per-inch)))))

(define pound-to-newton
    (let ((pounds-per-newton 0.224808943))
        (make-unit-conversion 
            (lambda (pounds) (/ pounds pounds-per-newton))
            (lambda (newtons) (* newtons pounds-per-newton)))))

(define psi-to-nsm 
    (compose pound-to-newton
        (unit:invert inch-to-meter)
        (unit:invert inch-to-meter)))

(define result ((unit:invert inch-to-meter)
    (sphere-radius
        (gas-law-volume
            (psi-to-nsm 14.7)
            ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
            1))))

(display result)
(newline)