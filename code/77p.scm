(load "./wrapper/unit.scm")
(load "./compose/combine.scm")
;load 관련 문서 https://edoras.sdsu.edu/doc/mit-scheme-9.2/mit-scheme-user/Loading-Files.html

(define fahrenheit-to-celsius
    (make-unit-conversion 
        (lambda (f) (* 5/9 (- f 32)))
        (lambda (c) (+ (* c 9/5) 32))))

(define celsius-to-kelvin
    (let ((zero-celsius 273.15))
        (make-unit-conversion 
            (lambda (c) (+ c zero-celsius))
            (lambda (k) (- k zero-celsius)))))

(display (fahrenheit-to-celsius -40))
(newline)
(display (fahrenheit-to-celsius 32))
(newline) ;32를 넣으면 0이 된다.
(display ((unit:invert fahrenheit-to-celsius) 0))
(newline) ;0을 넣으면 32가 된다.
(display ((compose celsius-to-kelvin fahrenheit-to-celsius) 80))
(newline) ;compose를 사용해서 화씨를 섭씨로 바꾼 후 섭씨를 켈빈으로 바꾼다.