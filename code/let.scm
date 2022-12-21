; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_3.html
 (display (let ((x 2) (y 3))
  (display (string "x * y : " (* x y) "\n"))
  (display (string "x + y : " (+ x y) "\n"))
  (- x y))))
(newline)
;let을 사용할 때 마지막 변수만 return이된다.

(define a 20)
(define b 30)
(display (* a b))
(newline)
; block 스코프 만들려면 let을 사용하면된다