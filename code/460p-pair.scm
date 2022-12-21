; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_8.html
; pair 관련된 문서
(define a-pair (cons 1 2))
; pair는 cons로 생성한다
(display a-pair)
(newline)
(display (string "car a-pair : " (car a-pair) "\n"))
(display (string "cdr a-pair : " (cdr a-pair) "\n"))