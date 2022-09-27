(load "./wrapper/unit.scm")
(load "./compose/combine.scm")
;load 관련 문서 https://edoras.sdsu.edu/doc/mit-scheme-9.2/mit-scheme-user/Loading-Files.html

;let 관련 문서 https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_6.html
(define result (let ((x 2) (y 3))
  (* x y)))
(display result)