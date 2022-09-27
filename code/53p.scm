(define (compose f g)
	(define (the-composition . args)
		(f (apply g args)))
	the-composition)

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(display (list 1 2))
(newline)

(define result ((parallel-combine list
  (lambda (x y z) (list 'foo x y z))
  (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c))

(display result)
(newline)

(define test '(+ 1 2))
; 아래 로직으로 single quote(')연산자는 평가를 하지 않는다.
; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Quoting.html
(display (list? test))
(newline)
(display (eval test user-initial-environment))
(newline)
; eval에 대한 설명
; https://courses.cs.washington.edu/courses/cse341/03wi/scheme/eval-apply.html