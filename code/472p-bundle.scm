(load "./common/overrides.scm")
(load "./common/utils.scm")

(define (make-point x y)
    (define (get-x) x)
    (define (get-y) y)
    (define (set-x! new-x) (set! x new-x))
    (define (set-y! new-y) (set! y new-y))
    (bundle point? get-x get-y set-x! set-y!))

(define point? (make-bundle-predicate 'point))
(define p1 (make-point 3 4))
(define p2 (make-point -1 1))
(display p1)
(newline)
(display p2)
(newline)
(display (point? p1))
(newline)
(display (point? p2))
(newline)
(display (p1 'get-x))
(newline)

;왜 point?를 빼면 에러가 날까?

(display point?)