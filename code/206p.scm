(load "./term-rewrite/matcher.scm")
(load "./term-rewrite/overrides.scm")
(load "./term-rewrite/collections.scm")
(load "./term-rewrite/predicates.scm")
(load "./term-rewrite/generic-procedures.scm")
(load "./term-rewrite/predicate-counter.scm")
(load "./term-rewrite/applicability.scm")
(load "./term-rewrite/match-utils.scm")
(load "./term-rewrite/rule-implementation.scm")
(load "./term-rewrite/rules.scm")

(define algebra-1
    (rule-simplifier
        (list
        ;;덧셈의 결합법칙
        (rule '(+ (? a) (+ (? b) (? c))) 
            `(+ (+ ,a ,b) ,c))
        ;;곱셈의 교환법칙
        (rule '(* (? b) (? a)) 
            (and (expr<? a b) 
                `(* ,a ,b)))
        ;;덧셈에 대한 곱셈의 분배법칙
        (rule '(* (? a) (+ (? b) (? c))) 
            `(+ (* ,a ,b) (* ,a ,c))))))

(define result (algebra-1 '(* (+ y (+ z w)) x)))
(display result)
(newline)
(define a (list-ref result 0)) ;;첫번째 원소를 뽑는 방법
(define r (car (cdr result))) ;;두번째 원소를 뽑는 방법
(display r)
(newline)
(display a)
(newline)