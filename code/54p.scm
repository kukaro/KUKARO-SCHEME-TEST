(define (compose f g)
	(define (the-composition . args)
		(f (apply g args)))
	the-composition)

(define (spread-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (display (string "n:" n ", m:" m ", t:" t "\n"))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
          (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (restrict-arity proc nargs) 
  (hash-table-set! arity-table proc nargs) 
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
    (let ((a (procedure-arity proc))) ;항수가 해시 테이블에 없는 경우 
      (assert (eqv? (procedure-arity-min a)
                    (procedure-arity-max a))) 
      (procedure-arity-min a))))

(define arity-table (make-key-weak-eqv-hash-table))

(define result ((spread-combine list
  (lambda (x y) (list 'foo x y))
  (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c 'd 'e))

(display result)
(newline)