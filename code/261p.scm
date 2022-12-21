(load "./pattern-matching-on-graphs/lists.scm")

(define (g:cons car cdr)
    (let ((pair (make-graph-node 'pair)))
        (pair 'connect! 'car car)
        (pair 'connect! 'cdr cdr)
        pair))
(define (g:car pair) (pair 'edge-value 'car))
(define (g:cdr pair) (pair 'edge-value 'cdr))