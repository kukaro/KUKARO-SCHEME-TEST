(define (make-unit-conversion to from)
  (let ((to-op (make-apply-hook to #f))
        (from-op (make-apply-hook from #f)))
    (set-apply-hook-extra! to-op
                           (make-unit-conversion-record from-op))
    (set-apply-hook-extra! from-op
                           (make-unit-conversion-record to-op))
    to-op))

(define-record-type <unit-conversion-record>
    (make-unit-conversion-record inverse)
    unit-conversion-record?
  (inverse unit-conversion-record-inverse))

(define (unit-conversion? object)
  (and (apply-hook? object)
       (unit-conversion-record? (apply-hook-extra object))))
(register-predicate! unit-conversion? 'unit-conversion)


(define (unit:invert unit-conversion)
  (guarantee unit-conversion? unit-conversion)
  (unit-conversion-record-inverse (apply-hook-extra unit-conversion)))

(define (unit-specializer procedure implicit-output-unit
                          . implicit-input-units)

  (define (specializer specific-output-unit
                       . specific-input-units)
    (let ((output-converter
           (make-converter implicit-output-unit
                           specific-output-unit))
          (input-converters
           (map make-converter
                specific-input-units
                implicit-input-units)))

      (define (specialized-procedure . arguments)
        (output-converter
         (apply procedure
                (map (lambda (converter argument)
                       (converter argument))
                     input-converters
                     arguments))))

      specialized-procedure))

  specializer)

(define (make-converter input-unit output-unit)
  (or (and (equal? input-unit output-unit)
           identity-unit-conversion)
      (hash-table-ref/default unit-conversion-table
                              (cons input-unit output-unit)
                              #f)
      (error "Unable to find unit converter:" input-unit output-unit)))

(define unit-conversion-table
  (make-equal-hash-table))

(define (register-unit-conversion input-unit output-unit unit-conversion)
  (hash-table-set! unit-conversion-table
                   (cons input-unit output-unit)
                   unit-conversion)
  (hash-table-set! unit-conversion-table
                   (cons output-unit input-unit)
                   (unit:invert unit-conversion)))

(define (register-expt-conversion input-type output-type exponent converter)
  (register-unit-conversion `(expt ,input-type ,exponent)
                            `(expt ,output-type ,exponent)
                            (unit:expt converter exponent)))

(let-syntax
    ((define-override
       (er-macro-transformer
        (lambda (form rename compare)
          `(,(rename 'define)
            ,(symbol 'n: (cadr form))
            (,(rename 'access)
             ,(cadr form)
             ,(rename 'system-global-environment)))))))
  (define-override *)
  (define-override +)
  (define-override -)
  (define-override /)
  (define-override <)
  (define-override <=)
  (define-override =)
  (define-override >)
  (define-override >=)
  (define-override abs)
  (define-override acos)
  (define-override angle)
  (define-override asin)
  (define-override atan)
  (define-override boolean?)
  (define-override ceiling)
  (define-override cell?)
  (define-override complex?)
  (define-override cos)
  (define-override exact-integer?)
  (define-override exact-nonnegative-integer?)
  (define-override exact-positive-integer?)
  (define-override exact-rational?)
  (define-override exp)
  (define-override expt)
  (define-override floor)
  (define-override imag-part)
  (define-override integer?)
  (define-override list?)
  (define-override log)
  (define-override magnitude)
  (define-override make-bundle-predicate)
  (define-override make-cell)
  (define-override make-polar)
  (define-override make-rectangular)
  (define-override max)
  (define-override min)
  (define-override negative?)
  (define-override non-empty-list?)
  (define-override null?)
  (define-override number?)
  (define-override pair?)
  (define-override positive?)
  (define-override pp)
  (define-override pretty-print)
  (define-override procedure?)
  (define-override rational?)
  (define-override real-part)
  (define-override real?)
  (define-override remainder)
  (define-override round)
  (define-override sin)
  (define-override sqrt)
  (define-override square)
  (define-override string?)
  (define-override symbol?)
  (define-override tan)
  (define-override truncate)
  (define-override vector?)
  (define-override zero?))
 
(define (n:sign n)
  (guarantee n:real? n 'n:sign)
  (cond ((n:positive? n) +1)
        ((n:negative? n) -1)
        (else 0)))
 
(define (n:negate x)
  (n:- 0 x))
 
(define (n:invert x)
  (n:/ 1 x))

(define (unit:expt conversion n)
  (let ((positive-case
         (lambda (conversion n)
           (let loop ((count n))
             (cond ((n:= count 1)
                    conversion)
                   ((even? count)
                    (let ((a (loop (n:/ count 2))))
                      (unit:* a a)))
                   (else
                    (unit:* conversion
                            (loop (n:- count 1)))))))))
    (cond ((n:= n 0)
           identity-unit-conversion)
          ((n:< n 0)
           (positive-case (unit:invert conversion)
                          (n:negate n)))
          (else
           (positive-case conversion n)))))

(define (unit:* u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit:invert u1)
                                 (unit:invert u2))))