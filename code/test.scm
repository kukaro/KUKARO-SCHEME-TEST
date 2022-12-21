(define (compose . args)
  (compose* args))
 
(define (compose* args)
  (case (length args)
    ((0) (lambda (x) x))
    ((1) (car args))
    (else (reduce-right (lambda (f g)
                          (lambda (x) (f (g x))))
                        (lambda (x) x)
                        args))))
 
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
 
(define (unit:* u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit:invert u1)
                                 (unit:invert u2))))
 
(define (unit:/ u1 u2)
  (unit:* u1 (unit:invert u2)))
 
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
 
;;;; Unit-conversion registry
 
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
 
(define (make-converter input-unit output-unit)
  (or (and (equal? input-unit output-unit)
           identity-unit-conversion)
      (hash-table-ref/default unit-conversion-table
                              (cons input-unit output-unit)
                              #f)
      (error "Unable to find unit converter:" input-unit output-unit)))
 
(define unit-conversion-table
  (make-equal-hash-table))
 
;;; Creates a factory that produces a procedure specialized for given units.
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
 
;;;; Unit converters
 
(define identity-unit-conversion
  (make-unit-conversion (lambda (x) x)
                        (lambda (x) x)))
 
(define inch-to-meter
  (let ((meters-per-inch .0254))
    (make-unit-conversion (lambda (inches)
                            (* inches meters-per-inch))
                          (lambda (meters)
                            (/ meters meters-per-inch)))))
(register-unit-conversion 'inch 'meter inch-to-meter)
 
(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))
(register-unit-conversion 'fahrenheit 'celsius fahrenheit-to-celsius)
 
(define celsius-to-kelvin
  (let ((zero-celsius 273.15))
    (make-unit-conversion (lambda (c) (+ c zero-celsius))
                          (lambda (k) (- k zero-celsius)))))
(register-unit-conversion 'celsius 'kelvin celsius-to-kelvin)
 
(define pound-to-newton
  (let ((pounds-per-newton 0.224808943))
    (make-unit-conversion (lambda (pounds)
                            (/ pounds pounds-per-newton))
                          (lambda (newtons)
                            (* newtons pounds-per-newton)))))
(register-unit-conversion 'pound 'newton pound-to-newton)
 
(register-expt-conversion 'inch 'meter 3 inch-to-meter)
 
;; coderef: fahrenheit-to-kelvin
(register-unit-conversion 'fahrenheit 'kelvin
                          (unit:* fahrenheit-to-celsius celsius-to-kelvin))
 
;; coderef: psi-to-nm2
(register-unit-conversion '(/ pound (expt inch 2))
                          '(/ newton (expt meter 2))
                          (unit:/ pound-to-newton
                                  (unit:expt inch-to-meter 2)))
 
 
(define psi-to-nsm
  (compose pound-to-newton
           (unit:invert inch-to-meter)
           (unit:invert inch-to-meter)))
 
(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))
 
(define gas-constant 8.3144621)         ;J/(K*mol)
 
(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))
 
(define pi (* 4 (atan 1 1)))
 
(define make-specialized-gas-law-volume
  (unit-specializer gas-law-volume
                    '(expt meter 3)
                    '(/ newton (expt meter 2))
                    'kelvin
                    'mole))
 
(define conventional-gas-law-volume
  (make-specialized-gas-law-volume '(expt inch 3)
                                   '(/ pound (expt inch 2))
                                   'fahrenheit
                                   'mole))