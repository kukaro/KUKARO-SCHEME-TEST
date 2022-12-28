(load "./common/overrides.scm")
(load "./common/collections.scm")
(load "./common/predicates.scm")
(load "./common/generic-procedures.scm")
(load "./common/applicability.scm")
(load "./user-defined-types/generic.scm")
(load "./common/memoizer.scm")
(load "./user-defined-types/tagging.scm")
(load "./user-defined-types/tags.scm")
(load "./common/arith.scm")
(load "./common/numeric-arith.scm")
(load "./propagation/example-support.scm")
(load "./propagation/hms-dms-radians.scm")

(define parsec-in-meters
  (/ AU-in-meters (tan (dms->radians (list 0 0 1)))))

(define AU-in-parsecs
  (/ AU-in-meters parsec-in-meters))
; AU를 선언하기 위서 가져온다

(define-c:prop (c:parallax<->distance parallax distance)
  (let-cells (t (AU AU-in-parsecs))
    (c:tan parallax t)
    (c:* t distance AU)))