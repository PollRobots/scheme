#|
 |  Test boolean operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "boolean"
  (test-case "(boolean? <obj>)" (lambda ()
    (assert (boolean? #t))
    (assert (boolean? #f))
    (assert (boolean? #true))
    (assert (boolean? #false))
    (assert-not (boolean? 0))
    (assert-not (boolean? '()))))

  (test-case "(boolean=? <boolean1> <boolean2> <boolean3> ...)" (lambda ()
    (assert (boolean=? #t #true (= 1 1)))
    (assert (boolean=? #f #false (= 1 2)))
    (assert-not (boolean=? #t #f))
    (assert-not (boolean=? 1 2))))

  (test-case "(not <obj>)" (lambda ()
    (assert-not (not #t))
    (assert-not (not 3))
    (assert-not (not (list 3)))
    (assert (not #f))
    (assert-not (not '()))
    (assert-not (not (list)))
    (assert-not (not 'nil))))
)
