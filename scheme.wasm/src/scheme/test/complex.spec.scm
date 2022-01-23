(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "complex"
  (test-case "(make-rectangular <x1> <x2>)" (lambda ()
    (assert-equal (make-rectangular 1 2) 1+2i)
    (assert-equal (make-rectangular 1 0) 1)
    (assert-equal (make-rectangular +inf.0 -inf.0) +inf.0-inf.0i)
    (assert-equal (make-rectangular 1.23 -4/5) 1.23-4/5i)))

  (test-case "(real-part <z>)" (lambda ()
    (assert-equal (real-part 1+2i) 1)
    (assert-equal (real-part +inf.0-nan.0i) +inf.0)
    (assert-equal (real-part 1.23-4/5i) 1.23)
    (assert-equal (real-part 7) 7)))

  (test-case "(imag-part <z>)" (lambda ()
    (assert-equal (imag-part 1+2i) 2)
    (assert (nan? (real-part +nan.0-nan.0i)))
    (assert-equal (imag-part 1.23-4/5i) -4/5)
    (assert-equal (imag-part 7) 0)))
)
