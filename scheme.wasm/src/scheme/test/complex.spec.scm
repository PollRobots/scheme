(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "complex"
  (test-case "(make-rectangular <x1> <x2>)" (lambda ()
    (assert-equal (number->string (make-rectangular 1 2)) "1+2i")
    (assert-equal (number->string (make-rectangular 1 0)) "1")
    (assert (number->string (make-rectangular 1.23 -4/5)))))

 (test-case "(real-part <z>)" (lambda ()
    (assert-equal (real-part (make-rectangular 1 2)) 1)
    (assert-equal (real-part (make-rectangular 1 0)) 1)
    (assert-equal (real-part (make-rectangular 1.23 -4/5)) 1.23)
    (assert-equal (real-part 7) 7)))

  (test-case "(imag-part <z>)" (lambda ()
    (assert-equal (imag-part (make-rectangular 1 2)) 2)
    (assert-equal (imag-part (make-rectangular 1 0)) 0)
    (assert-equal (imag-part (make-rectangular 1.23 -4/5)) -4/5)
    (assert-equal (imag-part 7) 0)))
)
