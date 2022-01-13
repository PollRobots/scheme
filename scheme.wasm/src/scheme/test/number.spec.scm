#|
 |  Test numerical operations.
 |
 |  per § 6.2.6 of the r7rs spec.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "number"
  (test-case "(number? <obj>)" (lambda ()
    (assert (number? 1))
    (assert (number? 2.3))
    (assert (number? 456789012345678901234567890))
    (assert (number? #xfabc0de))
    (assert (number? #o755))
    (assert (number? #b1001011001))
    (assert (number? 1/2))
    (assert (number? 7/19))
    (assert (number? #e1.23))
    (assert (number? #d1234))
    (assert (number? +inf.0))
    (assert (number? -inf.0))
    (assert (number? +nan.0))
    (assert (number? -nan.0))
    (assert-not (number? '()))
    (assert-not (number? 'a))
    (assert-not (number? #t))
    (assert-not (number? '(1 2 3)))
    (assert-not (number? #(1 2 3)))
    (assert-not (number? #u8(1 2 3)))
    (assert-not (number? (lambda (x) x)))))

  ; Complex numbers are not currently supported,
  ; so complex? is equivalent to number?
  (test-case "(complex? <obj>)" (lambda ()
    (assert (complex? 1))
    (assert (complex? 2.3))
    (assert (complex? 456789012345678901234567890))
    (assert (complex? #xfabc0de))
    (assert (complex? #o755))
    (assert (complex? #b1001011001))
    (assert (complex? 1/2))
    (assert (complex? 7/19))
    (assert (complex? #e1.23))
    (assert (complex? #d1234))
    (assert (complex? +inf.0))
    (assert (complex? -inf.0))
    (assert (complex? +nan.0))
    (assert (complex? -nan.0))
    (assert-not (complex? '()))
    (assert-not (complex? 'a))
    (assert-not (complex? #t))
    (assert-not (complex? '(1 2 3)))
    (assert-not (complex? #(1 2 3)))
    (assert-not (complex? #u8(1 2 3)))
    (assert-not (complex? (lambda (x) x)))))

  (test-case "(real? <obj>)" (lambda ()
    (assert (real? 1))
    (assert (real? 2.3))
    (assert (real? 456789012345678901234567890))
    (assert (real? #xfabc0de))
    (assert (real? #o755))
    (assert (real? #b1001011001))
    (assert (real? #d1234))
    (assert (real? 1/2))
    (assert (real? 7/19))
    (assert (real? #e1.23))
    (assert (real? +inf.0))
    (assert (real? -inf.0))
    (assert (real? +nan.0))
    (assert (real? -nan.0))
    (assert-not (real? '()))
    (assert-not (real? 'a))
    (assert-not (real? #t))
    (assert-not (real? '(1 2 3)))
    (assert-not (real? #(1 2 3)))
    (assert-not (real? #u8(1 2 3)))
    (assert-not (real? (lambda (x) x)))))

  (test-case "(rational? <obj>)" (lambda ()
    (assert (rational? 1))
    (assert-not (rational? 2.3))
    (assert (rational? 456789012345678901234567890))
    (assert (rational? #xfabc0de))
    (assert (rational? #o755))
    (assert (rational? #b1001011001))
    (assert (rational? #d1234))
    (assert (rational? 1/2))
    (assert (rational? 7/19))
    (assert (rational? #e1.23))
    (assert-not (rational? +inf.0))
    (assert-not (rational? -inf.0))
    (assert-not (rational? +nan.0))
    (assert-not (rational? -nan.0))
    (assert-not (rational? '()))
    (assert-not (rational? 'a))
    (assert-not (rational? #t))
    (assert-not (rational? '(1 2 3)))
    (assert-not (rational? #(1 2 3)))
    (assert-not (rational? #u8(1 2 3)))
    (assert-not (rational? (lambda (x) x)))))

  (test-case "(integer? <obj>)" (lambda ()
    (assert (integer? 1))
    (assert (integer? 3.0))
    (assert-not (integer? 2.3))
    (assert (integer? 456789012345678901234567890))
    (assert (integer? #xfabc0de))
    (assert (integer? #o755))
    (assert (integer? #b1001011001))
    (assert (integer? #d1234))
    (assert-not (integer? 1/2))
    (assert-not (integer? 7/19))
    (assert-not (integer? #e1.23))
    (assert-not (integer? +inf.0))
    (assert-not (integer? -inf.0))
    (assert-not (integer? +nan.0))
    (assert-not (integer? -nan.0))
    (assert-not (integer? '()))
    (assert-not (integer? 'a))
    (assert-not (integer? #t))
    (assert-not (integer? '(1 2 3)))
    (assert-not (integer? #(1 2 3)))
    (assert-not (integer? #u8(1 2 3)))
    (assert-not (integer? (lambda (x) x)))))

  (test-case "(exact? <z>)" (lambda ()
    (assert (exact? #e3.0))
    (assert (exact? #e1.23e2))
    (assert-not (exact? 3.0))))

  (test-case "(inexact? <z>)" (lambda ()
    (assert (inexact? 3.0))
    (assert-not (inexact? #e3.0))
    (assert-not (inexact? #e1.23e2))))

  (test-case "(exact-integer? <z>)" (lambda ()
    (assert (exact-integer? 32))
    (assert-not (exact-integer? 32.0))))

  (test-case "(finite? <z>)" (lambda ()
    (assert (finite? 3))
    (assert-not (finite? +inf.0))))

  (test-case "(infinite? <z>)" (lambda ()
    (assert-not (infinite? 3))
    (assert (infinite? +inf.0))
    (assert (infinite? -inf.0))
    (assert-not (infinite? +nan.0))))

  (test-case "(nan? <z>)" (lambda ()
    (assert (nan? +nan.0))
    (assert (nan? -nan.0))
    (assert-not (nan? 3.0))
    (assert-not (nan? 320))
    (assert-not (nan? +inf.0))))

  (test-case "(= <z_1> <z_2> <z_3> ...)" (lambda ()
    (assert (= 1 1 1))
    (assert (= 2 2.0 #i2 #e2))
    (assert-not (= 1 2))))

  (test-case "(< <z_1> <z_2> <z_3> ...)" (lambda ()
    (assert (< 1 2 3 40 500))
    (assert (< 1 2 3 #i40 5.123e2))
    (assert-not (< 1 2 3 3 40 50))
    (assert-not (< 1 1))
    (assert-not (< 2 1))))

  (test-case "(<= <z_1> <z_2> <z_3> ...)" (lambda ()
    (assert (<= 1 2 3 40 500))
    (assert (<= 1 2 3 3.0 #i40 5.123e2))
    (assert (<= 1 2 3 3 40 50))
    (assert (<= 1 1.0))
    (assert-not (<= 2 1))))

  (test-case "(> <z_1> <z_2> <z_3> ...)" (lambda ()
    (assert (> 500 40 3 2 1))
    (assert (> 5.123e2 #i40 3 2 1))
    (assert-not (> 50 40 3 3 2 1))
    (assert-not (> 1 1))
    (assert-not (> 1 2))))

  (test-case "(>= <z_1> <z_2> <z_3> ...)" (lambda ()
    (assert (>= 500 40 3 2 1))
    (assert (>= 5.123e2 #i40 3 3.0 2 1))
    (assert (>= 50 40 3 3 2 1))
    (assert (>= 1 1))
    (assert-not (>= 1 2))))

  (test-case "(zero? <z>)" (lambda ()
    (assert (zero? 0))
    (assert (zero? 0.0))
    (assert-not (zero? 123))
    (assert-not (zero? -nan.0))
    (assert-not (zero? -inf.0))
    (assert-not (zero? 1e-20))))

  (test-case "(positive? <z>)" (lambda ()
    (assert (positive? 1))
    (assert (positive? 12345678901234567890))
    (assert (positive? 1e-20))
    (assert-not (positive? 0))
    (assert-not (positive? -1))))

  (test-case "(negative? <z>)" (lambda ()
    (assert (negative? -1))
    (assert (negative? -12345678901234567890))
    (assert (negative? -1e-20))
    (assert-not (negative? 0))
    (assert-not (negative? 1))))

  (test-case "(min <x1> <x2> ...)" (lambda ()
    (assert-equal (min 3 4) 3)
    (assert-equal (min 4.1 4) 4)
    (assert (inexact? (min 4.1 4)))
    (assert-equal (min 1 4 73 -3 17 -54) -54)))

  (test-case "(max <x1> <x2> ...)" (lambda ()
    (assert-equal (max 3 4) 4)
    (assert-equal (max 3.9 4) 4)
    (assert (inexact? (max 3.9 4)))
    (assert-equal (max 1 4 73 -3 17 -54) 73)))

  (test-case "(+ <z1> ...)" (lambda ()
    (assert-equal (+ 3 4) 7)
    (assert-equal (+ 3) 3)
    (assert-equal (+) 0)))

  (test-case "(* <z1> ...)" (lambda ()
    (assert-equal (* 3 4) 12)
    (assert-equal (* 3) 3)
    (assert-equal (*) 1)))

  (test-case "(- <z1> ...)" (lambda ()
    (assert-equal (- 3 4) -1)
    (assert-equal (- 3 4 5) -6)
    (assert-equal (- 3) -3)))

  (test-case "(/ <z1> ...)" (lambda ()
    (assert-equal (/ 3 4 5) 3/20)
    (assert-equal (/ 3) 1/3)))

  (test-case "(floor/ <n1> <n2>)" (lambda ()
    (assert-equal (floor/ 5 2) (values 2 1))
    (assert-equal (floor/ -5 2) (values -3 1))
    (assert-equal (floor/ 5 -2) (values -3 -1))
    (assert-equal (floor/ -5 -2) (values 2 -1))))

  (test-case "(floor-quotient <n1> <n2>)" (lambda ()
    (assert-equal (floor-quotient 5 2) 2)
    (assert-equal (floor-quotient -5 2) -3)
    (assert-equal (floor-quotient 5 -2) -3)
    (assert-equal (floor-quotient -5 -2) 2)))

  (test-case "(floor-remainder <n1> <n2>)" (lambda ()
    (assert-equal (floor-remainder 5 2) 1)
    (assert-equal (floor-remainder -5 2) 1)
    (assert-equal (floor-remainder 5 -2) -1)
    (assert-equal (floor-remainder -5 -2) -1)))

  (test-case "(truncate/ <n1> <n2>)" (lambda ()
    (assert-equal (truncate/ 5 2) (values 2 1))
    (assert-equal (truncate/ -5 2) (values -2 -1))
    (assert-equal (truncate/ 5 -2) (values -2 1))
    (assert-equal (truncate/ -5 -2) (values 2 -1))
    (assert-equal (truncate/ -5.0 -2) (values 2 -1))))

  (test-case "(truncate-quotient <n1> <n2>)" (lambda ()
    (assert-equal (truncate-quotient 5 2) 2)
    (assert-equal (truncate-quotient -5 2) -2)
    (assert-equal (truncate-quotient 5 -2) -2)
    (assert-equal (truncate-quotient -5 -2) 2)))

  (test-case "(truncate-remainder <n1> <n2>)" (lambda ()
    (assert-equal (truncate-remainder 5 2) 1)
    (assert-equal (truncate-remainder -5 2) -1)
    (assert-equal (truncate-remainder 5 -2) 1)
    (assert-equal (truncate-remainder -5 -2) -1)))

  (test-case "(gcd <n1> ...)" (lambda ()
    (assert-equal (gcd 32 -36) 4)
    (assert-equal (gcd 32 36 40) 4)
    (assert-equal (gcd) 0)))

  (test-case "(lcm <n1> ...)" (lambda ()
    (assert-equal (lcm 32 -36) 288)
    (assert-equal (lcm 32 -36.0) 288)
    (assert-equal (lcm) 1)))

  (test-case "(numerator <q>)" (lambda ()
    (assert-equal (numerator (/ 6 4)) 3)
    (assert-equal (numerator 0.75) 3)
    (assert (inexact? (numerator 0.75)))))

  (test-case "(denominator <q>)" (lambda ()
    (assert-equal (denominator 0) 1)
    (assert-equal (denominator (/ 6 4)) 2)
    (assert-equal (denominator (inexact (/ 6 4))) 2)
    (assert (inexact? (denominator (inexact (/ 6 4)))))
    (assert-equal (denominator 0.25) 4)))

  (test-case "(floor <x>)" (lambda ()
    (assert-equal (floor -4.3) -5)
    (assert-equal (floor 3.5) 3)
    (assert-equal (floor +inf.0) +inf.0)
    (assert (nan? (floor +nan.0)))))

  (test-case "(ceiling <x>)" (lambda ()
    (assert-equal (ceiling -4.3) -4)
    (assert-equal (ceiling 3.5) 4)
    (assert-equal (ceiling +inf.0) +inf.0)
    (assert (nan? (ceiling +nan.0)))))

  (test-case "(truncate <x>)" (lambda ()
    (assert-equal (truncate -4.3) -4)
    (assert-equal (truncate 3.5) 3)
    (assert-equal (truncate +inf.0) +inf.0)
    (assert (nan? (truncate +nan.0)))))

  (test-case "(round <x>)" (lambda ()
    (assert-equal (round -4.3) -4)
    (assert-equal (round 3.5) 4)
    (assert-equal (round 7/2) 4)
    (assert-equal (round 7) 7)
    (assert-equal (round +inf.0) +inf.0)
    (assert (nan? (round +nan.0)))))

  (test-case "(rationalize <x> <y>)" (lambda ()
    (assert-equal (rationalize 3/10 1/10) 1/3)
    (assert-equal (rationalize 0.3 1/10) 1/3)))
)