#|
 |  Test conditional operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "control"
  (test-case "(cond <clause1> <clause2> ...)" (lambda ()
    (assert-equal
      (cond ((> 3 2) 'greater)
            ((< 3 2) 'less))
      'greater)
    (assert-equal
      (cond ((> 3 3) 'greater)
            ((< 3 3) 'less)
            (else 'equal))
      'equal)
    (assert-equal
      (cond ((assv 'b '((a 1) (b 2))) => cadr)
            (else #f))
      2)))

  (test-case "(case <key> <clause1> <clause2> ...)" (lambda ()
    (assert-equal
      (case (* 2 3)
        ((2 3 5 7) 'prime)
        ((1 4 6 8 9) 'composite))
      'composite)
    (assert-equal
      (case (car '(c d))
        ((a) 'a)
        ((b) 'b)
        (else 'unknown))
      'unknown)
    (assert-equal
      (case (car '(a b))
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
      'vowel)
    (assert-equal
      (case (car '(c d))
        ((a e i o u) 'vowel)
        ((w y) 'semivowel)
        (else => (lambda (x) x)))
      'c)))

  (test-case "(and <test1> ...)" (lambda ()
    (assert (and (= 2 2) (> 2 1)))
    (assert-not (and (= 2 2) (< 2 1)))
    (assert-equal (and 1 2 'c '(f g)) '(f g))
    (assert (and))))

  (test-case "(or <test1> ...)" (lambda ()
    (assert (or (= 2 2) (> 2 1)))
    (assert (or (= 2 2) (< 2 1)))
    (assert-not (or #f #f #f))
    (assert-equal (or (memq 'b '(a b c)) (/ 3 0)) '(b c))
    (assert-not (or))))

  (test-case "(when <test> <expr1> <expr2> ...)" (lambda ()
    (define x 0)
    (define y 0)
    (when (= 1 1)
      (set! x 1)
      (set! y 2))
    (assert-equal x 1)
    (assert-equal y 2)

    (when (= 1 0)
      (set! x 2)
      (set! y 1))
    (assert-equal x 1)
    (assert-equal y 2)))

  (test-case "(unless <test> <expr1> <expr2> ...)" (lambda ()
    (define x 0)
    (define y 0)
    (unless (= 1 1)
      (set! x 1)
      (set! y 2))
    (assert-equal x 0)
    (assert-equal y 0)

    (unless (= 1 0)
      (set! x 2)
      (set! y 1))
    (assert-equal x 2)
    (assert-equal y 1)))

    #|
     | (test-case "(cond-expand <ce-clause1> <ce-clause2> ...)" (lambda ()
     |   ; Implement test for cond-expand when cond-expand has been implemented.
     | ))
     |#
 )
  ))
)
    
    
