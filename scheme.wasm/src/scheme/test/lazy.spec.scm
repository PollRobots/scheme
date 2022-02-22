#|
 |  Test lazy operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "lazy"
  (test-case "delay, delay-force, and force" (lambda ()
    (assert-equal (force (delay (+ 1 2))) 3)

    (let ((p (delay (+ 1 2))))
      (assert-equal (force p) 3)
      (assert-equal (force p) 3))

    (define integers
      (letrec ((next
                  (lambda (n)
                    (delay (cons n (next (+ 1 n)))))))
        (next 0)))
    (define (head stream) (car (force stream)))
    (define (tail stream) (cdr (force stream)))

    (assert-equal (head (tail (tail integers))) 2)

    (define (stream-filter p? s)
      (delay-force
        (if (null? (force s))
          (delay '())
          (let ((h (car (force s)))
                (t (cdr (force s))))
            (if (p? h)
              (delay (cons h (stream-filter p? t)))
              (stream-filter p? t))))))
    (assert-equal (head (tail (tail (stream-filter odd? integers)))) 5)

    (define count 0)
    (define p
      (delay (begin (set! count (+ count 1))
                    (if (> count x)
                      count
                      (force p)))))
    (define x 5)
    (assert (promise? p) "p should be a promise, not: " p)
    (assert-equal (force p) 6)
    (assert (promise? p) "p should still be a promise, not: " p)
    (begin (set! x 10)
      (assert-equal (force p) 6))))

  (test-case "(promise? <obj>)" (lambda ()
    (assert (promise? (delay 1)))
    (assert (promise? (delay-force 1)))
    (assert (promise? (make-promise 1)))
    (assert-not (promise? 1))
    (assert-not (promise? #t))))

  (test-case "(make-promise <expression>)" (lambda ()
    (define x 2)
    (define p (make-promise (+ 1 x)))
    (assert (promise? p))
    (set! x 3)
    (assert-equal (force p) 3)

    (define pp (delay (+ 1 x)))
    (assert (promise? pp))
    (set! x 7)
    (assert-equal (force pp) 8)))
)
