#|
 |  Test case-lambda
 |#

(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "case-lambda"
  (test-case "(case-lambda <clause> ...)" (lambda ()
    (define range (case-lambda
                    ((a) (range 0 a 1))
                    ((a b) (range a b 1))
                    ((a b c) (letrec ((fn (lambda (x)
                                            (if (>= x b) 
                                                '() 
                                                (cons x (fn (+ x c)))))))
                                    (fn a)))))

    (assert-equal (range 4) '(0 1 2 3))
    (assert-equal (range 2 4) '(2 3))
    (assert-equal (range 4 2) '())
    (assert-equal (range 2 10 3) '(2 5 8))
    (assert-error (range 1 2 3 4))))
)