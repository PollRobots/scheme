#|
 |  Test trig operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "trig"
  (test-case "sin²x + cos²x = 1" (lambda ()
    (letrec ((id (lambda (x)
                (let ((sx (sin x)) (cx (cos x)))
                  (+ (* sx sx) (* cx cx)))))
             (test (lambda (n)
                (cond
                  ((< n 6.3) ;; test up to two pi:w
                    (assert
                      (< (abs (- 1 (id n))) 1e-13)
                      " sin²x + cos²x of " n " = " (id n))
                    (test (+ n 0.1)))
                  (else #t)))))
      (test 0)))))
