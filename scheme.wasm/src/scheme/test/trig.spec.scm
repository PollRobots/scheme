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
                  ((< n 6.3) ;; test up to 2π
                    (assert
                      (< (abs (- 1 (id n))) 1e-13)
                      " sin²x + cos²x of " n " = " (id n))
                    (test (+ n 0.05)))
                  (else #t)))))
      (test 0))))

  (test-case "tan x = sin x / cos x" (lambda ()
    (letrec ((id (lambda (x)
                  (let ((dx (- (tan x) (/ (sin x) (cos x)))))
                    (* dx dx))))
             (test (lambda (acc x n)
                (cond
                  ((< x 6.3) ;; test up to 2π
                   (assert
                      (< (id x) 1e-9)
                      "(tan x - sin x / cos x)² of " x " = " (id x))
                   (test (+ acc (id x)) (+ x 0.05) (+ n 1)))
                  (else (let ((rms (sqrt (/ acc n))))
                          (assert (< rms 1e-5) " rms error is " rms)))))))

      (test 0 0 0))))
)
