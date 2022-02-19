#|
 |  Test sequence operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "sequence"
  (test-case "begin at body scope" (lambda ()
    (begin
      (define x 10)
      (assert-equal x 10))))

  (test-case "begin at expression scope" (lambda ()
    (assert-error 
      (if #t
        (begin
          (define x 10)
          (assert-equal x 10))))))
)