(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "include-ci"
  (test-case "loads upper case file" (lambda ()
    (include-ci "test/include-ci.test.scm")
    (assert (procedure? fact))
    (assert-equal (fact 10) 3628800))))
