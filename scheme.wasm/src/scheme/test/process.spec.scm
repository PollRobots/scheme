(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "process"
  (test-case "(get-environment-variable <name>)" (lambda ()
    (assert-not (get-environment-variable "TEST_ONE"))
    (set-environment-variable "TEST_ONE" "TEST_ONE_VALUE")
    (set-environment-variable "TEST_TWO" "TEST_TWO_VALUE")
    (assert-equal (get-environment-variable "TEST_ONE") "TEST_ONE_VALUE")
  ))

  (test-case "(get-environment-variables)" (lambda ()
    (assert-not (assoc "TEST_THREE" (get-environment-variables)))
    (set-environment-variable "TEST_THREE" "TEST_THREE_VALUE")
    (set-environment-variable "TEST_FOUR" "TEST_FOUR_VALUE")
    (assert-equal (assoc "TEST_THREE" (get-environment-variables)) '("TEST_THREE" . "TEST_THREE_VALUE"))
    (assert-equal (assoc "TEST_FOUR" (get-environment-variables)) '("TEST_FOUR" . "TEST_FOUR_VALUE"))
  ))

  (test-case "(command-line)" (lambda ()
    (assert (list? (command-line)))
    (assert (> (length (command-line)) 0))
  ))
)
