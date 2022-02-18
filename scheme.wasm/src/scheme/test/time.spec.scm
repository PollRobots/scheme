#|
 |  Test time operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "time"
  (test-case "(current-second)" (lambda ()
    (assert (real? (current-second)))
    (assert (positive? (current-second)))
    ; current time must be after this code was first tested
    (assert (> (current-second) 1645151160))
    ))

  (test-case "(current-jiffy)" (lambda ()
    (assert (integer? (current-jiffy)))
    (assert (positive? (current-jiffy)))))

  (test-case "(jiffies-per-second)" (lambda ()
    (assert (real? (jiffies-per-second))
    ; should be at least millisecond jiffieness
    (assert (>= (jiffies-per-second 1000))))))

  (test-case "time sanity check (slow)" (lambda ()
    (define check
      (let* ((start-second (current-second))
            (start-jiffy (current-jiffy))
            (end-jiffy (+ start-jiffy (/ (jiffies-per-second) 4))))
            (letrec ((inner (lambda ()
                              (if (>= (current-jiffy) end-jiffy) 
                                  (- (current-second) start-second)
                                  (inner)))))
                  (inner))))
      
    (assert (>= check 0.25))
    (assert (< check 0.35))))
)
    