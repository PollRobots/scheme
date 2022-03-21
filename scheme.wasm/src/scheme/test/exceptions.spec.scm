#|
 |  Test exceptions.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "exceptions"
  (test-case "(with-exception-handler <handler> <thunk>)" (lambda ()
    (assert-equal
      (call-with-current-continuation
        (lambda (cont)
          (with-exception-handler
            (lambda (ex)
              (cont 'exception))
            (lambda ()
              (+ 1 (raise 'an-error))))))
      'exception)))

  (test-case "(raise <obj>)" (lambda ()
    (assert-error (raise 'an-error))))

  (test-case "(raise-continuable <obj>)" (lambda ()
    (assert-equal
      (with-exception-handler
        (lambda (ex) 'handled)
        (lambda () (raise-continuable 'an-error)))
      'handled)))

  (test-case "(error <message> <obj> ...)" (lambda ()
    (assert
      (error-object?
        (call-with-current-continuation (lambda (cont)
          (with-exception-handler
            (lambda (ex) (cont ex))
            (lambda () (error "an error")))))))
    (assert-not
      (error-object?
        (call-with-current-continuation (lambda (cont)
          (with-exception-handler
            (lambda (ex) (cont ex))
            (lambda () (raise "an error")))))))))

  (test-case "(error-object-message <error-object>)" (lambda ()
    (assert-equal
      (error-object-message
        (call-with-current-continuation (lambda (cont)
          (with-exception-handler
            (lambda (ex) (cont ex))
            (lambda () (error "an error"))))))
      "an error")))

  (test-case "(error-object-irritants <error-object>)" (lambda ()
    (assert-equal
      (error-object-irritants
        (call-with-current-continuation (lambda (cont)
          (with-exception-handler
            (lambda (ex) (cont ex))
            (lambda () (error "an error" 'foo 'bar 123))))))
      '(foo bar 123))
    (assert-equal
      (error-object-irritants
        (call-with-current-continuation (lambda (cont)
          (with-exception-handler
            (lambda (ex) (cont ex))
            (lambda () (error "an error"))))))
      '())))

  (test-case "(guard (<variable> <cond_1> <cond_2> ...) <body>)" (lambda ()
    (assert-equal
      (guard (condition
          ((assq 'a condition) => cdr)
          ((assq 'b condition)))
        (raise (list (cons 'a 42))))
      42)
    (assert-equal
      (guard (condition
          ((assq 'a condition) => cdr)
          ((assq 'b condition)))
        (raise (list (cons 'b 23))))
      '(b . 23))
    (assert-error
      (guard (condition
          ((assq 'a condition) => cdr)
          ((assq 'b condition)))
        (raise (list (cons 'c 7)))))))
)
)
