(define (display-all x) (for-each display x))

(define (assert (x . m)) 
  (if (not x) (apply error "Assert failed" m)))

(define (run-tests (name . tests))
  (let ((passed 0)
        (failed 0))
    (display-all name #\newline)
    (for-each 
      (lambda (test)
        (call/cc (lambda (cont)
            (display-all "    " (car test) " - ")
            (with-exception-handler 
              (lambda (ex) 
                (display "\x1B;[0;31mfailed: ")
                (if (error-object? ex)
                  (begin
                    (display (error-object-message ex))
                    (if (not (null? (error-object-irritants ex)))
                      (apply display-all ": " (error-object-irritants ex))))
                  (display ex))
                (display-all "\x1B;[0m" #\newline)
                (set! failed (+ 1 failed))
                (cont #f))
              (cadr test))
            (display-all "passed." #\newline)
            (set! passed (+ 1 passed))
            (cont #t))))
      tests)
    (display-all "\x1B;[0;32m" passed " passing.\x1B;[0m" #\newline)
    (if (> failed 0)
      (begin
        (display-all "\x1B;[0;31m" failed " failing.\x1B;[0m" #\newline)))))
    