(define-syntax call-with-values
  (syntax-rules ()
    ((call-with-values producer consumer)
      ((lambda ()
        (define-values els (producer))
        (apply consumer els))))))


(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
      ((call/cc
          (lambda (guard-k)
            (with-exception-handler
              (lambda (condition)
                ((call/cc
                  (lambda (handler-k)
                    (guard-k
                      (lambda ()
                        (let ((var condition))
                          (guard-aux
                            (handler-k 
                              (lambda () (raise-continuable var)))
                            clause ...))))))))
              (lambda ()
                (call-with-values
                  (lambda () e1 e2 ...)
                  (lambda args
                    (guard-k
                      (lambda () (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
      (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
      (let ((temp test))
        (if temp
          (result temp)
          reraise)))
    ((guard-aux reraise (test => result) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          (result temp)
          (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
      (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          temp
          (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
      (if test
        (begin result1 result2 ...)
        reraise))
    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
      (if test
        (begin result1 result2 ...)
        -aux reraise clause1 clause2 ...)))))