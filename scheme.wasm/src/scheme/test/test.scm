(define (display-all . x) (for-each display x))

(define (assert x . m) 
  (if (not x) (apply error "Assert failed" m)))

(define (assert-not x . m) 
  (if x (apply error "Assert failed" m)))

(define (assert-error x . m) 
  (if (not (error-object? x)) (apply error "Assert failed" x " should be an error, " m)))

(define (assert-equal x y . m) 
  (if (not (equal? x y)) (apply error "Assert failed" x " should equal " y ", " m)))

(define (run-tests name . tests)
  (let ((passed 0)
        (failed 0))
    (display-all name #\newline)
    (for-each 
      (lambda (test)
        (call/cc (lambda (cont)
            (with-exception-handler 
              (lambda (ex) 
                (display-all "    " #\escape "[0;31m" #\x2718 #\escape "[94m " (car test) #\newline)
                (display "\x1B;[0;31mfailed: ")
                (if (error-object? ex)
                  (begin
                    (display (error-object-message ex))
                    (if (not (null? (error-object-irritants ex)))
                      (apply display-all ": " (error-object-irritants ex))))
                  (display ex))
                (display #\newline)
                (set! failed (+ 1 failed))
                (cont #f))
              (cadr test))
            (display-all "    " #\escape "[0;32m" #\x2714 #\escape "[94m " (car test) #\newline)
            (set! passed (+ 1 passed))
            (cont #t))))
      tests)
    (display-all #\newline #\newline "\x1B;[0;32m" passed " passing.\x1B;[0m" #\newline)
    (if (> failed 0)
      (begin
        (display-all "\x1B;[0;31m" failed " failing.\x1B;[0m" #\newline)))))

  (define (test-case name fn)
    (cond 
      ((and (string? name) (procedure? fn)) (list name fn))
      ((string? name) 
        (list name (lambda () (error "test-case fn is not a procedure" name))))
      (else 
        (list 
          "unknown" 
          (lambda () (error "test-case name is not a string" name))))))
    