(define (display-all . x) (for-each display x))

(define verbose-asserts #f)

(define (verbose-display-all . x)
  (if verbose-asserts (for-each display x)))

(define-syntax assert
  (syntax-rules ()
    ((assert x) (assert x ""))
    ((assert x m ...) 
      (begin
        (verbose-display-all "\x1b;[90massert " 'x "\x1b;[0m" #\newline)
        (if (not x) (error "Assert failed" x m ..))))))

(define-syntax assert-not
  (syntax-rules ()
    ((assert-not x) (assert-not x ""))
    ((assert-not x m ...)
      (begin
        (verbose-display-all "\x1b;[90massert (not " 'x ")\x1b;[0m" #\newline)
        (if x (error "Assert failed" 'x m ...))))))

(define-syntax assert-equal
  (syntax-rules ()
    ((assert-equal x y) (assert-equal x y ""))
    ((assert-equal x y m ...)
      (begin
        (verbose-display-all "\x1b;[90massert (equal? " 'x " " 'y ")\x1b;[0m" #\newline)
        (if (not (equal? x y)) 
            (error "Assert failed" 'x " should equal " 'y ", " m ...))))))

(define-syntax assert-error
  (syntax-rules ()
    ((assert-error x) (assert-error x ""))
    ((assert-error x m ...)
      (begin
        (verbose-display-all "\x1b;[90massert (throws " 'x ")\x1b;[0m" #\newline)
        (if 
          (call/cc 
            (lambda (cont)
              (with-exception-handler
                (lambda (ex) (cont #f))
                (lambda () x))
              (cont #t)))
          (error "Assert failed" 'x " should have raised an exception " m ...))))))

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
            (display-all "    " #\escape "[0;32m" #\x2714 #\escape "[90m " (car test) #\newline)
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
    