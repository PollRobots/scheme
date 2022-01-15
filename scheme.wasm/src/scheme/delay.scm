(define-syntax delay-force
    (syntax-rules ()
        ((delay-force expression)
        (make-promise #f (lambda () expression)))))

(define-syntax delay
    (syntax-rules ()
        ((delay expression)
        (delay-force (make-promise #t expression)))))

(define-syntax make-promise
    (syntax-rules ()
        ((make-promise obj)
          (let ((val obj))
            (if (promise? val)
                val
                (list '<promise> #t val))))
        ((make-promise done? obj)
            (let ((val obj))
                (list '<promise> done? val)))))

(define (force promise)
    (if (not (promise? promise)) (error "not-promise" promise))
    (if (promise-done? promise)
        (promise-value promise)
        (let ((promise* ((promise-value promise))))
            (unless (promise-done? promise)
                (promise-update! promise* promise))
            (force promise))))

(define (promise? x) (and (pair? x) (eq? '<promise> (car x))))
(define (promise-done? x) (cadr x))
(define (promise-value x) (caddr x))
(define (promise-update! new old)
    (if (not (and (promise? new) (promise? old))) (error "not-promise" new old))
    (set-car! (cdr old) (promise-done? new))
    (set-car! (cddr old) (promise-value new))
    (set-car! new (car old)))
