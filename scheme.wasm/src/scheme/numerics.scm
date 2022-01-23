(define number? complex?)

(define (exact-integer? x) (and (exact? x) (integer? x)))

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? x) (zero? (floor-remainder (exact x) 2)))
(define (odd? x) (not (even? x)))

(define (make-polar r a) (make-rectangular (* r (cos a)) (* r (sin a))))
(define (magnitude z)
  (let ((r (real-part z))
        (i (imag-part z)))
    (sqrt (+ (* r r) (* i i)))))
(define (angle z) (atan (imag-part z) (real-part z)))

(define-syntax max
    (syntax-rules ()
        ((max a) a)
        ((max a b)
          (let ((x a) (y b))
            (if (not (eq? (inexact? x) (inexact? y)))
                (max (inexact x) (inexact y))
                (if (>= x y) x y))))
        ((max a b c ...) (max (max a b) c ...))))

(define-syntax min
    (syntax-rules ()
        ((min a) a)
        ((min a b)
          (let ((x a) (y b))
            (if (not (eq? (inexact? x) (inexact? y)))
                (min (inexact x) (inexact y))
                (if (<= x y) x y))))
        ((min a b c ...) (min (min a b) c ...))))

(define modulo floor-remainder)

(define (gcd . lst)
  (if (null? lst)
      0
      (letrec ((gcd-core (lambda (a b)
                            (if (zero? b)
                                a
                                (if (> a b)
                                  (gcd-core b (floor-remainder a b))
                                  (gcd-core a (floor-remainder b a))))))
                (result (abs (car lst))))
          (for-each
            (lambda (x) (set! result (gcd-core result (abs x))))
            (cdr lst))
          result)))

(define (lcm . lst)
  (if (null? lst)
      1
      (letrec ((lcm-core (lambda (a b)
                            (if (or (zero? a) (zero? b))
                              (+ a b)
                              (* a (truncate-quotient b (gcd a b))))))
               (result (abs (car lst))))
        (for-each
          (lambda (x) (set! result (lcm-core result (abs x))))
          (cdr lst))
        result)))

(define (square x) (* x x))
(define (expt a b)
  (cond
    ((and (integer? b) (positive? b))
      (* a (expt a (- b 1))))
    ((and (integer? b) (zero? b)) 1)
    ((integer? b)
      (/ (expt a (- b))))
    (else (exp (* b (log a))))))
(define (sqrt x)
  (cond
    ((negative? x) (error "Cannot take negative square-root"))
    ((zero? x) 0)
    ((integer? x)
      (let-values (((root rem) (exact-integer-sqrt x)))
        (if (zero? rem)
          root
          (expt x 0.5))))
    (else (expt x 0.5))))
(define pi (* 4 (atan 1)))
(define (acos x) (- (* pi 0.5) (asin x)))
