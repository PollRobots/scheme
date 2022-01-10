(define number? real?)
(define complex? number?)

(define (exact-integer? x) (and (exact? x) (integer? x)))

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? x) (zero? (floor-remainder (exact x) 2)))
(define (odd? x) (not (even? x)))

(define-syntax max
    (syntax-rules ()
        ((max a) a)
        ((max a b)
          (if (not (eq? (inexact? a) (inexact? b)))
              (max (inexact a) (inexact b))
              (if (>= a b) a b)))
        ((max a b c ...) (max (max a b) c ...))))

(define-syntax min
    (syntax-rules ()
        ((min a) a)
        ((min a b)
          (if (not (eq? (inexact? a) (inexact? b)))
              (min (inexact a) (inexact b))
              (if (<= a b) a b)))
        ((min a b c ...) (min (min a b) c ...))))

(define (modulo x y) (floor-remainder x y))
(define (gcd a b)
  (if (zero? b)
    a
    (if (> a b)
      (gcd b (floor-remainder a b))
      (gcd a (floor-remainder b a)))))
(define (lcm a b)
  (if (or (zero? a) (zero? b))
    (+ a b)
    (* a (truncate-quotient b (gcd a b)))))
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
