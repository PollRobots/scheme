;; (finite? <num>)
(func $finite? (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))
  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (global.get $g-true))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (global.get $g-true))))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (if (call $ieee-inf? (local.get $v)) (then
          (return (global.get $g-false))))
      (if (call $ieee-nan? (local.get $v)) (then
          (return (global.get $g-false))))
      (return (global.get $g-true))))

  (unreachable))

;; (infinite? <num>)
(func $infinite? (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))
  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (global.get $g-false))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (global.get $g-false))))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (if (call $ieee-inf? (local.get $v)) (then
          (return (global.get $g-true))))
      (return (global.get $g-false))))

  (unreachable))

;; (nan? <num>)
(func $nan? (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))
  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (global.get $g-false))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (global.get $g-false))))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (if (call $ieee-nan? (local.get $v)) (then
          (return (global.get $g-true))))
      (return (global.get $g-false))))

  (unreachable))

(func $floor (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-f64 (f64.floor (local.get $v))))))

  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%rational-type)) (then
      (local.set $num (call $inexact-impl (local.get $num)))
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-i64 (i64.trunc_f64_s (f64.floor (local.get $v)))))))

  (unreachable))

(func $ceiling (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-f64 (f64.ceil (local.get $v))))))

  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%rational-type)) (then
      (local.set $num (call $inexact-impl (local.get $num)))
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-i64 (i64.trunc_f64_s (f64.ceil (local.get $v)))))))

  (unreachable))

(func $truncate (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-f64 (f64.trunc (local.get $v))))))

  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%rational-type)) (then
      (local.set $num (call $inexact-impl (local.get $num)))
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-i64 (i64.trunc_f64_s (f64.trunc (local.get $v)))))))

  (unreachable))

(func $round (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $num-type i32)
  (local $v f64)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $num $args)
  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-f64 (f64.nearest (local.get $v))))))

  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (local.get $num))))
  (if (i32.eq (local.get $num-type) (%rational-type)) (then
      (local.set $num (call $inexact-impl (local.get $num)))
      (local.set $v (f64.load offset=4 (local.get $num)))
      (return (%alloc-i64 (i64.trunc_f64_s (f64.nearest (local.get $v)))))))

  (unreachable))

;; (log <num>)
;; (log <num> <base>)
(func $log (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $num i32)
  (local $base i32)
  (local $num-type i32)
  (local $ln-num f64)
  (local $ln-base f64)

  (block $b_check (block $b_fail
      (local.set $num-args (call $list-len (local.get $args)))
      (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (local.set $num (%car-l $args))

      (br_if $b_check (i32.lt_u (local.get $num-args) (i32.const 2)))
      (local.set $base (%car (%cdr-l $args)))
      (br_if $b_fail (i32.gt_u (local.get $num-args) (i32.const 2)))

      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $ln-num (call $logn-impl (local.get $num)))

  (if (i32.eq (local.get $num-args) (i32.const 1)) (then
      (return (%alloc-f64 (local.get $ln-num)))))

  (local.set $ln-base (call $logn-impl (local.get $base)))

  (return (%alloc-f64 (f64.div (local.get $ln-num) (local.get $ln-base)))))

;; compute logn using taylor series
(func $logn-taylor-impl (param $num i32) (result f64)
  (local $v f64)
  (local $e i32)
  (local $f i64)
  (local $x f64)
  (local $pow f64)
  (local $prev f64)
  (local $count i32)
  (local $accum f64)
  (local $sign f64)
  (local $div f64)

  (%define %kLn2 () (f64.const 0.69314718055994530941723212145818))

  ;; make sure this is a +ve real
  (local.set $num (call $inexact-impl (local.get $num)))
  (local.set $v (f64.load offset=4 (local.get $num)))
  (if (f64.lt (local.get $v) (f64.const 0)) (then (return (f64.const nan))))
  (if (f64.eq (local.get $v) (f64.const 0)) (then (return (f64.const -inf))))

  (local.set $e (i32.wrap_i64 (call $ieee-exponent-bits (local.get $v))))

  (local.set $f (call $ieee-significand-bits (local.get $v)))

  (if (i32.eqz (local.get $e))
    (then ;; TODO this is a denormalized number, handle later.
      (unreachable))
    (else
      ;; remove exponent bias
      (%minus-eq $e 0x3FF)))

  (;
    ln(f·2^e) = ln(f) + e·ln(2)

    f is currently between 1 >= f < 2
    if f == 1  --> e.ln(2)
    if f > 1.5 --> ln(f/2) + (e + 1)·ln(2)

    ln(1 + x) = taylor series
    x = f - 1

   ;)

  (if (i64.eqz (local.get $f)) (then
      (return (f64.mul (f64.convert_i32_s (local.get $e)) (%kLn2)))))

  (local.set $v (f64.reinterpret_i64 (i64.or
        (local.get $f)
        (i64.const 0x3FF0_0000_0000_0000))))

  (%assert (f64.ge (local.get $v) (f64.const 1)))
  (%assert (f64.lt (local.get $v) (f64.const 2)))

  (if (f64.gt (local.get $v) (f64.const 1.5)) (then
      (local.set $v (f64.mul (local.get $v) (f64.const 0.5)))
      (%inc $e)))

  ;; compute taylor series, either 52 times (once per bit),
  ;; or until it converges
  (local.set $x (f64.sub (local.get $v) (f64.const 1)))
  (local.set $pow (local.get $x))
  (local.set $prev (f64.const 0))
  (local.set $count (i32.const 52))
  (local.set $accum (f64.const 0))
  (local.set $sign (f64.const 1))
  (local.set $div (f64.const 1))

  (block $b_done (loop $b_start
      (local.set $accum (f64.add
          (local.get $accum)
          (f64.div
            (f64.mul (local.get $sign) (local.get $pow))
            (local.get $div))))

      (%dec $count)
      (br_if $b_done (i32.eqz (local.get $count)))
      (br_if $b_done (f64.eq (local.get $prev) (local.get $accum)))
      (local.set $prev (local.get $accum))

      (local.set $div (f64.add (local.get $div) (f64.const 1)))
      (local.set $sign (f64.neg (local.get $sign)))
      (local.set $pow (f64.mul (local.get $pow) (local.get $x)))
      (br $b_start)))

  (return (f64.add
      (local.get $accum)
      (f64.mul (f64.convert_i32_s (local.get $e)) (%kLn2)))))

;; compute logn using Carlson72
(func $logn-impl (param $num i32) (result f64)
  (local $v f64)
  (local $e i32)
  (local $f i64)
  (local $x f64)
  (local $a_0 f64)
  (local $a_1 f64)
  (local $a_2 f64)
  (local $a_3 f64)
  (local $a_4 f64)
  (local $a_5 f64)
  (local $g_0 f64)
  (local $g_1 f64)
  (local $g_2 f64)
  (local $g_3 f64)
  (local $g_4 f64)
  (local $d_1_1 f64)
  (local $d_1_2 f64)
  (local $d_2_2 f64)
  (local $d_1_3 f64)
  (local $d_2_3 f64)
  (local $d_3_3 f64)
  (local $d_1_4 f64)
  (local $d_2_4 f64)
  (local $d_3_4 f64)
  (local $d_4_4 f64)
  (local $d_1_5 f64)
  (local $d_2_5 f64)
  (local $d_3_5 f64)
  (local $d_4_5 f64)
  (local $d_5_5 f64)
  (local $log-x f64)

  (%define %kLn2 () (f64.const 0.69314718055994530941723212145818))

  ;; make sure this is a finite +ve real
  (local.set $num (call $inexact-impl (local.get $num)))
  (local.set $v (f64.load offset=4 (local.get $num)))
  (if (f64.lt (local.get $v) (f64.const 0)) (then (return (f64.const nan))))
  (if (f64.eq (local.get $v) (f64.const 0)) (then (return (f64.const -inf))))
  (if (call $ieee-inf? (local.get $v)) (then (return (local.get $v))))

  (local.set $e (i32.wrap_i64 (call $ieee-exponent-bits (local.get $v))))

  (local.set $f (call $ieee-significand-bits (local.get $v)))

  (if (i32.eqz (local.get $e))
    (then ;; TODO this is a denormalized number, handle later.
      (unreachable))
    (else
      ;; remove exponent bias
      (%minus-eq $e 0x3FF)))

  (;
    ln(f·2^e) = ln(f) + e·ln(2)

    f is currently between 1 >= f < 2
    if f == 1  --> e.ln(2)

    ln(f) = carlson

   ;)

  (if (i64.eqz (local.get $f)) (then
      (return (f64.mul (f64.convert_i32_s (local.get $e)) (%kLn2)))))

  (local.set $x (f64.reinterpret_i64 (i64.or
        (local.get $f)
        (i64.const 0x3FF0_0000_0000_0000))))

  (%assert (f64.ge (local.get $x) (f64.const 1)))
  (%assert (f64.lt (local.get $x) (f64.const 2)))

  (;
   ;  Compute ln(v) using Carlson 72 "An Algorithm for Computing Logarithms and Arctangents"
   ;
   ;  https://www.ams.org/journals/mcom/1972-26-118/S0025-5718-1972-0307438-2/S0025-5718-1972-0307438-2.pdf
   ;
   ;      a_0 = ½(1 + x),          g_0 = x^½
   ;  a_(n+1) = ½(a_n + g_n),  g_(n+1) = (a_(n+1)·g_n)^½,     n = 0, 1, 2, ...,
   ;
   ;  d(0, n) = a_n,    n = 0, 1, 2, ...
   ;
   ;           d(k-1, n) - 2^(-2k)·d(k - 1, n - 1)
   ;  d(k,n) = -----------------------------------,    k = 1, 2, ..., n.
   ;                      1 - 2^(-2k)
   ;
   ;           x - 1
   ;  log x = -------
   ;          d(n, n)
   ;
   ;  Limiting n to 5 provides sufficient precision.
   ;
   ;)

   (;
      Loops are completely unrolled.
      p2k = 2^(-2k)           - stored as constants
      inv-p2k = 1/(1 - 2-2k)  - stored as constants

      a_0 = 0.5 * (1 + x)
      g_0 = sqrt(x)
      a_1 = 0.5 * (a_0 + g_0)
      g_1 = sqrt(a_1 * g_0)
      a_2 = 0.5 * (a_1 + g_1)
      g_2 = sqrt(a_2 * g_1)
      a_3 = 0.5 * (a_2 + g_2)
      g_3 = sqrt(a_3 * g_2)
      a_4 = 0.5 * (a_3 + g_3)
      g_4 = sqrt(a_4 * g_3)
      a_5 = 0.5 * (a_4 + g_4)

      d_k_n
      d_1_1 = (d_0_1 - p2_1 * d_0_0) . inv-p2_1
      d_1_1 = (a_1 - p2_1 * a_0) . inv-p2_1

      d_1_2 = (a_2 - p2_1 * a_1) . inv-p2_1
      d_2_2 = (d_1_2 - p2_2 * d_1_1) . inv-p2_2

      d_1_3 = (a_3 - p2_1 * a_2) . inv-p2_1
      d_2_3 = (d_1_3 - p2_2 * d_1_2) . inv-p2_2
      d_3_3 = (d_2_3 - p2_3 * d_2_2) . inv-p2_3

      d_1_4 = (a_4 - p2_1 * a_3) . inv-p2_1
      d_2_4 = (d_1_4 - p2_2 * d_1_3) . inv-p2_2
      d_3_4 = (d_2_4 - p2_3 * d_2_3) . inv-p2_3
      d_4_4 = (d_3_4 - p2_4 * d_3_3) . inv-p2_4

      d_1_5 = (a_5 - p2_1 * a_4) . inv-p2_1
      d_2_5 = (d_1_5 - p2_2 * d_1_4) . inv-p2_2
      d_3_5 = (d_2_5 - p2_3 * d_2_4) . inv-p2_3
      d_4_5 = (d_3_5 - p2_4 * d_3_4) . inv-p2_4
      d_5_5 = (d_4_5 - p2_5 * d_4_4) . inv-p2_5

      log-x = (x - 1) / d_5_5
    ;)

  (%define %p2_1 () (f64.const 0.25)) ;;2^(-2) -> 1/4
  (%define %p2_2 () (f64.const 0.0625)) ;;2^(-4) -> 1/16
  (%define %p2_3 () (f64.const 0.015625)) ;;2^(-6)  -> 1/64
  (%define %p2_4 () (f64.const 0.00390625)) ;;2^(-8) -> 1/256
  (%define %p2_5 () (f64.const 0.0009765625)) ;;2^(-10) -> 1/1024
  (%define %inv-p2_1 () (f64.const 1.3333333333333333)) ;; 1 / (1 - 2^-2) 4/3
  (%define %inv-p2_2 () (f64.const 1.0666666666666667)) ;; 1 / (1 - 2^-2) 16/15
  (%define %inv-p2_3 () (f64.const 1.0158730158730158)) ;; 1 / (1 - 2^-2) 64/63
  (%define %inv-p2_4 () (f64.const 1.003921568627451)) ;; 1 / (1 - 2^-2) 256/255
  (%define %inv-p2_5 () (f64.const 1.0009775171065494)) ;; 1 / (1 - 2^-2) 1024/1023

  ;; a_0 = 0.5 * (1 + x)
  (local.set $a_0 (f64.mul
      (f64.const 0.5)
      (f64.add (f64.const 1) (local.get $x))))
  ;; g_0 = sqrt(x)
  (local.set $g_0 (f64.sqrt (local.get $x)))
  ;; a_1 = 0.5 * (a_0 + g_0)
  (local.set $a_1 (f64.mul
      (f64.const 0.5)
      (f64.add (local.get $a_0) (local.get $g_0))))
  ;; g_1 = sqrt(a_1 * g_0)
  (local.set $g_1 (f64.sqrt (f64.mul (local.get $a_1) (local.get $g_0))))
  ;; a_2 = 0.5 * (a_1 + g_1)
  (local.set $a_2 (f64.mul
      (f64.const 0.5)
      (f64.add (local.get $a_1) (local.get $g_1))))
  ;; g_2 = sqrt(a_2 * g_1)
  (local.set $g_2 (f64.sqrt (f64.mul (local.get $a_2) (local.get $g_1))))
  ;; a_3 = 0.5 * (a_2 + g_2)
  (local.set $a_3 (f64.mul
      (f64.const 0.5)
      (f64.add (local.get $a_2) (local.get $g_2))))
  ;; g_3 = sqrt(a_3 * g_2)
  (local.set $g_3 (f64.sqrt (f64.mul (local.get $a_3) (local.get $g_2))))
  ;; a_4 = 0.5 * (a_3 + g_3)
  (local.set $a_4 (f64.mul
      (f64.const 0.5)
      (f64.add (local.get $a_3) (local.get $g_3))))
  ;; g_4 = sqrt(a_4 * g_3)
  (local.set $g_4 (f64.sqrt (f64.mul (local.get $a_4) (local.get $g_3))))
  ;; a_5 = 0.5 * (a_4 + g_4)
  (local.set $a_5 (f64.mul
      (f64.const 0.5)
      (f64.add (local.get $a_4) (local.get $g_4))))

  ;; d_1_1 = (a_1 - p2_1 * a_0) . inv-p2_1
  (local.set $d_1_1 (f64.mul
      (f64.sub
        (local.get $a_1)
        (f64.mul (%p2_1) (local.get $a_0)))
      (%inv-p2_1)))

  ;; d_1_2 = (a_2 - p2_1 * a_1) . inv-p2_1
  (local.set $d_1_2 (f64.mul
      (f64.sub
        (local.get $a_2)
        (f64.mul (%p2_1) (local.get $a_1)))
      (%inv-p2_1)))
  ;; d_2_2 = (d_1_2 - p2_2 * d_1_1) . inv-p2_2
  (local.set $d_2_2 (f64.mul
      (f64.sub
        (local.get $d_1_2)
        (f64.mul (%p2_2) (local.get $d_1_1)))
      (%inv-p2_2)))

  ;; d_1_3 = (a_3 - p2_1 * a_2) . inv-p2_1
  (local.set $d_1_3 (f64.mul
      (f64.sub
        (local.get $a_3)
        (f64.mul (%p2_1) (local.get $a_2)))
      (%inv-p2_1)))
  ;; d_2_3 = (d_1_3 - p2_2 * d_1_2) . inv-p2_2
  (local.set $d_2_3 (f64.mul
      (f64.sub
        (local.get $d_1_3)
        (f64.mul (%p2_2) (local.get $d_1_2)))
      (%inv-p2_2)))
  ;; d_3_3 = (d_2_3 - p2_3 * d_2_2) . inv-p2_3
  (local.set $d_3_3 (f64.mul
      (f64.sub
        (local.get $d_2_3)
        (f64.mul (%p2_3) (local.get $d_2_2)))
      (%inv-p2_3)))

  ;; d_1_4 = (a_4 - p2_1 * a_3) . inv-p2_1
  (local.set $d_1_4 (f64.mul
      (f64.sub
        (local.get $a_4)
        (f64.mul (%p2_1) (local.get $a_3)))
      (%inv-p2_1)))
  ;; d_2_4 = (d_1_4 - p2_2 * d_1_3) . inv-p2_2
  (local.set $d_2_4 (f64.mul
      (f64.sub
        (local.get $d_1_4)
        (f64.mul (%p2_2) (local.get $d_1_3)))
      (%inv-p2_2)))
  ;; d_3_4 = (d_2_4 - p2_3 * d_2_3) . inv-p2_3
  (local.set $d_3_4 (f64.mul
      (f64.sub
        (local.get $d_2_4)
        (f64.mul (%p2_3) (local.get $d_2_3)))
      (%inv-p2_3)))
  ;; d_4_4 = (d_3_4 - p2_4 * d_3_3) . inv-p2_4
  (local.set $d_4_4 (f64.mul
      (f64.sub
        (local.get $d_3_4)
        (f64.mul (%p2_4) (local.get $d_3_3)))
      (%inv-p2_4)))

  ;; d_1_5 = (a_5 - p2_1 * a_4) . inv-p2_1
  (local.set $d_1_5 (f64.mul
      (f64.sub
        (local.get $a_5)
        (f64.mul (%p2_1) (local.get $a_4)))
      (%inv-p2_1)))
  ;; d_2_5 = (d_1_5 - p2_2 * d_1_4) . inv-p2_2
  (local.set $d_2_5 (f64.mul
      (f64.sub
        (local.get $d_1_5)
        (f64.mul (%p2_2) (local.get $d_1_4)))
      (%inv-p2_2)))
  ;; d_3_5 = (d_2_5 - p2_3 * d_2_4) . inv-p2_3
  (local.set $d_3_5 (f64.mul
      (f64.sub
        (local.get $d_2_5)
        (f64.mul (%p2_3) (local.get $d_2_4)))
      (%inv-p2_3)))
  ;; d_4_5 = (d_3_5 - p2_4 * d_3_4) . inv-p2_4
  (local.set $d_4_5 (f64.mul
      (f64.sub
        (local.get $d_3_5)
        (f64.mul (%p2_4) (local.get $d_3_4)))
      (%inv-p2_4)))
  ;; d_5_5 = (d_4_5 - p2_5 * d_4_4) . inv-p2_5
  (local.set $d_5_5 (f64.mul
      (f64.sub
        (local.get $d_4_5)
        (f64.mul (%p2_5) (local.get $d_4_4)))
      (%inv-p2_5)))


  ;; log-x = (x - 1) / d_5_5
  (local.set $log-x (f64.div
      (f64.sub (local.get $x) (f64.const 1))
      (local.get $d_5_5)))

  ;; log(f·2^e)  = log(f) + e * log(2)
  (return (f64.add
      (local.get $log-x)
      (f64.mul (f64.convert_i32_s (local.get $e)) (%kLn2)))))

(func $exp (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (%car-l $args))

  (return (%alloc-f64 (call $exp-impl (local.get $num)))))


(func $exp-impl (param $num i32) (result f64)
  (local $v f64)
  (local $f i64)
  (local $e i32)
  (local $neg i32)
  (local $index i32)
  (local $pow f64)
  (local $fact f64)
  (local $accum f64)
  (local $last f64)

  (%define %kE () (f64.const 2.7182818284590452353602874713527))

  ;; make sure this is a +ve real
  (local.set $num (call $inexact-impl (local.get $num)))
  (local.set $v (f64.load offset=4 (local.get $num)))

  ;; return 1 for e^0
  (if (f64.eq (local.get $v) (f64.const 0)) (then (return (f64.const 1))))
  ;; return inf for inf
  (if (call $ieee-inf? (local.get $v)) (then (return (local.get $v))))
  ;; return nan for nan
  (if (call $ieee-nan? (local.get $v)) (then (return (local.get $v))))

  (local.set $neg (call $ieee-negative? (local.get $v)))
  (local.set $f (call $ieee-significand-bits (local.get $v)))
  (local.set $e (i32.wrap_i64 (call $ieee-exponent-bits (local.get $v))))

  (if (i32.eqz (local.get $e))
    ;; TODO handle denormalized numbers
    (then (unreachable)))
  (local.set $e (i32.sub (local.get $e) (i32.const 0x3FF)))
  (if (i32.gt_s (local.get $e) (i32.const 0))
    (then
      ;; Remove exponent, adjust it later
      (local.set $v (f64.reinterpret_i64 (i64.or
            (local.get $f)
            (i64.const 0x3FF0_0000_0000_0000))))
      ;; we have a number v such that 1 <= v <= 2
      ;; divide by 2  to get it between 0.5 to converge a little faster (maybe?)
      (local.set $v (f64.div (local.get $v) (f64.const 2)))
      (%inc $e))
    (else
      ;; Work with the number as it is
      (local.set $e (i32.const 0))))

  (local.set $v (f64.abs (local.get $v)))
  (local.set $index (i32.const 1))
  (local.set $pow (local.get $v))
  (local.set $fact (f64.const 1))
  (local.set $accum (f64.const 1))
  (local.set $last (f64.const 1))

  ;; compute taylor series for exp x + x²/2 + x³/3 ...
  ;; either until it converges, or until x¹⁷
  (block $b_end (loop $b_start

      ;; accum + pow / fact
      (local.set $accum (f64.add
          (local.get $accum)
          (f64.div (local.get $pow) (local.get $fact))))

      ;; check for convergance
      ;; break if last == accum
      (br_if $b_end (f64.eq (local.get $last) (local.get $accum)))
      ;; last = accum
      (local.set $last (local.get $accum))

      (%inc $index)
      (br_if $b_end (i32.gt_u (local.get $index) (i32.const 17)))

      ;; pow *= v
      (local.set $pow (f64.mul (local.get $pow) (local.get $v)))
      ;; fact *= index
      (local.set $fact (f64.mul
          (local.get $fact)
          (f64.convert_i32_u (local.get $index))))

      (br $b_start)))

  (if (i32.gt_s (local.get $e) (i32.const 0)) (then
      (block $b_end (loop $b_start
          (br_if $b_end (i32.eqz (local.get $e)))

          (local.set $accum (f64.mul (local.get $accum) (local.get $accum)))
          (%dec $e)
          (br $b_start)))))

  (if (local.get $neg) (then
      (local.set $accum (f64.div (f64.const 1) (local.get $accum)))))

  ;; TODO handle values not between 1 and 2
  (return (local.get $accum)))
