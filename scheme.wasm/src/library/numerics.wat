;; (integer? obj)
(func $integer? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))

  (if (i32.eq (%get-type $obj) (%i64-type))
    (then (return (global.get $g-true))))

  (if (i32.eq (%get-type $obj) (%big-int-type))
    (then (return (global.get $g-true))))

  (return (global.get $g-false)))

;; (real? obj)
(func $real? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then 
      (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))

  (if (i32.eq (%get-type $obj) (%f64-type)) (then 
      (return (global.get $g-true))))

  (return (global.get $g-false)))

(func $all-numeric (param $args i32) (result i32)
  (local $temp i32)
  (local $temp-type i32)

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))
      (local.set $temp-type (%get-type $temp))

      (block $b_check
        (br_if $b_check (i32.eq (local.get $temp-type) (%i64-type)))
        (br_if $b_check (i32.eq (local.get $temp-type) (%f64-type)))
        (br_if $b_check (i32.eq (local.get $temp-type) (%big-int-type)))

        (return (i32.const 0)))

      (local.set $args (%cdr-l $args))
      (br $b_start))) 

  (return (i32.const 1))) 

(func $list-len (param $args i32) (result i32)
  (local $len i32)
  (local.set $len (i32.const 0))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (%inc $len)

      (local.set $args (%cdr-l $args))
      (br $b_start)))

  (return (local.get $len)))

;; (exact? <num>)
(func $exact? (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (%car-l $args))
  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.ne (%get-type $num) (%f64-type)))))

;; (inexact? <num>)
(func $inexact? (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (%car-l $args))
  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $num) (%f64-type)))))

(func $inexact (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (%car-l $args))
  (if (i32.eq (%get-type $num) (%f64-type))
    (then (return (local.get $num))))

  (return (call $inexact-impl (local.get $num))))

(func $exact (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (%car-l $args))
  (if (i32.eq (%get-type $num) (%big-int-type)) (then 
      (return (local.get $num))))

  (if (i32.eq (%get-type $num) (%i64-type)) (then 
      (return (local.get $num))))

  (return (call $exact-impl (local.get $num))))

(func $inexact-impl (param $num i32) (result i32)
  (local $num-type i32)

  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type) (%f64-type)) (then
      (return (local.get $num))))

  (if (i32.eq (local.get $num-type) (%i64-type)) (then
      (return (%alloc-f64 (f64.convert_i64_s 
            (i64.load offset=4 (local.get $num)))))))

  (if (i32.eq (local.get $num-type) (%big-int-type)) (then
      (return (%alloc-f64 (call $mp-algorithm-m 
            (%car-l $num) 
            (i32.const 0))))))

  (unreachable))

(func $exact-impl (param $num i32) (result i32)
  (local $num-type i32)
  (local $v f64)
  (local $e i32)
  (local $f i64)
  (local $neg i32)
  (local $mp i32)

  (local.set $num-type (%get-type $num))
  (if (i32.ne (local.get $num-type) (%f64-type)) (then (unreachable)))

  (local.set $v (f64.load offset=4 (local.get $num)))
  (block $b_check (block $b_fail
      (br_if $b_fail (call $ieee-inf? (local.get $v)))
      (br_if $b_fail (call $ieee-nan? (local.get $v)))
      (br $b_check))

    (return (call $argument-error 
        (%alloc-cons (local.get $num) (global.get $g-nil)))))

  (local.set $v (f64.nearest (local.get $v)))

  ;; if abs(v) < 1 --> 0
  (if (f64.lt (f64.abs (local.get $v)) (f64.const 1)) (then
    (return (%alloc-i64 (i64.const 0)))))

  (local.set $neg (call $ieee-negative? (local.get $v)))

  (local.set $e (i32.sub 
      (i32.wrap_i64 (call $ieee-exponent-bits (local.get $v)))
      (i32.const 0x433)))
  (local.set $f (i64.add 
      (i64.const 0x0010_0000_0000_0000) 
      (call $ieee-significand-bits (local.get $v))))

  ;; number is less than 53 bits
  (if (i32.lt_s (local.get $e) (i32.const 0)) (then
      (local.set $f (i64.shr_u 
          (local.get $f) 
          (i64.extend_i32_u (i32.sub (i32.const 0) (local.get $e)))))
      (local.set $e (i32.const 0))))

  ;; number is less than 63 bits
  (if (i32.lt_s (local.get $e) (i32.const 10)) (then
      (local.set $f (i64.shl (local.get $f) (i64.extend_i32_u (local.get $e))))
      (local.set $e (i32.const 0))))

  ;; if the number has an exponent of 1, then convert directly to an int
  (if (i32.eqz (local.get $e)) (then
      (if (local.get $neg)
        (then (return (%alloc-i64 (i64.sub (i64.const 0) (local.get $f)))))
        (else (return (%alloc-i64 (local.get $f)))))))

  (local.set $mp (call $mp-from-u64 (local.get $f)))

  (if (local.get $neg) (then (call $mp-neg (local.get $mp))))
  (local.set $mp (call $mp-shl-eq (local.get $mp) (local.get $e)))

  (return (%alloc-big-int (local.get $mp))))

(func $num-equal (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $num-cmp-impl (local.get $args) (i32.const 0))))

(func $num-gt (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $num-cmp-impl (local.get $args) (i32.const 1))))

(func $num-ge (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $num-cmp-impl (local.get $args) (i32.const 2))))

(func $num-lt (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $num-cmp-impl (local.get $args) (i32.const 3))))

(func $num-le (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $num-cmp-impl (local.get $args) (i32.const 4))))

;; cmp-ops are
;;  0 =
;;  1 >
;;  2 >=
;;  3 <
;;  4 <=

(func $num-cmp-impl (param $args i32) (param $cmp-op i32) (result i32)
  (local $temp i32)
  (local $left i32)
  (local $right i32)
  (local $cmp-res i32)

  (%pop-l $right $args)

  (block $done
    (loop $continue
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $left (local.get $right))
      (%pop-l $right $args )

      ;; switch (cmp-op)
      (block $b_cmp_check
        (block $b_cmp_fail

          (local.set $cmp-res (call $num-core-cmp 
              (local.get $left) 
              (local.get $right)))

          ;; case 0:  =
          (if (i32.eq (local.get $cmp-op) (i32.const 0)) 
            (then
              (br_if $b_cmp_fail (i32.ne (local.get $cmp-res) (i32.const 0)))
              (br $b_cmp_check)))

          ;; case 1:  >
          (if (i32.eq (local.get $cmp-op) (i32.const 1))  ;; >
            (then
              (br_if $b_cmp_fail (i32.le_s (local.get $cmp-res) (i32.const 0)))
              (br $b_cmp_check)))

          ;; case 2:  >= 
          (if (i32.eq (local.get $cmp-op) (i32.const 2)) ;; >=
            (then
              (br_if $b_cmp_fail (i32.lt_s (local.get $cmp-res) (i32.const 0)))
              (br $b_cmp_check)))

          ;; case 3:  <
          (if (i32.eq (local.get $cmp-op) (i32.const 3)) ;; <
            (then
              (br_if $b_cmp_fail (i32.ge_s (local.get $cmp-res) (i32.const 0)))
              (br $b_cmp_check)))

          ;; case 4:  <=
          (if (i32.eq (local.get $cmp-op) (i32.const 4)) ;; <=
            (then
              (br_if $b_cmp_fail (i32.gt_s (local.get $cmp-res) (i32.const 0)))
              (br $b_cmp_check))))

        (return (global.get $g-false)))

      (br $continue)))

  (return (global.get $g-true)))

;; (+ <num_1> ...)
(func $num-add (param $env i32) (param $args i32) (result i32)
  (local $accum i32)
  (local $num i32)

  (if (i32.eqz (call $all-numeric (local.get $args)))
    (then (return (call $argument-error (local.get $args)))))

  ;; accum = 0
  (local.set $accum (%alloc-i64 (i64.const 0)))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (%pop-l $num $args)
      
      (local.set $accum (call $num-core-add 
          (local.get $accum) 
          (local.get $num)))

      (br $b_start)))

  (return (local.get $accum)))

;; (* <num_1> ...)
(func $num-mul (param $env i32) (param $args i32) (result i32)
  (local $accum i32)
  (local $num i32)

  (if (i32.eqz (call $all-numeric (local.get $args)))
    (then (return (call $argument-error (local.get $args)))))

  ;; accum = 1
  (local.set $accum (%alloc-i64 (i64.const 1)))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (%pop-l $num $args)
      
      (local.set $accum (call $num-core-mul
          (local.get $accum) 
          (local.get $num)))

      (br $b_start)))

  (return (local.get $accum)))

;; (- <num_1> ...)
(func $num-sub (param $env i32) (param $args i32) (result i32)
  (local $accum i32)
  (local $num i32)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.eq (%get-type $args) (%nil-type)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (%pop-l $accum $args)

  (if (i32.eq (%get-type $args) (%nil-type))
    (then (return (call $num-core-neg (local.get $accum)))))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (%pop-l $num $args)

      (local.set $accum (call $num-core-sub
          (local.get $accum)
          (local.get $num)))

      (br $b_start)))

  (return (local.get $accum)))

;; (abs <num>)
(func $num-abs (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $fnum f64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (%car-l $args))

  (return (call $num-core-abs (local.get $num))))

(func $num-truncate-quotient (param $env i32) (param $args i32) (result i32)
  (local $dividend i32)
  (local $divisor i32)
  (local $res-64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (%pop-l $dividend $args)
  (%pop-l $divisor $args)

  (local.set $res-64 (call $num-core-div 
      (local.get $dividend)
      (local.get $divisor)
      (i32.const 0)))

  (if (i64.eqz (local.get $res-64))
    (then (return (%alloc-error-cons
          (global.get $g-div0)
          (global.get $g-nil)))))

  (return (i32.wrap_i64 (i64.shr_u (local.get $res-64) (i64.const 32)))))

(func $num-truncate-remainder (param $env i32) (param $args i32) (result i32)
  (local $dividend i32)
  (local $divisor i32)
  (local $res-64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (%pop-l $dividend $args)
  (%pop-l $divisor $args)

  (local.set $res-64 (call $num-core-div 
      (local.get $dividend)
      (local.get $divisor)
      (i32.const 0)))

  (if (i64.eqz (local.get $res-64))
    (then (return (%alloc-error-cons
          (global.get $g-div0)
          (global.get $g-nil)))))

  (return (i32.wrap_i64 (local.get $res-64) (i64.const 32))))
 
(func $num-truncate/ (param $env i32) (param $args i32) (result i32)
  (local $dividend i32)
  (local $divisor i32)
  (local $res-64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (%pop-l $dividend $args)
  (%pop-l $divisor $args)

  (local.set $res-64 (call $num-core-div 
      (local.get $dividend)
      (local.get $divisor)
      (i32.const 0)))

  (if (i64.eqz (local.get $res-64))
    (then (return (%alloc-error-cons
          (global.get $g-div0)
          (global.get $g-nil)))))

  (return
    (%alloc-values
      (i32.wrap_i64 (i64.shr_u (local.get $res-64) (i64.const 32)))
      (i32.wrap_i64 (local.get $res-64)))))

(func $num-floor-quotient (param $env i32) (param $args i32) (result i32)
  (local $dividend i32)
  (local $divisor i32)
  (local $res-64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (%pop-l $dividend $args)
  (%pop-l $divisor $args)

  (local.set $res-64 (call $num-core-div 
      (local.get $dividend)
      (local.get $divisor)
      (i32.const 1)))

  (if (i64.eqz (local.get $res-64))
    (then (return (%alloc-error-cons
          (global.get $g-div0)
          (global.get $g-nil)))))

  (return (i32.wrap_i64 (i64.shr_u (local.get $res-64) (i64.const 32)))))

(func $num-floor-remainder (param $env i32) (param $args i32) (result i32)
  (local $dividend i32)
  (local $divisor i32)
  (local $res-64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (%pop-l $dividend $args)
  (%pop-l $divisor $args)

  (local.set $res-64 (call $num-core-div 
      (local.get $dividend)
      (local.get $divisor)
      (i32.const 1)))

  (if (i64.eqz (local.get $res-64))
    (then (return (%alloc-error-cons
          (global.get $g-div0)
          (global.get $g-nil)))))

  (return (i32.wrap_i64 (local.get $res-64) (i64.const 32))))
 
(func $num-floor/ (param $env i32) (param $args i32) (result i32)
  (local $dividend i32)
  (local $divisor i32)
  (local $res-64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (%pop-l $dividend $args)
  (%pop-l $divisor $args)

  (local.set $res-64 (call $num-core-div 
      (local.get $dividend)
      (local.get $divisor)
      (i32.const 1)))

  (if (i64.eqz (local.get $res-64))
    (then (return (%alloc-error-cons
          (global.get $g-div0)
          (global.get $g-nil)))))

  (return
    (%alloc-values
      (i32.wrap_i64 (i64.shr_u (local.get $res-64) (i64.const 32)))
      (i32.wrap_i64 (local.get $res-64)))))
 
(func $num-exact-integer-sqrt (param $env i32) (param $args i32) (result i32)
  (local $num i32)
  (local $res i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (local.set $num (%car-l $args))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $res (call $num-core-int-sqrt (local.get $num)))
  (if (i32.eqz (local.get $res))
    (then (return (call $argument-error (local.get $args)))))

  (return (local.get $res)))

;; (string->number <str> [radix])
(func $num-string->number (param $env i32) (param $args i32) (result i32)
  (local $args-len i32)
  (local $radix i32)
  (local $str i32)

  (local.set $args-len (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.eqz (local.get $args-len)))

      (local.set $str (%car-l $args))
      (%chk-type $b_fail $str %str-type)
      (local.set $radix (i32.const 10))
      (br_if $b_check (i32.eq (local.get $args-len) (i32.const 1)))

      (br_if $b_fail (i32.ne (local.get $args-len) (i32.const 2)))
      (local.set $radix (%car (%cdr-l $args)))
      (%chk-type $b_fail $radix %i64-type)
      (br_if $b_fail (i32.ne (%get-type $radix) (%i64-type)))
      (br_if $b_fail (i64.gt_u (i64.load offset=4 (local.get $radix)) (i64.const 16)))
      (local.set $radix (i32.wrap_i64 (i64.load offset=4 (local.get $radix))))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 16)))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 10)))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 8)))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 2))))

    (return (call $argument-error (local.get $args))))

  (return (call $string->number-impl (local.get $str) (local.get $radix))))

;; (number->string <num> [radix])
(func $num-number->string (param $env i32) (param $args i32) (result i32)
  (local $args-len i32)
  (local $radix i32)
  (local $num i32)
  (local $num-type i32)
  (local $str i32)

  (local.set $args-len (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.eqz (local.get $args-len)))
      (local.set $radix (i32.const 10))
      (local.set $num (%car-l $args))

      (local.set $radix (%car (%cdr-l $args)))
      (%chk-type $b_fail $radix %i64-type)
      (br_if $b_fail (i64.gt_u (i64.load offset=4 (local.get $radix)) (i64.const 16)))
      (local.set $radix (i32.wrap_i64 (i64.load offset=4 (local.get $radix))))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 16)))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 10)))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 8)))
      (br_if $b_check (i32.eq (local.get $radix) (i32.const 2))))

   (return (call $argument-error (local.get $args))))

  (block $b_convert
    (local.set $num-type (%get-type $num))
    (if (i32.eq (local.get $num-type) (%i64-type))
      (then
        (local.set $str (call $integer->string-impl 
            (i64.load offset=4 (local.get $num)) 
            (local.get $radix)))
        (br $b_convert)))

    (if (i32.eq (local.get $num-type) (%big-int-type))
      (then
        (local.set $str (call $mp-mp->string
            (%car-l $num)
            (local.get $radix)))
        (br $b_convert)))

    ;; TODO implement float support
    ;; TODO implement rational support
    (return (call $argument-error (local.get $args))))

  (return (%alloc-str (local.get $str))))