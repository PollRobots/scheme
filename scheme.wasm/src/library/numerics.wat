(func $integer? (param $env i32) (param $ptr i32) (result i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (%get-type $ptr) (%cons-type)))
      (local.set $temp (%cdr-l $ptr))
      (br_if $b_fail (i32.ne (%get-type $temp) (%nil-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $ptr)))
  )

  (local.set $temp (%car-l $ptr))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $temp) (%i64-type))
    )
  )
)

(func $all-numeric (param $args i32) (result i32)
  (local $temp i32)
  (local $temp-type i32)

  (block $done 
    (loop $forever
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))
      (local.set $temp-type (%get-type $temp))

      (if 
        (i32.and 
          (i32.ne (local.get $temp-type) (%i64-type)) 
          (i32.ne (local.get $temp-type) (%f64-type))
        )
        (then
          (return (i32.const 0))
        )
      )

      (local.set $args (%cdr-l $args))
      (br $forever)
    )
  )

  (return (i32.const 1))
)

(func $list-len (param $args i32) (result i32)
  (local $len i32)
  (local.set $len (i32.const 0))

  (block $done
    (loop $forever
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (%inc $len)

      (local.set $args (%cdr-l $args))
      (br $forever)
    )
  )

  (return (local.get $len))
)

(func $num-equal (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $num-cmp-impl (local.get $args) (i32.const 0)))
)

(func $num-gt (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $num-cmp-impl (local.get $args) (i32.const 1)))
)

(func $num-ge (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $num-cmp-impl (local.get $args) (i32.const 2)))
)

(func $num-lt (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $num-cmp-impl (local.get $args) (i32.const 3)))
)

(func $num-le (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $num-cmp-impl (local.get $args) (i32.const 4)))
)

;; cmp-ops are
;;  0 =
;;  1 >
;;  2 >=
;;  3 <
;;  4 <=

(func $num-cmp-impl (param $args i32) (param $cmp-op i32) (result i32)
  (local $temp i32)
  (local $cmp-type i32)
  (local $left i64)
  (local $right i64)

  (local.set $temp (%car-l $args))
  (local.set $cmp-type (%get-type $temp))
  (local.set $args (%cdr-l $args))
  (local.set $right (i64.load offset=4 (local.get $temp)))

  (block $done
    (loop $continue
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))

      (block $b_cmp_check
        (block $b_cmp_fail
          (br_if $b_cmp_fail (i32.ne (%get-type $temp) (local.get $cmp-type)))

          (local.set $left (local.get $right))
          (local.set $right (i64.load offset=4 (local.get $temp)))

          (if (i32.eq (local.get $cmp-op) (i32.const 0)) 
            (then
              (if (i32.eq (local.get $cmp-type) (%i64-type))
                (then
                  (br_if $b_cmp_fail (i64.ne (local.get $left) (local.get $right)))
                )
                (else
                  (br_if $b_cmp_fail 
                    (f64.ne (f64.reinterpret_i64 (local.get $left)) (f64.reinterpret_i64 (local.get $right)))
                  )
                )
              )
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 1))  ;; >
            (then
              (if (i32.eq (local.get $cmp-type) (%i64-type))
                (then
                  (br_if $b_cmp_fail (i64.le_s (local.get $left) (local.get $right)))
                )
                (else
                  (br_if $b_cmp_fail 
                    (f64.le (f64.reinterpret_i64 (local.get $left)) (f64.reinterpret_i64 (local.get $right)))
                  )
                )
              )
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 2)) ;; >=
            (then
              (if (i32.eq (local.get $cmp-type) (%i64-type))
                (then
                  (br_if $b_cmp_fail (i64.lt_s (local.get $left) (local.get $right)))
                )
                (else
                  (br_if $b_cmp_fail 
                    (f64.lt (f64.reinterpret_i64 (local.get $left)) (f64.reinterpret_i64 (local.get $right)))
                  )
                )
              )
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 3)) ;; <
            (then
              (if (i32.eq (local.get $cmp-type) (%i64-type))
                (then
                  (br_if $b_cmp_fail (i64.ge_s (local.get $left) (local.get $right)))
                )
                (else
                  (br_if $b_cmp_fail 
                    (f64.ge (f64.reinterpret_i64 (local.get $left)) (f64.reinterpret_i64 (local.get $right)))
                  )
                )
              )
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 4)) ;; <=
            (then
              (if (i32.eq (local.get $cmp-type) (%i64-type))
                (then
                  (br_if $b_cmp_fail (i64.gt_s (local.get $left) (local.get $right)))
                )
                (else
                  (br_if $b_cmp_fail 
                    (f64.gt (f64.reinterpret_i64 (local.get $left)) (f64.reinterpret_i64 (local.get $right)))
                  )
                )
              )
              (br $b_cmp_check)
            )
          )
        )
        (return (global.get $g-false))
      )

      (local.set $args (%cdr-l $args))
      (br $continue)
    )
  )

  (return (global.get $g-true))
)

(func $num-add (param $env i32) (param $args i32) (result i32)
  (local $accum i64)
  (local $car i32)

  (if (i32.eqz (call $all-numeric (local.get $args)))
    (then
    (return (call $argument-error (local.get $args)))
    )
  )

  ;; accum = 0
  (local.set $accum (i64.const 0))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      ;; car = args[4]
      (local.set $car (i32.load offset=4 (local.get $args)))
      ;; TODO handle floats
      ;; accum += car[4]
      (local.set $accum 
        (i64.add 
          (local.get $accum) 
          (i64.load offset=4 (local.get $car))
        )
      )
      ;; args = args[8]
      (local.set $args (i32.load offset=8 (local.get $args)))
      (br $b_start)
    )
  ;; }
  )

  ;; return heap-alloc(g-heap, (%i64-type), (i32)accum, (i32)(accum >> 32))
  (return (%alloc-i64 (local.get $accum)))
)

(func $num-mul (param $env i32) (param $args i32) (result i32)
  (local $accum i64)
  (local $car i32)

  (if (i32.eqz (call $all-numeric (local.get $args)))
    (then
    (return (call $argument-error (local.get $args)))
    )
  )

  ;; accum = 1
  (local.set $accum (i64.const 1))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      ;; car = args[4]
      (local.set $car (i32.load offset=4 (local.get $args)))
      ;; TODO handle floats
      ;; accum *= car[4]
      (local.set $accum 
        (i64.mul 
          (local.get $accum) 
          (i64.load offset=4 (local.get $car))
        )
      )
      ;; args = args[8]
      (local.set $args (i32.load offset=8 (local.get $args)))
      (br $b_start)
    )
  ;; }
  )

  ;; return heap-alloc(g-heap, (%i64-type), (i32)accum, (i32)(accum >> 32))
  (return (%alloc-i64 (local.get $accum)))
)

(func $num-sub (param $env i32) (param $args i32) (result i32)
  (local $accum i64)
  (local $car i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.eq (%get-type $args) (%nil-type)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  ;; car = car(args) 
  (local.set $car (%car-l $args))
  ;; TODO handle float
  ;; accum = car.i64
  (local.set $accum (i64.load offset=4 (local.get $car)))

  ;; args = cdr(args)
  (local.set $args (%cdr-l $args))
  (if (i32.eq (%get-type $args) (%nil-type))
    (then
      (local.set $accum (i64.sub (i64.const 0) (local.get $accum)))
      (return (%alloc-i64 (local.get $accum)))
    )
  )

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      ;; car = args[4]
      (local.set $car (i32.load offset=4 (local.get $args)))
      ;; TODO handle floats
      ;; accum -= car[4]
      (local.set $accum 
        (i64.sub 
          (local.get $accum) 
          (i64.load offset=4 (local.get $car))
        )
      )
      ;; args = args[8]
      (local.set $args (i32.load offset=8 (local.get $args)))
      (br $b_start)
    )
  ;; }
  )

  ;; return heap-alloc(g-heap, (%i64-type), (i32)accum, (i32)(accum >> 32))
  (return (%alloc-i64 (local.get $accum)))
)

(func $num-abs (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $num i64)
  (local $fnum f64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg (%car-l $args))

  (if (i32.eq (%get-type $arg) (%i64-type))
    (then
      (local.set $num (i64.load offset=4 (local.get $arg)))
      (if (i64.ge_s (local.get $num) (i64.const 0))
        (then (return (local.get $arg)))
        (else (return (%alloc-i64 (i64.sub (i64.const 0) (local.get $num)))))
      ) 
    )
    (else
      (local.set $fnum (f64.load offset=4 (local.get $arg)))
      (local.set $num (i64.reinterpret_f64 (f64.abs (local.get $fnum))))
      (return
        (call $heap-alloc
          (global.get $g-heap) 
          (%f64-type) 
          (i32.wrap_i64 (local.get $num)) 
          (i32.wrap_i64 (i64.shr_u (local.get $num) (i64.const 32)))
        )
      )
    )
  )
  (unreachable)
)

(func $num-truncate-quotient (param $env i32) (param $args i32) (result i32)
  (local $arg-num i32)
  (local $arg-denom i32)
  (local $num-type i32)
  (local $denom-type i32)
  (local $idenom i64)
  (local $iquot i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg-num (%car-l $args))
  (local.set $arg-denom (%car (%cdr-l $args)))

  (local.set $num-type (%get-type $arg-num))
  (local.set $denom-type (%get-type $arg-denom))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (then
      (if (i32.eq (local.get $denom-type) (%i64-type))
        (then
          (local.set $idenom (i64.load offset=4 (local.get $arg-denom))) 
          (if (i64.eqz (local.get $idenom))
            (return (%alloc-error-cons (%sym-32 0x30766964 4) (global.get $g-nil)))
          )
          (local.set $iquot 
            (i64.div_s 
              (i64.load offset=4 (local.get $arg-num))
              (local.get $idenom)
            )
          )

          (return (%alloc-i64 (local.get $iquot)))
        )
      )
    )
  )

  ;; TODO implement floating point
  (unreachable)
)

(func $num-truncate-remainder (param $env i32) (param $args i32) (result i32)
  (local $arg-num i32)
  (local $arg-denom i32)
  (local $num-type i32)
  (local $denom-type i32)
  (local $idenom i64)
  (local $irem i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg-num (%car-l $args))
  (local.set $arg-denom (%car (%cdr-l $args)))

  (local.set $num-type (%get-type $arg-num))
  (local.set $denom-type (%get-type $arg-denom))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (then
      (if (i32.eq (local.get $denom-type) (%i64-type))
        (then
          (local.set $idenom (i64.load offset=4 (local.get $arg-denom))) 
          (if (i64.eqz (local.get $idenom))
            (return (%alloc-error-cons (%sym-32 0x30766964 4) (global.get $g-nil)))
          )
          (local.set $irem 
            (i64.rem_s 
              (i64.load offset=4 (local.get $arg-num))
              (local.get $idenom)
            )
          )

          (return (%alloc-i64 (local.get $irem)))
        )
      )
    )
  )

  ;; TODO implement floating point
  (unreachable)
)
 
(func $num-truncate/ (param $env i32) (param $args i32) (result i32)
  (local $arg-num i32)
  (local $arg-denom i32)
  (local $num-type i32)
  (local $denom-type i32)
  (local $idenom i64)
  (local $inum i64)
  (local $irem i64)
  (local $iquot i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg-num (%car-l $args))
  (local.set $arg-denom (%car (%cdr-l $args)))

  (local.set $num-type (%get-type $arg-num))
  (local.set $denom-type (%get-type $arg-denom))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (then
      (if (i32.eq (local.get $denom-type) (%i64-type))
        (then
          (local.set $idenom (i64.load offset=4 (local.get $arg-denom))) 
          (if (i64.eqz (local.get $idenom))
            (return (%alloc-error-cons (%sym-32 0x30766964 4) (global.get $g-nil)))
          )

          (local.set $inum (i64.load offset=4 (local.get $arg-num))) 

          (local.set $iquot (i64.div_s (local.get $inum) (local.get $idenom)))
          (local.set $irem (i64.rem_s (local.get $inum) (local.get $idenom))
          )

          (return
            (%alloc-values
              (%alloc-i64 (local.get $iquot))
              (%alloc-cons
                (%alloc-i64 (local.get $irem))
                (global.get $g-nil)
              )
            )
          )
        )
      )
    )
  )

  ;; TODO implement floating point
  (unreachable)
)
 
(func $num-floor-quotient (param $env i32) (param $args i32) (result i32)
  (local $arg-num i32)
  (local $arg-denom i32)
  (local $num-type i32)
  (local $denom-type i32)
  (local $idenom i64)
  (local $inum i64)
  (local $iquot i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg-num (%car-l $args))
  (local.set $arg-denom (%car (%cdr-l $args)))

  (local.set $num-type (%get-type $arg-num))
  (local.set $denom-type (%get-type $arg-denom))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (then
      (if (i32.eq (local.get $denom-type) (%i64-type))
        (then
          (local.set $idenom (i64.load offset=4 (local.get $arg-denom))) 
          (if (i64.eqz (local.get $idenom))
            (return (%alloc-error-cons (%sym-32 0x30766964 4) (global.get $g-nil)))
          )
          (local.set $inum (i64.load offset=4 (local.get $arg-num))) 

          (local.set $iquot   
            (i64.trunc_f64_s
              (f64.floor
                (f64.div
                  (f64.convert_i64_s (local.get $inum))
                  (f64.convert_i64_s (local.get $idenom))
                )
              )
            )
          )

          (return (%alloc-i64 (local.get $iquot)))
        )
      )
    )
  )

  ;; TODO implement floating point
  (unreachable)
)

(func $num-floor-remainder (param $env i32) (param $args i32) (result i32)
  (local $arg-num i32)
  (local $arg-denom i32)
  (local $num-type i32)
  (local $denom-type i32)
  (local $idenom i64)
  (local $inum i64)
  (local $iquot i64)
  (local $irem i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg-num (%car-l $args))
  (local.set $arg-denom (%car (%cdr-l $args)))

  (local.set $num-type (%get-type $arg-num))
  (local.set $denom-type (%get-type $arg-denom))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (then
      (if (i32.eq (local.get $denom-type) (%i64-type))
        (then
          (local.set $idenom (i64.load offset=4 (local.get $arg-denom))) 
          (if (i64.eqz (local.get $idenom))
            (return (%alloc-error-cons (%sym-32 0x30766964 4) (global.get $g-nil)))
          )
          (local.set $inum (i64.load offset=4 (local.get $arg-num))) 

          (local.set $iquot   
            (i64.trunc_f64_s
              (f64.floor
                (f64.div
                  (f64.convert_i64_s (local.get $inum))
                  (f64.convert_i64_s (local.get $idenom))
                )
              )
            )
          )
          ;; rem = num - denom * iquot
          (local.set $irem
            (i64.sub
              (local.get $inum)
              (i64.mul 
                (local.get $idenom)
                (local.get $iquot)
              )
            )
          )

          (return (%alloc-i64 (local.get $irem)))
        )
      )
    )
  )

  ;; TODO implement floating point
  (unreachable)
)

(func $num-floor/ (param $env i32) (param $args i32) (result i32)
  (local $arg-num i32)
  (local $arg-denom i32)
  (local $num-type i32)
  (local $denom-type i32)
  (local $idenom i64)
  (local $inum i64)
  (local $iquot i64)
  (local $irem i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg-num (%car-l $args))
  (local.set $arg-denom (%car (%cdr-l $args)))

  (local.set $num-type (%get-type $arg-num))
  (local.set $denom-type (%get-type $arg-denom))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (then
      (if (i32.eq (local.get $denom-type) (%i64-type))
        (then
          (local.set $idenom (i64.load offset=4 (local.get $arg-denom))) 
          (if (i64.eqz (local.get $idenom))
            (return (%alloc-error-cons (%sym-32 0x30766964 4) (global.get $g-nil)))
          )
          (local.set $inum (i64.load offset=4 (local.get $arg-num))) 

          (local.set $iquot   
            (i64.trunc_f64_s
              (f64.floor
                (f64.div
                  (f64.convert_i64_s (local.get $inum))
                  (f64.convert_i64_s (local.get $idenom))
                )
              )
            )
          )
          ;; rem = num - denom * iquot
          (local.set $irem
            (i64.sub
              (local.get $inum)
              (i64.mul 
                (local.get $idenom)
                (local.get $iquot)
              )
            )
          )

          (return 
            (%alloc-values
              (%alloc-i64 (local.get $iquot))
              (%alloc-cons
                (%alloc-i64 (local.get $irem))
                (global.get $g-nil)
              )
            )
          )
        )
      )
    )
  )

  ;; TODO implement floating point
  (unreachable)
)

(func $num-exact-integer-sqrt (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $x i64)
  (local $op i64)
  (local $res i64)
  (local $one i64)
  (local $temp i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (%chk-type $b_fail $arg %i64-type)
      (local.set $x (i64.load offset=4 (local.get $arg)))
      (br_if $b_fail (i64.lt_s (local.get $x) (i64.const 0)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

	;; op = x;
  (local.set $op (local.get $x))
	;; res = 0;
  (local.set $res (i64.const 0))

	;; /* "one" starts at the highest power of four <= than the argument. */

	;; one = 1 << 62;	/* second-to-top bit set */
  (local.set $one (i64.const 0x4000_0000_0000_0000))

  ;; temp = clz(op)
  ;; temp = (temp - 1) & 0xfe
  ;; one >>= temp
  (local.set $temp (i64.clz (local.get $op)))
  (local.set $temp (i64.and 
      (i64.sub (local.get $temp) (i64.const 1)) 
      (i64.const 0xFE)))
  (local.set $one (i64.shr_u (local.get $one) (local.get $temp)))


	;; while (one != 0) {
  (block $b_end (loop $b_start (br_if $b_end (i64.eqz (local.get $one)))
      (local.set $temp (i64.add (local.get $res) (local.get $one)))
      ;; if (op >= res + one) {
      (if (i64.ge_u (local.get $op) (local.get $temp))
        (then
          ;; op = op - (res + one);
          (local.set $op (i64.sub (local.get $op) (local.get $temp)))
          ;; res = res +  2 * one;
          (local.set $res (i64.add
              (local.get $res)
              (i64.shl (local.get $one) (i64.const 1))))))
      ;; }
      ;; res /= 2;
      (local.set $res (i64.shr_u (local.get $res) (i64.const 1)))
      ;; one /= 4;
      (local.set $one (i64.shr_u (local.get $one) (i64.const 2)))
      (br $b_start)))
	;; }

	;; return(res);
  (return 
    (%alloc-values
      (%alloc-i64 (local.get $res))
      (%alloc-cons
        (%alloc-i64 (local.get $op))
        (global.get $g-nil)))))

(func $num-string->number (param $env i32) (param $args i32) (result i32)
  (local $args-len i32)
  (local $radix i32)
  (local $str i32)

  (local.set $args-len (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (if (i32.eq (local.get $args-len) (i32.const 1))
        (then
          (local.set $radix (i32.const 10))
          (br $b_check)
        ) 
      )
      (if (i32.eq (local.get $args-len) (i32.const 2))
        (then
          (local.set $radix (%car (%cdr-l $args)))
          (br_if $b_fail (i32.ne (%get-type $radix) (%i64-type)))
          (br_if $b_fail (i64.gt_u (i64.load offset=4 (local.get $radix)) (i64.const 16)))
          (local.set $radix (i32.wrap_i64 (i64.load offset=4 (local.get $radix))))
          (if (i32.ne (local.get $radix) (i32.const 16))
            (then
              (if (i32.ne (local.get $radix) (i32.const 10))
                (then
                  (if (i32.ne (local.get $radix) (i32.const 8))
                    (then
                      (br_if $b_fail (i32.ne (local.get $radix) (i32.const 2)))
                    )
                  )
                )
              )
            )
          )
          (br $b_check)
        )
      )
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $str (%car-l $args))
  (if (i32.ne (%get-type $str) (%str-type))
    (then (return (call $argument-error (local.get $args))))
  )

  (return (call $string->number-impl (local.get $str) (local.get $radix)))
)

(func $num-number->string (param $env i32) (param $args i32) (result i32)
  (local $args-len i32)
  (local $radix i32)
  (local $num i32)
  (local $str i32)

  (local.set $args-len (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (if (i32.eq (local.get $args-len) (i32.const 1))
        (then
          (local.set $radix (i32.const 10))
          (br $b_check)
        ) 
      )
      (if (i32.eq (local.get $args-len) (i32.const 2))
        (then
          (local.set $radix (%car (%cdr-l $args)))
          (br_if $b_fail (i32.ne (%get-type $radix) (%i64-type)))
          (br_if $b_fail (i64.gt_u (i64.load offset=4 (local.get $radix)) (i64.const 16)))
          (local.set $radix (i32.wrap_i64 (i64.load offset=4 (local.get $radix))))
          (if (i32.ne (local.get $radix) (i32.const 16))
            (then
              (if (i32.ne (local.get $radix) (i32.const 10))
                (then
                  (if (i32.ne (local.get $radix) (i32.const 8))
                    (then
                      (br_if $b_fail (i32.ne (local.get $radix) (i32.const 2)))
                    )
                  )
                )
              )
            )
          )
          (br $b_check)
        )
      )
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $num (%car-l $args))
  (if (i32.ne (%get-type $num) (%i64-type))
    ;; TODO implement float support
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $str (call $integer->string-impl (i64.load offset=4 (local.get $num)) (local.get $radix)))

  (return (%alloc-str (local.get $str)))
)