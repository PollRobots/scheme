(func $num-regularize-type (param $ptr i32) (param $ptr-type i32) (param $other-type i32) (result i32)
  (local $temp-64 i64)
  (local $mp i32)

  (if (i32.eq (local.get $ptr-type) (local.get $other-type))
    (then (return (local.get $ptr))))

  (if (i32.eq (local.get $ptr-type) (%f64-type)) (then
      (return (local.get $ptr))))

  (if (i32.eq (local.get $other-type) (%f64-type)) (then
      (return (call $inexact-impl (local.get $ptr)))))

  (if (i32.eq (local.get $ptr-type) (%big-int-type))
    (then (return (local.get $ptr))))

  (if (i32.eq (local.get $other-type) (%big-int-type))
    (then
      (local.set $temp-64 (i64.load offset=4 (local.get $ptr)))
      (if (i64.lt_s (local.get $temp-64) (i64.const 0))
        (then
          (local.set $temp-64 (i64.sub (i64.const 0) (local.get $temp-64)))
          (local.set $mp (call $mp-from-u64 (local.get $temp-64)))
          (call $mp-neg (local.get $mp)))
        (else
          (local.set $mp (call $mp-from-u64 (local.get $temp-64)))))
      (return (%alloc-big-int (local.get $mp)))))

  (unreachable))

(func $num-core-cmp (param $left i32) (param $right i32) (result i32)
  (local $left-type i32)
  (local $right-type i32)
  (local $left-reg i32)
  (local $right-reg i32)
  (local $reg-type i32)

  (local.set $left-type (%get-type $left))
  (local.set $right-type (%get-type $right))

  (local.set $left-reg (call $num-regularize-type 
      (local.get $left) 
      (local.get $left-type)
      (local.get $right-type)))

  (local.set $right-reg (call $num-regularize-type 
      (local.get $right) 
      (local.get $right-type)
      (local.get $left-type)))

  (local.set $reg-type (%get-type $left-reg))

  (if (i32.eq (local.get $reg-type) (%i64-type))
    (then (return (call $num-core-cmp-64 
          (local.get $left-reg)
          (local.get $right-reg)))))


  (if (i32.eq (local.get $reg-type) (%big-int-type))
    (then (return (call $mp-cmp
          (%car-l $left-reg)
          (%car-l $right-reg)))))

  (if (i32.eq (local.get $reg-type) (%f64-type))
    (then (return (call $num-core-cmp-real
          (local.get $left-reg)
          (local.get $right-reg)))))
  
  (unreachable))

(func $num-core-cmp-64 (param $left i32) (param $right i32) (result i32)
  (local $diff i64)

  (local.set $diff (i64.sub 
      (i64.load offset=4 (local.get $left))
      (i64.load offset=4 (local.get $right))))

  (if (i64.eqz (local.get $diff))
    (then (return (i32.const 0))))

  (return (select
    (i32.const -1)
    (i32.const 1)
    (i64.lt_s (local.get $diff) (i64.const 0)))))

(func $num-core-cmp-real (param $left i32) (param $right i32) (result i32)
  (local $left-v f64)
  (local $right-v f64)

  (local.set $left-v (f64.load offset=4 (local.get $left)))
  (local.set $right-v (f64.load offset=4 (local.get $right)))

  (if (f64.eq (local.get $left-v) (local.get $right-v)) (then 
      (return (i32.const 0))))

  (return (select
      (i32.const -1)
      (i32.const 1)
      (f64.lt (local.get $left-v) (local.get $right-v)))))

(func $num-core-add (param $left i32) (param $right i32) (result i32)
  (local $left-type i32)
  (local $right-type i32)
  (local $left-reg i32)
  (local $right-reg i32)
  (local $reg-type i32)
  (local $left-64 i64)
  (local $right-64 i64)

  (local.set $left-type (%get-type $left))
  (local.set $right-type (%get-type $right))

  (local.set $left-reg (call $num-regularize-type 
      (local.get $left) 
      (local.get $left-type)
      (local.get $right-type)))

  (local.set $right-reg (call $num-regularize-type 
      (local.get $right) 
      (local.get $right-type)
      (local.get $left-type)))

  (local.set $reg-type (%get-type $left-reg))

  (if (i32.eq (local.get $reg-type) (%i64-type))
    (then 
      ;; check if this is a candidate for overflow
      (block $b_check (block $b_promote
          (br_if $b_promote (i32.ge_u 
              (call $num-core-64-abs-log2 (i64.load offset=4 (local.get $left-reg))) 
              (i32.const 62)))
          (br_if $b_promote (i32.ge_u 
              (call $num-core-64-abs-log2 (i64.load offset=4 (local.get $right-reg)))
              (i32.const 62)))
          (br $b_check))
        
        (return (call $num-core-add
            (call $num-regularize-type (local.get $left-reg) (local.get $reg-type) (%big-int-type))
            (call $num-regularize-type (local.get $right-reg) (local.get $reg-type) (%big-int-type)))))

      (local.set $left-64 (i64.load offset=4 (local.get $left-reg)))
      (local.set $right-64 (i64.load offset=4 (local.get $right-reg)))

      (return (%alloc-i64 (i64.add
            (i64.load offset=4 (local.get $left-reg))
            (i64.load offset=4 (local.get $right-reg)))))))

  (if (i32.eq (local.get $reg-type) (%big-int-type))
    (then 
      (return (call $num-core-maybe-demote-mp 
          (%alloc-big-int (call $mp-add
            (%car-l $left-reg)
            (%car-l $right-reg)))))))
  
  (if (i32.eq (local.get $reg-type) (%f64-type)) (then
      (return (%alloc-f64 (f64.add
            (f64.load offset=4 (local.get $left-reg))
            (f64.load offset=4 (local.get $right-reg)))))))
  
  (unreachable))

(func $num-core-64-abs-log2 (param $val i64) (result i32)
  (if (i64.ge_s (local.get $val) (i64.const 0))
    (then (return (i32.sub 
          (i32.const 64)
          (i32.wrap_i64 (i64.clz (local.get $val)))))))

  (return (i32.sub 
    (i32.const 64)
    (i32.wrap_i64 (i64.clz (i64.sub (i64.const 0) (local.get $val)))))))
    
  

(func $num-core-mul (param $left i32) (param $right i32) (result i32)
  (local $left-type i32)
  (local $right-type i32)
  (local $left-reg i32)
  (local $right-reg i32)
  (local $reg-type i32)

  (local.set $left-type (%get-type $left))
  (local.set $right-type (%get-type $right))

  (local.set $left-reg (call $num-regularize-type 
      (local.get $left) 
      (local.get $left-type)
      (local.get $right-type)))

  (local.set $right-reg (call $num-regularize-type 
      (local.get $right) 
      (local.get $right-type)
      (local.get $left-type)))

  (local.set $reg-type (%get-type $left-reg))

  (if (i32.eq (local.get $reg-type) (%i64-type))
    (then 
      ;; check if this is a candidate for overflow
      (if (i32.ge_u 
            (i32.add 
              (call $num-core-64-abs-log2 (i64.load offset=4 (local.get $left-reg)))
              (call $num-core-64-abs-log2 (i64.load offset=4 (local.get $right-reg))))
            (i32.const 62))
        (then
          (return (call $num-core-mul
              (call $num-regularize-type (local.get $left-reg) (local.get $reg-type) (%big-int-type))
              (call $num-regularize-type (local.get $right-reg) (local.get $reg-type) (%big-int-type))))))

      (return (%alloc-i64 (i64.mul
            (i64.load offset=4 (local.get $left-reg))
            (i64.load offset=4 (local.get $right-reg)))))))

  (if (i32.eq (local.get $reg-type) (%big-int-type)) (then 
      (return (call $num-core-maybe-demote-mp 
          (%alloc-big-int (call $mp-mul
            (%car-l $left-reg)
            (%car-l $right-reg)))))))

  (if (i32.eq (local.get $reg-type) (%f64-type)) (then
      (return (%alloc-f64 (f64.mul
            (f64.load offset=4 (local.get $left-reg))
            (f64.load offset=4 (local.get $right-reg)))))))
  
  (unreachable))

(func $num-core-neg (param $num i32) (result i32)
  (local $num-type i32)
  (local $mp i32)
  (local $temp-64 i64)

  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type (%i64-type)))
    (then 
      (local.set $temp-64 (i64.load offset=4 (local.get $num)))
      ;; There is one negative number that has no positive 64-bit integer, promote to big-int
      (if (i64.eq (local.get $temp-64) (i64.const 0x8000_0000_0000_0000))
        (then
          (local.set $mp (call $calloc (i32.const 3) (i32.const 4)))
          (i32.store offset=0 (local.get $mp) (i32.const 0x8000_0002))
          (i32.store offset=4 (local.get $mp) (i32.const 0x8000_0000))
          (return (%alloc-big-int (local.get $mp)))))
      (return (%alloc-i64 (i64.sub (i64.const 0) (local.get $temp-64))))))

  (if (i32.eq (local.get $num-type) (%big-int-type))
    (then 
      (local.get $mp (call $mp-copy (%car-l $num)))
      (call $mp-neg (local.get $mp))
      (return (call $num-core-maybe-demote-mp (%alloc-big-int (local.get $mp))))))
  
  (if (i32.eq (local.get $num-type) (%f64-type)) (then 
      (return (%alloc-f64 (f64.neg (f64.load offset=4 (local.get $num)))))))
  
  (unreachable))

(func $num-core-sub (param $left i32) (param $right i32) (result i32)
  (local $left-type i32)
  (local $right-type i32)
  (local $left-reg i32)
  (local $right-reg i32)
  (local $reg-type i32)
  (local $temp-64 i32)

  (local.set $left-type (%get-type $left))
  (local.set $right-type (%get-type $right))

  (local.set $left-reg (call $num-regularize-type 
      (local.get $left) 
      (local.get $left-type)
      (local.get $right-type)))

  (local.set $right-reg (call $num-regularize-type 
      (local.get $right) 
      (local.get $right-type)
      (local.get $left-type)))

  (local.set $reg-type (%get-type $left-reg))

  (if (i32.eq (local.get $reg-type) (%i64-type))
    (then 
      ;; check if this is a candidate for overflow
      (block $b_check (block $b_promote
          (br_if $b_promote (i32.ge_u 
              (call $num-core-64-abs-log2 (i64.load offset=4 (local.get $left-reg))) 
              (i32.const 62)))
          (br_if $b_promote (i32.ge_u 
              (call $num-core-64-abs-log2 (i64.load offset=4 (local.get $right-reg))) 
              (i32.const 62)))
          (br $b_check))
        
        (return (call $num-core-sub
            (call $num-regularize-type (local.get $left-reg) (local.get $reg-type) (%big-int-type))
            (call $num-regularize-type (local.get $right-reg) (local.get $reg-type) (%big-int-type)))))

      (return (%alloc-i64 (i64.sub
            (i64.load offset=4 (local.get $left-reg))
            (i64.load offset=4 (local.get $right-reg)))))))


  (if (i32.eq (local.get $reg-type) (%big-int-type))
    (then 
      (return (call $num-core-maybe-demote-mp 
          (%alloc-big-int (call $mp-sub
            (%car-l $left-reg)
            (%car-l $right-reg)))))))

  (if (i32.eq (local.get $reg-type) (%f64-type)) (then 
      (return (%alloc-f64 (f64.sub 
            (f64.load offset=4 (local.get $left-reg))
            (f64.load offset=4 (local.get $right-reg)))))))
          
  
  (unreachable))

(func $num-core-maybe-demote-mp (param $ptr i32) (result i32)
  (local $mp i32)
  (local $temp-64 i64)

  (local.set $mp (%car-l $ptr))

  (if (i32.gt_u (call $mp-log2 (local.get $mp)) (i32.const 63))
    (then (return (local.get $ptr))))

  (local.set $temp-64 (call $mp-to-u64 (local.get $mp)))
  (if (%mp-sign-l $mp)
    (then (local.set $temp-64 (i64.sub 
          (i64.const 0) 
          (local.get $temp-64)))))
  
  (return (%alloc-i64 (local.get $temp-64))))

(func $num-core-abs (param $num i32) (result i32)
  (local $type i32)
  (local $num-64 i64)
  (local $mp i32)
  (local $sign i32)
  (local $real f64)

  (local.set $type (%get-type $num))

  (if (i32.eq (local.get $type) (%i64-type)) (then
      (local.get $num-64 (i64.load offset=4 (local.get $num)))
      (if (i64.ge_s (local.get $num-64) (i64.const 0))
        (then (return (local.get $num))))
      (if (i64.eq (local.get $num-64) (i64.const 0x8000_0000_0000_0000)) (then
          (local.set $mp (call $calloc (i32.const 3) (i32.const 4)))
          (i32.store offset=0 (local.get $mp) (i32.const 0x8000_0002))
          (i32.store offset=4 (local.get $mp) (i32.const 0x8000_0000))
          (return (%alloc-big-int (local.get $mp)))))
      (return (%alloc-i64 (i64.sub (i64.const 0) (local.get $num-64))))))

  (if (i32.eq (local.get $type) (%big-int-type)) (then
      (local.set $mp (%car-l $num))
      (if (i32.and (i32.load (local.get $mp)) (i32.const 0x8000_0000)) (then
          (local.set $mp (call $mp-copy (local.get $mp)))
          (call $mp-neg (local.get $mp))
          (return (%alloc-big-int (local.get $mp)))))
      (return (local.get $num))))

  (if (i32.eq (local.get $type) (%f64-type)) (then
      (local.set $real (f64.load offset=4 (local.get $num)))
      (if (f64.ge (local.get $real) (f64.const 0)) (then
          (return (local.get $num))))
      (return (%alloc-f64 (f64.neg (local.get $real))))))

  (unreachable))

(func $num-core-div (param $dividend i32) (param $divisor i32) (param $floor i32) (result i64)
  (local $dividend-type i32)
  (local $divisor-type i32)
  (local $dividend-reg i32)
  (local $divisor-reg i32)
  (local $reg-type i32)
  (local $dvdnd-64 i64)
  (local $dvsr-64 i64)
  (local $quot-64 i64)
  (local $rem-64 i64)
  (local $res-64 i64)
  (local $dvdnd-mp i32)
  (local $dvsr-mp i32)
  (local $quot-mp i32)
  (local $rem-mp i32)
  (local $temp-mp i32)
  (local $one-mp i32)

  (local.set $dividend-type (%get-type $dividend))
  (local.set $divisor-type (%get-type $divisor))

  (local.set $dividend-reg (call $num-regularize-type 
      (local.get $dividend) 
      (local.get $dividend-type)
      (local.get $divisor-type)))

  (local.set $divisor-reg (call $num-regularize-type 
      (local.get $divisor) 
      (local.get $divisor-type)
      (local.get $dividend-type)))

  (local.set $reg-type (%get-type $dividend-reg))

  (if (i32.eq (local.get $reg-type) (%i64-type))
    (then
      (local.set $dvdnd-64 (i64.load offset=4 (local.get $dividend-reg)))
      (local.set $dvsr-64 (i64.load offset=4 (local.get $divisor-reg)))
      (if (i64.eqz (local.get $dvsr-64))
        (then (return (i64.const 0))))

      (local.set $quot-64 (i64.div_s (local.get $dvdnd-64) (local.get $dvsr-64)))
      (if (local.get $floor)
        (then 
          (if (i64.lt_s (local.get $quot-64) (i64.const 0))
            (then (%dec64 $quot-64)))))
      (local.set $rem-64 (i64.sub
          (local.get $dvdnd-64)
          (i64.mul (local.get $dvsr-64) (local.get $quot-64))))
      (return (i64.or
          (i64.shl (i64.extend_i32_u (%alloc-i64 (local.get $quot-64))) (i64.const 32))
          (i64.extend_i32_u (%alloc-i64 (local.get $rem-64)))))))

  (if (i32.eq (local.get $reg-type) (%big-int-type))
    (then
      (local.set $res-64 (call $mp-div (%car-l $dividend-reg) (%car-l $divisor-reg)))
      (if (i64.eqz (local.get $res-64))
        (then (return (i64.const 0))))

      (local.set $quot-mp (i32.wrap_i64 (i64.shr_u (local.get $res-64) (i64.const 32))))
      (local.set $rem-mp (i32.wrap_i64 (local.get $res-64)))

      (if (local.get $floor)
        (then (if (i32.and (i32.load (local.get $quot-mp)) (i32.const 0x8000_0000))
            (then
              (local.set $one-mp (call $mp-from-u64 (i64.const 1)))
              (local.set $temp-mp (call $mp-sub (local.get $quot-mp) (local.get $one-mp)))
              (call $malloc-free (local.get $one-mp))
              (call $malloc-free (local.get $quot-mp))
              (local.set $quot-mp (local.get $temp-mp))

              (call $malloc-free (local.get $rem-mp))
              (local.set $temp-mp (call $mp-mul (local.get $quot-mp) (%car-l $divisor-reg)))
              (local.set $rem-mp (call $mp-sub (%car-l $dividend-reg) (local.get $temp-mp)))
              (call $malloc-free (local.get $temp-mp))))))

      (return (i64.or
          (i64.shl (i64.extend_i32_u (call $num-core-maybe-demote-mp (%alloc-big-int (local.get $quot-mp)))) (i64.const 32))
          (i64.extend_i32_u (call $num-core-maybe-demote-mp (%alloc-big-int (local.get $rem-mp))))))))
  
  (unreachable))

(func $num-core-int-sqrt (param $num i32) (result i32)
  (local $num-type i32)

  (local.set $num-type (%get-type $num))

  (if (i32.eq (local.get $num-type) (%i64-type))
    (return (call $num-core-int-sqrt-64 (local.get $num))))

  (if (i32.eq (local.get $num-type) (%big-int-type))
    (return (call $num-core-int-sqrt-big-int (local.get $num))))

  (return (i32.const 0)))

(func $num-core-int-sqrt-64 (param $num i32) (result i32)
  (local $x i64)
  (local $op i64)
  (local $res i64)
  (local $one i64)
  (local $temp i64)
  
  (local.set $x (i64.load offset=4 (local.get $num)))

  ;; cannot take sqrt of < 0 (until/unless we support complex numbers)
  (if (i64.lt_s (local.get $x) (i64.const 0))
    (then (return (i32.const 0))))

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

(func $num-core-int-sqrt-big-int (param $num i32) (result i32)
  (local $x i32)
  (local $op i32)
  (local $res i32)
  (local $one i32)
  (local $resone i32)
  (local $log2 i32)
  
  (local.set $x (%car-l $num))

  ;; cannot take sqrt of < 0 (until/unless we support complex numbers)
  (if (i32.and (i32.load (local.get $x)) (i32.const 0x8000_0000))
    (then (return (i32.const 0))))

 	;; op = x;
  (local.set $op (call $mp-copy (local.get $x)))
	;; res = 0;
  (local.set $res (call $mp-from-u64 (i64.const 0)))

	;; /* "one" starts at the highest power of four <= than the argument. */
  (local.set $log2 (i32.and 
    (call $mp-log2 (local.get $op)) 
    (i32.const 0xFFFF_FFfE)))
  (local.set $one (call $mp-from-u64 (i64.const 1)))
  (local.set $one (call $mp-shl-eq (local.get $one) (local.get $log2)))

	;; while (one != 0) {
  (block $b_end (loop $b_start 
      (br_if $b_end (call $mp-zero? (local.get $one)))

      (local.set $resone (call $mp-add (local.get $res) (local.get $one)))
      ;; if (op >= res + one) {
      (if (call $mp-ge? (local.get $op) (local.get $resone))
        (then
          ;; op = op - (res + one);
          (local.set $op (call $mp-minus-eq (local.get $op) (local.get $resone)))
          ;; res = res +  2 * one;
          ;; res = res + one + one
          (local.set $res (call $mp-plus-eq (local.get $res) (local.get $one)))
          (local.set $res (call $mp-plus-eq (local.get $res) (local.get $one)))))
      ;; }
      (call $malloc-free (local.get $resone))

      ;; res /= 2;
      (call $mp-shr-ip (local.get $res) (i32.const 1))
      ;; one /= 4;
      (call $mp-shr-ip (local.get $one) (i32.const 2))
      (br $b_start)))
	;; }

  (call $malloc-free (local.get $one))
  (local.set $res (%alloc-big-int (call $mp-normalize (local.get $res))))
  (local.set $op (%alloc-big-int (call $mp-normalize (local.get $op))))

	;; return(res);
  (return 
    (%alloc-values
      (call $num-core-maybe-demote-mp (local.get $res))
      (%alloc-cons
        (call $num-core-maybe-demote-mp (local.get $op))
        (global.get $g-nil)))))

