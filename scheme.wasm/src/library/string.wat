
(func $string? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $arg (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $arg) (%str-type))
    )
  )
)

(func $make-string (param $env i32) (param $args i32) (result i32)
  (local $k i32)
  (local $char i32)
  (local $num-args i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $len i64)
  (local $str-len i32)
  (local $code-point i32)
  (local $fill-32 i32)
  (local $char-buffer i32)

  (block $b_check
    (block $b_one_arg
      (block $b_fail
        (local.set $num-args (call $list-len (local.get $args)))
        (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 1)))
        (local.set $k (%car-l $args))
        (br_if $b_fail (i32.ne (%get-type $k) (%i64-type)))
        (local.set $len (i64.load offset=4 (local.get $k)))
        (br_if $b_fail (i64.ge_u (local.get $len) (i64.const 0x7FFFFFFC)))
        (local.set $str-len (i32.wrap_i64 (local.get $len)))
        (br_if $b_one_arg (i32.eq (local.get $num-args) (i32.const 1)))

        (br_if $b_fail (i32.gt_u (local.get $num-args) (i32.const 2)))
        (local.set $char (%car (%cdr-l $args)))
        (br_if $b_fail (i32.ne (%get-type $char) (%char-type)))
        (local.set $code-point (%car-l $char))
        (br $b_check)
      )
      (return (call $argument-error (local.get $args)))
    )
    (local.set $code-point (i32.const 0x7E))
  )

  (if (i32.eq (call $utf8-code-point-size (local.get $code-point)) (i32.const 1))
    (then
      (local.set $str (call $malloc (i32.add (local.get $str-len) (i32.const 4)))) 
      (i32.store (local.get $str) (local.get $str-len))
      (local.set $str-ptr (i32.add (local.get $str) (i32.const 4)))

      (local.set $fill-32 (i32.mul (local.get $code-point) (i32.const 0x01010101)))
      (block $b_end
        (loop $b_start
          (br_if $b_end (i32.lt_u (local.get $str-len) (i32.const 4)))

          (i32.store (local.get $str-ptr) (local.get $fill-32))

          (%plus-eq $str-ptr 4)
          (%minus-eq $str-len 4)
          (br $b_start)
        )
      )
      (block $b_done 
        (br_if $b_done (i32.eqz (local.get $str-len)))
        (i32.store8 (local.get $str-ptr) (local.get $code-point))
        (%inc $str-ptr)
        (%dec $str-len)
        (br_if $b_done (i32.eqz (local.get $str-len)))
        (i32.store8 (local.get $str-ptr) (local.get $code-point))
        (%inc $str-ptr)
        (%dec $str-len)
        (br_if $b_done (i32.eqz (local.get $str-len)))
        (i32.store8 (local.get $str-ptr) (local.get $code-point))
      )

      (return (%alloc-str (local.get $str)))
    )
  )

  (local.set $str-ptr 
    (local.tee $char-buffer 
      (call $malloc (i32.shl (local.get $str-len) (i32.const 2)))
    )
  )

  (block $b_end_char
    (loop $b_start_char
      (br_if $b_end_char (i32.eqz (local.get $str-len)))

      (i32.store (local.get $str-ptr) (local.get $code-point))

      (%plus-eq $str-ptr 4)
      (%dec $str-len)
      (br $b_start_char)
    )
  )

  (local.set $str 
    (call $str-from-code-points 
      (local.get $char-buffer) 
      (i32.wrap_i64 (local.get $len))
    )
  )
  (call $malloc-free (local.get $char-buffer))
  (return (%alloc-str (local.get $str)))
)

(func $string (param $env i32) (param $args i32) (result i32)
  (local $len i32)
  (local $char-buffer i32)
  (local $ptr i32)
  (local $str i32)

  (if (i32.eqz (call $all-char (local.get $args)))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $len (call $list-len (local.get $args)))
  (local.set $ptr
    (local.tee $char-buffer
      (call $malloc (i32.shl (local.get $len) (i32.const 2)))
    )
  )

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (i32.store (local.get $ptr) (%car (%car-l $args)))

      (%plus-eq $ptr 4)
      (local.set $args (%cdr-l $args))
      (br $b_start)
    )
  )

  (local.set $str (call $str-from-code-points (local.get $char-buffer) (local.get $len)))
  (call $malloc-free (local.get $char-buffer))
  
  (return (%alloc-str (local.get $str)))
)

(func $string-length (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%str-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (%alloc-i32 (call $str-code-point-len (%car-l $arg))))
)

(func $string-ref (param $env i32) (param $args i32) (result i32)
  (local $str i32)
  (local $k i32)
  (local $offset i32)
  (local $code-point i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $str (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $str) (%str-type)))
      (local.set $k (%car (%cdr-l $args)))
      (br_if $b_fail (i32.ne (%get-type $k) (%i64-type)))
      (br_if $b_fail (i64.gt_u (i64.load offset=4 (local.get $k)) (i64.const 0x7fffffff)))
      (local.set $offset (i32.wrap_i64 (i64.load offset=4 (local.get $k))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $code-point (call $str-code-point-at (%car-l $str) (local.get $offset)))
  (if (i32.eq (local.get $code-point) (i32.const -1))
    (then (return (call $argument-error (local.get $args))))
  )

  (return (%alloc-char (local.get $code-point)))
)

(func $string-set! (param $env i32) (param $args i32) (result i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $k i32)
  (local $char i32)
  (local $offset i32)
  (local $code-point i32)
  (local $cp-len i32)
  (local $check-len i32)
  (local $cp-buffer i32)
  (local $new-str-ptr i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 3)))

      (local.set $str (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $str) (%str-type)))
      (local.set $str-ptr (%car-l $str))

      (local.set $k (%car (%cdr-l $args)))
      (br_if $b_fail (i32.ne (%get-type $k) (%i64-type)))
      (br_if $b_fail (i64.gt_u (i64.load offset=4 (local.get $k)) (i64.const 0x7fffffff)))
      (local.set $offset (i32.wrap_i64 (i64.load offset=4 (local.get $k))))
      (local.set $cp-len (call $str-code-point-len (local.get $str-ptr)))
      (br_if $b_fail (i32.ge_u (local.get $offset) (local.get $cp-len)))

      (local.set $char (%car (%cdr (%cdr-l $args))))
      (br_if $b_fail (i32.ne (%get-type $char) (%char-type)))
      (local.set $code-point (%car-l $char))


      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )


  (local.set $cp-buffer (call $malloc (i32.shl (local.get $cp-len) (i32.const 2))))
  (local.set $check-len 
    (call $str-to-code-points  
      (local.get $str-ptr) 
      (local.get $cp-buffer) 
      (local.get $cp-len)
    )
  )
  (if (i32.ne (local.get $check-len) (local.get $cp-len))
    (then unreachable)
  )
  (i32.store 
    (i32.add 
      (local.get $cp-buffer)
      (i32.shl (local.get $offset) (i32.const 2))
    )
    (local.get $code-point)
  )
  (local.set $new-str-ptr (call $str-from-code-points (local.get $cp-buffer) (local.get $cp-len)))
  (call $malloc-free (local.get $cp-buffer))
  (call $malloc-free (local.get $str-ptr))
  (%set-car!-l $str $new-str-ptr)

  (return (local.get $str))
)

(func $string=? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 0) (i32.const 0)))
)

(func $string-ci=? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 0) (i32.const 1)))
)

(func $string>? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 1) (i32.const 0)))
)

(func $string-ci>? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 1) (i32.const 1)))
)

(func $string>=? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 2) (i32.const 0)))
)

(func $string-ci>=? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 2) (i32.const 1)))
)

(func $string<? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 3) (i32.const 0)))
)

(func $string-ci<? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 3) (i32.const 1)))
)

(func $string<=? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 4) (i32.const 0)))
)

(func $string-ci<=? (param $env i32) (param $args i32) (result i32)
  (return (call $string-cmp-impl (local.get $args) (i32.const 4) (i32.const 1)))
)

(func $all-string (param $args i32) (result i32)
  (local $temp i32)
  (local $temp-type i32)

  (block $done 
    (loop $forever
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))
      (local.set $temp-type (%get-type $temp))

      (if (i32.ne (local.get $temp-type) (%str-type)) 
        (then (return (i32.const 0)))
      )

      (local.set $args (%cdr-l $args))
      (br $forever)
    )
  )

  (return (i32.const 1))
)

(func $string-cmp-impl (param $args i32) (param $cmp-op i32) (param $ci i32) (result i32)
  (local $temp i32)
  (local $right-str i32)
  (local $left-str i32)
  (local $cmp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-string (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $temp (%car-l $args))
  (local.set $args (%cdr-l $args))
  (local.set $right-str (%car-l $temp))

  (block $done
    (loop $continue
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))
      (local.set $left-str (local.get $right-str))
      (local.set $right-str (%car-l $temp))

      (block $b_cmp_check
        (block $b_cmp_fail
          (local.set $cmp (call $str-cmp (local.get $left-str) (local.get $right-str) (local.get $ci)))
          (if (i32.eq (local.get $cmp-op) (i32.const 0)) ;; =
            (then
              (br_if $b_cmp_fail (local.get $cmp))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 1)) ;; >
            (then
              (br_if $b_cmp_fail (i32.le_s (local.get $cmp) (i32.const 0)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 2)) ;; >=
            (then
              (br_if $b_cmp_fail (i32.lt_s (local.get $cmp) (i32.const 0)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 3)) ;; <
            (then
              (br_if $b_cmp_fail (i32.ge_s (local.get $cmp) (i32.const 0)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 4)) ;; <=
            (then
              (br_if $b_cmp_fail (i32.gt_s (local.get $cmp) (i32.const 0)))
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

(func $string-upcase (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $str i32)
  (local $cp-buffer i32)
  (local $cp-len i32)
  (local $result i32)
  (local $i i32)
  (local $ptr i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%str-type)))
      (local.set $str (%car-l $arg))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $cp-len (call $str-code-point-len (local.get $str)))
  (local.set $ptr
    (local.tee $cp-buffer 
      (call $malloc (i32.shl (local.get $cp-len) (i32.const 2)))
    )
  )
  (local.set $i 
    (call $str-to-code-points 
      (local.get $str) 
      (local.get $cp-buffer) 
      (local.get $cp-len)
    )
  )

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $i)))

      (i32.store
        (local.get $ptr)
        (call $char-upcase-impl (i32.load (local.get $ptr)))
      )

      (%plus-eq $ptr 4)
      (%dec $i)
      (br $b_start)
    )
  )
  
  (local.set $result (call $str-from-code-points (local.get $cp-buffer) (local.get $cp-len)))
  (call $malloc-free (local.get $cp-buffer))

  (return (%alloc-str (local.get $result)))
)

(func $string-downcase (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $str i32)
  (local $cp-buffer i32)
  (local $cp-len i32)
  (local $result i32)
  (local $i i32)
  (local $ptr i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%str-type)))
      (local.set $str (%car-l $arg))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $cp-len (call $str-code-point-len (local.get $str)))
  (local.set $ptr
    (local.tee $cp-buffer 
      (call $malloc (i32.shl (local.get $cp-len) (i32.const 2)))
    )
  )
  (local.set $i 
    (call $str-to-code-points 
      (local.get $str) 
      (local.get $cp-buffer) 
      (local.get $cp-len)
    )
  )

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $i)))

      (i32.store
        (local.get $ptr)
        (call $char-downcase-impl (i32.load (local.get $ptr)))
      )

      (%plus-eq $ptr 4)
      (%dec $i)
      (br $b_start)
    )
  )
  
  (local.set $result (call $str-from-code-points (local.get $cp-buffer) (local.get $cp-len)))
  (call $malloc-free (local.get $cp-buffer))

  (return (%alloc-str (local.get $result)))
)

(func $string-copy-impl (param $str i32) (param $start i32) (param $end i32) (result i32)
  (local $src-buffer i32)
  (local $dest-str i32)

  (local.set $src-buffer (call $malloc (i32.shl (local.get $end) (i32.const 2))))
  (call $str-to-code-points (local.get $str) (local.get $src-buffer) (local.get $end))
  (local.set $dest-str 
    (call $str-from-code-points
      (i32.add 
        (local.get $src-buffer) 
        (i32.shl (local.get $start) (i32.const 2))
      )
      (i32.sub (local.get $end) (local.get $start))
    )
  )
  (call $malloc-free (local.get $src-buffer))
  (return (local.get $dest-str))
)

(func $substring (param $env i32) (param $args i32) (result i32)
  (local $str i32)
  (local $start i32)
  (local $end i32)
  (local $str-len i32)
  (local $temp64 i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 3)))

      (local.set $str (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $str) (%str-type)))
      (local.set $str (%car-l $str))

      (local.set $str-len (call $str-code-point-len (local.get $str)))

      (local.set $start (%car (%cdr-l $args)))
      (br_if $b_fail (i32.ne (%get-type $start) (%i64-type)))
      (local.set $temp64 (i64.load offset=4 (local.get $start)))
      (br_if $b_fail (i64.ge_u (local.get $temp64) (i64.extend_i32_u (local.get $str-len))))
      (local.set $start (i32.wrap_i64 (local.get $temp64)))


      (local.set $end (%car (%cdr (%cdr-l $args))))
      (br_if $b_fail (i32.ne (%get-type $end) (%i64-type)))
      (local.set $temp64 (i64.load offset=4 (local.get $end)))
      (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.extend_i32_u (local.get $str-len))))
      (local.set $end (i32.wrap_i64 (local.get $temp64)))
      (br_if $b_fail (i32.le_u (local.get $end) (local.get $start)))

      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return 
    (%alloc-str 
      (call $string-copy-impl 
        (local.get $str) 
        (local.get $start) 
        (local.get $end)
      )
    )
  )
)

(func $string-copy (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $str i32)
  (local $start i32)
  (local $end i32)
  (local $str-len i32)
  (local $temp64 i64)


  (block $b_check
    (block $b_two_arg
      (block $b_one_arg
        (block $b_fail
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $b_fail (i32.eqz (local.get $num-args)))

          (local.set $str (%car-l $args))
          (br_if $b_fail (i32.ne (%get-type $str) (%str-type)))
          (local.set $str (%car-l $str))
          (local.set $str-len (call $str-code-point-len (local.get $str)))

          (br_if $b_one_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (local.set $start (%car (%cdr-l $args)))
          (br_if $b_fail (i32.ne (%get-type $start) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.ge_u (local.get $temp64) (i64.extend_i32_u (local.get $str-len))))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))

          (br_if $b_two_arg (i32.eq (local.get $num-args) (i32.const 2)))

          (local.set $end (%car (%cdr (%cdr-l $args))))
          (br_if $b_fail (i32.ne (%get-type $end) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.extend_i32_u (local.get $str-len))))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.le_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3)))
        )
        (return (call $argument-error (local.get $args)))
      )
      (local.set $start (i32.const 0))
    )
    (local.set $end (local.get $str-len))
  )

  (return 
    (%alloc-str 
      (call $string-copy-impl 
        (local.get $str) 
        (local.get $start) 
        (local.get $end)
      )
    )
  )
)

(func $string-list (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $str i32)
  (local $start i32)
  (local $end i32)
  (local $str-len i32)
  (local $temp64 i64)
  (local $buffer i32)
  (local $ptr i32)
  (local $char i32)
  (local $head i32)
  (local $tail i32)
  (local $temp i32)


  (block $b_check
    (block $b_two_arg
      (block $b_one_arg
        (block $b_fail
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $b_fail (i32.eqz (local.get $num-args)))

          (local.set $str (%car-l $args))
          (br_if $b_fail (i32.ne (%get-type $str) (%str-type)))
          (local.set $str (%car-l $str))
          (local.set $str-len (call $str-code-point-len (local.get $str)))

          (br_if $b_one_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (local.set $start (%car (%cdr-l $args)))
          (br_if $b_fail (i32.ne (%get-type $start) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.ge_u (local.get $temp64) (i64.extend_i32_u (local.get $str-len))))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))

          (br_if $b_two_arg (i32.eq (local.get $num-args) (i32.const 2)))

          (local.set $end (%car (%cdr (%cdr-l $args))))
          (br_if $b_fail (i32.ne (%get-type $end) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.extend_i32_u (local.get $str-len))))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.le_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3)))
        )
        (return (call $argument-error (local.get $args)))
      )
      (local.set $start (i32.const 0))
    )
    (local.set $end (local.get $str-len))
  )


  (call $str-to-code-points 
    (local.get $str) 
    (local.tee $buffer 
      (call $malloc (i32.shl (local.get $end) (i32.const 2)))
    )
    (local.get $end)
  )

  (local.set $ptr 
    (i32.add 
      (local.get $buffer)
      (i32.shl (local.get $start) (i32.const 2))
    )
  )

  (local.set $head (global.get $g-nil))
  (local.set $tail (i32.const 0))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (local.get $start) (local.get $end)))

      (local.set $char (%alloc-char (i32.load (local.get $ptr))))
      (local.set $temp (%alloc-cons (local.get $char) (global.get $g-nil)))

      (if (local.get $tail)
        (then (%set-cdr!-l $tail $temp))
        (else (local.set $head (local.get $temp)))
      )
      (local.set $tail (local.get $temp))

      (%plus-eq $ptr 4)
      (%inc $start)
      (br $b_start)
    )
  )

  (call $malloc-free (local.get $buffer))

  (return (local.get $head))
)