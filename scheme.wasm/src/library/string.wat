
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
      (call $malloc (%word-size-l $str-len))
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
      (call $malloc (%word-size-l $len))
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
      (br_if $b_fail (i32.and (%get-flags $str) (%immutable-flag)))
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


  (local.set $cp-buffer (call $malloc (%word-size-l $cp-len)))
  (local.set $check-len
    (call $str-to-code-points
      (local.get $str-ptr)
      (local.get $cp-buffer)
      (local.get $cp-len)
    )
  )
  (if (i32.ne (local.get $check-len) (local.get $cp-len))
    (then (unreachable))
  )
  (i32.store
    (i32.add (local.get $cp-buffer) (%word-size-l $offset))
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
      (call $malloc (%word-size-l $cp-len))
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

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%str-type)))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-str (call $string-downcase-impl (%car-l $arg)))))

(func $string-downcase-impl (param $str i32) (result i32)
  (local $cp-buffer i32)
  (local $cp-len i32)
  (local $result i32)
  (local $i i32)
  (local $ptr i32)

  (local.set $cp-len (call $str-code-point-len (local.get $str)))
  (local.set $ptr
    (local.tee $cp-buffer
      (call $malloc (%word-size-l $cp-len))))

  (local.set $i
    (call $str-to-code-points
      (local.get $str)
      (local.get $cp-buffer)
      (local.get $cp-len)))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $i)))

      (i32.store
        (local.get $ptr)
        (call $char-downcase-impl (i32.load (local.get $ptr))))

      (%plus-eq $ptr 4)
      (%dec $i)
      (br $b_start)))

  (local.set $result (call $str-from-code-points (local.get $cp-buffer) (local.get $cp-len)))
  (call $malloc-free (local.get $cp-buffer))
  (return (local.get $result)))

(func $string-copy-impl (param $str i32) (param $start i32) (param $end i32) (result i32)
  (local $src-buffer i32)
  (local $dest-str i32)

  (local.set $src-buffer (call $malloc (%word-size-l $end)))
  (drop (call $str-to-code-points
      (local.get $str)
      (local.get $src-buffer)
      (local.get $end)))
  (local.set $dest-str (call $str-from-code-points
      (i32.add (local.get $src-buffer) (%word-size-l $start))
      (i32.sub (local.get $end) (local.get $start))))
  (call $malloc-free (local.get $src-buffer))
  (return (local.get $dest-str)))

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
      (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

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
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

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

(func $string->list (param $env i32) (param $args i32) (result i32)
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


  (block $b_check (block $b_two_arg (block $b_one_arg (block $b_fail
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
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3))))

        (return (call $argument-error (local.get $args))))

      (local.set $start (i32.const 0)))

    (local.set $end (local.get $str-len)))

  (if (i32.eq (local.get $start) (local.get $end)) (then
      (return (global.get $g-nil))))

  (drop (call $str-to-code-points
      (local.get $str)
      (local.tee $buffer (call $malloc (%word-size-l $end)))
      (local.get $end)))

  (local.set $ptr (i32.add (local.get $buffer) (%word-size-l $start)))

  (local.set $head (global.get $g-nil))
  (local.set $tail (i32.const 0))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (local.get $start) (local.get $end)))

      (local.set $char (%alloc-char (i32.load (local.get $ptr))))
      (local.set $temp (%alloc-cons (local.get $char) (global.get $g-nil)))

      (if (local.get $tail)
        (then (%set-cdr!-l $tail $temp))
        (else (local.set $head (local.get $temp))))

      (local.set $tail (local.get $temp))

      (%plus-eq $ptr 4)
      (%inc $start)
      (br $b_start)))

  (call $malloc-free (local.get $buffer))

  (return (local.get $head)))

(func $list->string (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $arg-type i32)
  (local $cp-len i32)
  (local $buffer i32)
  (local $ptr i32)
  (local $str i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (local.set $arg-type (%get-type $arg))
      (if (i32.ne (local.get $arg-type) (%cons-type))
        (then
          (br_if $b_fail (i32.ne (local.get $arg-type) (%nil-type)))))
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $arg))))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $arg))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $cp-len (call $list-len (local.get $arg)))
  (local.set $ptr
    (local.tee $buffer
      (call $malloc (%word-size-l $cp-len))))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $arg) (%nil-type)))

      (i32.store
        (local.get $ptr)
        (%car (%car-l $arg)))

      (%plus-eq $ptr 4)
      (local.set $arg (%cdr-l $arg))
      (br $b_start)))

  (local.set $str (%alloc-str (call $str-from-code-points
        (local.get $buffer)
        (local.get $cp-len))))
  (call $malloc-free (local.get $buffer))
  (return (local.get $str)))

(func $string-append (param $env i32) (param $args i32) (result i32)
  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.eqz (call $all-string (local.get $args))))
      (br $b_check))
    (return (call $argument-error (local.get $args))))

  (return (call $string-append-impl (local.get $args))))

(func $string-append-impl (param $args i32) (result i32)
  (local $ptr i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $str-len i32)
  (local $buffer i32)
  (local $total i32)
  (local $dest i32)

  (local.set $ptr (local.get $args))
  (local.set $total (i32.const 0))

  ;; get the total byte length of the string
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $ptr) (%nil-type)))

      (local.set $str (%car-l $ptr))
      (local.set $str-ptr (%car-l $str))
      (local.set $total
        (i32.add
          (local.get $total)
          (i32.load (local.get $str-ptr))))

      (local.set $ptr (%cdr-l $ptr))
      (br $b_start)))

  ;; add 4 bytes to store the length
  (local.set $buffer (call $malloc (i32.add (local.get $total) (i32.const 4))))

  (local.set $ptr (local.get $args))
  (i32.store (local.get $buffer) (local.get $total))
  (local.set $dest (i32.add (local.get $buffer) (i32.const 4)))

  ;; copy each byte array
  (block $b_end_copy
    (loop $b_start_copy
      (br_if $b_end_copy (i32.eq (%get-type $ptr) (%nil-type)))

      (local.set $str (%car-l $ptr))
      (local.set $str-ptr (%car-l $str))
      (local.set $str-len (i32.load (local.get $str-ptr)))

      (call $memcpy
        (local.get $dest)
        (i32.add (local.get $str-ptr) (i32.const 4))
        (local.get $str-len))

      (local.set $dest (i32.add (local.get $dest) (local.get $str-len)))
      (local.set $ptr (%cdr-l $ptr))
      (br $b_start_copy)))

  (return (%alloc-str (local.get $buffer))))

;; (string-copy! to at from [start [end]])
(func $string-copy! (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $to i32)
  (local $to-str i32)
  (local $to-len i32)
  (local $at i32)
  (local $from i32)
  (local $from-str i32)
  (local $from-len i32)
  (local $temp64 i64)
  (local $start i32)
  (local $end i32)
  (local $src-buffer i32)
  (local $dest-buffer i32)
  (local $src-ptr i32)
  (local $dest-ptr i32)

  (block $b_check (block $b_4_args (block $b_3_args (block $b_fail
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 3)))

          (local.set $to (%car-l $args))
          (local.set $args (%cdr-l $args))
          (%chk-type $b_fail $to %str-type)
          (br_if $b_fail (i32.and (%get-flags $to) (%immutable-flag)))

          (local.set $at (%car-l $args))
          (local.set $args (%cdr-l $args))
          (br_if $b_fail (i32.ne (%get-type $at) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $at)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $at (i32.wrap_i64 (local.get $temp64)))

          (local.set $to-str (%car-l $to))
          (local.set $to-len (call $str-code-point-len (local.get $to-str)))

          (br_if $b_fail (i32.ge_u (local.get $at) (local.get $to-len)))

          (local.set $from (%car-l $args))
          (local.set $args (%cdr-l $args))
          (br_if $b_fail (i32.ne (%get-type $from) (%str-type)))

          (local.set $from-str (%car-l $from))
          (local.set $from-len (call $str-code-point-len (local.get $from-str)))

          (br_if $b_3_args (i32.lt_u (local.get $num-args) (i32.const 4)))

          (local.set $start (%car-l $args))
          (local.set $args (%cdr-l $args))
          (br_if $b_fail (i32.ne (%get-type $start) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))

          (br_if $b_fail (i32.ge_u (local.get $start) (local.get $from-len)))

          (br_if $b_4_args (i32.lt_u (local.get $num-args) (i32.const 5)))

          (local.set $end (%car-l $args))
          (br_if $b_fail (i32.ne (%get-type $end) (%i64-type)))
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))

          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))
          (br_if $b_fail (i32.gt_u (local.get $end) (local.get $from-len)))
          (br_if $b_fail (i32.gt_u
            (i32.sub (local.get $end) (local.get $start))
            (i32.sub (local.get $to-len) (local.get $at))))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 5))))
        (return (call $argument-error (local.get $args))))

      (local.set $start (i32.const 0)))

    (local.set $end (local.get $from-len))
    (if (i32.gt_u
        (i32.sub (local.get $end) (local.get $start))
        (i32.sub (local.get $to-len) (local.get $at)))
      (then
        (local.set $end (i32.sub
              (i32.add (local.get $start) (local.get $to-len))
              (local.get $at))))))

  (drop (call $str-to-code-points
      (local.get $to-str)
      (local.tee $dest-buffer (call $malloc (%word-size-l $to-len)))
      (local.get $to-len)))
  (drop (call $str-to-code-points
      (local.get $from-str)
      (local.tee $src-buffer (call $malloc (%word-size-l $end)))
      (local.get $end)))

  (local.set $dest-ptr (i32.add (local.get $dest-buffer) (%word-size-l $at)))
  (local.set $src-ptr (i32.add (local.get $src-buffer) (%word-size-l $start)))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.ge_u (local.get $start) (local.get $end)))

      (i32.store (local.get $dest-ptr) (i32.load (local.get $src-ptr)))

      (%plus-eq $dest-ptr 4)
      (%plus-eq $src-ptr 4)
      (%inc $start)
      (br $b_start)))

  (call $malloc-free (local.get $src-buffer))
  (call $malloc-free (local.get $to-str))
  (local.set $to-str (call $str-from-code-points (local.get $dest-buffer) (local.get $to-len)))
  (call $malloc-free (local.get $dest-buffer))

  (%set-car!-l $to $to-str)
  (return (local.get $to)))

;; (string-fill! string fill [start [end]])
(func $string-fill! (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $num-args i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $str-len i32)
  (local $fill i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $buffer i32)
  (local $ptr i32)

  (block $b_check (block $b_3_args (block $b_2_args (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))

          (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 2)))

          (%pop-l $str $temp)
          (%chk-type $b_fail $str %str-type)
          (br_if $b_fail (i32.and (%get-flags $str) (%immutable-flag)))
          (local.set $str-ptr (%car-l $str))
          (local.set $str-len (call $str-code-point-len (local.get $str-ptr)))

          (%pop-l $fill $temp)
          (%chk-type $b_fail $fill %char-type)
          (local.set $fill (%car-l $fill))

          (br_if $b_2_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $start) (local.get $str-len)))

          (br_if $b_3_args (i32.eq (local.get $num-args) (i32.const 3)))

          (%pop-l $end $temp)
          (%chk-type $b_fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $end) (local.get $str-len)))
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 4))))

        (return (call $argument-error (local.get $args))))

      (local.set $start (i32.const 0)))

    (local.set $end (local.get $str-len)))

  (drop (call $str-to-code-points
      (local.get $str-ptr)
      (local.tee $buffer (call $malloc (%word-size-l $str-len)))
      (local.get $str-len)))

  (local.set $ptr (i32.add (local.get $buffer) (%word-size-l $start)))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.ge_u (local.get $start) (local.get $end)))

      (i32.store (local.get $ptr) (local.get $fill))

      (%inc $start)
      (%plus-eq $ptr 4)
      (br $b_start)))

  (call $malloc-free (local.get $str-ptr))
  (local.set $str-ptr
    (call $str-from-code-points
      (local.get $buffer)
      (local.get $str-len)))
  (call $malloc-free (local.get $buffer))

  (%set-car!-l $str $str-ptr)
  (return (local.get $str)))
