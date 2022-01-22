;; (vector? obj)
(func $vector? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then
      (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $obj) (%vector-type)))))

;; (make-vector k [fill])
(func $make-vector (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $k i32)
  (local $temp64 i64)
  (local $fill i32)
  (local $buffer i32)
  (local $vec i32)

  (block $b_check
    (block $b_1_arg
      (block $b_fail
        (local.set $num-args (call $list-len (local.get $args)))
        (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 1)))

        (local.set $k (%car-l $args))
        (%chk-type $b_fail $k %i64-type)

        (local.set $temp64 (i64.load offset=4 (local.get $k)))
        (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
        (local.set $k (i32.wrap_i64 (local.get $temp64)))

        (br_if $b_1_arg (i32.eq (local.get $num-args) (i32.const 1)))

        (local.set $fill (%car (%cdr-l $args)))

        (br_if $b_check (i32.eq (local.get $num-args) (i32.const 2))))

      (return (call $argument-error (local.get $args))))

    (local.set $fill (global.get $g-nil)))

  (local.set $buffer (call $malloc (%word-size-l $k)))

  (local.set $vec
    (call $heap-alloc
      (global.get $g-heap)
      (%vector-type)
      (local.get $buffer)
      (local.get $k)))

  (loop $forever
    (if (i32.eqz (local.get $k))
      (then (return (local.get $vec))))

    (i32.store (local.get $buffer) (local.get $fill))

    (%plus-eq $buffer 4)
    (%dec $k)
    (br $forever))

  (unreachable))

;; (vector obj ...)
(func $vector (param $env i32) (param $args i32) (result i32)
  (return (call $make-vector-internal (local.get $args))))

;; (list->vector list)
(func $list->vector (param $env i32) (param $args i32) (result i32)
  (local $list i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $list (%car-l $args))
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $list))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $make-vector-internal (local.get $list))))

;; (vector-length vector)
(func $vector-length (param $env i32) (param $args i32) (result i32)
  (local $vector i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $vector (%car-l $args))
      (%chk-type $b_fail $vector %vector-type)
      (br $b_check))
    (return (call $argument-error (local.get $args))))

  (return (%alloc-i32 (%cdr-l $vector))))

;; (vector-ref vector k)
(func $vector-ref (param $env i32) (param $args i32) (result i32)
  (local $vector i32)
  (local $ptr i32)
  (local $temp64 i64)
  (local $k i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $vector (%car-l $args))
      (%chk-type $b_fail $vector %vector-type)

      (local.set $k (%car (%cdr-l $args)))
      (%chk-type $b_fail $k %i64-type)
      (local.set $temp64 (i64.load offset=4 (local.get $k)))
      (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
      (local.set $k (i32.wrap_i64 (local.get $temp64)))

      (br_if $b_fail (i32.ge_u (local.get $k) (%cdr-l $vector)))

      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $ptr (i32.add (%car-l $vector) (%word-size-l $k)))

  (return (i32.load (local.get $ptr))))

;; (vector-set! vector k obj)
(func $vector-set! (param $env i32) (param $args i32) (result i32)
  (local $vector i32)
  (local $ptr i32)
  (local $temp64 i64)
  (local $k i32)
  (local $obj i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.ne (call $list-len (local.get $temp)) (i32.const 3)))
      (%pop-l $vector $temp)
      (%chk-type $b_fail $vector %vector-type)

      (%pop-l $k $temp)
      (%chk-type $b_fail $k %i64-type)
      (local.set $temp64 (i64.load offset=4 (local.get $k)))
      (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
      (local.set $k (i32.wrap_i64 (local.get $temp64)))

      (br_if $b_fail (i32.ge_u (local.get $k) (%cdr-l $vector)))

      (local.set $obj (%car-l $temp))

      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $ptr (i32.add (%car-l $vector) (%word-size-l $k)))

  (i32.store (local.get $ptr) (local.get $obj))
  (return (local.get $vector)))

;; (vector-copy vector [start [end]])
(func $vector-copy (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $vector i32)
  (local $start i32)
  (local $end i32)
  (local $vector-len i32)
  (local $temp64 i64)
  (local $temp i32)
  (local $buffer i32)
  (local $copy-len i32)
  (local $buffer-len i32)


  (block $b_check
    (block $b_two_arg
      (block $b_one_arg
        (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))
          (br_if $b_fail (i32.eqz (local.get $num-args)))

          (%pop-l $vector $temp)
          (%chk-type $b_fail $vector %vector-type)
          (local.set $vector-len (%cdr-l $vector))

          (br_if $b_one_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.ge_u (local.get $temp64) (i64.extend_i32_u (local.get $vector-len))))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))

          (br_if $b_two_arg (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $b_fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.extend_i32_u (local.get $vector-len))))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3)))
        )
        (return (call $argument-error (local.get $args))))

      (local.set $start (i32.const 0)))

    (local.set $end (local.get $vector-len)))

  (local.set $copy-len (i32.sub (local.get $end) (local.get $start)))
  (local.set $buffer-len (%word-size-l $copy-len))
  (local.set $buffer (call $malloc (local.get $buffer-len)))
  (call $memcpy
    (local.get $buffer)
    (i32.add (%car-l $vector) (%word-size-l $start))
    (local.get $buffer-len))

  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%vector-type)
      (local.get $buffer)
      (local.get $copy-len))))

;; (vector-copy! to at from [start [end]])
(func $vector-copy! (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $to i32)
  (local $to-ptr i32)
  (local $to-len i32)
  (local $at i32)
  (local $from i32)
  (local $from-ptr i32)
  (local $from-len i32)
  (local $temp64 i64)
  (local $start i32)
  (local $end i32)
  (local $src-ptr i32)
  (local $dest-ptr i32)
  (local $temp i32)

  (block $b_check
    (block $b_4_args
      (block $b_3_args
        (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))
          (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 3)))

          (%pop-l $to $temp)
          (%chk-type $b_fail $to %vector-type)

          (%pop-l $at $temp)
          (%chk-type $b_fail $at %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $at)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $at (i32.wrap_i64 (local.get $temp64)))

          (local.set $to-ptr (%car-l $to))
          (local.set $to-len (%cdr-l $to))

          (br_if $b_fail (i32.ge_u (local.get $at) (local.get $to-len)))

          (%pop-l $from $temp)
          (%chk-type $b_fail $from %vector-type)

          (local.set $from-ptr (%car-l $from))
          (local.set $from-len (%cdr-l $from))

          (br_if $b_3_args (i32.lt_u (local.get $num-args) (i32.const 4)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))

          (br_if $b_fail (i32.ge_u (local.get $start) (local.get $from-len)))

          (br_if $b_4_args (i32.lt_u (local.get $num-args) (i32.const 5)))

          (local.set $end (%car-l $temp))
          (%chk-type $b_fail $end %i64-type)
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
    (if
      (i32.gt_u
        (i32.sub (local.get $end) (local.get $start))
        (i32.sub (local.get $to-len) (local.get $at)))
      (then
        (local.set $end
          (i32.sub
            (i32.add (local.get $start) (local.get $to-len))
              (local.get $at))))))


  (local.set $dest-ptr (i32.add (local.get $to-ptr) (%word-size-l $at)))
  (local.set $src-ptr (i32.add (local.get $from-ptr) (%word-size-l $start)))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.ge_u (local.get $start) (local.get $end)))

      (i32.store
        (local.get $dest-ptr)
        (i32.load (local.get $src-ptr)))

      (%plus-eq $dest-ptr 4)
      (%plus-eq $src-ptr 4)
      (%inc $start)
      (br $b_start)))

  (return (local.get $to)))

;; (vector->string vector [start [end]])
(func $vector->string (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $num-args i32)
  (local $vector i32)
  (local $vector-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $buffer i32)
  (local $src i32)
  (local $dest i32)
  (local $cp-len i32)
  (local $char i32)
  (local $str i32)

  (block $b_check
    (block $b_2_args
      (block $b_1_arg
        (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))

          (br_if $b_fail (i32.eqz (local.get $num-args)))

          (%pop-l $vector $temp)
          (%chk-type $b_fail $vector %vector-type)
          (local.set $vector-len (%cdr-l $vector))

          (br_if $b_1_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7FFFFFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.ge_u (local.get $start) (local.get $vector-len)))

          (br_if $b_2_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $b_fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7FFFFFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $end) (local.get $vector-len)))
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3))))

        (return (call $argument-error (local.get $args)))

      (local.set $start (i32.const 0))))

    (local.set $end (local.get $vector-len)))

  (local.set $cp-len (i32.sub (local.get $end) (local.get $start)))
  (local.set $dest
    (local.tee $buffer
      (call $malloc (%word-size-l $cp-len))))

  (local.set $src (i32.add (%car-l $vector) (%word-size-l $start)))

  (loop $forever
    (if (i32.eq (local.get $start) (local.get $end))
      (then
        (local.set $str
          (%alloc-str (call $str-from-code-points (local.get $buffer) (local.get $cp-len))))
        (call $malloc-free (local.get $buffer))
        (return (local.get $str))))


    (local.set $char (i32.load (local.get $src)))
    (if (i32.ne (%get-type $char) (%char-type))
      (then
        (call $malloc-free (local.get $buffer))
        (return (call $argument-error (local.get $args)))))

    (i32.store (local.get $dest) (%car-l $char))

    (%plus-eq $src 4)
    (%plus-eq $dest 4)
    (%inc $start)
    (br $forever))

 (unreachable))

;; (string->vector string [start [end]])
(func $string->vector (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $num-args i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $str-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $buffer i32)
  (local $src i32)
  (local $dest i32)
  (local $vector i32)
  (local $vec-ptr i32)
  (local $vec-len i32)

  (block $b_check (block $b_2_args (block $b_1_arg (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))

          (br_if $b_fail (i32.eqz (local.get $num-args)))

          (%pop-l $str $temp)
          (%chk-type $b_fail $str %str-type)
          (local.set $str-ptr (%car-l $str))
          (local.set $str-len (call $str-code-point-len (local.get $str-ptr)))

          (br_if $b_1_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7FFFFFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.ge_u (local.get $start) (local.get $str-len)))

          (br_if $b_2_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $b_fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7FFFFFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $end) (local.get $str-len)))
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3))))

        (return (call $argument-error (local.get $args)))

      (local.set $start (i32.const 0))))

    (local.set $end (local.get $str-len)))

  (drop (call $str-to-code-points
      (local.get $str-ptr)
      (local.tee $buffer (call $malloc (%word-size-l $end)))
      (local.get $end)))

  (local.set $src (i32.add (local.get $buffer) (%word-size-l $start)))

  (local.set $vec-len (i32.sub (local.get $end) (local.get $start)))
  (local.set $dest
    (local.tee $vec-ptr
      (call $malloc (%word-size-l $vec-len))))

  (loop $forever
    (if (i32.eq (local.get $start) (local.get $end)) (then
        (local.set $vector
          (call $heap-alloc
            (global.get $g-heap)
            (%vector-type)
            (local.get $vec-ptr)
            (local.get $vec-len)))
        (call $malloc-free (local.get $buffer))
        (return (local.get $vector))))

    (i32.store
      (local.get $dest)
      (%alloc-char (i32.load (local.get $src))))

    (%plus-eq $src 4)
    (%plus-eq $dest 4)
    (%inc $start)
    (br $forever))

 (unreachable))

;; (vector->list vector [start [end]])
(func $vector->list (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $num-args i32)
  (local $vector i32)
  (local $vector-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $src i32)
  (local $head i32)
  (local $tail i32)

  (block $b_check
    (block $b_2_args
      (block $b_1_arg
        (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))

          (br_if $b_fail (i32.eqz (local.get $num-args)))

          (%pop-l $vector $temp)
          (%chk-type $b_fail $vector %vector-type)
          (local.set $vector-len (%cdr-l $vector))

          (br_if $b_1_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7FFFFFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.ge_u (local.get $start) (local.get $vector-len)))

          (br_if $b_2_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $b_fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7FFFFFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $end) (local.get $vector-len)))
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3))))

        (return (call $argument-error (local.get $args)))

      (local.set $start (i32.const 0))))

    (local.set $end (local.get $vector-len)))

  (local.set $src (i32.add (%car-l $vector) (%word-size-l $start)))


  (local.set $tail (i32.const 0))
  (local.set $head (global.get $g-nil))

  (loop $forever
    (if (i32.eq (local.get $start) (local.get $end))
      (then (return (local.get $head))))

    (local.set $temp
      (%alloc-cons
        (i32.load (local.get $src))
        (global.get $g-nil)))

    (if (local.get $tail)
      (then (%set-cdr!-l $tail $temp))
      (else
        (local.set $head (local.get $temp))))

    (local.set $tail (local.get $temp))

    (%plus-eq $src 4)
    (%inc $start)
    (br $forever))

 (unreachable))

(func $all-vector (param $args i32) (result i32)
  (local $temp i32)
  (local $temp-type i32)

  (block $done
    (loop $forever
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))
      (local.set $temp-type (%get-type $temp))

      (if (i32.ne (local.get $temp-type) (%vector-type))
        (then (return (i32.const 0)))
      )

      (local.set $args (%cdr-l $args))
      (br $forever)
    )
  )

  (return (i32.const 1))
)

;; (vector-append vector ...)
(func $vector-append (param $env i32) (param $args i32) (result i32)
  (local $length i32)
  (local $curr i32)
  (local $curr-vec i32)
  (local $curr-byte-len i32)
  (local $vec-ptr i32)
  (local $ptr i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.eqz (call $list-len (local.get $args))))
      (br_if $b_fail (i32.eqz (call $all-vector (local.get $args))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  ;; accumulate the length
  (local.set $length (i32.const 0))
  (local.set $curr (local.get $args))
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $curr) (%nil-type)))

      (local.set $length
        (i32.add
          (local.get $length)
          (%cdr (%car-l $curr))))

      (local.set $curr (%cdr-l $curr))
      (br $b_start)))

  (local.set $ptr
    (local.tee $vec-ptr
      (call $malloc (%word-size-l $length))))

  ;; copy each vector contents over
  (local.set $curr (local.get $args))
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $curr) (%nil-type)))

      (local.set $curr-vec (%car-l $curr))
      (local.set $curr-byte-len (%word-size-l $curr-vec))

      (call $memcpy
        (local.get $ptr)
        (%car-l $curr-vec)
        (local.get $curr-byte-len)
      )

      (local.set $ptr (i32.add (local.get $ptr) (local.get $curr-byte-len)))

      (local.set $curr (%cdr-l $curr))
      (br $b_start)))

  (return (call $heap-alloc (global.get $g-heap) (%vector-type) (local.get $vec-ptr) (local.get $length))))

;; (vector-fill! vector fill [start [end]])
(func $vector-fill! (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $num-args i32)
  (local $vector i32)
  (local $vec-ptr i32)
  (local $vec-len i32)
  (local $fill i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $buffer i32)
  (local $ptr i32)

  (block $b_check
    (block $b_3_args
      (block $b_2_args
        (block $b_fail
          (local.set $temp (local.get $args))
          (local.set $num-args (call $list-len (local.get $temp)))

          (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 2)))

          (%pop-l $vector $temp)
          (%chk-type $b_fail $vector %vector-type)
          (local.set $vec-ptr (%car-l $vector))
          (local.set $vec-len (%cdr-l $vector))

          (%pop-l $fill $temp)

          (br_if $b_2_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $start $temp)
          (%chk-type $b_fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $start) (local.get $vec-len)))

          (br_if $b_3_args (i32.eq (local.get $num-args) (i32.const 3)))

          (%pop-l $end $temp)
          (%chk-type $b_fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $b_fail (i64.gt_u (local.get $temp64) (i64.const 0x7fffffff)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $b_fail (i32.gt_u (local.get $end) (local.get $vec-len)))
          (br_if $b_fail (i32.lt_u (local.get $end) (local.get $start)))

          (br_if $b_check (i32.eq (local.get $num-args) (i32.const 4)))
        )
        (return (call $argument-error (local.get $args))))

      (local.set $start (i32.const 0)))

    (local.set $end (local.get $vec-len)))

  (local.set $ptr (i32.add (local.get $vec-ptr) (%word-size-l $start)))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.ge_u (local.get $start) (local.get $end)))

      (i32.store (local.get $ptr) (local.get $fill))

      (%inc $start)
      (%plus-eq $ptr 4)
      (br $b_start)))

 (return (local.get $vector)))
