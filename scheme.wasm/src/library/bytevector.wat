;; (bytevector? <obj>)
(func $bytevector? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $obj) (%bytevector-type)))))

;; (make-bytevector <k> [<byte>])
(func $make-bytevector (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $k i32)
  (local $temp64 i64)
  (local $byte i32)
  (local $ptr i32)

  (block $check (block $fail (block $one_arg
        (local.set $num-args (call $list-len (local.get $args)))
        (br_if $fail (i32.eqz (local.get $num-args)))

        (local.set $temp (local.get $args))
        (%pop-l $k $temp)
        (%chk-type $fail $k %i64-type)
        (local.set $temp64 (i64.load offset=4 (local.get $k)))
        (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
        (local.set $k (i32.wrap_i64 (local.get $temp64)))

        (br_if $one_arg (i32.eq (local.get $num-args) (i32.const 1)))
        (br_if $fail (i32.gt_u (local.get $num-args) (i32.const 2)))

        (%pop-l $byte $temp)
        (%chk-type $fail $byte %i64-type)
        (local.set $temp64 (i64.load offset=4 (local.get $byte)))
        (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFF)))
        (local.set $byte (i32.wrap_i64 (local.get $temp64)))

        (br $check))

      (local.set $byte (i32.const 0))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $ptr (call $malloc (local.get $k)))
  (call $memset (local.get $ptr) (local.get $byte) (local.get $k))

  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%bytevector-type)
      (local.get $ptr)
      (local.get $k))))

;; (bytevector <byte> ...)
(func $bytevector (param $env i32) (param $args i32) (result i32)
  (local $ptr i32)

  (local.set $ptr (call $make-byte-vector-internal (local.get $args)))
  (if (i32.ne (%get-type $ptr) (%bytevector-type)) (then
      (return (call $argument-error (local.get $args)))))

  (return (local.get $ptr)))

;; (bytevector-length <bytevector>)
(func $bytevector-length (param $env i32) (param $args i32) (result i32)
  (local $ptr i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $ptr (%car-l $args))
      (%chk-type $fail $ptr %bytevector-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-i32 (%cdr-l $ptr))))

;; (bytevector-u8-ref <bytevector> <k>)
(func $bytevector-u8-ref (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $vector i32)
  (local $k i32)
  (local $temp64 i64)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))

      (%pop-l $vector $temp)
      (%chk-type $fail $vector %bytevector-type)

      (%pop-l $k $temp)
      (%chk-type $fail $k %i64-type)
      (local.set $temp64 (i64.load offset=4 (local.get $k)))
      (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
      (local.set $k (i32.wrap_i64 (local.get $temp64)))

      (br_if $fail (i32.ge_u (local.get $k) (%cdr-l $vector)))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-i32 (i32.load8_u (i32.add (%car-l $vector) (local.get $k))))))

;; (bytevector-u8-set! <bytevector> <k> <byte>)
(func $bytevector-u8-set! (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $vector i32)
  (local $k i32)
  (local $byte i32)
  (local $temp64 i64)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 3)))
      (local.set $temp (local.get $args))

      (%pop-l $vector $temp)
      (%chk-type $fail $vector %bytevector-type)

      (%pop-l $k $temp)
      (%chk-type $fail $k %i64-type)
      (local.set $temp64 (i64.load offset=4 (local.get $k)))
      (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
      (local.set $k (i32.wrap_i64 (local.get $temp64)))
      (br_if $fail (i32.ge_u (local.get $k) (%cdr-l $vector)))

      (%pop-l $byte $temp)
      (%chk-type $fail $byte %i64-type)
      (local.set $temp64 (i64.load offset=4 (local.get $byte)))
      (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFF)))
      (local.set $byte (i32.wrap_i64 (local.get $temp64)))

      (br $check))

    (return (call $argument-error (local.get $args))))

  (i32.store8
    (i32.add (%car-l $vector) (local.get $k))
    (local.get $byte))

  (return (global.get $g-nil)))

;; (bytevector-copy <bytevector> [<start> [<end>]])
(func $bytevector-copy (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $vector i32)
  (local $vector-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $dest-len i32)
  (local $ptr i32)

  (block $check (block $fail (block $two_args (block $one_arg
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $fail (i32.eqz (local.get $num-args)))
          (local.set $temp (local.get $args))

          (%pop-l $vector $temp)
          (%chk-type $fail $vector %bytevector-type)
          (local.set $vector-len (%cdr-l $vector))

          (br_if $one_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.ge_u (local.get $start) (local.get $vector-len)))

          (br_if $two_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.lt_u (local.get $end) (local.get $start)))
          (br_if $fail (i32.gt_u (local.get $end) (local.get $vector-len)))

          (br_if $fail (i32.gt_u (local.get $num-args) (i32.const 3)))
          (br $check))

        ;; one arg
        (local.set $start (i32.const 0)))
        ;; fall through

      ;; two args
      (local.set $end (local.get $vector-len))
      (br $check))

    ;; fail
    (return (call $argument-error (local.get $args))))

  (local.set $dest-len (i32.sub (local.get $end) (local.get $start)))
  (local.set $ptr (call $malloc (local.get $dest-len)))

  (if (local.get $dest-len) (then
      (call $memcpy
        (local.get $ptr)
        (i32.add (%car-l $vector) (local.get $start))
        (local.get $dest-len))))

  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%bytevector-type)
      (local.get $ptr)
      (local.get $dest-len))))

;; (bytevector-copy! <to> <at> <from> [<start> [<end>]])
(func $bytevector-copy! (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $to i32)
  (local $to-ptr i32)
  (local $to-len i32)
  (local $at i32)
  (local $from i32)
  (local $from-ptr i32)
  (local $from-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $src-len i32)
  (local $dest-len i32)

  (block $check (block $fail (block $four_args (block $three_args
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $fail (i32.lt_u (local.get $num-args) (i32.const 3)))
          (local.set $temp (local.get $args))

          (%pop-l $to $temp)
          (%chk-type $fail $to %bytevector-type)
          (local.set $to-len (%cdr-l $to))

          (%pop-l $at $temp)
          (%chk-type $fail $at %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $at)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $at (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.ge_u (local.get $at) (local.get $to-len)))
          (local.set $dest-len (i32.sub (local.get $to-len) (local.get $at)))

          (%pop-l $from $temp)
          (%chk-type $fail $from %bytevector-type)
          (local.set $from-len (%cdr-l $to))

          (br_if $three_args (i32.eq (local.get $num-args) (i32.const 3)))

          (%pop-l $start $temp)
          (%chk-type $fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.ge_u (local.get $start) (local.get $from-len)))

          (br_if $four_args (i32.eq (local.get $num-args) (i32.const 4)))

          (%pop-l $end $temp)
          (%chk-type $fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.lt_u (local.get $end) (local.get $start)))
          (br_if $fail (i32.gt_u (local.get $end) (local.get $from-len)))
          (local.set $src-len (i32.sub (local.get $end) (local.get $start)))

          (br_if $fail (i32.gt_u (local.get $src-len) (local.get $dest-len)))

          (br_if $fail (i32.gt_u (local.get $num-args) (i32.const 5)))
          (br $check))

        ;; three args
        (local.set $start (i32.const 0)))
        ;; fall through

      ;; four args
      (local.set $end (local.get $from-len))
      (local.set $src-len (i32.sub (local.get $end) (local.get $start)))

      (if (i32.gt_u (local.get $src-len) (local.get $dest-len)) (then
          (local.set $src-len (local.get $dest-len))
          (local.set $end (i32.add (local.get $start) (local.get $src-len)))))

      (br $check))

    ;; fail
    (return (call $argument-error (local.get $args))))

  (local.set $to-ptr (%car-l $to))
  (local.set $from-ptr (%car-l $from))

  (if (local.get $src-len) (then
      (call $memmove
        (i32.add (local.get $to-ptr) (local.get $at))
        (i32.add (local.get $from-ptr) (local.get $start))
        (local.get $src-len))))

  (return (global.get $g-nil)))

;; (bytevector-append <bytevector> ...)
(func $bytevector-append (param $env i32) (param $args i32) (result i32)
  (local $len i32)
  (local $dest-ptr i32)
  (local $dest-offset i32)
  (local $temp i32)
  (local $vector i32)
  (local $vector-len i32)

  (block $check (block $fail
      ;; find the length of all the bytevectors (and assert that they are)
      (local.set $temp (local.get $args))
      (local.set $len (i32.const 0))
      (block $end (loop $start
          (%chk-type $end $temp %cons-type)
          (%pop-l $vector $temp)
          (%chk-type $fail $vector %bytevector-type)
          (local.set $len (i32.add (local.get $len) (%cdr-l $vector)))

          (br $start)))

      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $dest-ptr (call $malloc (local.get $len)))
  (local.set $dest-offset (i32.const 0))

  ;; copy each vector in turn into the dest
  (local.set $temp (local.get $args))
  (block $end (loop $start
      (%chk-type $end $temp %cons-type)
      (%pop-l $vector $temp)
      (local.set $vector-len (%cdr-l $vector))
      (call $memcpy
        (i32.add (local.get $dest-ptr) (local.get $dest-offset))
        (%car-l $vector)
        (local.get $vector-len))
      (local.set $dest-offset (i32.add
          (local.get $dest-offset)
          (local.get $vector-len)))

      (br $start)))

  (%assert (i32.eq (local.get $dest-offset) (local.get $len)))

  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%bytevector-type)
      (local.get $dest-ptr)
      (local.get $len))))

;; (utf8->string <bytevector> [<start> [<end>]])
(func $utf8->string (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $vector i32)
  (local $vector-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $dest-len i32)
  (local $ptr i32)

  (block $check (block $fail (block $two_args (block $one_arg
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $fail (i32.eqz (local.get $num-args)))
          (local.set $temp (local.get $args))

          (%pop-l $vector $temp)
          (%chk-type $fail $vector %bytevector-type)
          (local.set $vector-len (%cdr-l $vector))

          (br_if $one_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.ge_u (local.get $start) (local.get $vector-len)))

          (br_if $two_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.lt_u (local.get $end) (local.get $start)))
          (br_if $fail (i32.gt_u (local.get $end) (local.get $vector-len)))

          (br_if $fail (i32.gt_u (local.get $num-args) (i32.const 3)))
          (br $check))

        ;; one arg
        (local.set $start (i32.const 0)))
        ;; fall through

      ;; two args
      (local.set $end (local.get $vector-len))
      (br $check))

    ;; fail
    (return (call $argument-error (local.get $args))))

  (local.set $dest-len (i32.sub (local.get $end) (local.get $start)))
  (local.set $ptr (call $malloc (i32.add (local.get $dest-len) (i32.const 4))))

  (i32.store (local.get $ptr) (local.get $dest-len))

  (if (local.get $dest-len) (then
      (call $memcpy
        (i32.add (local.get $ptr) (i32.const 4))
        (i32.add (%car-l $vector) (local.get $start))
        (local.get $dest-len))))

  (if (i32.eqz (call $str-is-valid (local.get $ptr))) (then
      (call $malloc-free (local.get $ptr))
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-str (local.get $ptr))))

;; (string->utf8 <string> [<start> [<end>]])
(func $string->utf8 (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $string i32)
  (local $string-len i32)
  (local $start i32)
  (local $end i32)
  (local $temp64 i64)
  (local $dest-len i32)
  (local $ptr i32)

  (block $check (block $fail (block $two_args (block $one_arg
          (local.set $num-args (call $list-len (local.get $args)))
          (br_if $fail (i32.eqz (local.get $num-args)))
          (local.set $temp (local.get $args))

          (%pop-l $string $temp)
          (%chk-type $fail $string %str-type)
          (local.set $string (%car-l $string))
          (local.set $string-len (i32.load (local.get $string)))

          (br_if $one_arg (i32.eq (local.get $num-args) (i32.const 1)))

          (%pop-l $start $temp)
          (%chk-type $fail $start %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $start)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $start (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.ge_u (local.get $start) (local.get $string-len)))

          (br_if $two_args (i32.eq (local.get $num-args) (i32.const 2)))

          (%pop-l $end $temp)
          (%chk-type $fail $end %i64-type)
          (local.set $temp64 (i64.load offset=4 (local.get $end)))
          (br_if $fail (i64.gt_u (local.get $temp64) (i64.const 0xFFFF_FFFF)))
          (local.set $end (i32.wrap_i64 (local.get $temp64)))
          (br_if $fail (i32.lt_u (local.get $end) (local.get $start)))
          (br_if $fail (i32.gt_u (local.get $end) (local.get $string-len)))

          (br_if $fail (i32.gt_u (local.get $num-args) (i32.const 3)))
          (br $check))

        ;; one arg
        (local.set $start (i32.const 0)))
        ;; fall through

      ;; two args
      (local.set $end (local.get $string-len))
      (br $check))

    ;; fail
    (return (call $argument-error (local.get $args))))

  (local.set $dest-len (i32.sub (local.get $end) (local.get $start)))
  (local.set $ptr (call $malloc (local.get $dest-len)))

  (if (local.get $dest-len) (then
      (call $memcpy
        (local.get $ptr)
        (i32.add (i32.add (local.get $string) (local.get $start)) (i32.const 4))
        (local.get $dest-len))))

  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%bytevector-type)
      (local.get $ptr)
      (local.get $dest-len))))
