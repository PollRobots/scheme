(func $symbol? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $arg (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $arg) (%symbol-type))
    )
  )
)

(func $symbol=? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.eqz (call $list-len (local.get $args))))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%symbol-type)))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $args (%cdr-l $args))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (if (i32.ne (local.get $arg) (%car-l $args)) (then
        (return (global.get $g-false))))

      (local.set $args (%cdr-l $args))
      (br $b_start)))

  (return (global.get $g-true)))

(func $symbol->string (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $str i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%symbol-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $arg (%car-l $args))
  (local.set $str (%alloc-str (call $str-dup (%car-l $arg))))
  (%set-flags $str (%immutable-flag))
  (return (local.get $str))
)

(func $string->symbol (param $env i32) (param $args i32) (result i32)
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

  (local.set $arg (%car-l $args))
  (return (%alloc-symbol (call $str-dup (%car-l $arg))))
)
