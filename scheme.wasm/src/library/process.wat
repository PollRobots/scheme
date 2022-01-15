(func $exit (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $exit-code i32)

  (if (i32.eq (%get-type $args) (%nil-type))
    (then
      (local.set $exit-code (i32.const 0))
    )
    (else
      (local.set $arg (%car-l $args))
      (if (i32.eq (%get-type $arg) (%i64-type))
        (then
          (local.set $exit-code (i32.wrap_i64 (i64.load offset=4 (local.get $arg))))
        )
      )
      (if (i32.eq (%get-type $arg) (%boolean-type))
        (then
          (local.set $exit-code
            (select
              (i32.const 0)
              (i32.const 1)
              (%car-l $arg)
            )
          )
        )
      )
    )
  )

  (call $process-exit (local.get $exit-code))

  (return (global.get $g-nil))
)

(func $version (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (local.get $args) (global.get $g-nil)) (then
      (return (call $argument-error (local.get $args)))))

  (return (global.get $g-curr-version)))

(func $panic (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (local.get $args) (global.get $g-nil)) (then
      (call $print-cons (%car-l $args) (%cdr-l $args) (i32.const 0))))
  (unreachable))
