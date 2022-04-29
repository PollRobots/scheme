(func $exit (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $exit-code i32)

  (if (i32.eq (%get-type $args) (%nil-type))
    (then
      (local.set $exit-code (i32.const 0)))
    (else
      (local.set $arg (%car-l $args))
      (if (i32.eq (%get-type $arg) (%i64-type)) (then
          (local.set $exit-code (i32.wrap_i64 (i64.load offset=4 (local.get $arg))))))
      (if (i32.eq (%get-type $arg) (%boolean-type)) (then
          (local.set $exit-code
            (select
              (i32.const 0)
              (i32.const 1)
              (%car-l $arg)))))))

  (call $process-exit (local.get $exit-code))
  (unreachable))

(func $get-environment-variable (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $value i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $name (%car-l $args))
      (%chk-type $fail $name %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (local.set $value (call $process-get-environment-variable (%car-l $name)))
  (if (i32.eqz (local.get $value)) (then
      (return (global.get $g-false))))
  (if (i32.eq (%get-type $value) (%str-type)) (then
      (return (local.get $value))))

  (unreachable))

(func $get-environment-variables (param $env i32) (param $args i32) (result i32)
  (local $list i32)

  (if (call $list-len (local.get $args)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $list (call $process-get-environment-variables))
  (if (i32.eqz (call $is-list-impl (local.get $list))) (then
      (unreachable)))

  (return (local.get $list)))

(func $set-environment-variable (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $value i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (%pop-l $name $args)
      (%chk-type $fail $name %str-type)

      (local.set $value (%car-l $args))
      (%chk-type $fail $value %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (call $process-set-environment-variable (%car-l $name) (%car-l $value))
  (return (global.get $g-nil)))

(func $version (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (local.get $args) (global.get $g-nil)) (then
      (return (call $argument-error (local.get $args)))))

  (return (global.get $g-curr-version)))

(func $panic (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (local.get $args) (global.get $g-nil)) (then
      (call $print-cons (%car-l $args) (%cdr-l $args) (i32.const 0))))
  (unreachable))
