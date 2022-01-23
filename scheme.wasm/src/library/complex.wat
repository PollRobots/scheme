;; (make-rectangular <x_1> <x_2>)
;; Creates a complex number with real part <x_1> and imaginary part <x_2>
;; i.e., <x_1>+<x_2>i
(func $make-rectangular (param $env i32) (param $args i32) (result i32)
  (local $real i32)
  (local $imag i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $fail (i32.eqz (call $all-numeric (local.get $args))))

      (local.set $real (%car-l $args))
      (%chk-type-ne $fail $real %complex-type)

      (local.set $imag (%car (%cdr-l $args)))
      (%chk-type-ne $fail $imag %complex-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (if (call $num-core-zero? (local.get $imag)) (then
      (return (local.get $real))))

  (return (%alloc-complex (local.get $real) (local.get $imag))))

;; (real-part <z>)
(func $real-part (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $fail (i32.eqz (call $all-numeric (local.get $args))))

      (local.set $num (%car-l $args))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (if (i32.eq (%get-type $num) (%complex-type)) (then
      (return (%car-l $num))))

  (return (local.get $num)))

;; (imag-part <z>)
(func $imag-part (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $fail (i32.eqz (call $all-numeric (local.get $args))))

      (local.set $num (%car-l $args))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (if (i32.eq (%get-type $num) (%complex-type)) (then
      (return (%cdr-l $num))))

  (return (global.get $g-zero)))
