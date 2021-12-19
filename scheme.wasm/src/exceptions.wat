;; (raise <obj>)
(func $raise (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-raise (%car-l $args))))

;; (cont-raise <x> <obj>)
;; This is used as a continuation by the guard for a raise exception
(func $cont-raise (param $env i32) (param $args i32) (result i32)
  (return (%alloc-raise (%car (%cdr-l $args)))))

;; (raise-continuable <obj>)
(func $raise-continuable (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-raise-continuable (%car-l $args))))

;; (error <msg> <obj> ...)
(func $error (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $msg i32)

  (block $check (block $fail
      (br_if $fail (i32.eqz (call $list-len (local.get $args))))
      (local.set $temp (local.get $args))
      (%pop-l $msg $temp)
      (%chk-type $fail $msg %str-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-raise (%alloc-error-cons 
        (local.get $msg) 
        (local.get $temp)))))

;; (error-object? <obj>)
(func $error-object? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  
  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $obj) (%error-type)))))

;; (error-object-message <error-object>)
(func $error-object-message (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $msg i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $obj (%car-l $args))
      (%chk-type $fail $obj %error-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%car-l $obj)))

;; (error-object-irritants <error-object>)
(func $error-object-irritants (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $msg i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $obj (%car-l $args))
      (%chk-type $fail $obj %error-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return  (%cdr-l $obj)))

;; (with-exception-handler <handler> <thunk>)
(func $with-exception-handler (param $env i32) (param $args i32) (result i32)
  (local $handler i32)
  (local $thunk i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $handler (%car-l $args))
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $handler))))
      (local.set $thunk (%car (%cdr-l $args)))
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $thunk))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-cont (call $cont-alloc
        (i32.const 0)
        (local.get $env)
        (%alloc-cons (local.get $thunk) (global.get $g-nil))
        (call $cont-alloc
          (%guard-fn)
          (local.get $env)
          (local.get $handler) 
          (i32.const 0))))))

  
