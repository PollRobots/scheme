(func $define (param $env i32) (param $args i32) (result i32)
  (local $var i32)
  (local $vars i32)
  (local $var-type i32)
  (local $temp i32)
  (local $expr i32)
  (local $formals i32)
  (local $lambda i32)

  (if (i32.lt_u (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $temp (local.get $args))
  (%pop-l $var $temp)
  
  (local.set $var-type (%get-type $var))

  (if (i32.eq (local.get $var-type) (%symbol-type))
    (then
      ;; format is (define var expr)
      (%pop-l $expr $temp)
      (if (i32.ne (%get-type $temp) (%nil-type))
        (then (return (call $argument-error (local.get $args))))))
    (else
      (block $b_check
        (block $b_fail
          (local.set $vars (local.get $var))
          (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $vars))))
          (br_if $b_fail (i32.ne (call $list-len (local.get $vars)) (i32.const 2)))

          (%pop-l $var $vars)
          (br_if $b_fail (i32.ne (%get-type $var) (%symbol-type)))
          (%pop-l $formals $vars)
          (br $b_check))
        
        (return (call $argument-error (local.get $args))))

      ;; format is (define (var formals) body ...)
      ;; 
      ;; convert to (define var (lambda formals body ...))
      (local.set $expr (%alloc-cons
          (global.get $lambda-sym)
          (%alloc-cons (local.get $formals) (local.get $temp))))))

  (if (call $environment-has (local.get $env) (local.get $var)) (then
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-cont (call $cont-alloc
        (i32.const 0) ;; eval
        (local.get $env)
        (local.get $expr)
        (call $cont-alloc
          (%cont-env-add)
          (local.get $env)
          (%alloc-cons (local.get $var) (global.get $g-nil))
          (i32.const 0))))))

;; (cont-env-add value name)
(func $cont-env-add (param $env i32) (param $args i32) (result i32)
  (local $value i32)
  (local $name i32)

  (%pop-l $value $args)
  (%pop-l $name $args)

  (call $environment-add (local.get $env) (local.get $name) (local.get $value))
  (return (global.get $g-nil)))

(func $set! (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $value i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (%pop-l $name $args)
      (br_if $b_fail (i32.ne (%get-type $name) (%symbol-type)))
      (%pop-l $value $args)
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-cont (call $cont-alloc
        (i32.const 0) ;; eval
        (local.get $env)
        (local.get $value)
        (call $cont-alloc
          (%cont-env-set!)
          (local.get $env)
          (%alloc-cons (local.get $name) (global.get $g-nil))
          (i32.const 0))))))

;; (cont-env-set! value name)
(func $cont-env-set! (param $env i32) (param $args i32) (result i32)
  (local $value i32)
  (local $name i32)

  (%pop-l $value $args)
  (%pop-l $name $args)

  (call $environment-set! (local.get $env) (local.get $name) (local.get $value))
  (return (global.get $g-nil)))