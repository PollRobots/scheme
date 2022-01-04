(func $define (param $env i32) (param $args i32) (result i32)
  (local $var i32)
  (local $vars i32)
  (local $var-type i32)
  (local $temp i32)
  (local $expr i32)
  (local $formals i32)
  (local $fn i32)

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
      (block $b_check (block $b_fail
          (local.set $vars (local.get $var))
          (%chk-type $b_fail $vars %cons-type)
          (%pop-l $var $vars)
          (br_if $b_fail (i32.ne (%get-type $var) (%symbol-type)))
          (br_if $b_fail (call $environment-has 
              (local.get $env) 
              (local.get $var)))
          (local.set $formals (local.get $vars))

          (local.set $fn (call $lambda 
              (local.get $env) 
              (%alloc-cons (local.get $formals) (local.get $temp))))
          (%chk-type $b_fail $fn %lambda-type)
          (br $b_check))
        
        (return (call $argument-error (local.get $args))))

      ;; format is (define (var formals) body ...)
      (call $environment-add (local.get $env) (local.get $var) (local.get $fn))
      (return (global.get $g-nil))))

  (if (call $environment-has (local.get $env) (local.get $var)) (then
      (return (call $argument-error (local.get $args)))))

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $expr)
        (call $cont-alloc
          (%cont-env-add)
          (local.get $env)
          (%alloc-cons (local.get $var) (global.get $g-nil))
          (i32.const 0)))))

;; (cont-env-add value name)
(func $cont-env-add (param $env i32) (param $args i32) (result i32)
  (local $value i32)
  (local $name i32)

  (%pop-l $value $args)
  (%pop-l $name $args)

  (call $environment-add (local.get $env) (local.get $name) (local.get $value))
  (return (global.get $g-nil)))

;; (define-values <formals> <expression>)
(func $define-values (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $formals i32)
  (local $expr i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))
      (%pop-l $formals $temp)
      (br_if $fail (i32.eqz (call $check-formals 
            (local.get $env) 
            (local.get $formals))))

      (local.set $expr (%car-l $temp))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (call $cont-alloc
      (%eval-fn)
      (local.get $env)
      (local.get $expr)
      (call $cont-alloc 
        (%cont-env-add-values)
        (local.get $env)
        (local.get $args)
        (i32.const 0)))))

;; (cont-env-add-values <values> <formals> <expr>)
(func $cont-env-add-values (param $env i32) (param $args i32) (result i32)
  (local $values i32)
  (local $formals i32)
  (local $var i32)
  (local $val i32)

  (%pop-l $values $args)

  (if (i32.ne (%get-type $values) (%values-type)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $formals (%car-l $args))
  ;; convert values to a list
  (local.set $values (%alloc-cons 
      (%car-l $values) 
      (%cdr-l $values)))

  (if (i32.lt_u 
      (call $list-len (local.get $values)) 
      (call $improper-list-len (local.get $formals)))
    (then (return (call $argument-error (local.get $args)))))

  (block $end (loop $start
      (br_if $end (i32.eq (local.get $formals) (global.get $g-nil)))

      (if (i32.eq (%get-type $formals) (%symbol-type)) (then
          ;; dotted formal consumes all remaining values, including a potentially empty list
          (call $environment-add (local.get $env) (local.get $formals) (local.get $values))
          (return (global.get $g-nil))))

      ;; because of the length check above, this should never happen
      (br_if $end (i32.eq (local.get $values) (global.get $g-nil)))

      (%pop-l $var $formals)
      (%pop-l $val $values)

      (call $environment-add (local.get $env) (local.get $var) (local.get $val))
      (br $start)))

  ;; BUG: if there are still values remaining then they will be silently dropped.
  (return (global.get $g-nil)))

(func $check-formals (param $env i32) (param $formals i32) (result i32)
  (local $tail-type i32)
  (local $curr i32)
  (local $have-formals i32)

  (local.set $have-formals (i32.const 0))

  (block $end (loop $start
      ;; if the tail is nil, then this is a good list, exit the loop, if the
      ;; list has had any entries then the result will be 1, otherwise 0
      (br_if $end (i32.eq (local.get $formals) (global.get $g-nil)))
      (local.set $have-formals (i32.const 1))

      (local.set $tail-type (%get-type $formals))
      ;; its ok if formals is an improper list IFF the tail is a symbol
      (if (i32.eq (local.get $tail-type) (%symbol-type)) (then
          (return (i32.eqz (call $environment-has 
                (local.get $env) 
                (local.get $formals))))))

      (if (i32.ne (local.get $tail-type) (%cons-type)) (then
          ;; improper lists with tail types that aren't symbols aren't valid
          (return (i32.const 0))))

      (%pop-l $curr $formals)
      (if (i32.ne (%get-type $curr) (%symbol-type)) (then
          ;; all list entries must be symbols.
          (return (i32.const 0))))
      (if (call $environment-has (local.get $env) (local.get $curr)) (then
          ;; formals must not already be in the environment
          (return (i32.const 0))))

      (br $start)))

  (return (local.get $have-formals)))

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

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $value)
        (call $cont-alloc
          (%cont-env-set!)
          (local.get $env)
          (%alloc-cons (local.get $name) (global.get $g-nil))
          (i32.const 0)))))

;; (cont-env-set! value name)
(func $cont-env-set! (param $env i32) (param $args i32) (result i32)
  (local $value i32)
  (local $name i32)

  (%pop-l $value $args)
  (%pop-l $name $args)

  (return (call $environment-set! 
      (local.get $env) 
      (local.get $name) 
      (local.get $value))))