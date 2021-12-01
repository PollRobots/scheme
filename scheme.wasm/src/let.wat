;; (let <bindings> <expression_1> ...)
;;    bindings ::=
;;      ((<variable_1> <init_1>) ...)
(func $let (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $var i32)
  (local $init i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $temp)) (i32.const 2)))

      (%pop-l $bindings $temp)
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  ;; child-env = environment-init(g-heap, env)
  (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $env)))

  ;; while (typeof bindings is cons) {
  (block $w_end
    (block $w_error
      (loop $w_start
        (br_if $w_end (i32.eq (%get-type $bindings) (%nil-type)))

        (%pop-l $binding $bindings)
        (br_if $w_error (i32.eqz (call $is-list-impl (local.get $bindings))))

        (%pop-l $var $binding)
        (br_if $w_error (i32.ne (%get-type $var) (%symbol-type)))

        (%pop-l $init $binding)
        (br_if $w_error (i32.ne (%get-type $binding) (%nil-type)))

        ;; environment-add(child-env, var, eval(env, init))
        (call $environment-add
          (local.get $child-env) 
          (local.get $var)
          (call $eval (local.get $env) (local.get $init)))

        (br $w_start)))
    (return (call $argument-error (local.get $args))))

  (return (call $cond-eval-exprs (local.get $child-env) (local.get $body))))


;; (let* <bindings> <expression_1> ...)
;;    bindings ::=
;;      ((<variable_1> <init_1>) ...)
(func $let* (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $var i32)
  (local $init i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $temp)) (i32.const 2)))

      (%pop-l $bindings $temp)
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  ;; child-env = environment-init(g-heap, env)
  (local.set $child-env (local.get $env))

  ;; while (typeof bindings is cons) {
  (block $w_end
    (block $w_error
      (loop $w_start
        (br_if $w_end (i32.eq (%get-type $bindings) (%nil-type)))

        (%pop-l $binding $bindings)
        (br_if $w_error (i32.eqz (call $is-list-impl (local.get $bindings))))

        (%pop-l $var $binding)
        (br_if $w_error (i32.ne (%get-type $var) (%symbol-type)))

        (%pop-l $init $binding)
        (br_if $w_error (i32.ne (%get-type $binding) (%nil-type)))

        (local.set $init (call $eval (local.get $child-env) (local.get $init)))

        ;; TODO special case an environment that only has one entry
        (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $child-env)))
        ;; environment-add(child-env, var, eval(child-env, init))
        (call $environment-add (local.get $child-env) (local.get $var) (local.get $init))

        (br $w_start)))
    (return (call $argument-error (local.get $args))))

  (return (call $cond-eval-exprs (local.get $child-env) (local.get $body))))

;; (letrec <bindings> <expression_1> ...)
;; (letrec* <bindings> <expression_1> ...)
;;    bindings ::=
;;      ((<variable_1> <init_1>) ...)
(func $letrec (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $var i32)
  (local $init i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $temp)) (i32.const 2)))

      (%pop-l $bindings $temp)
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  ;; child-env = environment-init(g-heap, env)
  (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $env)))

  ;; initialize child-env, add a nil entry for each binding
  (block $w_end
    (block $w_error
      (loop $w_start
        (br_if $w_end (i32.eq (%get-type $bindings) (%nil-type)))

        (%pop-l $binding $bindings)
        (br_if $w_error (i32.eqz (call $is-list-impl (local.get $bindings))))

        (%pop-l $var $binding)
        (br_if $w_error (i32.ne (%get-type $var) (%symbol-type)))

        (%pop-l $init $binding)
        (br_if $w_error (i32.ne (%get-type $binding) (%nil-type)))

        (call $environment-add (local.get $child-env) (local.get $var) (global.get $g-nil))
        (br $w_start)))
    (return (call $argument-error (local.get $args))))

  
  ;; reset bindings
  (local.set $bindings (%car-l $args))
  ;; call each initializer
  (block $w_end
    (loop $w_start
      (br_if $w_end (i32.eq (%get-type $bindings) (%nil-type)))

      (%pop-l $binding $bindings)
      (%pop-l $var $binding)
      (%pop-l $init $binding)

      ;; environment-set!(child-env, var, eval(env, init))
      (call $environment-set!
        (local.get $child-env) 
        (local.get $var)
        (call $eval (local.get $child-env) (local.get $init)))

      (br $w_start)))

  (return (call $cond-eval-exprs (local.get $child-env) (local.get $body))))

;; (let-values <mv binding spec> <body>)
;;    mv binding spec ::=
;;      ((<formals_1> <init_1>) ...)
(func $let-values (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $formals i32)
  (local $init i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $temp)) (i32.const 2)))

      (%pop-l $bindings $temp)
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  ;; child-env = environment-init(g-heap, env)
  (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $env)))

  (block $b_end
    (block $b_error
      (loop $b_start
        (br_if $b_end (i32.eq (%get-type $bindings) (%nil-type)))
        (%pop-l $binding $bindings)

        (br_if $b_error (i32.ne (call $list-len (local.get $binding)) (i32.const 2)))
        (%pop-l $formals $binding)
        (br_if $b_error (i32.eqz (call $let-check-formals (local.get $formals))))

        (%pop-l $init $binding)

        (local.set $init (call $eval (local.get $env) (local.get $init)))
        (if (i32.ne (%get-type $init) (%values-type))
          (then
            (local.set $init (%alloc-values (local.get $init) (global.get $g-nil)))))
        
        (block $f_end
          (loop $f_start
            (br_if $f_end (i32.eq (%get-type $init) (%nil-type)))
            (br_if $f_end (i32.eq (%get-type $formals) (%nil-type)))

            (call $environment-add (local.get $child-env) (%car-l $formals) (%car-l $init))

            (local.set $init (%cdr-l $init))
            (local.set $formals (%cdr-l $formals))
            (br $f_start)))

        (%chk-type $b_error $init %nil-type)
        (%chk-type $b_error $formals %nil-type)

        (br $b_start)))
    (return (call $argument-error (local.get $args))))

  (return (call $cond-eval-exprs (local.get $child-env) (local.get $body))))

(func $let-check-formals (param $args i32) (result i32)
  (local $type i32)
  (local $formal i32)

  (block $b_end
    (loop $b_start
      (local.set $type (%get-type $args))
      (if (i32.eq (local.get $type) (%nil-type))
        (then (return (i32.const 1))))
      (br_if $b_end (i32.ne (local.get $type) (%cons-type)))

      (%pop-l $formal $args)
      (br_if $b_end (i32.ne (%get-type $formal) (%symbol-type)))

      (br $b_start)))

  (return (i32.const 0)))

;; (let*-values <mv binding spec> <body>)
;;    mv binding spec ::=
;;      ((<formals_1> <init_1>) ...)
(func $let*-values (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $formals i32)
  (local $init i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $temp)) (i32.const 2)))

      (%pop-l $bindings $temp)
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $child-env (local.get $env))

  (block $b_end
    (block $b_error
      (loop $b_start
        (br_if $b_end (i32.eq (%get-type $bindings) (%nil-type)))
        (%pop-l $binding $bindings)

        (br_if $b_error (i32.ne (call $list-len (local.get $binding)) (i32.const 2)))
        (%pop-l $formals $binding)
        (br_if $b_error (i32.eqz (call $let-check-formals (local.get $formals))))

        (%pop-l $init $binding)

        (local.set $init (call $eval (local.get $child-env) (local.get $init)))
        (if (i32.ne (%get-type $init) (%values-type))
          (then
            (local.set $init (%alloc-values (local.get $init) (global.get $g-nil)))))
        
        (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $child-env)))
        (block $f_end
          (loop $f_start
            (br_if $f_end (i32.eq (%get-type $init) (%nil-type)))
            (br_if $f_end (i32.eq (%get-type $formals) (%nil-type)))

            (call $environment-add (local.get $child-env) (%car-l $formals) (%car-l $init))

            (local.set $init (%cdr-l $init))
            (local.set $formals (%cdr-l $formals))
            (br $f_start)))

        (%chk-type $b_error $init %nil-type)
        (%chk-type $b_error $formals %nil-type)

        (br $b_start)))
    (return (call $argument-error (local.get $args))))

  (return (call $cond-eval-exprs (local.get $child-env) (local.get $body))))