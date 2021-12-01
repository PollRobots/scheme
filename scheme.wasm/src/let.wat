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

        ;; TODO special case an environment that only has one entry
        (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $child-env)))
        ;; environment-add(child-env, var, eval(env, init))
        (call $environment-add
          (local.get $child-env) 
          (local.get $var)
          (call $eval (local.get $child-env) (local.get $init)))

        (br $w_start)))
    (return (call $argument-error (local.get $args))))

  (return (call $cond-eval-exprs (local.get $child-env) (local.get $body))))

;; (letrec <bindings> <expression_1> ...)
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