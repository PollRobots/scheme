;; (let <bindings> <expression_1> ...)
;; (let <name> <bindings> <expression_1> ...)
;;    bindings ::=
;;      ((<variable_1> <init_1>) ...)
(func $let (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $bindings i32)
  (local $name i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $var i32)
  (local $init i32)
  (local $vars i32)
  (local $inits i32)

  (block $check (block $fail
      (local.set $temp (local.get $args))
      (local.set $num-args (call $list-len (local.get $temp)))

      (br_if $fail (i32.lt_u (local.get $num-args) (i32.const 2)))

      (%pop-l $bindings $temp)
      (if (i32.eq (%get-type $bindings) (%symbol-type))
        (then
          ;; this is a 'named' let
          (br_if $fail (i32.lt_u (local.get $num-args) (i32.const 3)))
          (local.set $name (local.get $bindings))
          (%pop-l $bindings $temp))
        (else
          (local.set $name (i32.const 0))))

      (br_if $fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $vars (global.get $g-nil))
  (local.set $inits (global.get $g-nil))
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

        (%push-l $var $vars)
        (%push-l $init $inits)

        (br $w_start)))
    (return (call $argument-error (local.get $args))))

  (if (local.get $name)
    (then
      ;; order of formals and inits matters
      (local.set $vars (call $reverse-impl (local.get $vars)))
      (local.set $inits (call $reverse-impl (local.get $inits)))
      ;; (letrec ((name (lambda (formals ...) body ...)))
      ;;    (name inits ...))
      (local.set $temp (%alloc-list-3
        (global.get $g-letrec)
        (%alloc-list-1 (%alloc-list-2
            (local.get $name)
            (%alloc-cons
              (global.get $g-lambda)
              (%alloc-cons
                (local.get $vars)
                (local.get $body)))))
        (%alloc-cons (local.get $name) (local.get $inits)))))
    (else
      ;; create ((lambda (formals ...) body ...) inits ...)
      (local.set $temp (%alloc-cons
          (%alloc-cons
            (global.get $g-lambda)
            (%alloc-cons
              (local.get $vars)
              (local.get $body)))
          (local.get $inits)))))

  (return (call $cont-alloc
      (%eval-fn)
      (local.get $env)
      (local.get $temp)
      (i32.const 0))))

;; (let* <bindings> <expression_1> ...)
;;    bindings ::=
;;      ((<variable_1> <init_1>) ...)
(func $let* (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
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


  (if (i32.eq (local.get $bindings) (global.get $g-nil))
    (then
      ;; no bindings
      ;; <--- ((lambda () body ...))
      (local.set $temp (%alloc-list-1 (%alloc-cons
            (global.get $g-lambda)
            (%alloc-cons
              (local.get $bindings)
              (local.get $body))))))
    (else
      (block $end (block $error
          (br_if $end (i32.eq (%get-type $bindings) (%nil-type)))

          (%pop-l $binding $bindings)
          (br_if $error (i32.eqz (call $is-list-impl (local.get $bindings))))

          (%pop-l $var $binding)
          (br_if $error (i32.ne (%get-type $var) (%symbol-type)))

          (%pop-l $init $binding)
          (br_if $error (i32.ne (%get-type $binding) (%nil-type)))
          (br $end))

        (return (call $argument-error (local.get $args))))

      (if (i32.eq (local.get $bindings) (global.get $g-nil))
        (then
          ;; this is the final binding pair
          ;; (let (binding) body ...))
          (local.set $temp (%alloc-cons
              (global.get $g-let)
              (%alloc-cons
                (%alloc-list-1 (%alloc-list-2
                    (local.get $var)
                    (local.get $init)))
                (local.get $body)))))
        (else
          ;; (let (binding)
          ;;    (let* (bindings ...)
          ;;        body ...))
          (local.set $temp (%alloc-list-3
              (global.get $g-let)
              (%alloc-list-1 (%alloc-list-2 (local.get $var) (local.get $init)))
              (%alloc-cons
                (global.get $g-let-star)
                (%alloc-cons
                  (local.get $bindings)
                  (local.get $body)))))))))

  (return (call $cont-alloc
      (%eval-fn)
      (local.get $env)
      (local.get $temp)
      (i32.const 0))))

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
  (local $vars i32)
  (local $inits i32)

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

  (local.set $vars (global.get $g-nil))
  (local.set $inits (global.get $g-nil))
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

        (call $environment-add (local.get $child-env) (local.get $var) (global.get $g-nil))

        (%push-l $var $vars)
        (%push-l $init $inits)

        (br $w_start)))
    (return (call $argument-error (local.get $args))))

  (if (i32.eq (local.get $vars) (global.get $g-nil))
    ;; there are no bindings, so simply eval body in child environment
    (then (return (call $eval-body (i32.const 1) (local.get $child-env) (local.get $body)))))

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $child-env)
        (%car-l $inits)
        (call $cont-alloc
          (%cont-expr-list) ;; eval a list of expressions
          (local.get $child-env)
          (%alloc-cons (global.get $g-nil) (%cdr-l $inits))
          (call $cont-alloc
            (%cont-letrec) ;; finish the letrec
            (local.get $child-env)
            (%alloc-cons (local.get $vars) (local.get $body))
            (i32.const 0))))))

;; (cont-letrec inits vars body ...)
;; set! the initialized variables into the environment
;; and evaluate the body
(func $cont-letrec (param $env i32) (param $args i32) (result i32)
  (local $inits i32)
  (local $vars i32)
  (local $init i32)
  (local $var i32)

  (%pop-l $inits $args)
  (%pop-l $vars $args)

  (local.set $vars (call $reverse-impl (local.get $vars)))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $vars) (%nil-type)))

      (%pop-l $var $vars)
      (%pop-l $init $inits)

      (drop (call $environment-set!
          (local.get $env)
            (local.get $var)
            (local.get $init)))

      (br $b_start)))

  (return (call $eval-body (i32.const 1) (local.get $env) (local.get $args))))

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
  (local $formals-list i32)
  (local $init-list i32)

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

  (local.set $formals-list (global.get $g-nil))
  (local.set $init-list (global.get $g-nil))

  (block $b_end
    (block $b_error
      (loop $b_start
        (br_if $b_end (i32.eq (%get-type $bindings) (%nil-type)))
        (%pop-l $binding $bindings)

        (br_if $b_error (i32.ne (call $list-len (local.get $binding)) (i32.const 2)))
        (%pop-l $formals $binding)
        (br_if $b_error (i32.eqz (call $let-check-formals (local.get $formals))))

        (%pop-l $init $binding)

        (%push-l $init $init-list)
        (%push-l $formals $formals-list)

        (br $b_start)))

    (return (call $argument-error (local.get $args))))

  ;; if no bindings, simply eval
  (if (i32.eq (local.get $formals-list) (global.get $g-nil))
    (then (return (call $eval-body (i32.const 1) (local.get $child-env) (local.get $body)))))

  (%push-l $formals-list $body)
  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (%car-l $init-list)
        (call $cont-alloc
          (%cont-expr-list)
          (local.get $env)
          (%alloc-cons (global.get $g-nil) (%cdr-l $init-list))
          (call $cont-alloc
            (%cont-let-values)
            (local.get $child-env)
            (local.get $body)
            (i32.const 0))))))

;; (cont-let-values (values...) (formals...) body ...)
(func $cont-let-values (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $values-list i32)
  (local $formals-list i32)
  (local $values i32)
  (local $formals i32)
  (local $sym i32)
  (local $val i32)
  (local $body i32)

  (local.set $temp (local.get $args))
  (%pop-l $values-list $temp)
  (%pop-l $formals-list $temp)
  (local.set $body (local.get $temp))

  (local.set $formals-list (call $reverse-impl (local.get $formals-list)))

  (block $b_end
    (block $b_error
      (loop $b_start
        (br_if $b_end (i32.eq (%get-type $values-list) (%nil-type)))
        (%pop-l $values $values-list)
        (%pop-l $formals $formals-list)

        (block $i_end
          (loop $i_start
            (br_if $i_end (i32.eq (%get-type $values) (%nil-type)))
            (br_if $i_end (i32.eq (%get-type $formals) (%nil-type)))

            (%pop-l $sym $formals)
            (%pop-l $val $values)

            (call $environment-add (local.get $env) (local.get $sym) (local.get $val))

            (br $i_start)))

        (br_if $b_error (i32.ne (%get-type $values) (%nil-type)))
        (br_if $b_error (i32.ne (%get-type $formals) (%nil-type)))

        (br $b_start)))

    (return (call $argument-error (local.get $args))))

  (return (call $eval-body (i32.const 1) (local.get $env) (local.get $body))))

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
  (local $binding i32)
  (local $formals i32)
  (local $init i32)
  (local $init-list i32)
  (local $formals-list i32)

  (block $b_check
    (block $b_fail
      (local.set $temp (local.get $args))
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $temp)) (i32.const 2)))

      (%pop-l $bindings $temp)
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $bindings))))

      (local.set $body (local.get $temp))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $init-list (global.get $g-nil))
  (local.set $formals-list (global.get $g-nil))

  (block $b_end
    (block $b_error
      (loop $b_start
        (br_if $b_end (i32.eq (%get-type $bindings) (%nil-type)))
        (%pop-l $binding $bindings)

        (br_if $b_error (i32.ne (call $list-len (local.get $binding)) (i32.const 2)))
        (%pop-l $formals $binding)
        (br_if $b_error (i32.eqz (call $let-check-formals (local.get $formals))))

        (%pop-l $init $binding)

        (%push-l $formals $formals-list)
        (%push-l $init $init-list)
        (br $b_start)))

    (return (call $argument-error (local.get $args))))

  ;; if there are no bindings, simply call the body
  (if (i32.eq (local.get $init-list) (global.get $g-nil))
    (then (return (call $eval-body
          (i32.const 1)
          (call $environment-init (global.get $g-heap) (local.get $env))
          (local.get $body)))))

  (local.set $formals-list (call $reverse-impl (local.get $formals-list)))
  (local.set $init-list (call $reverse-impl (local.get $init-list)))

  (return (call $let*-values-init
      (local.get $env)
      (local.get $formals-list)
      (local.get $init-list)
      (local.get $body))))

(func $let*-values-init (param $env i32) (param $formals-list i32) (param $init-list i32) (param $body i32) (result i32)
  (local $formals i32)
  (local $init i32)
  (if (i32.eq (%get-type $formals-list) (%nil-type))
    (then (return (call $eval-body (i32.const 1) (local.get $env) (local.get $body)))))

  (%pop-l $init $init-list)

  (%push-l $init-list $body)
  (%push-l $formals-list $body)

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $init)
        (call $cont-alloc
          (%cont-let*-values)
          (call $environment-init (global.get $g-heap) (local.get $env))
          (local.get $body)
          (i32.const 0)))))

;; (cont-let*-values values (formals...) (init...) body ...)
(func $cont-let*-values (param $env i32) (param $args i32) (result i32)
  (local $values i32)
  (local $formals-list i32)
  (local $formals i32)
  (local $init-list i32)
  (local $body i32)
  (local $sym i32)
  (local $val i32)

  (%pop-l $values $args)
  (%pop-l $formals-list $args)
  (%pop-l $formals $formals-list)
  (%pop-l $init-list $args)
  (local.set $body (local.get $args))

  (block $b_error
    (block $b_end
      (loop $b_start
        (br_if $b_end (i32.eq (%get-type $values) (%nil-type)))
        (br_if $b_end (i32.eq (%get-type $formals) (%nil-type)))

        (%pop-l $sym $formals)
        (%pop-l $val $values)

        (call $environment-add (local.get $env) (local.get $sym) (local.get $val))

        (br $b_start)))

    (br_if $b_error (i32.ne (%get-type $values) (%nil-type)))
    (br_if $b_error (i32.ne (%get-type $formals) (%nil-type)))

    (return (call $let*-values-init
        (local.get $env)
        (local.get $formals-list)
        (local.get $init-list)
        (local.get $body))))

  (return (call $argument-error (local.get $args))))
