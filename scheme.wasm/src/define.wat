(func $define (param $env i32) (param $args i32) (result i32)
  (local $car i32)
  (local $cdr i32)
  (local $var i32)
  (local $temp i32)
  (local $formals i32)
  (local $body i32)
  (local $lambda i32)

  ;; car = car(args)
  (local.set $car (%car-l $args))

  ;; cdr = cdr(args)
  (local.set $cdr (%cdr-l $args))
  ;; verify that the args is a list of length 2
  ;; assert(cdr is cons)
  (%assert-cons $cdr)
  ;; temp = cdr(cdr)
  (local.set $temp (%cdr-l $cdr))
  ;; assert(temp is nil)
  (%assert-nil $temp)
  ;; body = car(cdr)
  (local.set $body (%car-l $cdr))

  ;; // format is (define var body)
  ;; if (car is symbol) {
  (if (i32.eq (%get-type $car) (%symbol-type))
    (then
      ;; environment-add(env, car, eval(env, body))
      (call $environment-add
        (local.get $env)
        (local.get $car)
        (call $eval (local.get $env) (local.get $body))
      )
      ;; return g-nil
      (return (global.get $g-nil))
    )
  ;; } 
  )

  ;; assert(car is cons)
  (%assert-cons $car)
  ;; var = car(car)
  (local.set $var (%car-l $car))
  ;; temp = cdr(car)
  (local.set $temp (%cdr-l $car))
  ;; formals = car(temp)
  (local.set $formals (%car-l $temp))
  ;; temp = cdr(temp)
  (local.set $temp (%cdr-l $temp))
  ;; assert(temp is nil)
  (%assert-nil $temp)

  ;; // construct lambda
  ;; lambda = cons(|lambda|, cons(formals, body))
  (local.set $lambda
    (%alloc-cons
      (global.get $lambda-sym)
      (%alloc-cons
        (local.get $formals)
        (%alloc-cons (local.get $body) (global.get $g-nil))
      )
    )
  )
  ;; environment-add(env, var, eval(env, lambda))
  (call $environment-add 
    (local.get $env) 
    (local.get $var) 
    (call $eval 
      (local.get $env) 
      (local.get $lambda)
    )
  )
  (return (global.get $g-nil))
)

(func $set! (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $value i32)

  (if (i32.eqz (call $list-of-n? (local.get $args) (i32.const 2))) 
    (then unreachable)
  )

  (local.set $name (%car-l $args))
  (%assert-symbol $name)
  (local.set $value (%car (%cdr-l $args)))

  (call $environment-set!
    (local.get $env) 
    (local.get $name)
    (call $eval (local.get $env) (local.get $value))
  )

  (return (global.get $g-nil))
)
 
(func $list-of-n? (param $list i32) (param $n i32) (result i32) 
  (loop $forever
    (if (i32.eqz (local.get $n))
      (then
        (return (i32.eq (%get-type $list) (%nil-type)))
      )
    )
    (if (i32.ne (%get-type $list) (%cons-type))
      (then (return (i32.const 0)))
    )

    (local.set $list (%cdr-l $list))

    (%dec $n)
    (br $forever)
  )
  unreachable
)