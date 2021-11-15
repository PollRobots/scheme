(func $if (param $env i32) (param $args i32) (result i32)
  (local $car i32)    ;; condition
  (local $cdar i32)   ;; true-branch
  (local $cddar i32)  ;; false-branch
  (local $temp i32)   ;; used for cdr, cddr

  ;; assert(args is cons)
  (%assert-cons $args)

  ;; car = args[4]
  (local.set $car (i32.load offset=4 (local.get $args)))
  ;; cdr = args[8]
  ;; temp = args[8]
  (local.set $temp (i32.load offset=8 (local.get $args)))
  ;; assert(cdr is cons)
  (%assert-cons $temp)
  ;; cdar = cdr[4]
  (local.set $cdar (i32.load offset=4 (local.get $temp)))
  ;; cddr = cdr[8]
  ;; temp = cdr[8]
  (local.set $temp (i32.load offset=8 (local.get $temp)))
  ;; assert (cddr is cons)
  (%assert-cons $temp)
  ;; cddar = cddr[4]
  (local.set $cddar (i32.load offset=4 (local.get $temp)))
  ;; cdddr = cddr[8]
  ;; temp = cddr[8]
  (local.set $temp (i32.load offset=8 (local.get $temp)))
  ;; assert(cdddr is nil)
  (%assert-nil $temp)

  ;; cond = eval(env, car)
  ;; return  eval(env, is-truthy(cond) ? cdar, cddar)
  ;;  point-free ->
  ;;    return eval(env, is-truthy(eval(env, car)) ? cdar, cddar)
  (return
    (call $eval
      (local.get $env)
      (select 
        (local.get $cdar)
        (local.get $cddar)
        (call $eval (local.get $env) (local.get $car))
      )
    )
  )
)

