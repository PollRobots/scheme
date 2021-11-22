(func $if (param $env i32) (param $args i32) (result i32)
  (local $test i32)    ;; condition
  (local $consequent i32)   ;; true-branch
  (local $alternate i32)  ;; false-branch
  (local $temp i32)   ;; used for cdr, cddr

  ;; assert(args is cons)
  (%assert-cons $args)

  ;; test = car(args)
  (local.set $test (%car-l $args))
  ;; temp = cdr(args)
  (local.set $temp (%cdr-l $args))
  ;; assert(temp is cons)
  (%assert-cons $temp)
  ;; consequent = car(cdr(args))
  ;; consequent = car(temp)
  (local.set $consequent (%car-l $temp))
  ;; temp = cdr(temp)
  (local.set $temp (%cdr-l $temp))
  ;; if (typeof temp is nil)
  (if (i32.eq (%get-type $temp) (%nil-type))
    (then
      ;; alternate = nil
      (local.set $alternate (global.get $g-nil))
    )
    (else
      ;; assert(temp is cons)
      (%assert-cons $temp)
      ;; alternate = car(cdr(cdr(args)))
      ;; alternate = car(temp)
      (local.set $alternate (%car-l $temp))
      ;; temp = cdr(temp)
      (local.set $temp (%cdr-l $temp))
      ;; assert(temp is nil)
      (%assert-nil $temp)
    )
  )

  ;; return eval(env, eval(env, test)  ? consequent : alternate)

  (return
    (call $eval
      (local.get $env)
      (select
        (local.get $consequent)
        (local.get $alternate)
        (call $is-truthy (call $eval (local.get $env) (local.get $test)))
      )
    )
  )
)

