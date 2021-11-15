(func $let (param $env i32) (param $args i32) (result i32)
  (local $bindings i32)
  (local $temp i32)
  (local $body i32)
  (local $child-env i32)
  (local $binding i32)
  (local $var i32)
  (local $init i32)
  (local $result i32)

  ;; bindings = car(args)
  (local.set $bindings (%car-l $args))
  ;; assert(bindings is cons)
  (%assert-cons $bindings)
  ;; temp = cdr(args)
  (local.set $temp (%cdr-l $args))
  ;; assert(temp is cons)
  (%assert-cons $temp)
  ;; body = temp
  (local.set $body (local.get $temp))

  ;; child-env = environment-init(g-heap, env)
  (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $env)))

  ;; while (typeof bindings is cons) {
  (block $w_end
    (loop $w_start
      (br_if $w_end (i32.ne (%get-type $bindings) (%cons-type)))

      ;; binding = car(bindings)
      (local.set $binding (%car-l $bindings))
      ;; assert(binding is cons)
      (%assert-cons $binding)
      ;; var = car(binding)
      (local.set $var (%car-l $binding))
      ;; assert(var is symbol)
      (%assert-symbol $var)
      ;; temp = cdr(binding)
      (local.set $temp (%cdr-l $binding))
      ;; assert(temp is cons)
      (%assert-cons $temp)
      ;; init = car(temp)
      (local.set $init (%car-l $temp))
      ;; temp = cdr(cons)
      (local.set $temp (%cdr-l $temp))
      ;; assert(temp is nil)
      (%assert-nil $temp)
      ;; environment-add(child-env, var, eval(env, init))
      (call $environment-add
        (local.get $child-env) 
        (local.get $var)
        (call $eval (local.get $env) (local.get $init))
      )
      ;; bindings = cdr(bindings)
      (local.set $bindings (%cdr-l $bindings))

      (br $w_start)
    )
    ;; }
  )

  ;; result = g-nil
  (local.set $result (global.get $g-nil))

  ;; while (typeof body is cons) {
  (block $ww_end
    (loop $ww_start
      (br_if $ww_end (i32.ne (%get-type $body) (%cons-type)))

      ;; result = eval(child-env, car(body))
      (local.set $result (call $eval (local.get $child-env) (%car-l $body)))
      ;; body = cdr(body)
      (local.set $body (%cdr-l $body))
      
      (br $ww_start)
    )
    ;; }
  )

  ;; return result
  (return (local.get $result))
)