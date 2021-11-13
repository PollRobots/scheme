(type $builtin-type (func (param i32 i32) (result i32)))

(%define %builtin-add ()  (i32.const 0))
(%define %builtin-mult () (i32.const 1))
(%define %special-if ()   (i32.const 2))
(%define %special-let ()  (i32.const 3))
(table $table-builtin 4 anyfunc)

(func $register-builtins (param $heap i32) (param $env i32)
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-32 (i32.const 1) (i32.const 0x2b)) ;; '+'
      (i32.const 0)
    )
    (call $heap-alloc (local.get $heap) (%builtin-type) (%builtin-add) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-32 (i32.const 1) (i32.const 0x2a)) ;; '*'
      (i32.const 0)
    )
    (call $heap-alloc (local.get $heap) (%builtin-type) (%builtin-mult) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-32 (i32.const 2) (i32.const 0x6669)) ;; 'if'
      (i32.const 0)
    )
    (call $heap-alloc (local.get $heap) (%special-type) (%special-if) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-32 (i32.const 3) (i32.const 0x74656c)) ;; 'let'
      (i32.const 0)
    )
    (call $heap-alloc (local.get $heap) (%special-type) (%special-let) (i32.const 0))
  )
)

(%define %assert-cons (%arg)
  ;; if ((%arg & 0xF) != %cons-type)
  (if (i32.ne (%get-type %arg) (%cons-type)) 
    ;; TODO: return error
    (then unreachable)
  )
)

(%define %assert-nil (%arg)
  ;; if ((%arg & 0xF) != %nil-type)
  (if (i32.ne (%get-type %arg) (%nil-type)) 
    ;; TODO: return error
    (then unreachable)
  )
)

(%define %assert-num (%arg)
  ;; if ((%arg & 0xF) != %i64-type)
  (if (i32.ne (%get-type %arg) (%i64-type)) 
    ;; TODO: return error
    (then unreachable)
  )
)

(%define %assert-symbol (%arg)
  ;; if ((%arg & 0xF) != %symbol-type)
  (if (i32.ne (%get-type %arg) (%symbol-type)) 
    ;; TODO: return error
    (then unreachable)
  )
)

(func $add (param $env i32) (param $args i32) (result i32)
  (local $accum i64)
  (local $car i32)

  ;; accum = 0
  (local.set $accum (i64.const 0))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end
    (loop $b_start
      (br_if $b_end 
        (i32.eq 
          (i32.and (i32.load (local.get $args)) (i32.const 0xf)) 
          (%nil-type)
        )
      )
      ;; assert(args is cons)
      (%assert-cons $args)
      ;; car = args[4]
      (local.set $car (i32.load offset=4 (local.get $args)))
      ;; assert(car is num)
      (%assert-num $car)
      ;; accum += car[4]
      (local.set $accum 
        (i64.add 
          (local.get $accum) 
          (i64.load offset=4 (local.get $car))
        )
      )
      ;; args = args[8]
      (local.set $args (i32.load offset=8 (local.get $args)))
      (br $b_start)
    )
  ;; }
  )

  ;; return heap-alloc(g-heap, (%i64-type), (i32)accum, (i32)(accum >> 32))
  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%i64-type)
      (i32.wrap_i64 (local.get $accum))
      (i32.wrap_i64 (i64.shr_u (local.get $accum) (i64.const 32)))
    )
  )
)

(elem $table-builtin (%builtin-add) $add)

(func $mult (param $env i32) (param $args i32) (result i32)
  (local $accum i64)
  (local $car i32)

  ;; accum = 0
  (local.set $accum (i64.const 1))

  ;; while ( (*args & 0x0f) != nil-type) {
  (block $b_end
    (loop $b_start
      (br_if $b_end 
        (i32.eq 
          (i32.and (i32.load (local.get $args)) (i32.const 0xf)) 
          (%nil-type)
        )
      )
      ;; assert(args is cons)
      (%assert-cons $args)
      ;; car = args[4]
      (local.set $car (i32.load offset=4 (local.get $args)))
      ;; assert(car is num)
      (%assert-num $car)
      ;; accum *= car[4]
      (local.set $accum 
        (i64.mul 
          (local.get $accum) 
          (i64.load offset=4 (local.get $car))
        )
      )
      ;; args = args[8]
      (local.set $args (i32.load offset=8 (local.get $args)))
      (br $b_start)
    )
  ;; }
  )

  ;; return heap-alloc(g-heap, (%i64-type), (i32)accum, (i32)(accum >> 32))
  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%i64-type)
      (i32.wrap_i64 (local.get $accum))
      (i32.wrap_i64 (i64.shr_u (local.get $accum) (i64.const 32)))
    )
  )
)

(elem $table-builtin (%builtin-mult) $mult)

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

(elem $table-builtin (%special-if) $if)


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

(elem $table-builtin (%special-let) $let)
