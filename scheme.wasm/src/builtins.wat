(type $builtin-type (func (param i32 i32) (result i32)))

(%define %builtin-add () (i32.const 0))
(%define %builtin-mult () (i32.const 1))

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
)

(%define %assert-cons (%arg)
  ;; if ((%arg & 0xF) != %cons-type)
  (if
    (i32.ne 
      (i32.and 
        (i32.load (local.get %arg)) 
        (i32.const 0xF)
      ) 
      (%cons-type)
    ) 
    ;; TODO: return error
    (then unreachable)
  )
)

(%define %assert-num (%arg)
  ;; if ((%arg & 0xF) != %i64-type)
  (if
    (i32.ne 
      (i32.and 
        (i32.load (local.get %arg)) 
        (i32.const 0xF)
      ) 
      (%i64-type)
    ) 
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

(table $table-builtin 2 anyfunc)