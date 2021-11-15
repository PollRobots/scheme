(type $builtin-type (func (param i32 i32) (result i32)))

(%define %builtin-add ()      (i32.const 0))
(%define %builtin-mult ()     (i32.const 1))
(%define %special-if ()       (i32.const 2))
(%define %special-let ()      (i32.const 3))
(%define %special-lambda ()   (i32.const 4))
(%define %special-quote ()    (i32.const 5))
(table $table-builtin 6 anyfunc)

(func $register-builtins (param $heap i32) (param $env i32)
  (local $quote i32)

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
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-64 (i32.const 6) (i64.const 0x6164626d616c)) ;; 'lambda'
      (i32.const 0)
    )
    (call $heap-alloc (local.get $heap) (%special-type) (%special-lambda) (i32.const 0))
  )
  (local.set $quote (call $heap-alloc (local.get $heap) (%special-type) (%special-quote) (i32.const 0)))
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-64 (i32.const 5) (i64.const 0x65746f7571)) ;; 'quote'
      (i32.const 0)
    )
    (local.get $quote)
  )
  (call $environment-add
    (local.get $env)
    (call $heap-alloc 
      (local.get $heap) 
      (%symbol-type) 
      (call $str-from-64 (i32.const 1) (i64.const 0x27)) ;; ' (0x27)
      (i32.const 0)
    )
    (local.get $quote)
  )
)

(elem $table-builtin (%builtin-add) $add)
(elem $table-builtin (%builtin-mult) $mult)
(elem $table-builtin (%special-if) $if)
(elem $table-builtin (%special-let) $let)
(elem $table-builtin (%special-lambda) $lambda)
(elem $table-builtin (%special-quote) $quote)