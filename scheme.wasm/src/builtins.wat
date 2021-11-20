(type $builtin-type (func (param i32 i32) (result i32)))

(%define %builtin-add ()        (i32.const 0))
(%define %builtin-mult ()       (i32.const 1))
(%define %special-if ()         (i32.const 2))
(%define %special-let ()        (i32.const 3))
(%define %special-lambda ()     (i32.const 4))
(%define %special-quote ()      (i32.const 5))
(%define %special-define ()     (i32.const 6))
(%define %special-set! ()       (i32.const 7))
(%define %builtin-sub ()        (i32.const 8))
(table $table-builtin 9 anyfunc)

(global $lambda-sym (mut i32) (i32.const 0))
(global $quote-sym (mut i32) (i32.const 0))

(func $register-builtins (param $heap i32) (param $env i32)
  (local $quote i32)

  (call $environment-add
    (local.get $env)
    (%sym-32 0x2b 1) ;; '+'
    (call $heap-alloc (local.get $heap) (%builtin-type) (%builtin-add) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (%sym-32 0x2d 1) ;; '-'
    (call $heap-alloc (local.get $heap) (%builtin-type) (%builtin-sub) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (%sym-32 0x2a 1) ;; '*'
    (call $heap-alloc (local.get $heap) (%builtin-type) (%builtin-mult) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (%sym-32 0x6669 2) ;; 'if'
    (call $heap-alloc (local.get $heap) (%special-type) (%special-if) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (%sym-32 0x74656c 3) ;; 'let'
    (call $heap-alloc (local.get $heap) (%special-type) (%special-let) (i32.const 0))
  )

  (global.set $lambda-sym (%sym-64 0x6164626d616c 6)) ;; 'lambda'
  (call $environment-add
    (local.get $env)
    (global.get $lambda-sym)
    (call $heap-alloc (local.get $heap) (%special-type) (%special-lambda) (i32.const 0))
  )

  (local.set $quote (call $heap-alloc (local.get $heap) (%special-type) (%special-quote) (i32.const 0)))
  (global.set $quote-sym (%sym-64 0x65746f7571 5)) ;; 'quote'
  (call $environment-add
    (local.get $env)
    (global.get $quote-sym)
    (local.get $quote)
  )
  (call $environment-add
    (local.get $env)
    (%sym-32 0x27 1) ;; ' (0x27)
    (local.get $quote)
  )
  (call $environment-add
    (local.get $env)
    (%sym-64 0x656e69666564 6) ;; 'define'
    (call $heap-alloc (local.get $heap) (%special-type) (%special-define) (i32.const 0))
  )
  (call $environment-add
    (local.get $env)
    (%sym-32 0x21746573 4) ;; 'set!'
    (call $heap-alloc (local.get $heap) (%special-type) (%special-set!) (i32.const 0))
  )
)

(elem $table-builtin (%builtin-add) $add)
(elem $table-builtin (%builtin-sub) $sub)
(elem $table-builtin (%builtin-mult) $mult)
(elem $table-builtin (%special-if) $if)
(elem $table-builtin (%special-let) $let)
(elem $table-builtin (%special-lambda) $lambda)
(elem $table-builtin (%special-quote) $quote)
(elem $table-builtin (%special-define) $define)
(elem $table-builtin (%special-set!) $set!)