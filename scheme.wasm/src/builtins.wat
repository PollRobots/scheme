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
(%define %builtin-integer? ()   (i32.const 9))
(%define %builtin-num-equal ()  (i32.const 10))
(%define %builtin-num-lt ()     (i32.const 11))
(%define %builtin-num-le ()     (i32.const 12))
(%define %builtin-num-gt ()     (i32.const 13))
(%define %builtin-num-ge ()     (i32.const 14))
(%define %builtin-num-abs ()    (i32.const 15))
(%define %builtin-num-truncate-quotient ()    (i32.const 16))
(%define %builtin-num-truncate-remainder ()   (i32.const 17))
(%define %builtin-num-floor-quotient ()       (i32.const 18))
(%define %builtin-num-floor-remainder ()      (i32.const 19))
(%define %builtin-num-floor/ ()               (i32.const 20))
(%define %builtin-num-truncate/ ()            (i32.const 21))
(%define %builtin-num-exact-integer-sqrt ()   (i32.const 22))
(%define %builtin-num-string->number ()       (i32.const 23))
(%define %builtin-num-number->string ()        (i32.const 24))
(table $table-builtin 25 anyfunc)

(global $lambda-sym (mut i32) (i32.const 0))
(global $quote-sym (mut i32) (i32.const 0))

(func $register-builtins (param $heap i32) (param $env i32)
  (local $quote i32)

  (%define %add-builtin (%sym %num)
    (call $environment-add 
      (local.get $env)
      %sym 
      (call $heap-alloc (local.get $heap) (%builtin-type) %num (i32.const 0)))
  )

  (%add-builtin (%sym-32 0x2B 1) (%builtin-add)) ;; '+'
  (%add-builtin (%sym-32 0x2D 1) (%builtin-sub)) ;; '-'
  (%add-builtin (%sym-32 0x2A 1) (%builtin-mult)) ;; '*'
  (%add-builtin (%sym-64 0x3f72656765746e69 8) (%builtin-integer?))
  (%add-builtin (%sym-32 0x3d 1) (%builtin-num-equal)) ;; '='
  (%add-builtin (%sym-32 0x3C 1) (%builtin-num-lt)) ;; '<'
  (%add-builtin (%sym-32 0x3D3C 2) (%builtin-num-le)) ;; '<='
  (%add-builtin (%sym-32 0x3E 1) (%builtin-num-gt)) ;; '>'
  (%add-builtin (%sym-32 0x3D3E 2) (%builtin-num-ge)) ;; '>='
  (%add-builtin (%sym-32 0x736261 3) (%builtin-num-abs))
  (%add-builtin (%sym-64 0x746e6569746f7571 8) (%builtin-num-truncate-quotient)) ;; 'quotient'
  (%add-builtin (%sym-192 0x657461636e757274 0x6e6569746f75712d 0x74 17) (%builtin-num-truncate-quotient)) ;; '
  (%add-builtin (%sym-128 0x65646e69616d6572 0x72 9) (%builtin-num-truncate-remainder)) ;; 'remainder'
  (%add-builtin (%sym-192 0x657461636e757274 0x646e69616d65722d 0x7265 18) (%builtin-num-truncate-remainder)) ;; '
  (%add-builtin (%sym-128 0x75712d726f6f6c66 0x746e6569746f 14) (%builtin-num-floor-quotient)) ;; 'floor-quotient'
  (%add-builtin (%sym-128 0x65722d726f6f6c66 0x7265646e69616d  15) (%builtin-num-floor-remainder)) ;; 'floor-remainder'
  (%add-builtin (%sym-64 0x2F726f6f6c66 6) (%builtin-num-floor/)) ;; 'floor/'
  (%add-builtin (%sym-128 0x657461636e757274 0x2F 9) (%builtin-num-truncate/)) ;; 'truncate/'
  (%add-builtin (%sym-192 0x6e692D7463617865 0x71732D7265676574 0x7472 18) (%builtin-num-exact-integer-sqrt)) ;; '
  (%add-builtin (%sym-128 0x3e2d676e69727473 0x7265626d756e 14 ) (%builtin-num-string->number)) ;; 'string->number'
  (%add-builtin (%sym-128 0x3e2d7265626d756e 0x676e69727473 14 ) (%builtin-num-number->string)) ;; 'number->string'

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

(elem $table-builtin (%special-if) $if)
(elem $table-builtin (%special-let) $let)
(elem $table-builtin (%special-lambda) $lambda)
(elem $table-builtin (%special-quote) $quote)
(elem $table-builtin (%special-define) $define)
(elem $table-builtin (%special-set!) $set!)

;; numerics
(elem $table-builtin (%builtin-add) $num-add)
(elem $table-builtin (%builtin-sub) $num-sub)
(elem $table-builtin (%builtin-mult) $num-mul)
(elem $table-builtin (%builtin-integer?) $integer?)
(elem $table-builtin (%builtin-num-equal) $num-equal)
(elem $table-builtin (%builtin-num-gt) $num-gt)
(elem $table-builtin (%builtin-num-ge) $num-ge)
(elem $table-builtin (%builtin-num-lt) $num-lt)
(elem $table-builtin (%builtin-num-le) $num-le)
(elem $table-builtin (%builtin-num-abs) $num-abs)
(elem $table-builtin (%builtin-num-truncate-quotient) $num-truncate-quotient)
(elem $table-builtin (%builtin-num-truncate-remainder) $num-truncate-remainder)
(elem $table-builtin (%builtin-num-floor-quotient) $num-floor-quotient)
(elem $table-builtin (%builtin-num-floor-remainder) $num-floor-remainder)
(elem $table-builtin (%builtin-num-floor/) $num-floor/)
(elem $table-builtin (%builtin-num-truncate/) $num-truncate/)
(elem $table-builtin (%builtin-num-exact-integer-sqrt) $num-exact-integer-sqrt)
(elem $table-builtin (%builtin-num-string->number) $num-string->number)
(elem $table-builtin (%builtin-num-number->string) $num-number->string)