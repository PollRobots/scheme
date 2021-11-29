(type $builtin-type (func (param i32 i32) (result i32)))

(%define %builtin-add ()                      (i32.const 0))
(%define %builtin-mult ()                     (i32.const 1))
(%define %special-if ()                       (i32.const 2))
(%define %special-let ()                      (i32.const 3))
(%define %special-lambda ()                   (i32.const 4))
(%define %special-quote ()                    (i32.const 5))
(%define %special-define ()                   (i32.const 6))
(%define %special-set! ()                     (i32.const 7))
(%define %builtin-sub ()                      (i32.const 8))
(%define %builtin-integer? ()                 (i32.const 9))
(%define %builtin-num-equal ()                (i32.const 10))
(%define %builtin-num-lt ()                   (i32.const 11))
(%define %builtin-num-le ()                   (i32.const 12))
(%define %builtin-num-gt ()                   (i32.const 13))
(%define %builtin-num-ge ()                   (i32.const 14))
(%define %builtin-num-abs ()                  (i32.const 15))
(%define %builtin-num-truncate-quotient ()    (i32.const 16))
(%define %builtin-num-truncate-remainder ()   (i32.const 17))
(%define %builtin-num-floor-quotient ()       (i32.const 18))
(%define %builtin-num-floor-remainder ()      (i32.const 19))
(%define %builtin-num-floor/ ()               (i32.const 20))
(%define %builtin-num-truncate/ ()            (i32.const 21))
(%define %builtin-num-exact-integer-sqrt ()   (i32.const 22))
(%define %builtin-num-string->number ()       (i32.const 23))
(%define %builtin-num-number->string ()       (i32.const 24))
(%define %builtin-bool-not ()                 (i32.const 25))
(%define %builtin-bool-boolean? ()            (i32.const 26))
(%define %builtin-bool-boolean=? ()           (i32.const 27))
(%define %builtin-pair? ()                    (i32.const 28))
(%define %builtin-pair-cons ()                (i32.const 29))
(%define %builtin-pair-car ()                 (i32.const 30))
(%define %builtin-pair-cdr ()                 (i32.const 31))
(%define %builtin-pair-set-car! ()            (i32.const 32))
(%define %builtin-pair-set-cdr! ()            (i32.const 33))
(%define %builtin-null? ()                    (i32.const 34))
(%define %builtin-list? ()                    (i32.const 35))
(%define %builtin-make-list ()                (i32.const 36))
(%define %builtin-list ()                     (i32.const 37))
(%define %builtin-length ()                   (i32.const 38))
(%define %builtin-eq? ()                      (i32.const 39))
(%define %builtin-eqv? ()                     (i32.const 40))
(%define %builtin-equal? ()                   (i32.const 41))
(%define %builtin-append ()                   (i32.const 42))
(%define %builtin-reverse ()                  (i32.const 43))
(%define %builtin-list-tail ()                (i32.const 44))
(%define %builtin-list-ref ()                 (i32.const 45))
(%define %builtin-list-set! ()                (i32.const 46))
(%define %builtin-memq ()                     (i32.const 47))
(%define %builtin-memv ()                     (i32.const 48))
(%define %builtin-member ()                   (i32.const 49))
(%define %builtin-assq ()                     (i32.const 50))
(%define %builtin-assv ()                     (i32.const 51))
(%define %builtin-assoc ()                    (i32.const 52))
(%define %builtin-list-copy ()                (i32.const 53))
(%define %builtin-symbol? ()                  (i32.const 54))
(%define %builtin-symbol=? ()                 (i32.const 55))
(%define %builtin-symbol->string ()           (i32.const 56))
(%define %builtin-string->symbol ()           (i32.const 57))
(%define %builtin-exit ()                     (i32.const 58))
(%define %builtin-char? ()                    (i32.const 59))
(%define %builtin-char-equal ()               (i32.const 60))
(%define %builtin-char-lt ()                  (i32.const 61))
(%define %builtin-char-le ()                  (i32.const 62))
(%define %builtin-char-gt ()                  (i32.const 63))
(%define %builtin-char-ge ()                  (i32.const 64))
(%define %builtin-char-alphabetic? ()         (i32.const 65))
(%define %builtin-char-numeric? ()            (i32.const 66))
(%define %builtin-char-whitespace? ()         (i32.const 67))
(%define %builtin-char-upper-case? ()         (i32.const 68))
(%define %builtin-char-lower-case? ()         (i32.const 69))
(%define %builtin-digit-value ()              (i32.const 70))
(%define %builtin-char->integer ()            (i32.const 71))
(%define %builtin-integer->char ()            (i32.const 72))
(%define %builtin-char-upcase ()              (i32.const 73))
(%define %builtin-char-downcase ()            (i32.const 74))
(%define %builtin-char-equal-ci ()            (i32.const 75))
(%define %builtin-char-lt-ci ()               (i32.const 76))
(%define %builtin-char-le-ci ()               (i32.const 77))
(%define %builtin-char-gt-ci ()               (i32.const 78))
(%define %builtin-char-ge-ci ()               (i32.const 79))
(%define %builtin-string? ()                  (i32.const 80))
(%define %builtin-make-string ()              (i32.const 81))
(%define %builtin-string ()                   (i32.const 82))
(%define %builtin-string-length ()            (i32.const 83))
(%define %builtin-string-ref ()               (i32.const 84))
(%define %builtin-string-set! ()              (i32.const 85))
(%define %builtin-string=? ()                 (i32.const 86))
(%define %builtin-string-ci=? ()              (i32.const 87))
(%define %builtin-string>? ()                 (i32.const 88))
(%define %builtin-string-ci>? ()              (i32.const 89))
(%define %builtin-string>=? ()                (i32.const 90))
(%define %builtin-string-ci>=? ()             (i32.const 91))
(%define %builtin-string<? ()                 (i32.const 92))
(%define %builtin-string-ci<? ()              (i32.const 93))
(%define %builtin-string<=? ()                (i32.const 94))
(%define %builtin-string-ci<=? ()             (i32.const 95))
(%define %builtin-string-upcase ()            (i32.const 96))
(%define %builtin-string-downcase ()          (i32.const 97))
(%define %builtin-substring ()                (i32.const 98))
(%define %builtin-string-copy ()              (i32.const 99))
(%define %builtin-string-list ()              (i32.const 100))

(table $table-builtin 101 anyfunc)

(global $lambda-sym (mut i32) (i32.const 0))
(global $quote-sym (mut i32) (i32.const 0))

(func $register-builtins (param $heap i32) (param $env i32)
  (local $quote i32)

  (%define %add-builtin (%sym %num)
    (call $environment-add 
      (local.get $env)
      %sym 
      (call $heap-alloc (local.get $heap) (%builtin-type) %num %sym))
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
  (%add-builtin (%sym-32 0x746f6e 3) (%builtin-bool-not))
  (%add-builtin (%sym-64 0x3f6e61656c6f6f62 8) (%builtin-bool-boolean?))
  (%add-builtin (%sym-128 0x3d6e61656c6f6f62 0x3f 9) (%builtin-bool-boolean=?))
  (%add-builtin (%sym-64 0x3f72696170 5) (%builtin-pair?)) ;; pair?
  (%add-builtin (%sym-32 0x736e6f63 4) (%builtin-pair-cons)) ;; cons
  (%add-builtin (%sym-32 0x726163 3) (%builtin-pair-car)) ;; car
  (%add-builtin (%sym-32 0x726463 3) (%builtin-pair-cdr)) ;; cdr
  (%add-builtin (%sym-64 0x217261632d746573 8) (%builtin-pair-set-car!)) ;; set-car!
  (%add-builtin (%sym-64 0x217264632d746573 8) (%builtin-pair-set-cdr!)) ;; set-cdr!
  (%add-builtin (%sym-64 0x3f6c6c756e 5) (%builtin-null?)) ;; null?
  (%add-builtin (%sym-64 0x3f7473696c 5) (%builtin-list?)) ;; list?
  (%add-builtin (%sym-128 0x73696c2d656b616d 0x74 9) (%builtin-make-list)) ;; make-list
  (%add-builtin (%sym-32 0x7473696c 4) (%builtin-list)) ;; list
  (%add-builtin (%sym-64 0x6874676e656c 6) (%builtin-length)) ;; length
  (%add-builtin (%sym-32 0x3f7165 3) (%builtin-eq?)) ;; eq?
  (%add-builtin (%sym-32 0x3f767165 4) (%builtin-eqv?)) ;; eqv?
  (%add-builtin (%sym-64 0x3f6c61757165 6) (%builtin-equal?)) ;; equal?
  (%add-builtin (%sym-64 0x646e65707061 6) (%builtin-append)) ;; append
  (%add-builtin (%sym-64 0x65737265766572 7) (%builtin-reverse)) ;; reverse
  (%add-builtin (%sym-128 0x6961742d7473696c 0x6c 9) (%builtin-list-tail)) ;; list-tail
  (%add-builtin (%sym-64 0x6665722d7473696c 8) (%builtin-list-ref)) ;; list-ref
  (%add-builtin (%sym-128 0x7465732d7473696c 0x21 9) (%builtin-list-set!)) ;; list-set!
  (%add-builtin (%sym-32 0x716d656d 4) (%builtin-memq)) ;; memq
  (%add-builtin (%sym-32 0x766d656d 4) (%builtin-memv)) ;; memv
  (%add-builtin (%sym-64 0x7265626d656d 6) (%builtin-member)) ;; member
  (%add-builtin (%sym-32 0x71737361 4) (%builtin-assq)) ;; assq
  (%add-builtin (%sym-32 0x76737361 4) (%builtin-assv)) ;; assv
  (%add-builtin (%sym-64 0x636f737361 5) (%builtin-assoc)) ;; assoc
  (%add-builtin (%sym-128 0x706f632d7473696c 0x79 9) (%builtin-list-copy)) ;; list-copy
  (%add-builtin (%sym-64 0x3f6c6f626d7973 7) (%builtin-symbol?)) ;; symbol?
  (%add-builtin (%sym-64 0x3f3d6c6f626d7973 8) (%builtin-symbol=?)) ;; symbol=?
  (%add-builtin (%sym-128 0x3e2d6c6f626d7973 0x676e69727473 14) (%builtin-symbol->string)) ;; symbol->string
  (%add-builtin (%sym-128 0x3e2d676e69727473 0x6c6f626d7973 14) (%builtin-string->symbol)) ;; symbol->string
  (%add-builtin (%sym-32 0x74697865 4) (%builtin-exit)) ;; exit
  (%add-builtin (%sym-64 0x3f72616863 5) (%builtin-char?)) ;; char?
  (%add-builtin (%sym-64 0x3f3d72616863  6) (%builtin-char-equal)) ;; 'char=?'
  (%add-builtin (%sym-64 0x3f3C72616863  6) (%builtin-char-lt)) ;; 'char<?'
  (%add-builtin (%sym-64 0x3f3D3C72616863  7) (%builtin-char-le)) ;; 'char<=?'
  (%add-builtin (%sym-64 0x3f3E72616863  6) (%builtin-char-gt)) ;; 'char>?'
  (%add-builtin (%sym-64 0x3f3D3E72616863  7) (%builtin-char-ge)) ;; 'char>=?'
  (%add-builtin (%sym-128 0x706c612d72616863 0x3f63697465626168 16) (%builtin-char-alphabetic?)) ;; 'char-alphabetic?'
  (%add-builtin (%sym-128 0x6d756e2d72616863 0x3f63697265 13) (%builtin-char-numeric?)) ;; 'char-numeric?'
  (%add-builtin (%sym-128 0x6968772d72616863 0x3f65636170736574 16) (%builtin-char-whitespace?)) ;; 'char-whitespace?'
  (%add-builtin (%sym-128 0x7070752d72616863 0x3f657361632d7265 16) (%builtin-char-upper-case?)) ;; 'char-upper-case?'
  (%add-builtin (%sym-128 0x776f6c2d72616863 0x3f657361632d7265 16) (%builtin-char-lower-case?)) ;; 'char-lower-case?'
  (%add-builtin (%sym-128 0x61762d7469676964 0x65756c 11) (%builtin-digit-value)) ;; 'digit-value'
  (%add-builtin (%sym-128 0x6e693e2d72616863 0x7265676574 13) (%builtin-char->integer)) ;; 'char->integer'
  (%add-builtin (%sym-128 0x2d72656765746e69 0x726168633e 13) (%builtin-integer->char)) ;; 'integer->char'
  (%add-builtin (%sym-128 0x6370752d72616863 0x657361 11) (%builtin-char-upcase)) ;; 'char-upcase'
  (%add-builtin (%sym-128 0x776f642d72616863 0x657361636e 13) (%builtin-char-downcase)) ;; 'char-downcase'
  (%add-builtin (%sym-128 0x6c6f662d72616863 0x6573616364 13) (%builtin-char-downcase)) ;; 'char-foldcase'
  (%add-builtin (%sym-128 0x3d69632d72616863 0x3f 9) (%builtin-char-equal-ci)) ;; 'char-ci=?'
  (%add-builtin (%sym-128 0x3C69632d72616863 0x3f 9) (%builtin-char-lt-ci)) ;; 'char-ci<?'
  (%add-builtin (%sym-128 0x3C69632D72616863 0x3f3D 10) (%builtin-char-le-ci)) ;; 'char-ci<=?'
  (%add-builtin (%sym-128 0x3E69632D72616863 0x3f 9) (%builtin-char-gt-ci)) ;; 'char-ci>?'
  (%add-builtin (%sym-128 0x3E69632D72616863 0x3f3D 10) (%builtin-char-ge-ci)) ;; 'char-ci>=?'
  (%add-builtin (%sym-64 0x3f676e69727473 7) (%builtin-string?)) ;; 'string?'
  (%add-builtin (%sym-128 0x7274732d656b616d 0x676e69 11) (%builtin-make-string)) ;; 'make-string'
  (%add-builtin (%sym-64 0x676e69727473 6) (%builtin-string)) ;; 'string'
  (%add-builtin (%sym-128 0x6c2d676e69727473 0x6874676e65 13) (%builtin-string-length)) ;; 'string-length'
  (%add-builtin (%sym-128 0x722d676e69727473 0x6665 10) (%builtin-string-ref)) ;; 'string-ref'
  (%add-builtin (%sym-128 0x732d676e69727473 0x217465 11) (%builtin-string-set!)) ;; 'string-set!'
  (%add-builtin (%sym-64 0x3f3d676e69727473 8) (%builtin-string=?)) ;; 'string=?'
  (%add-builtin (%sym-128 0x632d676e69727473 0x3f3d69 11) (%builtin-string-ci=?)) ;; 'string-ci=?'
  (%add-builtin (%sym-64 0x3f3e676e69727473 8) (%builtin-string>?)) ;; 'string>?'
  (%add-builtin (%sym-128 0x632d676e69727473 0x3f3e69 11) (%builtin-string-ci>?)) ;; 'string-ci>?'
  (%add-builtin (%sym-128 0x3d3e676e69727473 0x3f 9) (%builtin-string>=?)) ;; 'string>=?'
  (%add-builtin (%sym-128 0x632d676e69727473 0x3f3d3e69 12) (%builtin-string-ci>=?)) ;; 'string-ci>=?'
  (%add-builtin (%sym-64 0x3f3c676e69727473 8) (%builtin-string<?)) ;; 'string<?'
  (%add-builtin (%sym-128 0x632d676e69727473 0x3f3c69 11) (%builtin-string-ci<?)) ;; 'string-ci<?'
  (%add-builtin (%sym-128 0x3c3e676e69727473 0x3f3e 9) (%builtin-string<=?)) ;; 'string<=?'
  (%add-builtin (%sym-128 0x632d676e69727473 0x3f3e3c69 12) (%builtin-string-ci<=?)) ;; 'string-ci<=?'
  (%add-builtin (%sym-128 0x752D676e69727473 0x6573616370 13) (%builtin-string-upcase)) ;; 'string-upcase'
  (%add-builtin (%sym-128 0x642D676e69727473 0x657361636e776f 15) (%builtin-string-downcase)) ;; 'string-downcase'
  (%add-builtin (%sym-128 0x662D676e69727473 0x65736163646c6f 15) (%builtin-string-downcase)) ;; 'string-foldcase'
  (%add-builtin (%sym-128 0x6e69727473627573 0x67 9) (%builtin-substring)) ;; 'substring'
  (%add-builtin (%sym-128 0x632D676e69727473 0x79706f 11) (%builtin-string-copy)) ;; 'string-copy'
  (%add-builtin (%sym-128 0x6c2D676e69727473 0x747369 11) (%builtin-string-list)) ;; 'string-list'

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
(elem $table-builtin (%builtin-bool-not) $bool-not)
(elem $table-builtin (%builtin-bool-boolean?) $bool-boolean?)
(elem $table-builtin (%builtin-bool-boolean=?) $bool-boolean=?)
(elem $table-builtin (%builtin-pair?) $pair?)
(elem $table-builtin (%builtin-pair-cons) $pair-cons)
(elem $table-builtin (%builtin-pair-car) $pair-car)
(elem $table-builtin (%builtin-pair-cdr) $pair-cdr)
(elem $table-builtin (%builtin-pair-set-car!) $pair-set-car!)
(elem $table-builtin (%builtin-pair-set-cdr!) $pair-set-cdr!)
(elem $table-builtin (%builtin-null?) $null?)
(elem $table-builtin (%builtin-list?) $list?)
(elem $table-builtin (%builtin-make-list) $make-list)
(elem $table-builtin (%builtin-list) $list)
(elem $table-builtin (%builtin-length) $length)
(elem $table-builtin (%builtin-eq?) $eq?)
(elem $table-builtin (%builtin-eqv?) $eqv?)
(elem $table-builtin (%builtin-equal?) $equal?)
(elem $table-builtin (%builtin-append) $append)
(elem $table-builtin (%builtin-reverse) $reverse)
(elem $table-builtin (%builtin-list-tail) $list-tail)
(elem $table-builtin (%builtin-list-ref) $list-ref)
(elem $table-builtin (%builtin-list-set!) $list-set!)
(elem $table-builtin (%builtin-memq) $memq)
(elem $table-builtin (%builtin-memv) $memv)
(elem $table-builtin (%builtin-member) $member)
(elem $table-builtin (%builtin-assq) $assq)
(elem $table-builtin (%builtin-assv) $assv)
(elem $table-builtin (%builtin-assoc) $assoc)
(elem $table-builtin (%builtin-list-copy) $list-copy)
(elem $table-builtin (%builtin-symbol?) $symbol?)
(elem $table-builtin (%builtin-symbol=?) $symbol=?)
(elem $table-builtin (%builtin-symbol->string) $symbol->string)
(elem $table-builtin (%builtin-string->symbol) $string->symbol)
(elem $table-builtin (%builtin-exit) $exit)
(elem $table-builtin (%builtin-char?) $char?)
(elem $table-builtin (%builtin-char-equal) $char-equal)
(elem $table-builtin (%builtin-char-gt) $char-gt)
(elem $table-builtin (%builtin-char-ge) $char-ge)
(elem $table-builtin (%builtin-char-lt) $char-lt)
(elem $table-builtin (%builtin-char-le) $char-le)
(elem $table-builtin (%builtin-char-alphabetic?) $char-alphabetic?)
(elem $table-builtin (%builtin-char-numeric?) $char-numeric?)
(elem $table-builtin (%builtin-char-whitespace?) $char-whitespace?)
(elem $table-builtin (%builtin-char-upper-case?) $char-upper-case?)
(elem $table-builtin (%builtin-char-lower-case?) $char-lower-case?)
(elem $table-builtin (%builtin-digit-value) $digit-value)
(elem $table-builtin (%builtin-char->integer) $char->integer)
(elem $table-builtin (%builtin-integer->char) $integer->char)
(elem $table-builtin (%builtin-char-upcase) $char-upcase)
(elem $table-builtin (%builtin-char-downcase) $char-downcase)
(elem $table-builtin (%builtin-char-equal-ci) $char-equal-ci)
(elem $table-builtin (%builtin-char-gt-ci) $char-gt-ci)
(elem $table-builtin (%builtin-char-ge-ci) $char-ge-ci)
(elem $table-builtin (%builtin-char-lt-ci) $char-lt-ci)
(elem $table-builtin (%builtin-char-le-ci) $char-le-ci)
(elem $table-builtin (%builtin-string?) $string?)
(elem $table-builtin (%builtin-make-string) $make-string)
(elem $table-builtin (%builtin-string) $string)
(elem $table-builtin (%builtin-string-length) $string-length)
(elem $table-builtin (%builtin-string-ref) $string-ref)
(elem $table-builtin (%builtin-string-set!) $string-set!)
(elem $table-builtin (%builtin-string=?) $string=?)
(elem $table-builtin (%builtin-string-ci=?) $string-ci=?)
(elem $table-builtin (%builtin-string>?) $string>?)
(elem $table-builtin (%builtin-string-ci>?) $string-ci>?)
(elem $table-builtin (%builtin-string>=?) $string>=?)
(elem $table-builtin (%builtin-string-ci>=?) $string-ci>=?)
(elem $table-builtin (%builtin-string<?) $string<?)
(elem $table-builtin (%builtin-string-ci<?) $string-ci<?)
(elem $table-builtin (%builtin-string<=?) $string<=?)
(elem $table-builtin (%builtin-string-ci<=?) $string-ci<=?)
(elem $table-builtin (%builtin-string-upcase) $string-upcase)
(elem $table-builtin (%builtin-string-downcase) $string-downcase)
(elem $table-builtin (%builtin-substring) $substring)
(elem $table-builtin (%builtin-string-copy) $string-copy)
(elem $table-builtin (%builtin-string-list) $string-list)