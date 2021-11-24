(%define %inc (%var) (local.set %var (i32.add (local.get %var) (i32.const 1))))

(%define %ginc (%var) (global.set %var (i32.add (global.get %var) (i32.const 1))))

(%define %inc64 (%var) (local.set %var (i64.add (local.get %var) (i64.const 1))))

(%define %dec (%var) (local.set %var (i32.sub (local.get %var) (i32.const 1))))

(%define %dec64 (%var) (local.set %var (i64.sub (local.get %var) (i64.const 1))))

(%define %plus-eq (%var %amt) (local.set %var (i32.add (local.get %var) (i32.const %amt))))

(%define %plus-eq64 (%var %amt) (local.set %var (i64.add (local.get %var) (i64.const %amt))))

(%define %minus-eq (%var %amt) (local.set %var (i32.sub (local.get %var) (i32.const %amt))))

(%define %minus-eq64 (%var %amt) (local.set %var (i64.sub (local.get %var) (i64.const %amt))))

;; Type - enum
;;   empty = 0
;;   nil = 1
;;   boolean = 2
;;   cons = 3
;;   i64 = 4
;;   f64 = 5
;;   symbol = 6
;;   str = 7
;;   char = 8
;;   env = 9
;;   special = 10
;;   builtin = 11
;;   lambda = 12
;;   error = 13
;;   values = 14
;; kMaxType = 14
(%define %empty-type () (i32.const 0))
(%define %nil-type () (i32.const 1))
(%define %boolean-type () (i32.const 2))
(%define %cons-type () (i32.const 3))
(%define %i64-type () (i32.const 4))
(%define %f64-type () (i32.const 5))
(%define %symbol-type () (i32.const 6))
(%define %str-type () (i32.const 7))
(%define %char-type () (i32.const 8))
(%define %env-type () (i32.const 9))
(%define %special-type () (i32.const 10))
(%define %builtin-type () (i32.const 11))
(%define %lambda-type () (i32.const 12))
(%define %error-type () (i32.const 13))
(%define %values-type () (i32.const 14))
(%define %max-heap-type () (i32.const 14))

(%define %get-type (%arg) (i32.and (i32.load (local.get %arg)) (i32.const 0xF)))

(%define %car (%cons) (i32.load offset=4 %cons))
(%define %car-l (%cons) (i32.load offset=4 (local.get %cons)))
(%define %car-g (%cons) (i32.load offset=4 (global.get %cons)))
(%define %cdr (%cons) (i32.load offset=8 %cons))
(%define %cdr-l (%cons) (i32.load offset=8 (local.get %cons)))
(%define %cdr-g (%cons) (i32.load offset=8 (global.get %cons)))

(%define %set-car!-l (%cons %val) (i32.store offset=4 (local.get %cons) (local.get %val)))
(%define %set-cdr!-l (%cons %val) (i32.store offset=8 (local.get %cons) (local.get %val)))

(%define %assert (%cond) (if (i32.eqz %cond) (then unreachable)))

(%define %assert-cons (%arg) (%assert (i32.eq (%get-type %arg) (%cons-type))))

(%define %assert-nil (%arg) (%assert (i32.eq (%get-type %arg) (%nil-type))))

(%define %assert-num (%arg) (%assert (i32.eq (%get-type %arg) (%i64-type))))

(%define %assert-symbol (%arg) (%assert (i32.eq (%get-type %arg) (%symbol-type))))

(%define %assert-str (%arg) (%assert (i32.eq (%get-type %arg) (%str-type))))

(%define %sym-32 (%name %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-32 (i32.const %len) (i32.const %name)) (i32.const 0)))
(%define %sym-64 (%name %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-64 (i32.const %len) (i64.const %name)) (i32.const 0)))
(%define %sym-128 (%name1 %name2 %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-128 (i32.const %len) (i64.const %name1) (i64.const %name2)) (i32.const 0)))
(%define %sym-192 (%name1 %name2 %name3 %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-192 (i32.const %len) (i64.const %name1) (i64.const %name2) (i64.const %name3)) (i32.const 0)))

(%define %alloc-char (%val) (call $heap-alloc (global.get $g-heap) (%char-type) %val (i32.const 0)))
(%define %alloc-i32 (%val) (call $heap-alloc (global.get $g-heap) (%i64-type) %val (i32.const 0)))
(%define %alloc-i64 (%val) (call $heap-alloc (global.get $g-heap) (%i64-type) (i32.wrap_i64 %val) (i32.wrap_i64 (i64.shr_u %val (i64.const 32)))))
(%define %alloc-cons (%car %cdr) (call $heap-alloc (global.get $g-heap) (%cons-type) %car %cdr))
(%define %alloc-str (%str) (call $heap-alloc (global.get $g-heap) (%str-type) %str (i32.const 0)))
(%define %alloc-symbol (%str) (call $heap-alloc (global.get $g-heap) (%symbol-type) %str (i32.const 0)))
(%define %alloc-error-cons (%sym %args) (call $heap-alloc (global.get $g-heap) (%error-type) %sym %args))
(%define %alloc-error (%sym %args) (call $heap-alloc (global.get $g-heap) (%error-type) %sym (%alloc-cons %args (global.get $g-nil))))
(%define %alloc-values (%car %cdr) (call $heap-alloc (global.get $g-heap) (%values-type) %car %cdr))
(%define %alloc-quote (%val) (%alloc-cons (global.get $quote-sym) (%alloc-cons %val (global.get $g-nil))))
