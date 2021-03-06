(%define %inc (%var) (local.set %var (i32.add (local.get %var) (i32.const 1))))

(%define %ginc (%var) (global.set %var (i32.add (global.get %var) (i32.const 1))))

(%define %inc64 (%var) (local.set %var (i64.add (local.get %var) (i64.const 1))))

(%define %dec (%var) (local.set %var (i32.sub (local.get %var) (i32.const 1))))

(%define %gdec (%var) (global.set %var (i32.sub (global.get %var) (i32.const 1))))

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
;;   vector = 15
;;   bytevector = 16
;;   cont(inuation) = 17
;;   big-int = 18
;;   except = 19
;;   cont-proc = 20
;;   syntax-rules = 21
;;   rational-type = 22
;;   complex-type = 23
;;   record-type = 24
;;   record-meta-type = 25
;;   record-method-type = 26
;;   case-lambda = 27
;;   port = 28
;;   eof = 29
;; kMaxType = 29

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
(%define %vector-type () (i32.const 15))
(%define %bytevector-type () (i32.const 16))
(%define %cont-type () (i32.const 17))
(%define %big-int-type () (i32.const 18))
(%define %except-type () (i32.const 19))
(%define %cont-proc-type () (i32.const 20))
(%define %syntax-rules-type () (i32.const 21))
(%define %rational-type () (i32.const 22))
(%define %complex-type () (i32.const 23))
(%define %record-type () (i32.const 24))
(%define %record-meta-type () (i32.const 25))
(%define %record-method-type () (i32.const 26))
(%define %case-lambda-type () (i32.const 27))
(%define %port-type () (i32.const 28))
(%define %eof-type () (i32.const 29))
(%define %max-heap-type () (i32.const 29))

(%define %get-type (%arg) (i32.and (i32.load8_u (local.get %arg)) (i32.const 0x1F)))
(%define %get-gc-flags (%arg) (i32.load8_u offset=1 (local.get %arg)))
(%define %set-gc-flags (%arg %flags) (i32.store8 offset=1 (local.get %arg) %flags))
(%define %get-flags (%arg) (i32.load8_u offset=2 (local.get %arg)))
(%define %set-flags (%arg %flags) (i32.store8 offset=2 (local.get %arg) %flags))
(%define %visited-flag () (i32.const 1))
(%define %visited-not-flag () (i32.const 0xFE))
(%define %immutable-flag () (i32.const 2))
(%define %port-input-flag ()   (i32.const 0x01))
(%define %port-output-flag ()  (i32.const 0x02))
(%define %port-text-flag ()    (i32.const 0x04))
(%define %port-open-flag ()    (i32.const 0x08))
(%define %port-string-flag ()  (i32.const 0x10))
(%define %port-bytevec-flag () (i32.const 0x20))

(%define %car (%cons) (i32.load offset=4 %cons))
(%define %car-l (%cons) (i32.load offset=4 (local.get %cons)))
(%define %car-g (%cons) (i32.load offset=4 (global.get %cons)))
(%define %cdr (%cons) (i32.load offset=8 %cons))
(%define %cdr-l (%cons) (i32.load offset=8 (local.get %cons)))
(%define %cdr-g (%cons) (i32.load offset=8 (global.get %cons)))
(%define %pop-l (%var %cons) (local.set %var (%car-l %cons)) (local.set %cons (%cdr-l %cons)))
(%define %push-l (%var %cons) (local.set %cons (%alloc-cons (local.get %var) (local.get %cons))))
(%define %push (%var %cons) (local.set %cons (%alloc-cons %var (local.get %cons))))
(%define %chk-type (%lbl %var %type) (br_if %lbl (i32.ne (%get-type %var) (%type))))
(%define %chk-type-ne (%lbl %var %type) (br_if %lbl (i32.eq (%get-type %var) (%type))))

(%define %set-car!-l (%cons %val) (i32.store offset=4 (local.get %cons) (local.get %val)))
(%define %set-car! (%cons %val) (i32.store offset=4 %cons %val))
(%define %set-cdr!-l (%cons %val) (i32.store offset=8 (local.get %cons) (local.get %val)))
(%define %set-cdr! (%cons %val) (i32.store offset=8 %cons %val))

(%define %word-size-l (%val) (i32.shl (local.get %val) (i32.const 2)))
(%define %word-size (%val) (i32.shl %val (i32.const 2)))

(%define %assert (%cond) (if (i32.eqz %cond) (then (unreachable))))

(%define %assert-cons (%arg) (%assert (i32.eq (%get-type %arg) (%cons-type))))

(%define %assert-nil (%arg) (%assert (i32.eq (%get-type %arg) (%nil-type))))

(%define %assert-num (%arg) (%assert (i32.eq (%get-type %arg) (%i64-type))))

(%define %assert-symbol (%arg) (%assert (i32.eq (%get-type %arg) (%symbol-type))))

(%define %assert-str (%arg) (%assert (i32.eq (%get-type %arg) (%str-type))))


(%define %sym-32 (%name %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-32 (i32.const %len) (i32.const %name)) (i32.const 0)))
(%define %sym-64 (%name %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-64 (i32.const %len) (i64.const %name)) (i32.const 0)))
(%define %sym-128 (%name1 %name2 %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-128 (i32.const %len) (i64.const %name1) (i64.const %name2)) (i32.const 0)))
(%define %sym-192 (%name1 %name2 %name3 %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-192 (i32.const %len) (i64.const %name1) (i64.const %name2) (i64.const %name3)) (i32.const 0)))
(%define %sym-256 (%name1 %name2 %name3 %name4 %len) (call $heap-alloc (global.get $g-heap) (%symbol-type) (call $str-from-256 (i32.const %len) (i64.const %name1) (i64.const %name2) (i64.const %name3) (i64.const %name4)) (i32.const 0)))

(%define %alloc-char (%val) (call $heap-alloc (global.get $g-heap) (%char-type) %val (i32.const 0)))
(%define %alloc-i32 (%val) (call $heap-alloc (global.get $g-heap) (%i64-type) %val (i32.const 0)))
(%define %alloc-rational (%num %denom) (call $heap-alloc (global.get $g-heap) (%rational-type) %num %denom))
(%define %alloc-complex (%real %imag) (call $heap-alloc (global.get $g-heap) (%complex-type) %real %imag))
(%define %alloc-big-int (%val) (call $heap-alloc (global.get $g-heap) (%big-int-type) %val (i32.const 0)))
(%define %alloc-i64 (%val) (call $heap-alloc (global.get $g-heap) (%i64-type) (i32.wrap_i64 %val) (i32.wrap_i64 (i64.shr_u %val (i64.const 32)))))
(%define %alloc-f64 (%val) (call $heap-alloc (global.get $g-heap) (%f64-type) (i32.wrap_i64 (i64.reinterpret_f64 %val)) (i32.wrap_i64 (i64.shr_u (i64.reinterpret_f64 %val) (i64.const 32)))))
(%define %alloc-cons (%car %cdr) (call $heap-alloc (global.get $g-heap) (%cons-type) %car %cdr))
(%define %alloc-list-1 (%v1) (%alloc-cons %v1 (global.get $g-nil)))
(%define %alloc-list-2 (%v1 %v2) (%alloc-cons %v1 (%alloc-list-1 %v2)))
(%define %alloc-list-3 (%v1 %v2 %v3) (%alloc-cons %v1 (%alloc-list-2 %v2 %v3)))
(%define %alloc-list-4 (%v1 %v2 %v3 %v4) (%alloc-cons %v1 (%alloc-list-3 %v2 %v3 %v4)))
(%define %alloc-str (%str) (call $heap-alloc (global.get $g-heap) (%str-type) %str (i32.const 0)))
(%define %alloc-symbol (%str) (call $heap-alloc (global.get $g-heap) (%symbol-type) %str (i32.const 0)))
(%define %alloc-error-cons (%sym %args) (call $heap-alloc (global.get $g-heap) (%error-type) %sym %args))
(%define %alloc-error (%sym %args) (call $heap-alloc (global.get $g-heap) (%error-type) %sym (%alloc-cons %args (global.get $g-nil))))
(%define %alloc-values (%car %cdr) (call $heap-alloc (global.get $g-heap) (%values-type) %car %cdr))
(%define %alloc-quote (%val) (%alloc-cons (global.get $g-quote) (%alloc-cons %val (global.get $g-nil))))
(%define %alloc-raise (%error) (call $heap-alloc (global.get $g-heap) (%except-type) %error (i32.const 1)))
(%define %alloc-raise-continuable (%error) (call $heap-alloc (global.get $g-heap) (%except-type) %error (i32.const 2)))
(%define %alloc-port (%fd %mode) (call $heap-alloc (global.get $g-heap) (%port-type) %fd %mode))


(%define %pack-64 (%hi %lo) (i64.or (i64.shl (i64.extend_i32_u %hi) (i64.const 32)) (i64.extend_i32_u %lo)))
(%define %pack-64-l (%hi %lo) (%pack-64 (local.get %hi) (local.get %lo)))
(%define %unpack-64-hi (%packed) (i32.wrap_i64 (i64.shr_u %packed (i64.const 32))))
(%define %unpack-64-hi-l (%packed) (%unpack-64-hi (local.get %packed)))
(%define %unpack-64-lo (%packed) (i32.wrap_i64 %packed))
(%define %unpack-64-lo-l (%packed) (%unpack-64-lo (local.get %packed)))
(%define %unpack-64-l (%packed %hi %lo)
  (local.set %hi (%unpack-64-hi-l %packed))
  (local.set %lo (%unpack-64-lo-l %packed)))


;; Trig constants

(%define %kPI ()        (f64.const 3.14159265358979323846))
(%define %kPIbyTwo ()   (f64.const 1.57079632679489661923))
(%define %kPIbyFour ()  (f64.const 0.78539816339744830962))
(%define %kTwoPI ()     (f64.const 6.28318530717958647693))
