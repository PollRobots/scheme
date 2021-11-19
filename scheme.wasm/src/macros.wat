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
;; kMaxType = 12
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

(%define %get-type (%arg) (i32.and (i32.load (local.get %arg)) (i32.const 0xF)))

(%define %car (%cons) (i32.load offset=4 %cons))
(%define %car-l (%cons) (i32.load offset=4 (local.get %cons)))
(%define %cdr (%cons) (i32.load offset=8 %cons))
(%define %cdr-l (%cons) (i32.load offset=8 (local.get %cons)))

(%define %assert (%cond) (if (i32.eqz %cond) (then unreachable)))

(%define %assert-cons (%arg) (%assert (i32.eq (%get-type %arg) (%cons-type))))

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

(%define %sym-32 (%name %len) (call $heap-alloc (local.get $heap) (%symbol-type) (call $str-from-32 (i32.const %len) (i32.const %name)) (i32.const 0)))
(%define %sym-64 (%name %len) (call $heap-alloc (local.get $heap) (%symbol-type) (call $str-from-64 (i32.const %len) (i64.const %name)) (i32.const 0)))

(%define %alloc-cons (%car %cdr) (call $heap-alloc (global.get $g-heap) (%cons-type) %car %cdr))
