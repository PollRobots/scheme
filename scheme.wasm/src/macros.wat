(%define %inc (%var) (local.set %var (i32.add (local.get %var) (i32.const 1))))

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
