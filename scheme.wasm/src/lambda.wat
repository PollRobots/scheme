(func $lambda (param $env i32) (param $args i32) (result i32)
  (if (i32.eqz (call $valid-formals? (%car-l $args))) (then
      (return (call $argument-error (local.get $args)))))

  ;; return heap-alloc(heap, lambda-type, env, args)
  (return (call $heap-alloc
      (global.get $g-heap)
      (%lambda-type)
      (local.get $env)
      (local.get $args))))

;; checks that formals is a valid formals list. 
;; It must be either
;; A Symbol
;; A well formed list of Symbols
;; A list of symbols with a dotted terminator that is a symbol.
(func $valid-formals? (param $formals i32) (result i32)
  (local $f-type i32)
  (local $f-val i32)

  (block $end (loop $start
      ;; f-type = get-type(formals)
      (local.set $f-type (%get-type $formals))
      ;; if (f-type == symbol) break
      (br_if $end (i32.eq (local.get $f-type) (%symbol-type)))
      ;; if (f-type == nil) break
      (br_if $end (i32.eq (local.get $f-type) (%nil-type)))
      ;; if (f-type != cons) return 0
      (if (i32.ne (local.get $f-type) (%cons-type)) (then 
          (return (i32.const 0))))

      (%pop-l $f-val $formals) 

      ;; if (fval not symbol) return 0
      (if (i32.ne (%get-type $f-val) (%symbol-type)) (then
          (return (i32.const 0))))

      (br $start)))

  (return (i32.const 1)))

;; (case-lambda <clause> ...)
(func $case-lambda (param $env i32) (param $args i32) (result i32)
  (local $clauses i32)
  (local $clause i32)

  (block $check (block $fail
      (br_if $fail (i32.eqz (call $all-list? (local.get $args))))

      ;; check that each clause starts with a valid formals list (same syntax
      ;; as for lambda)
      (local.set $clauses (local.get $args))
      (block $end (loop $start
          (br_if $end (i32.eq (local.get $clauses) (global.get $g-nil)))
          (%pop-l $clause $clauses)
          (br_if $fail (i32.eqz (call $valid-formals? (%car-l $clause))))
          (br $start)))

      (br $check))

    (return (call $argument-error (local.get $args))))

  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%case-lambda-type)
      (local.get $env)
      (local.get $args)))
)