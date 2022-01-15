(func $lambda (param $env i32) (param $args i32) (result i32)
  (local $formals i32)
  (local $f-type i32)
  (local $f-val i32)

  ;; formals = car(args)
  (local.set $formals (%car-l $args))

  ;; while (true) {
  (block $b_end
    (loop $b_start
      ;; f-type = get-type(formals)
      (local.set $f-type (%get-type $formals))
      ;; if (f-type == symbol) break
      (br_if $b_end (i32.eq (local.get $f-type) (%symbol-type)))
      ;; if (f-type == nil) break
      (br_if $b_end (i32.eq (local.get $f-type) (%nil-type)))
      ;; if (f-type != cons) trap
      (if (i32.ne (local.get $f-type) (%cons-type))
        (then (unreachable))
      )

      ;; fval = car(formals)
      (local.set $f-val (%car-l $formals))
      ;; assert(fval is symbol)
      (%assert-symbol $f-val)
      ;; formals = cdr(formals)
      (local.set $formals (%cdr-l $formals))

      (br $b_start)
    )
    ;; }
  )

  ;; return heap-alloc(heap, lambda-type, env, args)
  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%lambda-type)
      (local.get $env)
      (local.get $args)
    )
  )
)
