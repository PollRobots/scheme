;; (procedure? <obj>)
(func $procedure? (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (return (select 
      (global.get $g-true)
      (global.get $g-false)
      (call $procedure?-impl (%car-l $args)))))


(func $procedure?-impl (param $obj i32) (result i32)
  (local $type i32)
  (local.set $type (%get-type $obj))

  (if (i32.eq (local.get $type) (%special-type)) (then (return (i32.const 1))))
  (if (i32.eq (local.get $type) (%builtin-type)) (then (return (i32.const 1))))
  (if (i32.eq (local.get $type) (%lambda-type)) (then (return (i32.const 1))))

  (return (i32.const 0)))

;; (apply <proc> <arg> ... <args>)
(func $proc-apply (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $proc i32)
  (local $curr i32)
  (local $head i32)
  (local $tail i32)

  (block $check (block $fail
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))

      (%pop-l $proc $temp)
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $proc))))

      ;; collect all except the last argument into a list
      (local.set $head (i32.const 0))
      (block $end (loop $start
          (%pop-l $curr $temp)
          ;; break out of the loop for the last item
          (%chk-type $end $temp %cons-type)

          (local.set $curr (%alloc-list-1 (local.get $curr)))
          (if (i32.eqz (local.get $head))
            (then (local.set $head (local.get $curr)))
            (else (%set-cdr!-l $tail $curr)))
          (local.set $tail (local.get $curr))

          (br $start)))

      ;; curr should now be the last argument
      (br_if $fail (i32.eqz (call $is-list-impl (local.get $curr))))
      (if (i32.eqz (local.get $head))
        (then (local.set $head (local.get $curr)))
        (else (%set-cdr!-l $tail $curr)))

      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (call $apply-internal 
      (local.get $env) 
      (local.get $proc)
      (local.get $head))))

;; (map <proc> <list> ...)
(func $map (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $proc i32)
  (local $zipped i32)
  (local $list-of-lists i32)
  (local $list i32)
  (local $curr i32)
  (local $zip-item-head i32)
  (local $zip-item-tail i32)

  (block $check (block $fail
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))

      (%pop-l $proc $temp)
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $proc))))

      (br_if $fail (i32.eqz (call $all-list? (local.get $temp))))

      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $zipped (global.get $g-nil))

  (local.set $list-of-lists (local.get $temp))

  (block $outer_end (loop $outer_start
      (local.set $temp (local.get $list-of-lists))
      (local.set $zip-item-head (i32.const 0))

      (block $inner_end (loop $inner_start
          (%chk-type $inner_end $temp %cons-type)

          (local.set $list (%car-l $temp))
          ;; done when any list is done
          (%chk-type $outer_end $list %cons-type)

          (%pop-l $curr $list)

          (local.set $curr (%alloc-list-1 (local.get $curr)))

          (if (i32.eqz (local.get $zip-item-head))
            (then (local.set $zip-item-tail 
                (local.tee $zip-item-head (local.get $curr))))
            (else (%set-cdr!-l $zip-item-tail $curr)))

          (local.set $zip-item-tail (local.get $curr))

          (%set-car!-l $temp $list)

          (local.set $temp (%cdr-l $temp))
          (br $inner_start)))

      (local.set $zipped (%alloc-cons 
          (local.get $zip-item-head) 
          (local.get $zipped)))

      (br $outer_start)))

  (if (i32.eq (local.get $zipped) (global.get $g-nil)) (then
    (return (local.get $zipped))))

  ;; zipped is a reversed list of tuples from the input lists, i.e.
  ;; if the call was (map proc '(a b c d) '(1 2 3 4))
  ;; zipped will now be ((d 4) (c 3) (b 2) (a 1))

  (return (call $cont-map 
      (local.get $env) 
      (%alloc-list-2 (local.get $proc) (local.get $zipped)))))

;; (cont-map proc zipped)
;; (cont-map map-res all-res proc zipped)
(func $cont-map (param $env i32) (param $args i32) (result i32)
  (local $map-res i32)
  (local $all-res i32)
  (local $proc i32)
  (local $zipped i32)

  (if (i32.eq (call $list-len (local.get $args)) (i32.const 4))
    (then
      (%pop-l $map-res $args)
      (if (i32.eq (%get-type $map-res) (%error-type)) (then
          (return (local.get $map-res))))
      (%pop-l $all-res $args)
      (local.set $all-res (%alloc-cons
          (local.get $map-res)
          (local.get $all-res))))
    (else (local.set $all-res (global.get $g-nil))))
  
  (%pop-l $proc $args)
  (%pop-l $zipped $args)

  (if (i32.eq (%get-type $zipped) (%nil-type)) (then 
      (return (local.get $all-res))))

  (%pop-l $args $zipped)

  (return (call $cont-alloc
        (%cont-apply-internal)
        (local.get $env)
        (%alloc-cons (local.get $proc) (local.get $args))
        (call $cont-alloc
          (%cont-map)
          (local.get $env)
          (%alloc-list-3 
            (local.get $all-res) 
            (local.get $proc) 
            (local.get $zipped))
            (i32.const 0)))))

;; (cont-apply-internal proc args ...)
(func $cont-apply-internal (param $env i32) (param $args i32) (result i32)
  (return (call $apply-internal 
      (local.get $env) 
      (%car-l $args) 
      (%cdr-l $args))))

;; (string-map <proc> <string_1> <string_2> ...)
(func $string-map (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $proc i32)
  (local $num-args i32)

  (block $check (block $fail
      (local.set $num-args (call $list-len (local.get $args)))
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))

      (%pop-l $proc $temp)
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $proc))))

      (br_if $fail (i32.eqz (call $all-string (local.get $temp))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (call $cont-alloc
      (%cont-string-map)
      (local.get $env)
      (local.get $args)
      (i32.const 0))))


;; (cont-string-map <proc> <string_1> <string_2> ...) 
;; (cont-string-map <map-res> <idx> <all-res> <proc> <string_1> <string_2> ...)
(func $cont-string-map (param $env i32) (param $args i32) (result i32)
  (local $strings i32)
  (local $proc i32)
  (local $all-res i32)
  (local $map-res i32)
  (local $map-res-type i32)
  (local $idx i32)
  (local $chars i32)
  (local $curr i32)
  (local $char i32)
  (local $temp64 i64)
  (local $tail i32)

  (local.set $idx (%car (%cdr-l $args)))

  (if (i32.eq (%get-type $idx) (%i64-type))
    (then
      (%pop-l $map-res $args)
      (local.set $map-res-type (%get-type $map-res))
      (if (i32.eq (local.get $map-res-type) (%error-type)) (then
          (return (local.get $map-res))))
      (if (i32.ne (local.get $map-res-type) (%char-type)) (then
          (return (%alloc-error (%sym-32 0x65707974 4) (local.get $map-res)))))

      (%pop-l $idx $args)
      (local.set $temp64 (i64.load offset=4 (local.get $idx)))
      (local.set $idx (i32.wrap_i64 (local.get $temp64)))
    
      (%pop-l $all-res $args)
      (%pop-l $proc $args)
      (local.set $strings (local.get $args))
      (local.set $all-res (%alloc-cons (local.get $map-res) (local.get $all-res)))
    )
    (else 
      (%pop-l $proc $args)
      (local.set $strings (local.get $args))
      (local.set $all-res (global.get $g-nil))
      (local.set $idx (i32.const 0))))


  (local.set $tail (local.tee $chars (%alloc-list-1 (local.get $proc))))

  (block $end (loop $start
      (br_if $end (i32.ne (%get-type $strings) (%cons-type)))  
      (%pop-l $curr $strings)

      (local.set $char (call $str-code-point-at (%car-l $curr) (local.get $idx)))
      (if (i32.eq (local.get $char) (i32.const -1)) (then
          (local.set $all-res (call $reverse-impl (local.get $all-res)))
          (return (call $list->string 
              (local.get $env) 
              (%alloc-list-1 (local.get $all-res))))))

      (local.set $char (%alloc-list-1 (%alloc-char (local.get $char))))
      (%set-cdr!-l $tail $char)
      (local.set $tail (local.get $char))

      (br $start)))

  (%inc $idx)

  (return (call $cont-alloc
      (%cont-apply-internal)
      (local.get $env)
      (local.get $chars)
      (call $cont-alloc
        (%cont-string-map)
        (local.get $env)
        (%alloc-cons
          (%alloc-i32 (local.get $idx))
          (%alloc-cons
            (local.get $all-res)
            (%alloc-cons
              (local.get $proc)
              (local.get $args))))
        (i32.const 0)))))
        
;; (vector-map <proc> <vector_1> <vector_2> ...)
(func $vector-map (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $proc i32)
  (local $num-args i32)

  (block $check (block $fail
      (local.set $num-args (call $list-len (local.get $args)))
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))

      (%pop-l $proc $temp)
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $proc))))

      (br_if $fail (i32.eqz (call $all-vector (local.get $temp))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (call $cont-alloc
      (%cont-vector-map)
      (local.get $env)
      (local.get $args)
      (i32.const 0))))


;; (cont-vector-map <proc> <vector_1> <vector_2> ...) 
;; (cont-vector-map <map-res> <idx> <all-res> <proc> <vetor_1> <vector_2> ...)
(func $cont-vector-map (param $env i32) (param $args i32) (result i32)
  (local $vectors i32)
  (local $proc i32)
  (local $all-res i32)
  (local $map-res i32)
  (local $idx i32)
  (local $els i32)
  (local $el i32)
  (local $curr i32)
  (local $temp64 i64)
  (local $tail i32)

  (local.set $idx (%car (%cdr-l $args)))

  (if (i32.eq (%get-type $idx) (%i64-type))
    (then
      (%pop-l $map-res $args)
      (if (i32.eq (%get-type $map-res) (%error-type)) (then
          (return (local.get $map-res))))
      (%pop-l $idx $args)
      (local.set $temp64 (i64.load offset=4 (local.get $idx)))
      (local.set $idx (i32.wrap_i64 (local.get $temp64)))
    
      (%pop-l $all-res $args)
      (%pop-l $proc $args)
      (local.set $vectors (local.get $args))
      (local.set $all-res (%alloc-cons (local.get $map-res) (local.get $all-res)))
    )
    (else 
      (%pop-l $proc $args)
      (local.set $vectors (local.get $args))
      (local.set $all-res (global.get $g-nil))
      (local.set $idx (i32.const 0))))


  (local.set $tail (local.tee $els (%alloc-list-1 (local.get $proc))))

  (block $end (loop $start
      (br_if $end (i32.ne (%get-type $vectors) (%cons-type)))  
      (%pop-l $curr $vectors)

      (if (i32.eq (local.get $idx) (%cdr-l $curr)) (then
          (local.set $all-res (call $reverse-impl (local.get $all-res)))
          (return (call $vector
              (local.get $env) 
              (local.get $all-res)))))

      (local.set $el (i32.load (i32.add (%car-l $curr) (%word-size-l $idx))))

      (local.set $el (%alloc-list-1 (local.get $el)))
      (%set-cdr!-l $tail $el)
      (local.set $tail (local.get $el))

      (br $start)))

  (%inc $idx)

  (return (call $cont-alloc
      (%cont-apply-internal)
      (local.get $env)
      (local.get $els)
      (call $cont-alloc
        (%cont-vector-map)
        (local.get $env)
        (%alloc-cons
          (%alloc-i32 (local.get $idx))
          (%alloc-cons
            (local.get $all-res)
            (%alloc-cons
              (local.get $proc)
              (local.get $args))))
        (i32.const 0)))))

;; (for-each <proc> <list> ...)
(func $for-each (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $proc i32)
  (local $zipped i32)
  (local $list-of-lists i32)
  (local $list i32)
  (local $curr i32)
  (local $zip-item-head i32)
  (local $zip-item-tail i32)

  (block $check (block $fail
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $temp (local.get $args))

      (%pop-l $proc $temp)
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $proc))))

      (br_if $fail (i32.eqz (call $all-list? (local.get $temp))))

      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $zipped (global.get $g-nil))

  (local.set $list-of-lists (local.get $temp))

  (block $outer_end (loop $outer_start
      (local.set $temp (local.get $list-of-lists))
      (local.set $zip-item-head (i32.const 0))

      (block $inner_end (loop $inner_start
          (%chk-type $inner_end $temp %cons-type)

          (local.set $list (%car-l $temp))
          ;; done when any list is done
          (%chk-type $outer_end $list %cons-type)

          (%pop-l $curr $list)

          (local.set $curr (%alloc-list-1 (local.get $curr)))

          (if (i32.eqz (local.get $zip-item-head))
            (then (local.set $zip-item-tail 
                (local.tee $zip-item-head (local.get $curr))))
            (else (%set-cdr!-l $zip-item-tail $curr)))

          (local.set $zip-item-tail (local.get $curr))

          (%set-car!-l $temp $list)

          (local.set $temp (%cdr-l $temp))
          (br $inner_start)))

      (local.set $zipped (%alloc-cons 
          (local.get $zip-item-head) 
          (local.get $zipped)))

      (br $outer_start)))

  (if (i32.eq (local.get $zipped) (global.get $g-nil)) (then
    (return (local.get $zipped))))

  ;; zipped is a reversed list of tuples from the input lists, i.e.
  ;; if the call was (map proc '(a b c d) '(1 2 3 4))
  ;; zipped will now be ((d 4) (c 3) (b 2) (a 1))

  (return (call $cont-for-each 
      (local.get $env) 
      (%alloc-list-2 
        (local.get $proc) 
        (call $reverse-impl (local.get $zipped))))))

;; (cont-for-each proc zipped)
;; (cont-for-each proc-res proc zipped)
(func $cont-for-each (param $env i32) (param $args i32) (result i32)
  (local $proc-res i32)
  (local $proc i32)
  (local $zipped i32)

  (if (i32.eq (call $list-len (local.get $args)) (i32.const 3)) (then
      (%pop-l $proc-res $args)
      (if (i32.eq (%get-type $proc-res) (%error-type)) (then
          (return (local.get $proc-res))))))
  
  (%pop-l $proc $args)
  (%pop-l $zipped $args)

  (if (i32.eq (%get-type $zipped) (%nil-type)) (then 
      (return (global.get $g-nil))))

  (%pop-l $args $zipped)

  (return (call $cont-alloc
        (%cont-apply-internal)
        (local.get $env)
        (%alloc-cons (local.get $proc) (local.get $args))
        (call $cont-alloc
          (%cont-for-each)
          (local.get $env)
          (%alloc-list-2 
            (local.get $proc) 
            (local.get $zipped))
            (i32.const 0)))))

;; (call-with-current-continuation <proc>)
;; (call/cc <proc>)
(func $call/cc (param $env i32) (param $args i32) (result i32)
  (local $proc i32)
  (local $cont-proc i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $proc (%car-l $args))
      (br_if $fail (i32.eqz (call $procedure?-impl (local.get $proc))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $cont-proc (call $heap-alloc
      (global.get $g-heap)
      (%cont-proc-type)
      (%cdr (local.get $env)) ;; env is actually the continuation
      (i32.const 0)))

  ;; extract env from continuation
  (local.set $env (i32.load offset=4 (%car-l $env)))

  (return (call $apply-internal 
      (local.get $env) 
      (local.get $proc) 
      (%alloc-list-1 (local.get $cont-proc)))))