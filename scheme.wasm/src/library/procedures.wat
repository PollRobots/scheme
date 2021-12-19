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

  (return (%alloc-cont (call $cont-alloc
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
            (i32.const 0))))))

;; (cont-apply-internal proc args ...)
(func $cont-apply-internal (param $env i32) (param $args i32) (result i32)
  (return (call $apply-internal 
      (local.get $env) 
      (%car-l $args) 
      (%cdr-l $args))))
