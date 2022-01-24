(func $pair? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $arg (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $arg) (%cons-type))
    )
  )
)

(func $pair-cons (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (return
    (%alloc-cons
      (%car-l $args)
      (%car (%cdr-l $args))
    )
  )
)

(func $pair-car (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%cons-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (%car-l $arg))
)

(func $pair-cdr (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%cons-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (%cdr-l $arg))
)

(func $pair-set-car! (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $val i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%cons-type)))
      (br_if $b_fail (i32.and (%get-flags $arg) (i32.const 2)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $val (%car (%cdr-l $args)))
  (i32.store offset=4 (local.get $arg) (local.get $val))

  (return (global.get $g-nil))
)

(func $pair-set-cdr! (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $val i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%cons-type)))
      (br_if $b_fail (i32.and (%get-flags $arg) (i32.const 2)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $val (%car (%cdr-l $args)))
  (i32.store offset=8 (local.get $arg) (local.get $val))

  (return (global.get $g-nil))
)

(func $null? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $arg (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $arg) (%nil-type))
    )
  )
)

(func $is-list-impl (param $arg i32) (result i32)
  (local $type i32)
  (local $flags i32)
  (local $result i32)
  (local $head i32)

  (local.set $head (local.get $arg))

  ;; check for list-ness and circularity
  (block $b_end (loop $b_start
      (local.set $type (%get-type $arg))
      (if (i32.eq (local.get $type) (%nil-type)) (then
        ;; this is a list
        (local.set $result (i32.const 1))
        (br $b_end)))

      (if (i32.ne (local.get $type) (%cons-type)) (then
          ;; this isn't a list
          (local.set $result (i32.const 0))
          (br $b_end)))

      (local.set $flags (%get-flags $arg))
      (if (i32.and (local.get $flags) (i32.const 1)) (then
        ;; this list is circular
        (local.set $result (i32.const 0))
        (br $b_end)))

      ;; mark the list item as visited
      (%set-flags $arg (i32.or (local.get $flags) (i32.const 1)))

      (local.set $arg (%cdr-l $arg))
      (br $b_start)))

  ;; clear circularity flags
  (local.set $arg (local.get $head))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.ne (%get-type $arg) (%cons-type)))

      (local.set $flags (%get-flags $arg))
      (br_if $b_end (i32.eqz (i32.and (local.get $flags) (i32.const 1))))
      (%set-flags $arg (i32.and (local.get $flags) (i32.const 0xFE)))

      (local.set $arg (%cdr-l $arg))
      (br $b_start)))

  (return (local.get $result)))

(func $all-list? (param $args i32) (result i32)
  (local $temp i32)

  (block $done (loop $forever
      (%chk-type $done $args %cons-type)

      (%pop-l $temp $args)

      (if (i32.eqz (call $is-list-impl (local.get $temp))) (then
          (return (i32.const 0))))

      (br $forever)))

  (return (i32.const 1)))

(func $list? (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (call $is-list-impl (%car-l $args)))))

(func $make-list (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $count-arg i32)
  (local $count i64)
  (local $fill i32)
  (local $list i32)

  (local.set $num-args (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.eqz (local.get $num-args)))
      (br_if $b_fail (i32.gt_u (local.get $num-args) (i32.const 2)))

      (local.set $count-arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $count-arg) (%i64-type)))
      (local.set $count (i64.load offset=4 (local.get $count-arg)))
      (br_if $b_fail (i64.lt_s (local.get $count) (i64.const 0)))

      (if (i32.eq (local.get $num-args) (i32.const 2))
        (then
          (local.set $fill (%car (%cdr-l $args)))
        )
        (else
          (local.set $fill (global.get $g-nil))
        )
      )

      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )


  (local.set $list (global.get $g-nil))
  (block $b_end
    (loop $b_start
      (br_if $b_end (i64.eqz (local.get $count)))

      (local.set $list (%alloc-cons (local.get $fill) (local.get $list)))

      (%dec64 $count)
      (br $b_start)
    )
  )
  (return (local.get $list))
)

(func $list (param $env i32) (param $args i32) (result i32)
  (return (local.get $args)))

(func $values (param $env i32) (param $args i32) (result i32)
  (return (%alloc-values (%car-l $args) (%cdr-l $args))))

(func $length (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $arg-type i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (local.set $arg-type (%get-type $args))
      (br_if $b_check (i32.ne (local.get $arg-type) (%cons-type)))
      (br_if $b_check (i32.ne (local.get $arg-type) (%nil-type)))
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (%alloc-i32 (call $list-len (local.get $arg))))
)

(func $append (param $env i32) (param $args i32) (result i32)
  (local $rev-args i32)
  (local $appended i32)
  (local $curr i32)
  (local $curr-type i32)
  (local $head i32)
  (local $tail i32)
  (local $temp i32)

  (if (i32.eqz (call $list-len (local.get $args)))
    (then (return (call $argument-error (local.get $args))))
  )

  ;; reverse the args
  (local.set $rev-args (global.get $g-nil))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (local.set $rev-args (%alloc-cons (%car-l $args) (local.get $rev-args)))
      (local.set $args (%cdr-l $args))

      (br $b_start)
    )
  )

  (local.set $appended (%car-l $rev-args))
  (local.set $rev-args (%cdr-l $rev-args))

  ;; loop over reversed args
  (block $b_ap_end
    (loop $b_ap_start
      (br_if $b_ap_end (i32.eq (%get-type $rev-args) (%nil-type)))

      ;; current arg
      (local.set $curr (%car-l $rev-args))
      ;; loop over current arg
      (local.set $head (i32.const 0))
      (local.set $tail (i32.const 0))
      (block $b_in_end
        (loop $b_in_start
          (local.set $curr-type (%get-type $curr))
          (br_if $b_in_end (i32.eq (local.get $curr-type) (%nil-type)))
          (if (i32.ne (local.get $curr-type) (%cons-type))
            (then (return (call $argument-error (local.get $args))))
          )

          (local.set $temp (%alloc-cons (%car-l $curr) (global.get $g-nil)))
          (if (local.get $head)
            (then
              (%set-cdr!-l $tail $temp)
              (local.set $tail (local.get $temp))
            )
            (else
              (local.set $head (local.tee $tail (local.get $temp)))
            )
          )

          (local.set $curr (%cdr-l $curr))
          (br $b_in_start)
        )
      )
      (if (local.get $head)
        (then
          (%set-cdr!-l $tail $appended)
          (local.set $appended (local.get $head))
        )
      )

      (local.set $rev-args (%cdr-l $rev-args))
      (br $b_ap_start)
    )
  )

  (return (local.get $appended))
)

(func $reverse (param $env i32) (param $args i32) (result i32)
  (local $head i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $head (%car-l $args))
      (br_if $b_fail (i32.eqz (call $is-list-impl (local.get $head))))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (return (call $reverse-impl (local.get $head))))

(func $reverse-impl (param $list i32) (result i32)
  (local $rev i32)
  (local $head i32)

  (local.set $rev (global.get $g-nil))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $list) (%nil-type)))

      (%pop-l $head $list)
      (%push-l $head $rev)
      (br $b_start)))

  (return (local.get $rev)))

(func $list-tail-impl (param $args i32) (param $obj i32) (param $k i32) (result i32)
  (local $count i64)

  (local.set $count (i64.load offset=4 (local.get $k)))
  (block $b_end
    (loop $b_start
      (br_if $b_end (i64.eqz (local.get $count)))

      (if (i32.ne (%get-type $obj) (%cons-type))
        (then (return (call $argument-error (local.get $args)))))

      (local.set $obj (%cdr-l $obj))
      (%dec64 $count)
      (br $b_start)))

  (return (local.get $obj)))

(func $list-tail (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $k i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $obj (%car-l $args))
      (local.set $k (%car (%cdr-l $args)))
      (br_if $b_fail (i32.ne (%get-type $k) (%i64-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $list-tail-impl (local.get $args) (local.get $obj) (local.get $k)))
)

(func $list-ref (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $k i32)
  (local $tail i32)

  (block $b_check (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $obj (%car-l $args))
      (local.set $k (%car (%cdr-l $args)))
      (br_if $b_fail (i32.ne (%get-type $k) (%i64-type)))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $tail (call $list-tail-impl (local.get $args) (local.get $obj) (local.get $k)))
  (if (i32.eq (%get-type $tail) (%except-type)) (then
    (return (local.get $tail))))

  (return (%car-l $tail)))

(func $list-set! (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $k i32)
  (local $val i32)
  (local $tail i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 3)))
      (local.set $obj (%car-l $args))
      (local.set $k (%car (%cdr-l $args)))
      (br_if $b_fail (i32.ne (%get-type $k) (%i64-type)))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $tail (call $list-tail-impl (local.get $args) (local.get $obj) (local.get $k)))
  (if (i32.eq (%get-type $tail) (%except-type)) (then
      (return (local.get $tail))))

  (if (i32.and (%get-flags $tail) (i32.const 2)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $val (%car (%cdr (%cdr-l $args))))
  (%set-car!-l $tail $val)

  (return (local.get $obj)))

(func $memq (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $list i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $obj (%car-l $args))
  (local.set $list (%car (%cdr-l $args)))

  (block $b_end
    (loop $b_start
      (if (i32.eq (%get-type $list) (%nil-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (%get-type $list) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (br_if $b_end (i32.eq (%car-l $list) (local.get $obj)))

      (local.set $list (%cdr-l $list))
      (br $b_start)
    )
  )

  (return (local.get $list))
)

(func $memv (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $list i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $obj (%car-l $args))
  (local.set $list (%car (%cdr-l $args)))

  (block $b_end
    (loop $b_start
      (if (i32.eq (%get-type $list) (%nil-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (%get-type $list) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (br_if $b_end (call $equal-inner (%car-l $list) (local.get $obj) (i32.const 0)))

      (local.set $list (%cdr-l $list))
      (br $b_start)
    )
  )

  (return (local.get $list))
)

(func $member (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $obj i32)
  (local $list i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $cmp-args i32)

  (local.set $num-args (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 2)))
      (br_if $b_fail (i32.gt_u (local.get $num-args) (i32.const 3)))
      (if (i32.eq (local.get $num-args) (i32.const 3))
        (then
          (local.set $cmp (%car (%cdr (%cdr-l $args))))
          (local.set $cmp-type (%get-type $cmp))
          (br_if $b_check (i32.eq (local.get $cmp-type) (%builtin-type)))
          (br_if $b_check (i32.eq (local.get $cmp-type) (%special-type)))
          (br_if $b_check (i32.eq (local.get $cmp-type) (%lambda-type)))
          (br $b_fail)
        )
        (else (local.set $cmp (i32.const 0)))
      )
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $obj (%car-l $args))
  (local.set $list (%car (%cdr-l $args)))

  (block $b_end
    (loop $b_start
      (if (i32.eq (%get-type $list) (%nil-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (%get-type $list) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (if (i32.eqz (local.get $cmp))
        (then
          (br_if $b_end
            (call $equal-inner (%car-l $list) (local.get $obj) (i32.const 1))
          )
        )
        (else
          (local.set $cmp-args
            (%alloc-cons
              (%car-l $list)
              (%alloc-cons (local.get $obj) (global.get $g-nil))
            )
          )
          (br_if $b_end
            (i32.ne
              (global.get $g-false)
              (call $apply-internal
                (local.get $env)
                (local.get $cmp)
                (local.get $cmp-args)
              )
            )
          )
        )
      )

      (local.set $list (%cdr-l $list))
      (br $b_start)
    )
  )

  (return (local.get $list))
)

(func $assq (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $list i32)
  (local $item i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $obj (%car-l $args))
  (local.set $list (%car (%cdr-l $args)))

  (block $b_end
    (loop $b_start
      (if (i32.eq (%get-type $list) (%nil-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (%get-type $list) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (local.set $item (%car-l $list))
      (if (i32.ne (%get-type $item) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (br_if $b_end (i32.eq (%car-l $item) (local.get $obj)))

      (local.set $list (%cdr-l $list))
      (br $b_start)
    )
  )

  (return (%car-l $list))
)

(func $assv (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $list i32)
  (local $item i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $obj (%car-l $args))
  (local.set $list (%car (%cdr-l $args)))

  (block $b_end
    (loop $b_start
      (if (i32.eq (%get-type $list) (%nil-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (%get-type $list) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (local.set $item (%car-l $list))
      (if (i32.ne (%get-type $item) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (br_if $b_end (call $equal-inner (%car-l $item) (local.get $obj) (i32.const 0)))

      (local.set $list (%cdr-l $list))
      (br $b_start)
    )
  )

  (return (%car-l $list))
)

(func $assoc (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $obj i32)
  (local $list i32)
  (local $item i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $cmp-args i32)

  (local.set $num-args (call $list-len (local.get $args)))

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 2)))
      (br_if $b_fail (i32.gt_u (local.get $num-args) (i32.const 3)))
      (if (i32.eq (local.get $num-args) (i32.const 3))
        (then
          (local.set $cmp (%car (%cdr (%cdr-l $args))))
          (local.set $cmp-type (%get-type $cmp))
          (br_if $b_check (i32.eq (local.get $cmp-type) (%builtin-type)))
          (br_if $b_check (i32.eq (local.get $cmp-type) (%special-type)))
          (br_if $b_check (i32.eq (local.get $cmp-type) (%lambda-type)))
          (br $b_fail)
        )
        (else (local.set $cmp (i32.const 0)))
      )
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $obj (%car-l $args))
  (local.set $list (%car (%cdr-l $args)))

  (block $b_end
    (loop $b_start
      (if (i32.eq (%get-type $list) (%nil-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (%get-type $list) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (local.set $item (%car-l $list))
      (if (i32.ne (%get-type $item) (%cons-type))
        (then (return (call $argument-error (local.get $args))))
      )

      (if (i32.eqz (local.get $cmp))
        (then
          (br_if $b_end
            (call $equal-inner (%car-l $item) (local.get $obj) (i32.const 1))
          )
        )
        (else
          (local.set $cmp-args
            (%alloc-cons
              (%car-l $item)
              (%alloc-cons (local.get $obj) (global.get $g-nil))
            )
          )
          (br_if $b_end
            (i32.ne
              (global.get $g-false)
              (call $apply-internal
                (local.get $env)
                (local.get $cmp)
                (local.get $cmp-args)
              )
            )
          )
        )
      )

      (local.set $list (%cdr-l $list))
      (br $b_start)
    )
  )

  (return (%car-l $list))
)

(func $list-copy (param $env i32) (param $args i32) (result i32)
  (local $head i32)
  (local $tail i32)
  (local $arg i32)
  (local $temp i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $arg (%car-l $args))


  (local.set $tail
    (local.tee $head
      (%alloc-cons
        (global.get $g-nil)
        (global.get $g-nil)
      )
    )
  )

  (block $b_end
    (loop $b_start
      (if (i32.ne (%get-type $arg) (%cons-type))
        (then
          (%set-cdr!-l $tail $arg)
          (br $b_end)
        )
      )

      (local.set $temp (%alloc-cons (%car-l $arg) (global.get $g-nil)))
      (%set-cdr!-l $tail $temp)

      (local.set $tail (%cdr-l $tail))
      (local.set $arg (%cdr-l $arg))
      (br $b_start)
    )
  )

  (return (%cdr-l $head))
)
