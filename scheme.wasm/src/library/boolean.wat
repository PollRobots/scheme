(func $bool-not (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (return
    (select
      (global.get $g-false)
      (global.get $g-true)
      (call $is-truthy (%car-l $args))
    )
  )
)

(func $bool-boolean? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $obj (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $obj) (%boolean-type))
    )
  )
)

(func $bool-boolean=? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $val i32)

  (if (i32.lt_u (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%boolean-type))
    (then (return (global.get $g-false)))
  )
  (local.set $val (%car-l $obj))
  (local.set $args (%cdr-l $args))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (local.set $obj (%car-l $args))
      (if (i32.ne (%get-type $obj) (%boolean-type))
        (then (return (global.get $g-false)))
      )
      (if (i32.ne (local.get $val) (%car-l $obj))
        (then (return (global.get $g-false)))
      )

      (local.set $args (%cdr-l $args))
      (br $b_start)
    )
  )

  (return (global.get $g-true))
)