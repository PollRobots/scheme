(func $eq? (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (return
    (select 
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%car-l $args) (%car (%cdr-l $args)))
    )
  )
)

(func $equal-inner (param $a i32) (param $b i32) (param $deep i32) (result i32)
  (local $a-type i32)
  (local $b-type i32)
  (local $result i32)

  (if (i32.eq (local.get $a) (local.get $b))
    (then (return (i32.const 1))))

  (local.set $a-type (%get-type $a))
  (local.set $b-type (%get-type $b))

  (if (i32.ne (local.get $a-type) (local.get $b-type))
    (then (return (i32.const 0))))

  (local.set $result (i32.const 0))

  (block $b_cmp
    (if (i32.eq (local.get $a-type) (%nil-type))
      (then
        (local.set $result (i32.const 1))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%boolean-type))
      (then
        (local.set $result (i32.eq (%car-l $a) (%car-l $b)))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%i64-type))
      (then
        (local.set $result (i64.eq (i64.load offset=4 (local.get $a)) (i64.load offset=4 (local.get $b))))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%f64-type))
      (then
        (local.set $result (f64.eq (f64.load offset=4 (local.get $a)) (f64.load offset=4 (local.get $b))))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%symbol-type))
      (then
        (local.set $result (call $str-eq (%car-l $a) (%car-l $b)))
        (br $b_cmp)))
        
    (if (i32.eq (local.get $a-type) (%char-type))
      (then
        (local.set $result (i32.eq (%car-l $a) (%car-l $b)))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%big-int-type))
      (then
        (local.set $result (call $mp-eq? (%car-l $a) (%car-l $b)))
        (br $b_cmp)))

    (br_if $b_cmp (i32.eqz (local.get $deep)))

    (if (i32.eq (local.get $a-type) (%str-type))
      (then
        (local.set $result (call $str-eq (%car-l $a) (%car-l $b)))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%cons-type))
      (then
        (if (i32.eqz (call $equal-inner (%car-l $a) (%car-l $b) (local.get $deep)))
          (then 
            (local.set $result (i32.const 0))
            (br $b_cmp)))
        (local.set $result (call $equal-inner (%cdr-l $a) (%cdr-l $b) (local.get $deep)))
        (br $b_cmp)))

    (if (i32.eq (local.get $a-type) (%values-type))
      (then
        (if (i32.eqz (call $equal-inner (%car-l $a) (%car-l $b) (local.get $deep)))
          (then 
            (local.set $result (i32.const 0))
            (br $b_cmp)))
        (local.set $result (call $equal-inner (%cdr-l $a) (%cdr-l $b) (local.get $deep)))
        (br $b_cmp))))

  (return (local.get $result)))

(func $eqv? (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (call $equal-inner (%car-l $args) (%car (%cdr-l $args)) (i32.const 0))
    )
  )
)

(func $equal? (param $env i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args))))
  )

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (call $equal-inner (%car-l $args) (%car (%cdr-l $args)) (i32.const 1))
    )
  )
)