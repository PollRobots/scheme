;; (display <obj> [<port>])
(func $display (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $obj i32)
  (local $obj-type i32)
  (local $cp i32)
  (local $str-ptr i32)

  (block $b_check (block $b_fail
      (local.set $num-args (call $list-len (local.get $args)))
      (if (i32.eq (local.get $num-args) (i32.const 2)) (then
          (return (call $not-implemented-error (local.get $args)))))
      (br_if $b_fail (i32.ne (local.get $num-args) (i32.const 1)))
      (%pop-l $obj $args)
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $obj-type (%get-type $obj)) 

  (if (i32.eq (local.get $obj-type) (%str-type)) (then
      (call $io-write (%car-l $obj))
      (return (global.get $g-nil))))

  (if (i32.eq (local.get $obj-type) (%char-type)) (then
      (local.set $cp (%car-l $obj))
      (local.set $str-ptr (call $str-from-32
          (call $utf8-code-point-size (local.get $cp))
          (call $utf8-from-code-point (local.get $cp))))
      (call $io-write (local.get $str-ptr))
      (call $malloc-free (local.get $str-ptr))
      (return (global.get $g-nil))))

  (call $print (local.get $obj))
  (return (global.get $g-nil)))

  