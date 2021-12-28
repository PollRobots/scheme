;; (include <string_1> <string_2> ...)
(func $include (param $env i32) (param $args i32) (result i32)
  (block $check (block $fail
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $fail (i32.eqz (call $all-string (local.get $args))))
      (br $check))

    (return (Call $argument-error (local.get $args))))

  (return (call $cont-include
      (%cont-include)
      (local.get $env)
      (local.get $args))))

;; (cont-include <string_1> <string_2> ...)
(func $cont-include (param $env i32) (param $args i32) (result i32)
  (local $filename i32)
  (local $promise i32)

  (if (i32.ne (%get-type $args) (%cons-type)) (then
      ;; all args have been processed
      (return (global.get $g-nil))))

  (%pop-l $filename $args)

  (local.set $promise (call $file-read (local.get $filename)))

  (return (call $cont-alloc
      (%cont-import-promise)
      (local.get $env)
      (local.get $promise)
      (call $cont-alloc
        (%cont-include-read)
        (local.get $env)
        (local.get $args)
        (i32.const 0)))))

;; (cont-include-read <contents> <string_1> ...)
(func $cont-include_read (param $env i32) (param $args i32) (result i32)
  (local $contents i32)

  (%pop-l $contents $args)
  
