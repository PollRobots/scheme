;; (include <string_1> <string_2> ...)
(func $include (param $env i32) (param $args i32) (result i32)
  (block $check (block $fail
      (br_if $fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $fail (i32.eqz (call $all-string (local.get $args))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (call $cont-include
      (%cont-include)
      (local.get $env)
      (%alloc-cons (global.get $g-nil) (local.get $args)))))

;; (cont-include <ingore> <string_1> <string_2> ...)
(func $cont-include (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $filename i32)
  (local $promise i32)

  ;; discard (either a nil, or the result of the eval of an included file)
  (%pop-l $temp $args)

  (if (i32.ne (%get-type $args) (%cons-type)) (then
      ;; all args have been processed
      (return (global.get $g-nil))))

  (%pop-l $filename $args)

  (local.set $promise (call $file-read (%car-l $filename)))

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
(func $cont-include-read (param $env i32) (param $args i32) (result i32)
  (local $contents i32)
  (local $contents-type i32)
  (local $reader i32)
  (local $datum i32)

  (%pop-l $contents $args)
  (local.set $contents-type (%get-type $contents))

  (if (i32.eq (local.get $contents-type) (%error-type)) (then
      (return (local.get $contents))))

  (if (i32.ne (local.get $contents-type) (%str-type)) (then
      (return (%alloc-error-cons 
          (%str %sym-64 64 "bad-read") 
          (global.get $g-nil)))))

  ;; validate that input from the host is well formed.
  (if (i32.eqz (call $str-is-valid (%car-l $contents))) (then
      (return (%alloc-error-cons 
          (%str %sym-128 128 "invalid-string") 
          (global.get $g-nil)))))

  ;; Crease a string reader.
  (local.set $reader (call $reader-init (i32.const -1)))
  ;; Push the contents onto the read-cache.
  (i32.store offset=16
    (local.get $reader)
    (%alloc-list-3
      ;; "(begin "
      (%alloc-str (call $str-from-64 (i32.const 7) (i64.const 0x206e6967656228)))
      (local.get $contents)
      ;; ")"
      (%alloc-str (call $str-from-32 (i32.const 1) (i32.const 0x29)))))

  (local.set $datum (call $read-with-reader (local.get $reader)))
  (call $reader-free (local.get $reader))
  (if (i32.eq (%get-type $datum) (%error-type)) (then
      (return (local.get $datum))))

  (return (call $cont-alloc
      (%eval-fn)
      (local.get $env)
      (local.get $datum)
      (call $cont-alloc
        (%cont-include)
        (local.get $env)
        (local.get $args)
        (i32.const 0)))))


