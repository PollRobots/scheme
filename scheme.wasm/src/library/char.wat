(global $g-char-env (mut i32) (i32.const 0))
(global $g-invalid-char (mut i32) (i32.const 0))
(global $g-char-data (mut i32) (i32.const 0))
(global $g-char-prefix (mut i32) (i32.const 0))
(global $g-char-alarm (mut i32) (i32.const 0))
(global $g-char-backspace (mut i32) (i32.const 0))
(global $g-char-delete (mut i32) (i32.const 0))
(global $g-char-escape (mut i32) (i32.const 0))
(global $g-char-newline (mut i32) (i32.const 0))
(global $g-char-null (mut i32) (i32.const 0))
(global $g-char-return (mut i32) (i32.const 0))
(global $g-char-space (mut i32) (i32.const 0))
(global $g-char-tab (mut i32) (i32.const 0))

;; char data is 
;;    next-ptr : i32 
;;    char-data-entry : CharEntry[128] : 8 bytes * 128 (1024)

;; CharEntry::
;;    code-block : i32
;;    ptr: i32

(func $char-init
  (local $env i32)

  (%define %add-char-64 (%glob %name %len %val) (global.set %glob (%sym-64 %name %len))
  (call $environment-add
      (local.get $env)
      (global.get %glob)
      (%alloc-char (i32.const %val))
    )
  )

  (%define %add-char-128 (%glob %name1 %name2 %len %val) (global.set %glob (%sym-128 %name1 %name2 %len))
  (call $environment-add
      (local.get $env)
      (global.get %glob)
      (%alloc-char (i32.const %val))
    )
  )

  (local.set $env (call $environment-init (global.get $g-heap) (i32.const 0)))

  ;; #\alarm ; U+0007
  (%add-char-64 $g-char-alarm 0x6d72616c615c23 7 0x0007)
  ;; #\backspace ; U+0008
  (%add-char-128 $g-char-backspace 0x70736b6361625c23 0x656361 11 0x0008 )
  ;; #\delete ; U+007F
  (%add-char-64 $g-char-delete 0x6574656c65645c23 8 0x007F)
  ;; #\escape ; U+001B
  (%add-char-64 $g-char-escape 0x6570616373655c23 8 0x001b)
  ;; #\newline ; the linefeed character, U+000A
  (%add-char-128 $g-char-newline 0x6e696c77656e5c23 0x65 9 0x000A)
  ;; #\null ; the null character, U+0000
  (%add-char-64 $g-char-null 0x6c6c756e5c23 6 0x0000)
  ;; #\return ; the return character, U+000D
  (%add-char-64 $g-char-return 0x6e72757465725c23 8 0x000D)
  ;; #\space ; the preferred way to write a space
  (%add-char-64 $g-char-space 0x65636170735c23 7 0x0020)
  ;; #\tab ; the tab character, U+0009
  (%add-char-64 $g-char-tab 0x6261745c23 5 0x0009)


  (global.set $g-char-env (local.get $env))
  (global.set $g-invalid-char (%sym-128 0x2d64696c61766e69 0x72616863 12))

  (global.set $g-char-data (call $malloc (i32.const 1028)))
  (call $malloc-zero (global.get $g-char-data) (i32.const 1028))

  (global.set $g-char-prefix (%sym-32 0x5c23 2))
)

(func $char-cleanup
  (global.set $g-char-env (i32.const 0))
  (call $char-cleanup-data (global.get $g-char-data))
)

(func $read-char (param $str i32) (result i32)
  (local $first i32)
  (local $char i32)
  (local $digit i32)
  (local $index i32)
  (local $accum i32)
  (local $str-len i32)

  (block $b_invalid_char
    ;; get the first character (i.e the char at position 2)
    (local.set $first (call $str-code-point-at (local.get $str) (i32.const 2)))

    ;; if there is nothing at position 3, then this is a literal character 
    (if (i32.eq (call $str-code-point-at (local.get $str) (i32.const 3)) (i32.const -1))
      (then (return (%alloc-char (local.get $first))))
    )

    ;; lookup name in name table
    (local.set $char (call $hashtable-get (%car (global.get $g-char-env)) (local.get $str)))
    (if (local.get $char)
      (then (return (local.get $char)))  
    )

    (if (i32.ne (local.get $first) (i32.const 0x78))
      (then
        (br_if $b_invalid_char (i32.ne (local.get $first) (i32.const 0x58)))
      )
    )

    (local.set $index (i32.const 3))
    (local.set $accum (i32.const 0))
    (local.set $str-len (i32.load (local.get $str)))

    (block $b_end
      (loop $b_start
        (br_if $b_end (i32.ge_u (local.get $index) (local.get $str-len)))  


        (local.set $digit (call $get-radix-digit (local.get $str) (local.get $index) (i32.const 16)))
        (br_if $b_invalid_char (i32.lt_s (local.get $digit) (i32.const 0)))

        (local.set $accum
          (i32.add 
            (i32.shl (local.get $accum) (i32.const 4))
            (local.get $digit)
          )
        )

        (%inc $index)
        (br $b_start)
      )
    )

    ;; check that hex value is in the unicode range (0 - 0x10FFFF)
    (br_if $b_invalid_char (i32.gt_u (local.get $accum) (i32.const 0x10FFFF)))
    ;; check that hex value is not in the surrogate pair range  (0xD800 - 0xDFFF)
    (if (i32.ge_u (local.get $accum) (i32.const 0xD800))
      (then
        (br_if $b_invalid_char (i32.le_u (local.get $accum) (i32.const 0xDFFF)))
      )
    )

    (return (%alloc-char (local.get $accum)))
  )

  (return 
    (%alloc-error (global.get $g-invalid-char) (%alloc-str (call $str-dup (local.get $str))))
  )
)

(func $char-cleanup-data (param $ptr i32)
  (local $next i32)
  (local $i i32)
  (local $entry i32)
  (local $data-ptr i32)

  (local.set $entry (i32.add (local.get $ptr) (i32.const 4)))
  (local.set $i (i32.const 128))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $i)))

      (local.set $data-ptr (i32.load offset=4 (local.get $entry)))
      (br_if $b_end (i32.eqz (local.get $data-ptr)))
      (call $malloc-free (local.get $data-ptr))

      (%plus-eq $entry 8)
      (%dec $i)
      (br $b_start)
    )
  )

  (local.set $next (i32.load (local.get $ptr)))
  (call $malloc-free (local.get $ptr))

  (if (local.get $next)
    (call $char-cleanup-data (local.get $next))
  )
)

(func $char? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args))))
  )

  (local.set $arg (%car-l $args))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $arg) (%char-type))
    )
  )
)

(func $all-char (param $args i32) (result i32)
  (local $temp i32)
  (local $temp-type i32)

  (block $done 
    (loop $forever
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))
      (local.set $temp-type (%get-type $temp))

      (if (i32.ne (local.get $temp-type) (%char-type)) 
        (then (return (i32.const 0)))
      )

      (local.set $args (%cdr-l $args))
      (br $forever)
    )
  )

  (return (i32.const 1))
)

(func $char-equal (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 0) (i32.const 0)))
)

(func $char-gt (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 1) (i32.const 0)))
)

(func $char-ge (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 2) (i32.const 0)))
)

(func $char-lt (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 3) (i32.const 0)))
)

(func $char-le (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 4) (i32.const 0)))
)

(func $char-equal-ci (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 0) (i32.const 1)))
)

(func $char-gt-ci (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 1) (i32.const 1)))
)

(func $char-ge-ci (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 2) (i32.const 1)))
)

(func $char-lt-ci (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 3) (i32.const 1)))
)

(func $char-le-ci (param $env i32) (param $args i32) (result i32)
  (local $cmp i32)
  (local $cmp-type i32)
  (local $temp i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.lt_u (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $b_fail (i32.eqz (call $all-char (local.get $args))))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (call $char-cmp-impl (local.get $args) (i32.const 4) (i32.const 1)))
)

;; cmp-ops are
;;  0 =
;;  1 >
;;  2 >=
;;  3 <
;;  4 <=

(func $char-cmp-impl (param $args i32) (param $cmp-op i32) (param $ci i32) (result i32)
  (local $temp i32)
  (local $cmp-type i32)
  (local $left i32)
  (local $right i32)

  (local.set $temp (%car-l $args))
  (local.set $cmp-type (%get-type $temp))
  (local.set $args (%cdr-l $args))
  (local.set $right (%car-l $temp))
  (if (local.get $ci)
    (then (local.set $right (call $char-downcase-impl (local.get $right))))
  )

  (block $done
    (loop $continue
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (local.set $temp (%car-l $args))

      (block $b_cmp_check
        (block $b_cmp_fail
          (br_if $b_cmp_fail (i32.ne (%get-type $temp) (local.get $cmp-type)))

          (local.set $left (local.get $right))
          (local.set $right (%car-l $temp))
          (if (local.get $ci)
            (then (local.set $right (call $char-downcase-impl (local.get $right))))
          )

          (if (i32.eq (local.get $cmp-op) (i32.const 0)) 
            (then 
              (br_if $b_cmp_fail (i32.ne (local.get $left) (local.get $right)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 1))  ;; >
            (then
              (br_if $b_cmp_fail (i32.le_s (local.get $left) (local.get $right)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 2)) ;; >=
            (then
              (br_if $b_cmp_fail (i32.lt_s (local.get $left) (local.get $right)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 3)) ;; <
            (then
              (br_if $b_cmp_fail (i32.ge_s (local.get $left) (local.get $right)))
              (br $b_cmp_check)
            )
          )
          (if (i32.eq (local.get $cmp-op) (i32.const 4)) ;; <=
            (then
              (br_if $b_cmp_fail (i32.gt_s (local.get $left) (local.get $right)))
              (br $b_cmp_check)
            )
          )
        )
        (return (global.get $g-false))
      )

      (local.set $args (%cdr-l $args))
      (br $continue)
    )
  )

  (return (global.get $g-true))
)

(func $char-get-data-block (param $data-ptr i32) (param $block i32) (result i32)
  (local $i i32)
  (local $entry i32)
  (local $entry-block i32)
  (local $entry-ptr i32)
  (local $next-ptr i32)

  (local.set $entry (i32.add (local.get $data-ptr) (i32.const 4)))
  (local.set $i (i32.const 128))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $i)))

      (local.set $entry-ptr (i32.load offset=4 (local.get $entry)))
      (if (i32.eqz (local.get $entry-ptr))
        (then
          ;; no entry found, allocate a new one
          (local.set $entry-ptr (call $malloc (i32.const 2048)))
          (call $unicode-load-data (local.get $block) (local.get $entry-ptr))
          (i32.store (local.get $entry) (local.get $block))
          (i32.store offset=4 (local.get $entry) (local.get $entry-ptr))
          (return (local.get $entry-ptr))
        )
      )
      (local.set $entry-block (i32.load (local.get $entry)))
      (if (i32.eq (local.get $entry-block) (local.get $block))
        (then (return (local.get $entry-ptr)))
      )

      (%plus-eq $entry 8)
      (%dec $i)
      (br $b_start)
    )
  )

  (if (i32.eqz (local.tee $next-ptr (i32.load (local.get $data-ptr))))
    (then
      (local.set $next-ptr (call $malloc (i32.const 1028)))
      (call $malloc-zero (local.get $next-ptr) (i32.const 1028))
      (i32.store (local.get $data-ptr) (local.get $next-ptr))
    )
  )
  (return (call $char-get-data-block (local.get $next-ptr) (local.get $block)))
)

(func $char-get-code-point-props (param $code-point i32) (result i32)
  (local $block i32)
  (local $data-ptr i32)
  (local $offset i32)

  (local.set $block (i32.shr_u (local.get $code-point) (i32.const 8)))
  (local.set $data-ptr (call $char-get-data-block (global.get $g-char-data) (local.get $block)))
  ;; offset = (code-point & 0xFF) * 8
  ;; offset = (code-point & 0xFF) << 3
  (local.set $offset 
    (i32.shl 
      (i32.and (local.get $code-point) (i32.const 0xFF)) 
      (i32.const 3)
    )
  )

  (return (i32.load16_u (i32.add (local.get $data-ptr) (local.get $offset))))
)

(func $char-alphabetic? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $props i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $props (call $char-get-code-point-props (%car-l $arg))) 

  (return 
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (local.get $props) (i32.const 0x1))
    )
  )
)

(func $char-numeric? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $props i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $props (call $char-get-code-point-props (%car-l $arg))) 

  (return 
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.ne 
        (i32.and (local.get $props) (i32.const 0xff00)) 
        (i32.const 0xFF00)
      )
    )
  )
)

(func $char-whitespace? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $props i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $props (call $char-get-code-point-props (%car-l $arg))) 

  (return 
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (local.get $props) (i32.const 0x8))
    )
  )
)

(func $char-upper-case? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $props i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $props (call $char-get-code-point-props (%car-l $arg))) 

  (return 
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (local.get $props) (i32.const 0x2))
    )
  )
)

(func $char-lower-case? (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $props i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $props (call $char-get-code-point-props (%car-l $arg))) 

  (return 
    (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (local.get $props) (i32.const 0x4))
    )
  )
)

(func $digit-value (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $props i32)
  (local $digit i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $props (call $char-get-code-point-props (%car-l $arg))) 
  (local.set $digit (i32.shr_u (local.get $props) (i32.const 8)))

  (if (i32.lt_u (local.get $digit) (i32.const 10))
    (then (return (%alloc-i32 (local.get $digit))))
  )
  (return (global.get $g-false))
)

(func $char->integer (param $env i32) (param $args i32) (result i32)
  (local $arg i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (%alloc-i32 (%car-l $arg)))
)

(func $integer->char (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $val i64)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%i64-type)))
      (local.set $val (i64.load offset=4 (local.get $arg)))
      (br_if $b_fail (i64.ge_u (local.get $val) (i64.const 0x10ffff)))
      (br_if $b_fail 
        (i32.and 
          (i64.ge_u (local.get $val) (i64.const 0xd800))
          (i64.lt_u (local.get $val) (i64.const 0xE000))
        )
      )
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (return (%alloc-char (i32.wrap_i64 (local.get $val))))
)

(func $char-upcase-impl (param $code-point i32) (result i32)
  (local $data-ptr i32)
  (local $offset i32)

  (local.set $data-ptr 
    (call $char-get-data-block 
      (global.get $g-char-data) 
      (i32.shr_u (local.get $code-point) (i32.const 8))
    )
  )
  ;; offset = (code-point & 0xFF) * 8
  (local.set $offset (i32.shl (i32.and (local.get $code-point) (i32.const 0xFF)) (i32.const 3)))

  (return
    (i32.and
      (i32.load offset=2 (i32.add (local.get $data-ptr) (local.get $offset)))
      (i32.const 0x1FFFFF)
    )
  )
)

(func $char-upcase (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $code-point i32)
  (local $upper i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $code-point (%car-l $arg))
  (local.set $upper (call $char-upcase-impl (local.get $code-point)))

  (if 
    (i32.or 
      (i32.eqz (local.get $upper)) 
      (i32.eq (local.get $upper) (local.get $code-point))
    )
    (then (return (local.get $arg)))
  )

  (return (%alloc-char (local.get $upper)))
)

(func $char-downcase-impl (param $code-point i32) (result i32)
  (local $data-ptr i32)
  (local $offset i32)
  (local $lower i32)

  (local.set $data-ptr 
    (call $char-get-data-block 
      (global.get $g-char-data) 
      (i32.shr_u (local.get $code-point) (i32.const 8))
    )
  )
  ;; offset = (code-point & 0xFF) * 8
  (local.set $offset (i32.shl (i32.and (local.get $code-point) (i32.const 0xFF)) (i32.const 3)))

  (local.set $lower 
    (i32.and
      (i32.shr_u 
        (i32.load offset=4 (i32.add (local.get $data-ptr) (local.get $offset)))
        (i32.const 8)
      )
      (i32.const 0x1FFFFF)
    )
  )

  (if 
    (i32.or 
      (i32.eqz (local.get $lower)) 
      (i32.eq (local.get $lower) (local.get $code-point))
    )
    (then (return (local.get $code-point)))
  )

  (return (local.get $lower))
)

(func $char-downcase (param $env i32) (param $args i32) (result i32)
  (local $arg i32)
  (local $code-point i32)
  (local $lower i32)

  (block $b_check
    (block $b_fail
      (br_if $b_fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $arg (%car-l $args))
      (br_if $b_fail (i32.ne (%get-type $arg) (%char-type)))
      (br $b_check)
    )
    (return (call $argument-error (local.get $args)))
  )

  (local.set $code-point (%car-l $arg))
  (local.set $lower (call $char-downcase-impl (local.get $code-point)))

  (if (i32.eq (local.get $lower) (local.get $code-point))
    (then (return (local.get $arg)))
  )

  (return (%alloc-char (local.get $lower)))
)

