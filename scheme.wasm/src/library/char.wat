(global $g-char-env (mut i32) (i32.const 0))
(global $g-invalid-char (mut i32) (i32.const 0))

(func $char-init
  (local $env i32)

  (%define %add-char-64 (%name %len %val) (call $environment-add
      (local.get $env)
      (%sym-64 %name %len)
      (%alloc-char (i32.const %val))
    )
  )

  (%define %add-char-128 (%name1 %name2 %len %val) (call $environment-add
      (local.get $env)
      (%sym-128 %name1 %name2 %len)
      (%alloc-char (i32.const %val))
    )
  )

  (local.set $env (call $environment-init (global.get $g-heap) (i32.const 0)))

  ;; #\alarm ; U+0007
  (%add-char-64 0x6d72616c615c23 7 0x0007)
  ;; #\backspace ; U+0008
  (%add-char-128 0x70736b6361625c23 0x656361 11 0x0008 )
  ;; #\delete ; U+007F
  (%add-char-64 0x6574656c65645c23 8 0x007F)
  ;; #\escape ; U+001B
  (%add-char-64 0x6570616373655c23 8 0x001b)
  ;; #\newline ; the linefeed character, U+000A
  (%add-char-128 0x6e696c77656e5c23 0x65 9 0x000A)
  ;; #\null ; the null character, U+0000
  (%add-char-64 0x6c6c756e5c23 6 0x0000)
  ;; #\return ; the return character, U+000D
  (%add-char-64 0x6e72757465725c23 8 0x0000)
  ;; #\space ; the preferred way to write a space
  (%add-char-64 0x65636170735c23 7 0x0020)
  ;; #\tab ; the tab character, U+0009
  (%add-char-64 0x6261745c23 5 0x0009)

  (global.set $g-char-env (local.get $env))
  (global.set $g-invalid-char (%sym-128 0x2d64696c61766e69 0x72616863 12))
)

(func $char-cleanup
  (global.set $g-char-env (i32.const 0))
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
    (if (i32.eqz (call $str-code-point-at (local.get $str) (i32.const 3)))
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