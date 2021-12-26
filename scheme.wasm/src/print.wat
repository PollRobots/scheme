(func $print (param $ptr i32)
  (local $type i32) 

  ;; type = *ptr & 0x0F
  (local.set $type (%get-type $ptr))

  (block $b_switch
  ;; switch (type) {
    ;; case nil (1):
    (if (i32.eq (local.get $type) (%nil-type))
      (then
        ;; print-nil()
        (call $print-nil)
        ;; break
        (br $b_switch)))

    ;; case boolean (2):
    (if (i32.eq (local.get $type) (%boolean-type))
      (then
        ;; print-boolean(ptr[4])
        (call $print-boolean (i32.load offset=4 (local.get $ptr)))
        ;; break
        (br $b_switch)))

    ;; case cons (3):
    (if (i32.eq (local.get $type) (%cons-type))
      (then
        ;; print-cons(ptr[4], ptr[8])
        (call $print-cons (%car-l $ptr) (%cdr-l $ptr) (i32.const 1))
        ;; break
        (br $b_switch)))
        
    ;; case i64 (4):
    (if (i32.eq (local.get $type) (%i64-type))
      (then
        ;; print-integer((i64)ptr[4])
        (call $print-integer (i64.load offset=4 (local.get $ptr)) (i32.const 10))
        ;; break
        (br $b_switch)))

    ;; case f64 (5):
    (if (i32.eq (local.get $type) (%f64-type)) (then
        (call $print-real (f64.load offset=4 (local.get $ptr)))
        (br $b_switch)))

    ;; case symbol (6):
    (if (i32.eq (local.get $type) (%symbol-type))
      (then
        ;; print-symbol(ptr[4])
        (call $print-symbol (local.get $ptr))
        ;; break
        (br $b_switch)))
        
    ;; case str (7):
    (if (i32.eq (local.get $type) (%str-type))
      (then
        (call $print-str (local.get $ptr))
        ;; break
        (br $b_switch)))

    ;; case char (8)
    (if (i32.eq (local.get $type) (%char-type))
      (then
        (call $print-char (local.get $ptr))
        ;; break
        (br $b_switch)))

    ;; case env (9):
    (if (i32.eq (local.get $type) (%env-type))
      (then
        ;; print-env(ptr)
        (call $print-other (global.get $g-env) (local.get $type) (local.get $ptr))
        ;; break
        (br $b_switch)))

    ;; case special:
    (if (i32.eq (local.get $type) (%special-type))
      (then
        (call $print-other (global.get $g-special) (local.get $type) (local.get $ptr))
        (br $b_switch)))

    ;; case builtin:
    (if (i32.eq (local.get $type) (%builtin-type))
      (then
        (call $print-builtin (local.get $type) (local.get $ptr))
        (br $b_switch)))

    ;; case lambda:
    (if (i32.eq (local.get $type) (%lambda-type))
      (then
        (call $print-lambda (local.get $ptr))
        (br $b_switch)))

    ;; case error:
    (if (i32.eq (local.get $type (%error-type)))
      (then
        (call $print-error (local.get $ptr))
        (br $b_switch)))

    ;; case values:
    (if (i32.eq (local.get $type (%values-type)))
      (then
        (call $print-cons (%car-l $ptr) (%cdr-l $ptr) (i32.const 0))
        (br $b_switch)))

    ;; case vector:
    (if (i32.eq (local.get $type (%vector-type)))
      (then
        (call $print-vector (local.get $ptr))
        (br $b_switch)))

    ;; case bytevector:
    (if (i32.eq (local.get $type (%bytevector-type)))
      (then
        (call $print-bytevector (local.get $ptr))
        (br $b_switch)))

    ;; case BigInt
    (if (i32.eq (local.get $type) (%big-int-type))
      (then
        (call $print-big-int (local.get $ptr) (i32.const 10))
        (br $b_switch)))

    ;; default:
    (call $print-other (global.get $g-unknown) (local.get $type) (local.get $ptr))
      ;; print-error();
      ;; break;
  ;; }
  )
)

(func $print-nil 
  (call $io-write (i32.load offset=4 (global.get $g-nil-str)))
)

(func $print-char (param $ptr i32)
  (local $code-point i32)
  (local $props i32)
  (local $str i32)

  (local.set $code-point (%car-l $ptr))

  (%define %named-char (%name %val) (if (i32.eq (i32.const %val) (local.get $code-point)) (then
      (call $print-symbol (global.get %name))
      (return))))

  (%named-char $g-char-alarm 0x7)
  (%named-char $g-char-backspace 0x8)
  (%named-char $g-char-delete 0x7f)
  (%named-char $g-char-escape 0x1b)
  (%named-char $g-char-newline 0xa)
  (%named-char $g-char-null 0x0)
  (%named-char $g-char-return 0xd)
  (%named-char $g-char-space 0x20)
  (%named-char $g-char-tab 0x09)

  (call $print-symbol (global.get $g-char-prefix))

  (local.set $props (call $char-get-code-point-props (local.get $code-point)))

  (if (i32.and (local.get $props) (i32.const 0x10)) (then
      (local.set $str
        (call $str-from-32
          (call $utf8-code-point-size (local.get $code-point))
          (call $utf8-from-code-point (local.get $code-point))))
      (call $io-write (local.get $str))
      (call $malloc-free (local.get $str)))
    (else
      (local.set $str (call $str-from-32 (i32.const 1) (i32.const 0x78)))
      (call $io-write (local.get $str))
      (call $malloc-free (local.get $str))
      (call $print-integer 
        (i64.extend_i32_u (local.get $code-point)) 
        (i32.const 16)))))

(func $print-error (param $ptr i32)
  (local $data i32)

  (call $print-symbol (global.get $g-lt))
  (call $print-symbol (global.get $g-error))
  (call $print-symbol (global.get $g-space))
  (call $print-cons (%car-l $ptr) (%cdr-l $ptr) (i32.const 0))
  (call $print-symbol (global.get $g-gt))
)

(func $print-other (param $sym i32) (param $type i32) (param $ptr i32)
  (call $print-symbol (global.get $g-lt))
  (call $print-symbol (local.get $sym))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (local.get $type)) (i32.const 10))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 16))
  (call $print-symbol (global.get $g-gt)))

(func $print-lambda (param $ptr i32)
  (call $print-symbol (global.get $g-lt))
  (call $print-symbol (global.get $lambda-sym))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 16))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (%car-l $ptr)) (i32.const 16))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (%cdr-l $ptr)) (i32.const 16))
  (call $print-symbol (global.get $g-gt)))

(func $print-builtin (param $type i32) (param $ptr i32)
  (call $print-symbol (global.get $g-lt))
  (call $print-symbol (global.get $g-builtin))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (local.get $type)) (i32.const 10))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 16))
  (call $print-symbol (global.get $g-space))
  (call $print (%cdr-l $ptr))
  (call $print-symbol (global.get $g-gt))
)

(func $print-vector (param $vector i32)
  (local $ptr i32)
  (local $len i32)

  (local.set $ptr (%car-l $vector))
  (local.set $len (%cdr-l $vector))

  (call $print-symbol (global.get $g-vec-open))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $len)))

      (call $print (i32.load (local.get $ptr)))
      (if (i32.gt_u (local.get $len) (i32.const 1))
        (then (call $print-symbol (global.get $g-space))))

      (%plus-eq $ptr 4)
      (%dec $len)
      (br $b_start)))

  (call $print-symbol (global.get $g-close)))

(func $print-bytevector (param $u8 i32)
  (local $ptr i32)
  (local $len i32)

  (local.set $ptr (%car-l $u8))
  (local.set $len (%cdr-l $u8))

  (call $print-symbol (global.get $g-u8-open))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $len)))

      (call $print-integer (i64.load8_u (local.get $ptr)) (i32.const 10))
      (if (i32.gt_u (local.get $len) (i32.const 1))
        (then (call $print-symbol (global.get $g-space))))

      (%plus-eq $ptr 1)
      (%dec $len)
      (br $b_start)))

  (call $print-symbol (global.get $g-close)))

(func $print-env (param $env i32)
  (call $print (global.get $g-env))
)

(func $print-boolean (param $bool i32)
  (call $io-write
    (i32.load offset=4 
      (select (global.get $g-true-str) (global.get $g-false-str) (local.get $bool))
    )
  )
)

(func $print-integer (param $num i64) (param $radix i32)
  (local $str-ptr i32) 

  (local.set $str-ptr (call $integer->string-impl (local.get $num) (local.get $radix)))
  (call $io-write (local.get $str-ptr))
  (call $malloc-free (local.get $str-ptr))
)

(func $integer->string-impl (param $num i64) (param $radix i32) (result i32)
  (local $buffer i32) ;; buffer for characters
  (local $ptr i32)
  (local $digit i32)
  (local $len i32)    ;; number of characters
  (local $is-negative i32)
  (local $str i32)
  (local $r64 i64)

  (local.set $r64 (i64.extend_i32_u (local.get $radix)))

  ;; if (num < 0) {
  (if (i64.lt_s (local.get $num) (i64.const 0))
    (then
      ;; is-negative = true;
      (local.set $is-negative (i32.const 1))
      ;; num = 0 - num;
      (local.set $num (i64.sub (i64.const 0) (local.get $num)))
    )
    ;; } else {
    (else 
      ;; is-negative = false;
      (local.set $is-negative (i32.const 0))
    )
    ;; }
  )

  ;; buffer = malloc(0x100) ;; 100 characters 
  (local.set $buffer (call $malloc (i32.const 0x100)))
  ;; ptr = buffer + 0x100;
  (local.set $ptr (i32.add (local.get $buffer) (i32.const 0x100)))
  ;; len = 0
  (local.set $len (i32.const 0))

  ;; if (@num) {
  (if (i64.eqz (local.get $num))
    (then
      ;; zero case
      ;; ptr -= 4
      (%minus-eq $ptr 4)
      ;; *ptr = 0x30
      (i32.store (local.get $ptr) (i32.const 0x30))
      ;; len++
      (%inc $len)
    )
    ;; } else {
    (else
      ;; while (num != 0) {
      (block $b_end
        (loop $b_start
          (br_if $b_end (i64.eqz (local.get $num)))

          ;; digit = num % radix
          (local.set $digit (i32.wrap_i64 (i64.rem_u (local.get $num) (local.get $r64))))
          ;; num = num / radix
          (local.set $num (i64.div_u (local.get $num) (local.get $r64)))
          ;; ptr -= 4;
          (%minus-eq $ptr 4)
          (if (i32.lt_u (local.get $digit) (i32.const 10)) 
            (then
              ;; *ptr = 0x30 + digit
              (i32.store (local.get $ptr) (i32.add (i32.const 0x30) (local.get $digit)))
            )
            (else
              ;; *ptr = 0x41 + digit - 10
              ;; *ptr = 0x37 + digit
              (i32.store (local.get $ptr) (i32.add (i32.const 0x37) (local.get $digit)))
            )
          )
          ;; len++
          (%inc $len)
          (br $b_start)
        )
      )
      ;; }
    )
    ;; }
  )

  ;; if (is-negative) {
  (if (local.get $is-negative)
    (then
      ;; ptr -= 4;
      (%minus-eq $ptr 4)
      ;; *ptr = 0x2D (-)
      (i32.store (local.get $ptr) (i32.const 0x2d))
      ;; len++
      (%inc $len)
    )
  )
  ;; }

  ;; str = str-from-code-points(ptr, len)
  (local.set $str (call $str-from-code-points (local.get $ptr) (local.get $len)))
  ;; malloc-free(buffer)
  (call $malloc-free (local.get $buffer))

  (return (local.get $str))
)

(func $print-real (param $num f64)
  (local $str i32)

  (local.set $str (call $real->string (local.get $num)))
  (call $io-write (local.get $str))
  (call $malloc-free (local.get $str)))

(func $print-big-int (param $num i32) (param $radix i32)
  (local $str-ptr i32) 

  (local.set $str-ptr (call $mp-mp->string (%car-l $num) (local.get $radix)))
  (call $io-write (local.get $str-ptr))
  (call $malloc-free (local.get $str-ptr)))
 
(func $print-symbol (param $sym i32)
  ;; TODO handle symbols with non-standard characters
  (call $io-write (i32.load offset=4 (local.get $sym)))
)

(func $print-symbol-rep (param $sym i32) (param $count i32)
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.le_s (local.get $count) (i32.const 0)))
      (call $io-write (i32.load offset=4 (local.get $sym)))
      (%dec $count)
      (br $b_start)))
)

(func $print-str (param $str i32)
  ;; TODO handle strings with non-print characters
  (call $print-symbol (global.get $g-quote))
  (call $print-symbol (local.get $str))
  (call $print-symbol (global.get $g-quote))
)

(func $print-cons (param $car i32) (param $cdr i32) (param $paren i32)
  (local $cdr-type i32)

  ;; if (paren) {
  (if (local.get $paren)
    ;; write(g-open)
    (then (call $io-write (%car-g $g-open)))
  )
  ;; }
  ;; print(car)
  (call $print (local.get $car))

  ;; while(true) {
  (block $b_end
    (loop $b_start
      ;; cdr-type = *cdr & 0xF;
      (local.set $cdr-type (%get-type $cdr))
      ;; if (cdr-type == 1) {
        ;; break
      (br_if $b_end (i32.eq (local.get $cdr-type) (%nil-type)))
      ;; } else if (cdr-type == 3) {
      (if (i32.eq (local.get $cdr-type) (%cons-type))
        (then
          ;; another cons cell, continue list view
          ;; write(g-dot)
          (call $io-write (%car-g $g-space))
          ;; print(cdr[4])
          (call $print (%car-l $cdr))
          ;; cdr = cdr[8]
          (local.set $cdr (%cdr-l $cdr))
        )
        ;; } else {
        (else
          ;; something else, use dot view
          ;; write(g-space)
          (call $io-write (%car-g $g-dot))
          ;; print(cdr)
          (call $print (local.get $cdr))
          (br $b_end)
        ;; break
        )
      ;; }
      )
      (br $b_start)
    ;; }
    )
  )

  (if (local.get $paren)
    (then
      ;; write(g-close)
      (call $io-write (%car-g $g-close))
    )
  )
)