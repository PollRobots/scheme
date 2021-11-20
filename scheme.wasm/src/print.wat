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
        (br $b_switch)
      )
    )
    ;; case boolean (2):
    (if (i32.eq (local.get $type) (%boolean-type))
      (then
        ;; print-boolean(ptr[4])
        (call $print-boolean (i32.load offset=4 (local.get $ptr)))
        ;; break
        (br $b_switch)
      )
    )
    ;; case cons (3):
    (if (i32.eq (local.get $type) (%cons-type))
      (then
        ;; print-cons(ptr[4], ptr[8])
        (call $print-cons
          (i32.load offset=4 (local.get $ptr))
          (i32.load offset=8 (local.get $ptr))
          (i32.const 1)
        )
        ;; break
        (br $b_switch)
      )
    )
    ;; case i64 (4):
    (if (i32.eq (local.get $type) (%i64-type))
      (then
        ;; print-integer((i64)ptr[4])
        (call $print-integer (i64.load offset=4 (local.get $ptr)))
        ;; break
        (br $b_switch)
      )
    )
    ;; case symbol (6):
    (if (i32.eq (local.get $type) (%symbol-type))
      (then
        ;; print-symbol(ptr[4])
        (call $print-symbol (local.get $ptr))
        ;; break
        (br $b_switch)
      )
    )
    ;; case str (7):
    (if (i32.eq (local.get $type) (%str-type))
      (then
        ;; print-str(ptr[4])
        ;; break
        (br $b_switch)
      )
    )
    ;; case env (9):
    (if (i32.eq (local.get $type) (%env-type))
      (then
        ;; print-env(ptr)
        (call $print-other (global.get $g-env) (local.get $type) (local.get $ptr))
        ;; break
        (br $b_switch)
      )
    )
    ;; case special:
    (if (i32.eq (local.get $type) (%special-type))
      (then
        (call $print-other (global.get $g-special) (local.get $type) (local.get $ptr))
        (br $b_switch)
      )
    )
    ;; case builtin:
    (if (i32.eq (local.get $type) (%builtin-type))
      (then
        (call $print-other (global.get $g-special) (local.get $type) (local.get $ptr))
        (br $b_switch)
      )
    )
    ;; case lambda:
    (if (i32.eq (local.get $type) (%lambda-type))
      (then
        (call $print-other (global.get $lambda-sym) (local.get $type) (local.get $ptr))
        (br $b_switch)
      )
    )
    ;; case error:
    (if (i32.eq (local.get $type (%error-type)))
      (then
        (call $print-error (local.get $ptr))
        (br $b_switch)
      )
    )
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
  (call $print-integer (i64.extend_i32_u (local.get $type)))
  (call $print-symbol (global.get $g-space))
  (call $print-integer (i64.extend_i32_u (local.get $ptr)))
  (call $print-symbol (global.get $g-space))
  (call $print-symbol (global.get $g-gt))
)

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

(func $print-integer (param $num i64)
  (local $buffer i32) ;; buffer for characters
  (local $ptr i32)
  (local $digit i32)
  (local $len i32)    ;; number of characters
  (local $is-negative i32)
  (local $str i32)

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

  ;; buffer = malloc(80) ;; 20 characters 
  (local.set $buffer (call $malloc (i32.const 80)))
  ;; ptr = buffer + 80;
  (local.set $ptr (i32.add (local.get $buffer) (i32.const 80)))
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

          ;; digit = num % 10
          (local.set $digit (i32.wrap_i64 (i64.rem_u (local.get $num) (i64.const 10))))
          ;; num = num / 10
          (local.set $num (i64.div_u (local.get $num) (i64.const 10)))
          ;; ptr -= 4;
          (%minus-eq $ptr 4)
          ;; *ptr = 0x30 + digit
          (i32.store (local.get $ptr) (i32.add (i32.const 0x30) (local.get $digit)))
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
  ;; write(str)
  (call $io-write (local.get $str))

  ;; malloc-free(buffer)
  (call $malloc-free (local.get $buffer))
  ;; malloc-free(str)
  (call $malloc-free (local.get $str))
)
 
(func $print-symbol (param $sym i32)
  ;; TODO handle symbols with non-standard characters
  (call $io-write (i32.load offset=4 (local.get $sym)))
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