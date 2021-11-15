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
        (call $print-symbol (i32.load offset=4 (local.get $ptr)))
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
        ;; break
        (br $b_switch)
      )
    )
    ;; default:
      ;; print-error();
      ;; break;
  ;; }
  )
)

(func $print-nil 
  (call $io-write (i32.load offset=4 (global.get $g-nil-str)))
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
  (call $io-write (local.get $sym))
)

(func $print-cons (param $car i32) (param $cdr i32)
  (local $str i32)
  (local $cdr-type i32)

  ;; str = str-from-32(1, 0x28)
  (local.set $str (call $str-from-32 (i32.const 1) (i32.const 0x28)))
  ;; write(str)
  (call $io-write (local.get $str))
  ;; print(car)
  (call $print (local.get $car))

  ;; while(true) {
  (block $b_end
    (loop $b_start
      ;; cdr-type = *cdr & 0xF;
      (local.set $cdr-type (%get-type $cdr))
      ;; if (cdr-type == 1) {
        ;; break
      (br_if $b_end (i32.eq (local.get $cdr-type) (i32.const 1)))
      ;; } else if (cdr-type == 3) {
      (if (i32.eq (local.get $cdr-type) (%cons-type))
        (then
          ;; another cons cell, continue list view
          ;; *str = 1
          (i32.store (local.get $str) (i32.const 1))
          ;; str[4] = 0x20 (' ')
          (i32.store offset=4 (local.get $str) (i32.const 0x20))
          ;; write(str)
          (call $io-write (local.get $str))
          ;; print(cdr[4])
          (call $print (i32.load offset=4 (local.get $cdr)))
          ;; cdr = cdr[8]
          (local.set $cdr (i32.load offset=8 (local.get $cdr)))
        )
        ;; } else {
        (else
          ;; something else, use dot view
          ;; *str = 3
          (i32.store (local.get $str) (i32.const 3))
          ;; str[4] = 0x202E20 (' . ')
          (i32.store offset=4 (local.get $str) (i32.const 0x202E20))
          ;; write(str)
          (call $io-write (local.get $str))
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

  ;; *str = 1
  (i32.store (local.get $str) (i32.const 1))
  ;; str[4] = 0x29 (')')
  (i32.store offset=4 (local.get $str) (i32.const 0x29))
  ;; write(str)
  (call $io-write (local.get $str))
  ;; malloc-free(str)
  (call $malloc-free (local.get $str))
)