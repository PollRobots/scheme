(global $g-reader     (mut i32) (i32.const 0))
(global $g-heap       (mut i32) (i32.const 0))
(global $g-interned   (mut i32) (i32.const 0))
(global $g-true       (mut i32) (i32.const 0))
(global $g-true-str   (mut i32) (i32.const 0))
(global $g-false      (mut i32) (i32.const 0))
(global $g-false-str  (mut i32) (i32.const 0))
(global $g-nil        (mut i32) (i32.const 0))
(global $g-nil-str    (mut i32) (i32.const 0))
(global $g-newline    (mut i32) (i32.const 0))
(global $g-collect    (mut i32) (i32.const 0))
(global $g-builtin    (mut i32) (i32.const 0))
(global $g-special    (mut i32) (i32.const 0))
(global $g-env        (mut i32) (i32.const 0))
(global $g-lt         (mut i32) (i32.const 0))
(global $g-gt         (mut i32) (i32.const 0))
(global $g-unknown    (mut i32) (i32.const 0))
(global $g-space      (mut i32) (i32.const 0))
(global $g-error      (mut i32) (i32.const 0))
(global $g-open       (mut i32) (i32.const 0))
(global $g-close      (mut i32) (i32.const 0))
(global $g-dot        (mut i32) (i32.const 0))
(global $g-eof        (mut i32) (i32.const 0))
(global $g-quote      (mut i32) (i32.const 0))
(global $g-args       (mut i32) (i32.const 0))
(global $g-vec-open   (mut i32) (i32.const 0))

(func $runtime-init
  (global.set $g-reader (call $reader-init))
  (global.set $g-heap (call $heap-create (i32.const 1024)))
  (global.set $g-interned (call $hashtable-init (i32.const 1024)))
  (global.set $g-true (call $heap-alloc (global.get $g-heap) (%boolean-type) (i32.const 1) (i32.const 0x7423)))
  (global.set $g-true-str (%sym-32 0x7423 2))
  (global.set $g-false (call $heap-alloc (global.get $g-heap) (%boolean-type) (i32.const 0) (i32.const 0x6623)))
  (global.set $g-false-str (%sym-32 0x6623 2))
  (global.set $g-nil (call $heap-alloc (global.get $g-heap) (%nil-type) (i32.const 0) (i32.const 0x2928)))
  (global.set $g-nil-str (%sym-32 0x2928 2))
  (global.set $g-newline  (%sym-32 0x0A 1))
  (global.set $g-collect (%sym-64 0x207463656c6c6f43 8))
  (global.set $g-builtin  (%sym-64  0x6e69746c697562 7))
  (global.set $g-special (%sym-64 0x6c616963657073 7))
  (global.set $g-env (%sym-32 0x766e65 3))
  (global.set $g-lt (%sym-32 0x3c 1))
  (global.set $g-gt (%sym-32 0x3e 1))
  (global.set $g-unknown (%sym-64 0x6e776f6e6b6e75 7))
  (global.set $g-space (%sym-32 0x20 1))
  (global.set $g-error (%sym-64 0x726f727265 5))
  (global.set $g-open (%sym-32 0x28 1))
  (global.set $g-close (%sym-32 0x29 1))
  (global.set $g-dot (%sym-32 0x202e20 3))
  (global.set $g-eof (%sym-32 0x666f65 3))
  (global.set $g-quote (%sym-32 0x22 1))
  (global.set $g-args (%sym-32 0x73677261 4))
  (global.set $g-vec-open (%sym-32 0x2823 2))

  (call $char-init)
)

(func $runtime-cleanup
  (call $char-cleanup)

  (global.set $g-nil (i32.const 0))
  (global.set $g-nil-str (i32.const 0))
  (global.set $g-false (i32.const 0))
  (global.set $g-false-str (i32.const 0))
  (global.set $g-true (i32.const 0))
  (global.set $g-true-str (i32.const 0))

  (call $malloc-free (global.get $g-interned))
  (global.set $g-interned (i32.const 0))

  (call $heap-destroy (global.get $g-heap))
  (global.set $g-heap (i32.const 0))

  (call $reader-free (global.get $g-reader))
  (global.set $g-reader (i32.const 0))
)

(func $read (result i32)
  (local $token-str i32)
  (local $result i32)

  ;; token-str = reader-read-token(g-reader)
  (local.set $token-str (call $reader-read-token (global.get $g-reader)))

  ;; return string->datum(token-str)
  (local.set $result (call $string->datum (local.get $token-str)))
  (if (i32.eq (%get-type $result) (%error-type))
    (then 
      (if (i32.eq (global.get $g-eof) (%car-l $result))
        (then 
          (call $reader-rollback (global.get $g-reader))
          (return (local.get $result))
        )
      )
    )
  )

  (call $reader-commit (global.get $g-reader))
  (return (local.get $result))
)

(func $string->datum (param $token-str i32) (result i32)
  (local $car-str i32)
  (local $car i32)
  (local $cdr-str i32)
  (local $cdr i32)
  (local $raw-token i32)
  (local $head i32)
  (local $curr i32)
  (local $list-vector i32)

  (%define %check-str (%str) 
    (if (i32.eqz (local.get %str))
      (then
        (return 
          (call $heap-alloc
            (global.get $g-heap)
            (%error-type)
            (global.get $g-eof)
            (global.get $g-nil)
          )
        )
      )
    )
  )

  (%check-str $token-str)

  (block $b_lv
    (block $b_list
      (br_if $b_list (call $short-str-eq (local.get $token-str) (i32.const 0x28) (i32.const 1)))
      (local.set $list-vector (select
        (i32.const 2)
        (i32.const 0)
        (call $short-str-eq (local.get $token-str) (i32.const 0x2823) (i32.const 2))))
      (br $b_lv))
    (local.set $list-vector (i32.const 1))
  )


  ;; if (token-str == '(' || token-str == '#(') {
  (if (local.get $list-vector)
    (then
      ;; malloc-free(token-str)
      (call $malloc-free (local.get $token-str))

      ;; car-str = reader-read-token(g-reader)
      (local.set $car-str (call $reader-read-token (global.get $g-reader)))
      (%check-str $car-str)
      ;; if (car-str == ')') {
      (if (call $short-str-eq (local.get $car-str) (i32.const 0x29) (i32.const 1))
        (then
          ;; malloc-free(car-str)
          (call $malloc-free (local.get $car-str))

          (if (i32.eq (local.get $list-vector) (i32.const 1))
            (then (return (global.get $g-nil)))
            (else (return (call $make-vector-internal (global.get $g-nil))))
          )
        )
        ;; }
      )
      ;; car = string->datum(car-str)
      (local.set $car (call $string->datum (local.get $car-str)))
      ;; curr = head = heap-alloc(3, car, g-nil)
      (local.set $curr
        (local.tee $head 
          (call $heap-alloc (global.get $g-heap) (%cons-type) (local.get $car) (global.get $g-nil))
        )
      )

      ;; while (true) {
      (loop $forever
        ;; cdr-str = reader-read-token(g-reader)
        (local.set $cdr-str (call $reader-read-token (global.get $g-reader)))
        (%check-str $cdr-str)
        ;; if (cdr-str == ')') {
        (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x29) (i32.const 1))
          (then
            ;; malloc-free(cdr-str)
            (call $malloc-free (local.get $cdr-str))

            (if (i32.eq (local.get $list-vector) (i32.const 1))
              (then (return (local.get $head)))
              (else (return (call $make-vector-internal (local.get $head))))
            )
          )
        )
        ;; }
        ;; if (cdr-str == '.') {
        (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x2E) (i32.const 1))
          (then
            ;; malloc-free(cdr-str)
            (call $malloc-free (local.get $cdr-str))
            ;; cdr-str = reader-read-token(g-reader)
            (local.set $cdr-str (call $reader-read-token (global.get $g-reader)))
            (%check-str $cdr-str)
            ;; cdr = string->datum(cdr-str)
            (local.set $cdr (call $string->datum (local.get $cdr-str)))
            ;; curr[8] = cdr
            (i32.store offset=8 (local.get $curr) (local.get $cdr))
            ;; cdr-str = reader-read-token(g-reader)
            (local.set $cdr-str (call $reader-read-token (global.get $g-reader)))
            (%check-str $cdr-str)
            ;; if (cdr-str == ')') {
            (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x29) (i32.const 1))
              (then
                ;; malloc-free(cdr-str)
                (call $malloc-free (local.get $cdr-str))
                ;; TODO error if vector
                ;; return head
                (return (local.get $head))
              )
              ;; } else {
              (else
                ;; TODO return error
                ;; trap
                unreachable
              )
              ;; }
            )
          )
        ;; } 
        )
        ;; cdr = string->datum(cdr-str)
        (local.set $cdr (call $string->datum (local.get $cdr-str)))

        ;; curr[8] = heap-alloc(3, cdr, g-nil)
        (i32.store
          offset=8
          (local.get $curr)
          (call $heap-alloc (global.get $g-heap) (%cons-type) (local.get $cdr) (global.get $g-nil))
        )

        ;; curr = curr[8]
        (local.set $curr (i32.load offset=8 (local.get $curr)))

        (br $forever)
      )
    )
  )
  ;; }
  ;; if (token-str == ')') {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x29) (i32.const 1))
    (then
      ;;  TODO: return error
      ;;  trap
      unreachable
    )
  )
  ;; if (token-str == '.') 0x2E {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x2E) (i32.const 1))
    (then
      ;;  TODO: return error
      ;;  trap
      unreachable
    )
  )
  ;; }
  ;; if (token-str == "'") 0x27 {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x27) (i32.const 1))
    (then
      ;; malloc-free(token-str)
      (call $malloc-free (local.get $token-str))
      ;; return cons('quote', cons(read(), nil))
      (return
        (%alloc-cons 
          (global.get $quote-sym) 
          (%alloc-cons (call $read) (global.get $g-nil))
        )
      )
    )
  ;; }
  )

  ;; raw-token = heap-alloc(7, token-str, 0)
  (local.set $raw-token (%alloc-str (local.get $token-str)))
  ;; return atom(raw-token);
  (return (call $atom (local.get $raw-token)))
)

(func $make-vector-internal (param $list i32) (result i32)
  (local $len i32)
  (local $ptr i32)
  (local $vec i32)

  (local.set $len (call $list-len (local.get $list)))
  (local.set $ptr (call $malloc (i32.shl (local.get $len) (i32.const 2))))

  (local.set $vec 
    (call $heap-alloc 
      (global.get $g-heap)
      (%vector-type)
      (local.get $ptr)
      (local.get $len)))

  (loop $forever
    (if (i32.eq (%get-type $list) (%nil-type))
      (then (return (local.get $vec))))

    (i32.store (local.get $ptr) (%car-l $list))

    (local.set $list (%cdr-l $list)) 
    (%plus-eq $ptr 4)
    (br $forever))

  (unreachable))

(func $atom (param $token i32)  (result i32)
  (local $token-str i32)
  (local $str-len i32)
  (local $atom-str i32)
  (local $atom i32)

  ;; if ((token[0] & 0xF) != 7 {
  (if (i32.ne (%get-type $token) (%str-type))
    ;; trap
    (then unreachable)
  ;; }
  )

  ;; token-str == token[4]
  (local.set $token-str (i32.load offset=4 (local.get $token)))

  ;; if (token-str == '#t') {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x7423) (i32.const 2))
    (then
      ;; return g_true
      (return (global.get $g-true))
    )
    ;; }
  )
  ;; if (token-str == '#f') {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x6623) (i32.const 2))
    (then 
      ;; return g_false
      (return (global.get $g-false))
    )
    ;; }
  )

  ;; if token-str.startsWith('str ')
  ;; if (short-str-start-with(token-str, 0, 'str ', 4)) {
  (if (call $short-str-start-with (local.get $token-str) (i32.const 0) (i32.const 0x20727473) (i32.const 4))
    (then
      (local.set $str-len (i32.load (local.get $token-str)))
      (i32.store offset=4 
        (local.get $token-str) 
        (i32.sub (local.get $str-len) (i32.const 4))
      )
      (local.set $atom-str (call $str-dup (i32.add (local.get $token-str) (i32.const 4))))
      (i32.store offset=4 (local.get $token-str) (i32.const 0x20727473))
      (return (%alloc-str (local.get $atom-str)))
    )
    ;; }
  )

  ;; if token.str.startsWith('#\')
  (if (call $short-str-start-with (local.get $token-str) (i32.const 0) (i32.const 0x5c23) (i32.const 2))
    (then
      (return (call $read-char (local.get $token-str)))
    )
  )

  ;; atom = string->number-impl(token, 10)
  (local.set $atom (call $string->number-impl (local.get $token) (i32.const 10)))
  ;; if (is-truthy(atom)) {
  (if (call $is-truthy (local.get $atom))
    (then
      ;; return atom
      (return (local.get $atom))
    )
    ;; }
  )

  ;; return heap-alloc(g-heap, %symbol-type, str-dup(token-str), 0)
  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%symbol-type)
      (call $str-dup (local.get $token-str))
      (i32.const 0)
    )
  )
)

(func $short-str-eq 
  (param $str i32)            ;; a string pointer
  (param $short-str i32)      ;; 32bit value containing utf8 encoded short string
  (param $short-str-len i32)  ;; byte length of the short string
  (result i32)                ;; 1 if strings are the same, 0 otherwise

  ;; if (short-str-len > 4) {
  (if (i32.gt_u (local.get $short-str-len) (i32.const 4))
    ;; trap
    (then unreachable)
  ;; }
  )

  ;; if (*str != short-str-len) {
  (if (i32.ne (i32.load (local.get $str)) (local.get $short-str-len))
    ;; return 0;
    (then (return (i32.const 0)))
  ;; } else {
    (else
      ;; return (str[4] == short-str)
      (return 
        (i32.eq
          (i32.load offset=4 (local.get $str))
          (local.get $short-str)
        )
      )
    )
  ;; }
  )
  unreachable
)

(func $short-str-start-with
  (param $str i32)
  (param $offset i32)
  (param $short-str i32)
  (param $short-str-len i32)
  (result i32)

  (local $word i32)
  (local $mask i32)

  (if (i32.gt_u (local.get $short-str-len) (i32.const 4))
    (then unreachable)
  )

  ;; if (offset + short-str-len > *ptr) {
  (if (i32.gt_u (i32.add (local.get $offset) (local.get $short-str-len))
                (i32.load (local.get $str)))
    (then
      ;; return 0;
      (return (i32.const 0))
    )
  )

  ;; word = str[4 + offset] 
  (local.set $word (i32.load offset=4 (i32.add (local.get $str) (local.get $offset))))
  ;; mask = -1 >> (4-len) << 3
  (local.set $mask 
    (i32.shr_u 
      (i32.const -1) 
      (i32.shl 
        (i32.sub (i32.const 4) (local.get $short-str-len))
        (i32.const 3)
      )
    )
  )
  (return 
    (i32.eq 
      (i32.and (local.get $mask) (local.get $word))
      (local.get $short-str)
    )
  )
)

(func $is-truthy (param $token i32) (result i32)
  ;; if (token[0] & 0xF == 2) {
  (if (i32.eq (i32.and (i32.load (local.get $token)) (i32.const 0xF)) (i32.const 2))
    (then
      ;; if (token[4] == 0) {
      (if (i32.eqz (i32.load offset=4 (local.get $token)))
        ;; return 0
        (then (return (i32.const 0)))
      )
      ;; }
    )
    ;; }
  )

  ;; return 1
  (return (i32.const 1))
)

(func $get-radix-digit (param $str i32) (param $offset i32) (param $radix i32) (result i32)
  (local $c i32)
  (local $digit i32)

  (local.set $c (i32.load8_u offset=4 (i32.add (local.get $str) (local.get $offset))))

  (block $b_digit
    ;; c >= 0x30 && c <= 0x39 ('0' <= c <= '9')
    (if (i32.ge_u (local.get $c) (i32.const 0x30))
      (then
        (if (i32.le_u (local.get $c) (i32.const 0x39))
          (then
            (local.set $digit (i32.sub (local.get $c) (i32.const 0x30)))
            (br $b_digit)
          )
        )
      )
    )
    ;; c >= 0x61 && c <= 66 ('a' <= c <= 'f')
    (if (i32.ge_u (local.get $c) (i32.const 0x61))
      (then
        (if (i32.le_u (local.get $c) (i32.const 0x66))
          (then
            ;; digit = c - 0x61 + 0x0A
            ;; digit = c - 0x57
            (local.set $digit (i32.sub (local.get $c) (i32.const 0x57)))
            (br $b_digit)
          )
        )
      )
    )
    ;; c >= 0x41 && c <= 46 ('A' <= c <= 'F')
    (if (i32.ge_u (local.get $c) (i32.const 0x41))
      (then
        (if (i32.le_u (local.get $c) (i32.const 0x46))
          (then
            ;; digit = c - 0x41 + 0x0A
            ;; digit = c - 0x37
            (local.set $digit (i32.sub (local.get $c) (i32.const 0x37)))
            (br $b_digit)
          )
        )
      )
    )

    ;; else
    (return (i32.const -1))
  )

  (return 
    (select 
      (local.get $digit) 
      (i32.const -1) 
      (i32.lt_u (local.get $digit) (local.get $radix))
    )
  )
)

(func $string->number-impl (param $str i32) (param $radix i32) (result i32)
  (local $str-ptr i32)    ;; pointer to the string
  (local $offset i32)     ;; byte offset in the string
  (local $str-len i32)
  (local $exact i32)      ;; is exact
  (local $inexact i32)    ;; is inexact
  (local $radix-count i32);; count of radix prefixes (greater than 1 is an error)
  (local $negative i32)   ;; is negative
  (local $integer i64)
  (local $fraction-digits i32)
  (local $fraction i64)
  (local $integer-digits i32)
  (local $digit i32)
  (local $decimal i32)
  (local $have-exp i32)
  (local $exponent i64)
  (local $exponent-digits i32)
  (local $neg-exp i32)


  (if (i32.ne (%get-type $str) (%str-type))
    (then (return (global.get $g-false)))
  )

  (local.set $str-ptr (%car-l $str))
  (local.set $str-len (i32.load (local.get $str-ptr)))
  (local.set $offset (i32.const 0))
  (local.set $exact (i32.const 0))
  (local.set $inexact (i32.const 0))
  (local.set $radix-count (i32.const 0))
  (local.set $negative (i32.const 0))
  (local.set $decimal (i32.const 0))
  (local.set $integer (i64.const 0))
  (local.set $fraction (i64.const 0))
  (local.set $fraction-digits (i32.const 0))
  (local.set $integer-digits (i32.const 0))
  (local.set $exponent-digits (i32.const 0))
  (local.set $have-exp (i32.const 0))
  (local.set $exponent (i64.const 0))
  (local.set $neg-exp (i32.const 0))

  (%define %sssw (%cmp %len) (call $short-str-start-with (local.get $str-ptr) (local.get $offset) (i32.const %cmp) (i32.const %len)))

  ;; look for prefix elements #e #i #b #d #o #x
  ;; while (true) {
  (loop $prefix_loop
    (if (%sssw 0x4523 2) ;; #E
      (then
        (%inc $exact)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x6523 2) ;; #e
      (then
        (%inc $exact)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x4923 2) ;; #I
      (then
        (%inc $inexact)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x6923 2) ;; #i
      (then
        (%inc $inexact)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x4223 2) ;; #B
      (then
        (local.set $radix (i32.const 2))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x6223 2) ;; #b
      (then
        (local.set $radix (i32.const 2))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x4423 2) ;; #D
      (then
        (local.set $radix (i32.const 10))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x6423 2) ;; #d
      (then
        (local.set $radix (i32.const 10))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x4F23 2) ;; #O
      (then
        (local.set $radix (i32.const 8))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x6F23 2) ;; #0
      (then
        (local.set $radix (i32.const 8))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x5823 2) ;; #X
      (then
        (local.set $radix (i32.const 16))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    (if (%sssw 0x7823 2) ;; #x
      (then
        (local.set $radix (i32.const 16))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)
      )
    )
    ;; }
  )
  ;; }

  (block $prefix_fail
    (block $prefix_check
      ;; multiple inexact
      (br_if $prefix_check (i32.gt_u (local.get $inexact) (i32.const 1)))
      ;; multiple exact
      (br_if $prefix_check (i32.gt_u (local.get $exact) (i32.const 1)))
      ;; multiple radix
      (br_if $prefix_check (i32.gt_u (local.get $radix-count) (i32.const 1)))
      ;; inexact and exact
      (br_if $prefix_check (i32.and (local.get $inexact) (local.get $exact)))
      ;; prefix was ok
      (br $prefix_fail)
    )
    (return (%alloc-error (%sym-64 0x786966657270 6) (local.get $str)))
  )

  (if (local.get $exact)
    (then
      (return (%alloc-error (%sym-64 0x7463617865 5) (local.get $str)))
    )
  )
  (if (local.get $inexact)
    (then
      (return (%alloc-error (%sym-64 0x74636178656e69 7) (local.get $str)))
    )
  )

  ;; check for sign
  (if (%sssw 0x2d 1) ;; '-'
    (then
      (local.set $negative (i32.const 1))
      (%inc $offset)
    )
    (else 
      (if (%sssw 0x2B 1) ;; '+'
        (then (%inc $offset))
      )
    )
  )

  (block $b_integer_end
    (loop $b_integer
      ;; break if offset >= str-len
      (br_if $b_integer_end (i32.ge_u (local.get $offset) (local.get $str-len)))

      (local.set $digit (call $get-radix-digit (local.get $str-ptr) (local.get $offset) (local.get $radix)))
      (br_if $b_integer_end (i32.lt_s (local.get $digit) (i32.const 0)))
      (%inc $offset)

      ;; integer = integer * radix + digit
      (local.set $integer
        (i64.add
          (i64.mul (local.get $integer) (i64.extend_i32_u (local.get $radix)))
          (i64.extend_i32_u (local.get $digit))
        )
      )
      (%inc $integer-digits)

      (br $b_integer)
    )
  )

  (if (i32.eq (local.get $radix) (i32.const 10))
    (then
      ;; check for decimal
      (if (%sssw 0x2e 1) ;; '.'
        (then
          (local.set $decimal (i32.const 1))
          (%inc $offset)

          (block $b_fraction_end
            (loop $b_fraction
              ;; break if offset >= str-len
              (br_if $b_fraction_end (i32.ge_u (local.get $offset) (local.get $str-len)))

              (local.set $digit (call $get-radix-digit (local.get $str-ptr) (local.get $offset) (local.get $radix)))
              (br_if $b_fraction_end (i32.lt_s (local.get $digit) (i32.const 0)))
              (%inc $offset)

              ;; fraction = fraction * radix + digit
              (local.set $fraction
                (i64.add
                  (i64.mul (local.get $fraction) (i64.extend_i32_u (local.get $radix)))
                  (i64.extend_i32_u (local.get $digit))
                )
              )
              (%inc $fraction-digits)

              (br $b_fraction)
            )
          )
        )
      )

      ;; check for exponent
      (if (i32.or (%sssw 0x65 1) (%sssw 0x45 1))
        (then
          (local.set $have-exp (i32.const 1))
          (%inc $offset)

          ;; check for sign
          (if (%sssw 0x2d 1) ;; '-'
            (then
              (local.set $neg-exp (i32.const 1))
              (%inc $offset)
            )
            (else 
              (if (%sssw 0x2B 1) ;; '+'
                (then (%inc $offset))
              )
            )
          )

          (block $b_exponent_end
            (loop $b_exponent
              ;; break if offset >= str-len
              (br_if $b_exponent_end (i32.ge_u (local.get $offset) (local.get $str-len)))

              (local.set $digit (call $get-radix-digit (local.get $str-ptr) (local.get $offset) (local.get $radix)))
              (br_if $b_exponent_end (i32.lt_s (local.get $digit) (i32.const 0)))
              (%inc $offset)

              ;; exponent = fraction * radix + digit
              (local.set $exponent
                (i64.add
                  (i64.mul (local.get $exponent) (i64.extend_i32_u (local.get $radix)))
                  (i64.extend_i32_u (local.get $digit))
                )
              )
              (%inc $exponent-digits)

              (br $b_exponent)
            )
          )

          (if (i32.eqz (local.get $exponent-digits))
            (then (%dec $offset))
          )
        )
      )
    )
  )

  (if (i32.ne (local.get $offset) (local.get $str-len))
    (then (return (global.get $g-false)))
  )
  
  (if (i32.eqz (i32.add (local.get $integer-digits) (local.get $fraction-digits)))
    (then (return (global.get $g-false)))
  )

  (if (i32.eqz (i32.or (local.get $decimal) (local.get $have-exp)))
    (then
      (if (local.get $negative)
        (then
          (local.set $integer (i64.sub (i64.const 0) (local.get $integer)))
        )
      ) 
      (return 
        (call $heap-alloc
          (global.get $g-heap) 
          (%i64-type) 
          (i32.wrap_i64 (local.get $integer))
          (i32.wrap_i64 (i64.shr_u (local.get $integer) (i64.const 32)))
        )
      )
    )
    (else
      ;; TODO actual floating point support lol
      (return (%alloc-error (%sym-32 0x6c616572 4) (local.get $str)))
    )
  ) 

  (return (global.get $g-false))
)

(func $eval (param $env i32) (param $args i32) (result i32)
  (local $type i32)
  (local $op i32)
  (local $cdr i32)


  ;; type = *args & 0xf
  (local.set $type (%get-type $args))

  ;; switch (type) {
  ;; case symbol:
  (if (i32.eq (local.get $type) (%symbol-type))
    (then
      ;; return environment-lookup(env, args)
      (return
        (call $environment-get (local.get $env) (local.get $args))
      )
    )
  )
  ;; case cons:
  (if (i32.eq (local.get $type) (%cons-type))
    (then
      ;; car = args[4]
      ;; op = eval(car)
      ;;  pointfree ->
      ;;    op = eval(env, args[4])
      ;; cdr = args[8]
      ;; return apply(env, op, cdr)
      ;;  pointfree ->
      ;;    return apply(env, eval(env, args[4]), args[8])
      (return
        (call $apply
          (local.get $env)
          (call $eval (local.get $env) (%car-l $args))
          (%cdr-l $args)
        )
      )
    )
  )
  ;; default:
  ;; return args
  (return (local.get $args))
  ;; }
)

(func $eval-list (param $env i32) (param $args i32) (result i32)
  (local $type i32)
  ;; type =*args & 0xF 
  (local.set $type (i32.and (i32.load (local.get $args)) (i32.const 0xF)))
  ;; if (type == %nil-type) {
  (if (i32.eq (local.get $type) (%nil-type))
    (then
    ;; return args
      (return (local.get $args))
    )
  ;; }
  )
  ;; if (type != %cons-type) {
  (if (i32.ne (local.get $type) (%cons-type)) 
    ;; trap
    (then unreachable)
  ;; }
  )

  ;; return heap-alloc(g-heap, %cons-type, eval(env, args[4]), eval-list(env, args[8])) 
  (return
    (call $heap-alloc
      (global.get $g-heap)
      (%cons-type)
      (call $eval (local.get $env) (i32.load offset=4 (local.get $args)))
      (call $eval-list (local.get $env) (i32.load offset=8 (local.get $args)))
    )
  )
)

(func $apply (param $env i32) (param $op i32) (param $args i32) (result i32)
  (local $op-type i32)
  (local $curr i32)
  (local $head i32)

  ;; op-type = *op & 0xF
  (local.set $op-type (%get-type $op))

  ;; switch (op-type) {
  (block $b_check
    ;; case %special-type:
    ;;   special forms are called without having their arguments executed first
    ;;   break
    (br_if $b_check (i32.eq (local.get $op-type) (%special-type)))

    ;; all other calls to apply execute arguments
    ;; args = eval-list(env, args)
    (local.set $args (call $eval-list (local.get $env) (local.get $args)))
    ;; case $builtin-type:
    (br_if $b_check (i32.eq (local.get $op-type) (%builtin-type)))

    ;; case %lambda-type:
    (if (i32.eq (local.get $op-type) (%lambda-type))
      (then
        (return 
          (call $apply-lambda (local.get $env) (local.get $op) (local.get $args))
        )
      )
    )
    ;; default:
    ;;   any other type:
    (return
      (%alloc-error-cons (%sym-64 0x796c707061 5) (%alloc-cons (local.get $op) (local.get $args)))
    )
  )

  (local.get $env)
  (local.get $args)
  (i32.load offset=4 (local.get $op))
  call_indirect $table-builtin (type $builtin-type)
)

(func $apply-lambda (param $env i32) (param $lambda i32) (param $args i32) (result i32)
  (local $closure i32)
  (local $lambda-args i32)
  (local $formals i32)
  (local $body i32)
  (local $child-env i32)
  (local $result i32)

  ;; closure = lambda[4]
  (local.set $closure (i32.load offset=4 (local.get $lambda)))
  ;; lambda-args = lambda[8]
  (local.set $lambda-args (i32.load offset=8 (local.get $lambda)))

  ;; formals = car(lambda-args)
  (local.set $formals (%car-l $lambda-args))
  ;; body = cdr(lambda-args)
  (local.set $body (%cdr-l $lambda-args))

  ;; child-env = environment-init(gHeap, env)
  (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $closure)))

  ;; zip-lambda-args(child-env, formals, args)
  (call $zip-lambda-args (local.get $child-env) (local.get $formals) (local.get $args))

  ;; result = g-nil
  (local.set $result (global.get $g-nil))
  ;; while (typeof body == cons) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.ne (%get-type $body) (%cons-type)))

      ;; result = eval(child-env, car(body))
      (local.set $result (call $eval (local.get $child-env) (%car-l $body)))
      ;; body = cdr(body)
      (local.set $body (%cdr-l $body))

      (br $b_start)
    )
  ;; }
  )

  ;; return result
  (return (local.get $result))
)

(func $cons (param $car i32) (param $cdr i32) (result i32)
  (return 
    (call $heap-alloc
      (global.get $g-heap)
      (%cons-type)
      (local.get $car)
      (local.get $cdr)
    )
  )
)

(func $zip-lambda-args (param $env i32) (param $formals i32) (param $args i32)
  (loop $forever
    ;; if (get-type(formals) == nil-type) {
    (if (i32.eq (%get-type $formals) (%nil-type))
      ;; return g-nil
      (then return)
    ;; }
    )
    ;; if (get-type(formals) == symbol-type) {
    (if (i32.eq (%get-type $formals) (%symbol-type))
      (then
        ;; environment-add(env, formals, args)
        (call $environment-add (local.get $env) (local.get $formals) (local.get $args))
        ;; return
        (return)
      )
      ;; }
    )

    ;; if (get-type(args) == nil-type) {
    (if (i32.eq (%get-type $args) (%nil-type))
      ;; TODO return an error for too few args
      ;; trap
      (then unreachable)
    ;; }
    )

    ;; environment-add(env, car(formals), car(args))
    ;; formals = cdr(formals)
    ;; args = cdr(args)
    (call $environment-add (local.get $env) (%car-l $formals) (%car-l $args))
    (local.set $formals (%cdr-l $formals))
    (local.set $args (%cdr-l $args))

    (br $forever)
  )
)

(func $argument-error (param $args i32) (result i32)
  (return
    (%alloc-error-cons (global.get $g-args) (local.get $args))
  )
)
