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
(global $g-not-impl   (mut i32) (i32.const 0))
(global $g-vec-open   (mut i32) (i32.const 0))
(global $g-u8vec      (mut i32) (i32.const 0))
(global $g-u8-open    (mut i32) (i32.const 0))
(global $g-else       (mut i32) (i32.const 0))
(global $g-arrow      (mut i32) (i32.const 0))
(global $g-apply      (mut i32) (i32.const 0))
(global $g-interned-str   (mut i32) (i32.const 0))
(global $g-eval       (mut i32) (i32.const 0))
(global $g-gc-run     (mut i32) (i32.const 0))
(global $g-div0       (mut i32) (i32.const 0))
(global $g-inf        (mut i32) (i32.const 0))
(global $g-neg-inf    (mut i32) (i32.const 0))
(global $g-nan        (mut i32) (i32.const 0))
(global $g-neg-nan    (mut i32) (i32.const 0))
(global $g-fzero      (mut i32) (i32.const 0))
(global $g-exp        (mut i32) (i32.const 0))
(global $g-neg        (mut i32) (i32.const 0))
(global $g-cont-type  (mut i32) (i32.const 0))
(global $g-curr-cont  (mut i32) (i32.const 0))

(global $g-eval-count (mut i32) (i32.const 0))
(%define %gc-threshold () (i32.const 256))

(func $runtime-init
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
  (global.set $g-space (%sym-32 0x20 1))          ;; ' '
  (global.set $g-error (%sym-64 0x726f727265 5))  ;; error
  (global.set $g-open (%sym-32 0x28 1))           ;; (
  (global.set $g-close (%sym-32 0x29 1))          ;; )
  (global.set $g-dot (%sym-32 0x202e20 3))        ;; ' . '
  (global.set $g-eof (%sym-32 0x666f65 3))        ;; eof
  (global.set $g-quote (%sym-32 0x22 1))          ;; "
  (global.set $g-args (%sym-32 0x73677261 4))     ;; args
  (global.set $g-not-impl (%sym-64 0x6c706d692D746f6e 8))     ;; not-impl
  (global.set $g-vec-open (%sym-32 0x2823 2))     ;; #(
  (global.set $g-u8vec (%sym-64 0x6365763875 5))  ;; u8vec
  (global.set $g-u8-open (%sym-32 0x28387523 4))  ;; #u8(
  (global.set $g-else (%sym-32 0x65736c65 4))     ;; else
  (global.set $g-arrow (%sym-32 0x3E3D 2))        ;; =>
  (global.set $g-apply (%sym-64 0x796c707061 5))  ;; apply
  (global.set $g-interned-str (%sym-128 0x64656e7265746e69 0x203A 10)) ;; 'interned: '
  (global.set $g-eval (%sym-64 0x203A6c617665 6))  ;; 'eval: '
  (global.set $g-gc-run (%sym-32 0x0A6367 3)) ;; 'gc\n'
  (global.set $g-div0 (%sym-32 0x30766964 4)) ;; 'div0'
  (global.set $g-inf (%sym-64 0x302e666e692b 6)) ;; '+inf.0'
  (global.set $g-neg-inf (%sym-64 0x302e666e692d 6)) ;; '-inf.0'
  (global.set $g-nan (%sym-64 0x302e6e616e2b 6)) ;; '+nan.0'
  (global.set $g-neg-nan (%sym-64 0x302e6e616e2d 6)) ;; '-nan.0'
  (global.set $g-fzero (%sym-32 0x302e30 3)) ;; '0.0'
  (global.set $g-exp (%sym-32 0x65 1)) ;; 'e'
  (global.set $g-neg (%sym-32 0x2d 1)) ;; '-'
  (global.set $g-cont-type (%sym-32 0x746e6f63 4)) ;; 'cont'

  ;; global reader is attached to stdin
  (global.set $g-reader (call $reader-init (i32.const 0)))
  (call $char-init)
  (call $cont-init)
  (call $grisu-init)
)

(func $runtime-cleanup
  (call $grisu-cleanup)
  (call $cont-cleanup)
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

  (call $mp-cleanup)
)

(func $read (result i32)
  (return (call $read-with-reader (global.get $g-reader))))

(func $read-with-reader (param $reader i32) (result i32)
  (local $token-str i32)
  (local $result i32)

  ;; token-str = reader-read-token(g-reader)
  (local.set $token-str (call $reader-read-token (local.get $reader)))

  ;; return string->datum(token-str)
  (local.set $result (call $string->datum-with-reader 
      (local.get $token-str) 
      (local.get $reader)))
  (if (i32.eq (%get-type $result) (%error-type))
    (then 
      (if (i32.eq (global.get $g-eof) (%car-l $result))
        (then 
          (call $reader-rollback (local.get $reader))
          (return (local.get $result))
        )
      )
    )
  )

  (call $reader-commit (local.get $reader))
  (return (local.get $result))
)

(func $string->datum (param $token-str i32) (result i32)
  (return (call $string->datum-with-reader 
      (local.get $token-str)
      (global.get $g-reader))))

(func $string->datum-with-reader (param $token-str i32) (param $reader i32) (result i32)
  (local $car-str i32)
  (local $car i32)
  (local $cdr-str i32)
  (local $cdr i32)
  (local $raw-token i32)
  (local $head i32)
  (local $curr i32)
  (local $list-vector i32)

  (%define %check-str (%str) (if (i32.eqz (local.get %str)) (then
        (return (%alloc-error-cons 
            (global.get $g-eof) 
            (global.get $g-nil))))))

  (%check-str $token-str)

  (block $b_lv (block $b_list (block $b_vector (block $b_byte_vector
          (br_if $b_list 
            (call $short-str-eq (local.get $token-str) (i32.const 0x28) (i32.const 1)))
          (br_if $b_vector 
            (call $short-str-eq (local.get $token-str) (i32.const 0x2823) (i32.const 2)))
          (br_if $b_byte_vector
            (call $short-str-eq (local.get $token-str) (i32.const 0x28387523) (i32.const 4)))

          (local.set $list-vector (i32.const 0))
          (br $b_lv))
        (local.set $list-vector (i32.const 3))
        (br $b_lv))
      (local.set $list-vector (i32.const 2))
      (br $b_lv))
    (local.set $list-vector (i32.const 1)))

  ;; if (token-str == '(' || token-str == '#(' || token-str == '#u8(') {
  (if (local.get $list-vector) (then
      ;; malloc-free(token-str)
      (call $malloc-free (local.get $token-str))

      ;; car-str = reader-read-token(g-reader)
      (local.set $car-str (call $reader-read-token (local.get $reader)))
      (%check-str $car-str)
      ;; if (car-str == ')') {
      (if (call $short-str-eq (local.get $car-str) (i32.const 0x29) (i32.const 1))
        (then
          ;; malloc-free(car-str)
          (call $malloc-free (local.get $car-str))

          (if (i32.eq (local.get $list-vector) (i32.const 1))
            (then (return (global.get $g-nil))))
          (if (i32.eq (local.get $list-vector) (i32.const 2))
            (then (return (call $make-vector-internal (global.get $g-nil))))
            (else (return (call $make-byte-vector-internal (global.get $g-nil)))))))

      ;; car = string->datum(car-str)
      (local.set $car (call $string->datum-with-reader
          (local.get $car-str) 
          (local.get $reader)))
      ;; curr = head = heap-alloc(3, car, g-nil)
      (local.set $curr (local.tee $head (%alloc-cons 
            (local.get $car) 
            (global.get $g-nil))))

      ;; while (true) {
      (loop $forever
        ;; cdr-str = reader-read-token(g-reader)
        (local.set $cdr-str (call $reader-read-token (local.get $reader)))
        (%check-str $cdr-str)
        ;; if (cdr-str == ')') {
        (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x29) (i32.const 1))
          (then
            ;; malloc-free(cdr-str)
            (call $malloc-free (local.get $cdr-str))

            (if (i32.eq (local.get $list-vector) (i32.const 1))
              (then (return (local.get $head)))
            )
            (if (i32.eq (local.get $list-vector) (i32.const 2))
              (then (return (call $make-vector-internal (local.get $head))))
            )
            (return (call $make-byte-vector-internal (local.get $head)))))

        ;; }
        ;; if (cdr-str == '.') {
        (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x2E) (i32.const 1))
          (then
            ;; malloc-free(cdr-str)
            (call $malloc-free (local.get $cdr-str))
            ;; cdr-str = reader-read-token(g-reader)
            (local.set $cdr-str (call $reader-read-token (local.get $reader)))
            (%check-str $cdr-str)
            ;; cdr = string->datum(cdr-str)
            (local.set $cdr (call $string->datum-with-reader
                (local.get $cdr-str) 
                (local.get $reader)))
            ;; curr[8] = cdr
            (i32.store offset=8 (local.get $curr) (local.get $cdr))
            ;; cdr-str = reader-read-token(g-reader)
            (local.set $cdr-str (call $reader-read-token (local.get $reader)))
            (%check-str $cdr-str)
            ;; if (cdr-str == ')') {
            (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x29) (i32.const 1))
              (then
                ;; malloc-free(cdr-str)
                (call $malloc-free (local.get $cdr-str))
                ;; TODO error if vector or bytevector
                ;; return head
                (return (local.get $head)))
              ;; } else {
                ;; TODO return error
              (else (unreachable)))))

        ;; cdr = string->datum(cdr-str)
        (local.set $cdr (call $string->datum-with-reader
            (local.get $cdr-str) 
            (local.get $reader)))

        ;; curr[8] = heap-alloc(3, cdr, g-nil)
        (i32.store offset=8
          (local.get $curr)
          (%alloc-cons (local.get $cdr) (global.get $g-nil)))

        ;; curr = curr[8]
        (local.set $curr (i32.load offset=8 (local.get $curr)))

        (br $forever))))

  ;; if (token-str == ')') {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x29) (i32.const 1))
    ;;  TODO: return error
    (then (unreachable)))

  ;; if (token-str == '.') 0x2E {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x2E) (i32.const 1))
    ;;  TODO: return error
    (then (unreachable)))

  ;; }
  ;; if (token-str == "'") 0x27 {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x27) (i32.const 1))
    (then
      ;; malloc-free(token-str)
      (call $malloc-free (local.get $token-str))
      ;; return cons('quote', cons(read(), nil))
      (return (%alloc-cons 
          (global.get $quote-sym) 
          (%alloc-cons 
            (call $read-with-reader (local.get $reader)) 
            (global.get $g-nil))))))

  ;; return atom(token);
  (return (call $atom (%alloc-str (local.get $token-str)))))

(func $make-vector-internal (param $list i32) (result i32)
  (local $len i32)
  (local $ptr i32)
  (local $vec i32)

  (local.set $len (call $list-len (local.get $list)))
  (local.set $ptr (call $malloc (%word-size-l $len)))

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

(func $make-byte-vector-internal (param $list i32) (result i32)
  (local $bytes i32)
  (local $len i32)
  (local $ptr i32)
  (local $byte-vec i32)
  (local $curr i32)
  (local $temp64 i64)

  (local.set $len (call $list-len (local.get $list)))
  (local.set $ptr (call $malloc (local.get $len)))

  (local.set $byte-vec 
    (call $heap-alloc 
      (global.get $g-heap)
      (%bytevector-type)
      (local.get $ptr)
      (local.get $len)))

  (local.set $bytes (local.get $list))
  (block $b_error
    (loop $forever
      (if (i32.eq (%get-type $bytes) (%nil-type))
        (then (return (local.get $byte-vec))))

      (%pop-l $curr $bytes)
      (%chk-type $b_error $curr %i64-type)
      (local.set $temp64 (i64.load offset=4 (local.get $curr)))
      (br_if $b_error (i64.gt_u (local.get $temp64) (i64.const 0xFF)))

      (i32.store8 (local.get $ptr) (i32.wrap_i64 (local.get $temp64)))

      (%inc $ptr)
      (br $forever)))

  (return (%alloc-error-cons (global.get $g-u8vec) (local.get $list))))

(func $atom (param $token i32)  (result i32)
  (local $token-str i32)
  (local $str-len i32)
  (local $atom-str i32)
  (local $atom i32)

  ;; if ((token[0] & 0xF) != 7 {
  (if (i32.ne (%get-type $token) (%str-type)) (then unreachable))

  ;; token-str == token[4]
  (local.set $token-str (i32.load offset=4 (local.get $token)))

  ;; if (token-str == '#t') {
  (if (call $short-str-eq
      (local.get $token-str)
      (i32.const 0x7423)
      (i32.const 2))
    (then (return (global.get $g-true))))

  ;; if (token-str == '#f') {
  (if (call $short-str-eq 
      (local.get $token-str) 
      (i32.const 0x6623) 
      (i32.const 2))
    (then (return (global.get $g-false))))

  ;; if token-str.startsWith('str ')
  ;; if (short-str-start-with(token-str, 0, 'str ', 4)) {
  (if (call $short-str-start-with 
      (local.get $token-str) 
      (i32.const 0) 
      (i32.const 0x20727473) 
      (i32.const 4))
    (then
      (local.set $str-len (i32.load (local.get $token-str)))
      (i32.store offset=4 
        (local.get $token-str) 
        (i32.sub (local.get $str-len) (i32.const 4)))
      (local.set $atom-str (call $str-dup (i32.add (local.get $token-str) (i32.const 4))))
      (i32.store offset=4 (local.get $token-str) (i32.const 0x20727473))
      (return (%alloc-str (local.get $atom-str)))))

  ;; if token.str.startsWith('#\')
  (if (call $short-str-start-with
      (local.get $token-str) 
      (i32.const 0) 
      (i32.const 0x5c23) 
      (i32.const 2))
    (then (return (call $read-char (local.get $token-str)))))

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
    (then unreachable))

  ;; if (*str != short-str-len) {
  (if (i32.ne (i32.load (local.get $str)) (local.get $short-str-len))
    ;; return 0;
    (then (return (i32.const 0)))
  ;; } else {
    (else
      ;; return (str[4] == short-str)
      (return (i32.eq
          (i32.load offset=4 (local.get $str))
          (local.get $short-str)))))
  (unreachable))

(func $short-str-start-with
  (param $str i32)
  (param $offset i32)
  (param $short-str i32)
  (param $short-str-len i32)
  (result i32)

  (local $word i32)
  (local $mask i32)

  (if (i32.gt_u (local.get $short-str-len) (i32.const 4))
    (then (unreachable)))

  ;; if (offset + short-str-len > *ptr) {
  (if (i32.gt_u (i32.add (local.get $offset) (local.get $short-str-len))
                (i32.load (local.get $str)))
    (then
      ;; return 0;
      (return (i32.const 0))))

  ;; word = str[4 + offset] 
  (local.set $word (i32.load offset=4 (i32.add (local.get $str) (local.get $offset))))
  ;; mask = -1 >> (4-len) << 3
  (local.set $mask 
    (i32.shr_u 
      (i32.const -1) 
      (i32.shl 
        (i32.sub (i32.const 4) (local.get $short-str-len))
        (i32.const 3))))

  (return 
    (i32.eq 
      (i32.and (local.get $mask) (local.get $word))
      (local.get $short-str))))

(func $is-truthy (param $token i32) (result i32)
  (if (i32.eq (%get-type $token) (%boolean-type))
    (then
      ;; if (car(token) == 0) {
      (if (i32.eqz (%car-l $token))
        ;; return 0
        (then (return (i32.const 0))))))

  ;; return 1
  (return (i32.const 1)))

(func $get-radix-digit (param $str i32) (param $offset i32) (param $radix i32) (result i32)
  (local $c i32)
  (local $digit i32)

  (local.set $c (i32.load8_u offset=4 (i32.add (local.get $str) (local.get $offset))))

  (block $b_digit
    ;; c >= 0x30 && c <= 0x39 ('0' <= c <= '9')
    (if (i32.ge_u (local.get $c) (i32.const 0x30)) (then
        (if (i32.le_u (local.get $c) (i32.const 0x39)) (then
            (local.set $digit (i32.sub (local.get $c) (i32.const 0x30)))
            (br $b_digit)))))

    ;; c >= 0x61 && c <= 66 ('a' <= c <= 'f')
    (if (i32.ge_u (local.get $c) (i32.const 0x61)) (then
        (if (i32.le_u (local.get $c) (i32.const 0x66)) (then
            ;; digit = c - 0x61 + 0x0A
            ;; digit = c - 0x57
            (local.set $digit (i32.sub (local.get $c) (i32.const 0x57)))
            (br $b_digit)))))

    ;; c >= 0x41 && c <= 46 ('A' <= c <= 'F')
    (if (i32.ge_u (local.get $c) (i32.const 0x41)) (then
        (if (i32.le_u (local.get $c) (i32.const 0x46)) (then
            ;; digit = c - 0x41 + 0x0A
            ;; digit = c - 0x37
            (local.set $digit (i32.sub (local.get $c) (i32.const 0x37)))
            (br $b_digit)))))

    ;; else
    (return (i32.const -1)))

  (return (select 
      (local.get $digit) 
      (i32.const -1) 
      (i32.lt_u (local.get $digit) (local.get $radix)))))

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
  (local $fraction-start i32)
  (local $integer-start i32)
  (local $integer-digits i32)
  (local $integer-overflow i32)
  (local $digit i32)
  (local $decimal i32)
  (local $have-exp i32)
  (local $exponent i32)
  (local $exponent-overflow i32)
  (local $exponent-digits i32)
  (local $neg-exp i32)
  (local $big-int i32)
  (local $big-int-fraction i32)
  (local $num i32)
  (local $real f64)


  (if (i32.ne (%get-type $str) (%str-type)) (then 
      (return (global.get $g-false))))

  (local.set $str-ptr (%car-l $str))
  (local.set $str-len (i32.load (local.get $str-ptr)))
  (local.set $offset (i32.const 0))
  (local.set $exact (i32.const 0))
  (local.set $inexact (i32.const 0))
  (local.set $radix-count (i32.const 0))
  (local.set $negative (i32.const 0))
  (local.set $decimal (i32.const 0))
  (local.set $integer (i64.const 0))
  (local.set $fraction-start (i32.const 0))
  (local.set $fraction-digits (i32.const 0))
  (local.set $integer-start (i32.const 0))
  (local.set $integer-digits (i32.const 0))
  (local.set $integer-overflow (i32.const 0))
  (local.set $exponent-overflow (i32.const 0))
  (local.set $exponent-digits (i32.const 0))
  (local.set $have-exp (i32.const 0))
  (local.set $exponent (i32.const 0))
  (local.set $neg-exp (i32.const 0))

  (%define %sssw (%cmp %len) (call $short-str-start-with (local.get $str-ptr) (local.get $offset) (i32.const %cmp) (i32.const %len)))

  ;; check for +inf.0 -inf.0 +nan.0 -nan.0
  (block $b_no_real_special
    (block $b_real_special
      (if (call $str-eq (local.get $str-ptr) (%car (global.get $g-inf)))
        (then
          (local.set $real (f64.const inf))
          (br $b_real_special)))
      (if (call $str-eq (local.get $str-ptr) (%car (global.get $g-neg-inf)))
        (then
          (local.set $real (f64.const -inf))
          (br $b_real_special)))
      (if (call $str-eq (local.get $str-ptr) (%car (global.get $g-nan)))
        (then
          (local.set $real (f64.const nan))
          (br $b_real_special)))
      (if (call $str-eq (local.get $str-ptr) (%car (global.get $g-neg-nan)))
        (then
          (local.set $real (f64.reinterpret_i64 (i64.const 0xFFF0_0000_0000_0001)))
          (br $b_real_special)))
      (br $b_no_real_special))
    
    (return (%alloc-f64 (local.get $real))))

  ;; look for prefix elements #e #i #b #d #o #x
  ;; while (true) {
  (loop $prefix_loop
    (if (%sssw 0x4523 2) ;; #E
      (then
        (%inc $exact)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x6523 2) ;; #e
      (then
        (%inc $exact)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x4923 2) ;; #I
      (then
        (%inc $inexact)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x6923 2) ;; #i
      (then
        (%inc $inexact)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x4223 2) ;; #B
      (then
        (local.set $radix (i32.const 2))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x6223 2) ;; #b
      (then
        (local.set $radix (i32.const 2))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x4423 2) ;; #D
      (then
        (local.set $radix (i32.const 10))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x6423 2) ;; #d
      (then
        (local.set $radix (i32.const 10))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x4F23 2) ;; #O
      (then
        (local.set $radix (i32.const 8))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x6F23 2) ;; #0
      (then
        (local.set $radix (i32.const 8))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x5823 2) ;; #X
      (then
        (local.set $radix (i32.const 16))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop)))

    (if (%sssw 0x7823 2) ;; #x
      (then
        (local.set $radix (i32.const 16))
        (%inc $radix-count)
        (%plus-eq $offset 2)
        (br $prefix_loop))))

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
      (br $prefix_fail))

    (return (%alloc-error (%sym-64 0x786966657270 6) (local.get $str))))

  ;; check for sign
  (if (%sssw 0x2d 1) ;; '-'
    (then
      (local.set $negative (i32.const 1))
      (%inc $offset))
    (else 
      (if (%sssw 0x2B 1) ;; '+'
        (then (%inc $offset)))))

  (local.set $integer-start (local.get $offset))
  (block $b_integer_end
    (loop $b_integer
      ;; break if offset >= str-len
      (br_if $b_integer_end (i32.ge_u (local.get $offset) (local.get $str-len)))

      (local.set $digit (call $get-radix-digit (local.get $str-ptr) (local.get $offset) (local.get $radix)))
      (br_if $b_integer_end (i32.lt_s (local.get $digit) (i32.const 0)))

      (if (i32.eqz (local.get $integer-overflow))
        ;; integer = integer * radix + digit
        (local.set $integer
          (i64.add
            (i64.mul (local.get $integer) (i64.extend_i32_u (local.get $radix)))
            (i64.extend_i32_u (local.get $digit)))))

      ;; check for overflow, if it is detected then a big int conversion 
      ;; will be done below
      (if 
        (i64.eq 
          (i64.and (local.get $integer) (i64.const 0x8000_0000_0000_0000))
          (i64.const 0x8000_0000_0000_0000))
        (then (local.set $integer-overflow (i32.const 1))))

      (%inc $offset)
      (%inc $integer-digits)

      (br $b_integer)))

  (if (i32.eq (local.get $radix) (i32.const 10))
    (then
      ;; check for decimal
      (if (%sssw 0x2e 1) ;; '.'
        (then
          (local.set $decimal (i32.const 1))
          (%inc $offset)

          (local.set $fraction-start (local.get $offset))
          (block $b_fraction_end
            (loop $b_fraction
              ;; break if offset >= str-len
              (br_if $b_fraction_end (i32.ge_u (local.get $offset) (local.get $str-len)))

              (local.set $digit (call $get-radix-digit (local.get $str-ptr) (local.get $offset) (local.get $radix)))
              (br_if $b_fraction_end (i32.lt_s (local.get $digit) (i32.const 0)))

              (%inc $offset)
              (%inc $fraction-digits)
              (br $b_fraction)))))

      ;; check for exponent
      (if (i32.or (%sssw 0x65 1) (%sssw 0x45 1)) (then
          (local.set $have-exp (i32.const 1))
          (%inc $offset)

          ;; check for sign
          (if (%sssw 0x2d 1) ;; '-'
            (then
              (local.set $neg-exp (i32.const 1))
              (%inc $offset))
            (else 
              (if (%sssw 0x2B 1) ;; '+'
                (then (%inc $offset)))))

          (block $b_exp_end (loop $b_exp
              ;; break if offset >= str-len
              (br_if $b_exp_end (i32.ge_u 
                  (local.get $offset) 
                  (local.get $str-len)))

              (local.set $digit (call $get-radix-digit
                  (local.get $str-ptr)
                  (local.get $offset)
                  (local.get $radix)))

              (br_if $b_exp_end (i32.lt_s (local.get $digit) (i32.const 0)))
              (%inc $offset)

              ;; exponent = fraction * radix + digit
              (local.set $exponent (i32.add
                  (i32.mul (local.get $exponent) (local.get $radix))
                  (local.get $digit)))
              (if (i32.and (local.get $exponent) (i32.const 0x8000_0000))
                (local.set $exponent-overflow (i32.const 1)))

              (%inc $exponent-digits)
              (br $b_exp)))

          (if (i32.eqz (local.get $exponent-digits))
            (then (%dec $offset)))))))

  ;; There are extra characters in the string, not a valid number.
  (if (i32.ne (local.get $offset) (local.get $str-len))
    (then (return (global.get $g-false))))

  ;; There are no integer or fractional digits, not a valid number. 
  (if (i32.eqz (i32.add (local.get $integer-digits) (local.get $fraction-digits)))
    (then (return (global.get $g-false))))

  ;; The exponent overflowed a signed 32-bit integer, not a valid number.
  (if (local.get $exponent-overflow)
    (then (return (global.get $g-false))))

  (if (i32.eqz (i32.or (local.get $decimal) (local.get $have-exp)))
    ;; There was no decimal point and no exponent, therefore this is an integer
    (then
      (if (i32.eqz (local.get $integer-overflow))
        (then
          (if (local.get $negative)
            (then
              (local.set $integer (i64.sub (i64.const 0) (local.get $integer))))) 
          (local.set $num (%alloc-i64 (local.get $integer))))
        (else
          (local.set $big-int (call $mp-string->mp
              (i32.add (i32.add (local.get $str-ptr) (local.get $integer-start)) (i32.const 4))
              (local.get $integer-digits)
              (local.get $radix)))
          (if (local.get $negative)
            (then (call $mp-neg (local.get $big-int))))
          (local.set $num (%alloc-big-int (local.get $big-int)))))
      (if (local.get $inexact) (then
        (return (call $inexact-impl (local.get $num)))))
      (return (local.get $num))))

  ;; apply the exponent sign
  (if (local.get $neg-exp) (then 
      (local.set $exponent (i32.sub (i32.const 0) (local.get $exponent)))))

  ;; create a big-int containing both the integer and fractional parts
  (if (local.get $integer-overflow)
    (then (local.set $big-int (call $mp-string->mp
          (i32.add 
            (i32.add 
              (local.get $str-ptr) 
              (local.get $integer-start))

            (i32.const 4))
          (local.get $integer-digits)
          (local.get $radix))))
    (else (local.set $big-int (call $mp-from-u64 (local.get $integer)))))

  (if (local.get $fraction-digits) (then
      (local.set $big-int-fraction (call $mp-string->mp
              (i32.add (i32.add 
                  (local.get $str-ptr) 
                  (local.get $fraction-start)) 
                (i32.const 4))
              (local.get $fraction-digits)
              (local.get $radix)))
      (local.set $big-int (call $mp-times-eq 
          (local.get $big-int) 
          (call $mp-10-pow (local.get $fraction-digits))))
      (local.set $big-int (call $mp-plus-eq
          (local.get $big-int) 
          (local.get $big-int-fraction)))
      (local.set $exponent (i32.sub
          (local.get $exponent) 
          (local.get $fraction-digits)))
      (call $malloc-free (local.get $big-int-fraction))))

  (if (local.get $exact) (then 
      ;; should not exactify a fraction
      (if (i32.lt_s (local.get $exponent) (i32.const 0)) (then
          (return (%alloc-error (%sym-64 0x786966657270 6) (local.get $str)))))

      ;; apply the exponent
      (if (i32.gt_s (local.get $exponent) (i32.const 0)) (then
          (local.set $big-int (call $mp-times-eq
              (local.get $big-int)
              (call $mp-10-pow (local.get $exponent))))))

      (if (local.get $negative) (then 
          (call $mp-neg (local.get $big-int)))

      (return (%alloc-big-int (local.get $big-int))))))

  (local.set $real (call $mp-algorithm-m 
      (local.get $big-int) 
      (local.get $exponent)))
  (call $malloc-free (local.get $big-int))

  (if (local.get $negative) (then 
          (local.set $real (f64.neg (local.get $real)))))

  (return (%alloc-f64 (local.get $real))))

(func $eval (param $env i32) (param $args i32) (result i32)
  (local $result i32)
  (local $result-type i32)
  (local $fn i32)
  (local $gray i32)

  (local $cont-stack i32)
  (local $prev-cont i32)
  (local $temp-cont i32)
  (local $next-cont i32)
  (local $curr-cont i32)

  (local $res-mode i32)
  (local $handler i32)

  (if (i32.eq (%get-type $args) (%cont-type))
    (then
      (local.set $cont-stack (local.get $args))
      (local.set $curr-cont (%car-l $cont-stack))
      
      (local.set $fn (i32.load offset=0 (local.get $curr-cont)))
      (local.set $env (i32.load offset=4 (local.get $curr-cont)))
      (local.set $args (i32.load offset=8 (local.get $curr-cont))))
    (else
      (local.set $cont-stack (call $cont-alloc
        (%eval-fn)
        (local.get $env)
        (local.get $args)
        (i32.const 0)))
      (local.set $fn (%eval-fn))))

  (loop $forever
    ;; set the current continuation (so that call/cc can get it)
    ;; TODO solve this without globals?
    (global.set $g-curr-cont (local.get $cont-stack))

    (block $b_eval_cont
      (if (i32.eqz (local.get $fn)) (then
          ;; check if a gc has been indicated
          (if (i32.ge_u (global.get $g-eval-count) (%gc-threshold))
            (then
              ;; gray set must include cont stack, args, and env. If there is already
              ;; a collection, simply allocating these will add them to the gray set
              ;; otherwise they are passed into the call to gc-run (and thence to 
              ;; gc-init)
              (local.set $gray (%alloc-list-3 
                  (local.get $env) 
                  (local.get $args)
                  (local.get $cont-stack)))

              ;; (call $print-symbol (global.get $g-gc-run))
              (call $gc-run (local.get $gray))
              (global.set $g-eval-count (i32.const 0))))

          ;; this is a call to eval. So pass to eval inner
          (local.set $result (call $eval-inner (local.get $env) (local.get $args)))
          (br $b_eval_cont)))

      (if (i32.eq (local.get $fn) (%guard-fn)) (then
          ;; executing a guard fn is a no-op, with no return value, if the args 
          ;; stack has a value simply pop it and set it as the result, otherwise
          ;; set result to 0
          (if (i32.eq 
              (local.get $args) 
              (i32.load offset=8 (local.get $cont-stack))) 
            (then
              ;; args is the same as included in the buffer, set result to 0
              (local.set $result (i32.const 0)))
            (else
              ;; there is an argument to pass to the next continuation
              (local.set $result (%car-l $args))))
          
          (br $b_eval_cont)))

      ;; this is a promise from the "other-side" (the host), so we will return 
      ;; this, and resume when the host sends it back to us.
      (if (i32.eq (local.get $fn) (%cont-import-promise)) (then
          (global.set $g-curr-cont (i32.const 0))
          (return (local.get $cont-stack))))

      ;; some other function is the continuation
      (local.get $env)
      (local.get $args)
      (local.get $fn)
      call_indirect $table-builtin (type $builtin-type)
      (local.set $result))

    ;; done with this item, so pop it off the cont-stack
    (local.set $cont-stack (%cdr-l $cont-stack))

    (block $b_done
      (local.set $result-type (%get-type $result))
      (if (i32.eq (local.get $result-type) (%cont-proc-type)) (then
          ;; Result is a continuation proc,  check if it is populated 
          ;; (i.e. it has been "called")
          (if (%cdr-l $result) (then
              ;; result is populated, so set contintuation stack, and the result
              (local.set $cont-stack (%car-l $result))
              (local.set $result (%car (%cdr-l $result)))
              (local.set $result-type (%get-type $result))))))

      (if (i32.eq (local.get $result-type) (%cont-type)) (then 
          ;; result is a continuation (or list of), place them on the top of the 
          ;; continuation stack
          (local.set $temp-cont (local.get $result))
          (block $t_end
            (loop $t_start
              (local.set $next-cont (%cdr-l $temp-cont))
              (br_if $t_end (i32.eqz (local.get $next-cont)))
              (local.set $temp-cont (local.get $next-cont))
              (br $t_start)))
          (%set-cdr!-l $temp-cont $cont-stack)
          (local.set $cont-stack (local.get $result))
          (local.set $result (i32.const 0))
          (br $b_done)))

      (if (i32.eq (local.get $result-type) (%except-type)) (then
          (local.set $res-mode (%cdr-l $result))

          (if (i32.eq (local.get $res-mode) (i32.const 1)) (then
              ;; result is an raised exception
              ;; pop from the cont stack until a guard is found, or the stack 
              ;; is empty
              (block $b_raise_end (loop $b_raise_start
                  (if (i32.eqz (local.get $cont-stack)) (then
                      (global.set $g-curr-cont (i32.const 0))
                      ;; no guard frame, return the exception as the result
                      (return (%car-l $result))))

                  (local.set $curr-cont (%car-l $cont-stack))

                  ;; if this stack frame is a guard function, then stop here
                  (br_if $b_raise_end (i32.eq 
                      (i32.load (local.get $curr-cont)) 
                      (%guard-fn)))

                  (local.set $cont-stack (%cdr-l $cont-stack))
                  ;; free the old continuation

                  (br $b_raise_start)))

              ;; extract the env and the handler from the top of the cont stack
              (local.set $env (i32.load offset=4 (local.get $curr-cont)))
              (local.set $handler (i32.load offset=8 (local.get $curr-cont)))
              ;; remove the guard frame from the stack
              (local.set $cont-stack (%cdr-l $cont-stack))

              (local.set $temp-cont (call $cont-alloc
                  ;; add an eval frame for the handler
                  (%eval-fn)
                  (local.get $env)
                  (%alloc-list-2 (local.get $handler)  (%car-l $result))
                  (call $cont-alloc
                    ;; add a re-raise frame to the stack
                    (%cont-raise)
                    (local.get $env)
                    (%alloc-list-1 (%car-l $result))
                    (local.get $cont-stack))))

              (local.set $cont-stack (local.get $temp-cont))
              (local.set $result (i32.const 0))

              (br $b_done)))

          ;; A continuable exception was thrown 
          (if (i32.eq (local.get $res-mode) (i32.const 2)) (then
              ;; result is an raised, continuable, exception

              ;; look through the stack to see if there is a guard
              (local.set $temp-cont (local.get $cont-stack))
              (block $b_raise_end (loop $b_raise_start
                  (br_if $b_raise_end (i32.eqz (local.get $temp-cont)))

                  (local.set $curr-cont (%car-l $cont-stack))

                  ;; if this stack frame is a guard function, then stop here
                  (br_if $b_raise_end (i32.eq 
                      (i32.load (local.get $curr-cont)) 
                      (%guard-fn)))

                  (local.set $temp-cont (%cdr-l $temp-cont))
                  (br $b_raise_start)))

              (if (i32.eqz (local.get $temp-cont)) (then
                  (global.set $g-curr-cont (i32.const 0))
                  ;; no guard frame, return the exception as the result
                  (return (%car-l $result))))

              ;; extract the env and the handler from the top of the cont stack
              (local.set $env (i32.load offset=4 (local.get $curr-cont)))
              (local.set $handler (i32.load offset=8 (local.get $curr-cont)))

              (local.set $temp-cont (call $cont-alloc
                  ;; add an eval frame for the handler, the result from this
                  ;; will end up on the cont stack
                  (%eval-fn)
                  (local.get $env)
                  (%alloc-list-2 (local.get $handler)  (%car-l $result))
                  (local.get $cont-stack)))

              (local.set $cont-stack (local.get $temp-cont))
              (local.set $result (i32.const 0))

              (br $b_done)))

          ;; unexpected result mode
          (unreachable)))

        ;; not a continuation and not an exception
      (if (i32.eqz (local.get $cont-stack))
        (then
          (global.set $g-curr-cont (i32.const 0))
          ;; nothing on the cont stack, simply return this result
          (return (local.get $result))
        )))

    (local.set $curr-cont (%car-l $cont-stack))
    
    (local.set $fn (i32.load offset=0 (local.get $curr-cont)))
    (local.set $env (i32.load offset=4 (local.get $curr-cont)))
    (if (local.get $result)
      (then
        ;; there is a useful result value pass it to the continuation
        (local.set $args (%alloc-cons
            (local.get $result)
            (i32.load offset=8 (local.get $curr-cont)))))
      (else
        (local.set $args (i32.load offset=8 (local.get $curr-cont)))))

    (br $forever))

  (unreachable))


(func $eval-inner (param $env i32) (param $args i32) (result i32)
  (local $type i32)
  (local $result i32)

  ;; type = *args & 0xf
  (local.set $type (%get-type $args))

  ;; switch (type) {
  ;; case symbol:
  (if (i32.eq (local.get $type) (%symbol-type))
    (then
      ;; return environment-lookup(env, args)
      (return
        (call $environment-get (local.get $env) (local.get $args)))))

  ;; case cons:
  (if (i32.eq (local.get $type) (%cons-type))
    (then
      (%ginc $g-eval-count)

      (if (global.get $g-dump-eval)
        (then
          (call $print-symbol (global.get $g-eval))
          (call $print-symbol-rep (global.get $g-space) (global.get $g-dump-eval-indent))
          (call $print (local.get $args))
          (call $print-symbol (global.get $g-newline))
          (%ginc $g-dump-eval-indent)))

      ;; return apply(env, eval(env, car(args)), cdr(args))
      ;; this is represented in continuation passing as...
      ;;  eval(env, car(args)) => cont-apply(env, args)
      (return (call $cont-alloc
          (%eval-fn) ;; eval
          (local.get $env)
          (%car-l $args)
          (call $cont-alloc
            (%cont-apply)
            (local.get $env)
            (%cdr-l $args)
            (i32.const 0))))))

  ;; default:
  ;; return args
  (return (local.get $args)))

(func $cont-apply (param $env i32) (param $args i32) (result i32)
  (local $op i32)
  (local $result i32)

  (%pop-l $op $args)
  (local.set $result (call $apply (local.get $env) (local.get $op) (local.get $args)))

  (if (global.get $g-dump-eval)
    (then
      (%gdec $g-dump-eval-indent)
      (call $print-symbol-rep 
        (global.get $g-space) 
        (i32.add (global.get $g-dump-eval-indent) (i32.const 6)))
      (call $print-symbol (global.get $g-arrow))
      (call $print-symbol (global.get $g-space))
      (call $print (local.get $result))
      (call $print-symbol (global.get $g-newline))))

  (return (local.get $result)))


(func $apply (param $env i32) (param $op i32) (param $args i32) (result i32)
  (local $op-type i32)
  (local $curr i32)
  (local $head i32)

  ;; op-type = *op & 0xF
  (local.set $op-type (%get-type $op))

  (if (i32.eq (local.get $op-type) (%special-type)) (then
      (local.get $env)
      (local.get $args)
      (i32.load offset=4 (local.get $op))
      call_indirect $table-builtin (type $builtin-type)
      (return)))

  (if (i32.eq (%get-type $args) (%nil-type)) (then
      (return (call $cont-alloc
          (%cont-apply-form)
          (local.get $env)
          (%alloc-cons 
            (global.get $g-nil) 
            (%alloc-cons (local.get $op) (global.get $g-nil)))
          (i32.const 0)))))

  (return (call $cont-alloc
      (%eval-fn)
      (local.get $env)
      (%car-l $args)
      (call $cont-alloc
        (%cont-expr-list)
        (local.get $env)
        (%alloc-cons (global.get $g-nil) (%cdr-l $args))
        (call $cont-alloc
          (%cont-apply-form)
          (local.get $env)
          (%alloc-cons (local.get $op) (global.get $g-nil))
          (i32.const 0))))))

(func $cont-apply-form (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $op i32)
  (local $op-type i32)

  (%pop-l $temp $args)
  (%pop-l $op $args)
  (local.set $op-type (%get-type $op))

  (local.set $args (call $reverse-impl (local.get $temp)))

  (if (i32.eq (local.get $op-type) (%builtin-type)) (then
      (local.get $env)
      (local.get $args)
      (i32.load offset=4 (local.get $op))
      call_indirect $table-builtin (type $builtin-type)
      (return)))

  (if (i32.eq (local.get $op-type (%lambda-type))) (then
      (return (call $apply-lambda 
        (local.get $env) 
        (local.get $op) 
        (local.get $args)))))

  (if (i32.eq (local.get $op-type) (%cont-proc-type)) (then
      (return (call $apply-cont-proc
          (local.get $op)
          (local.get $args)))))

  ;;   any other type:
  (return (%alloc-error-cons 
      (global.get $g-apply) 
      (%alloc-cons (local.get $op) (local.get $args)))))

;; (cont-expr-list val stack args ...)
(func $cont-expr-list (param $env i32) (param $args i32) (result i32)
  (local $val i32)
  (local $stack i32)

  (%pop-l $val $args)
  (%pop-l $stack $args)
  (%push-l $val $stack)

  (if (i32.eq (%get-type $args) (%nil-type))
    (return (local.get $stack)))

  (return (call $cont-alloc
      (%eval-fn) ;; eval
      (local.get $env)
      (%car-l $args)
      (call $cont-alloc
        (%cont-expr-list)
        (local.get $env)
        (%alloc-cons (local.get $stack) (%cdr-l $args))
        (i32.const 0)))))

(func $apply-internal (param $env i32) (param $op i32) (param $args i32) (result i32)
  (local $op-type i32)
  (local $curr i32)
  (local $head i32)

  ;; op-type = *op & 0xF
  (local.set $op-type (%get-type $op))

  ;; switch (op-type) {
  (block $b_check
    ;; case %special-type:
    (br_if $b_check (i32.eq (local.get $op-type) (%special-type)))
    ;; case $builtin-type:
    (br_if $b_check (i32.eq (local.get $op-type) (%builtin-type)))

    ;; case %lambda-type:
    (if (i32.eq (local.get $op-type) (%lambda-type)) (then
        (return (call $apply-lambda 
            (local.get $env) 
            (local.get $op) 
            (local.get $args)))))

    ;; case %cont-proc-type:
    (if (i32.eq (local.get $op-type) (%cont-proc-type)) (then
        (return (call $apply-cont-proc
            (local.get $op)
            (local.get $args)))))

    ;; default:
    ;;   any other type:
    (return
      (%alloc-error-cons 
        (global.get $g-apply) 
        (%alloc-cons (local.get $op) (local.get $args)))))

  (local.get $env)
  (local.get $args)
  (i32.load offset=4 (local.get $op))
  call_indirect $table-builtin (type $builtin-type))

(func $apply-lambda (param $env i32) (param $lambda i32) (param $args i32) (result i32)
  (local $closure i32)
  (local $lambda-args i32)
  (local $formals i32)
  (local $body i32)
  (local $body-len i32)
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

  (return (call $eval-body (local.get $child-env) (local.get $body))))

(func $apply-cont-proc (param $cont-proc i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))
  (return (call $heap-alloc
      (global.get $g-heap)
      (%cont-proc-type)
      (%car-l $cont-proc)
      (local.get $args))))

(func $eval-body (param $env i32) (param $args i32) (result i32)
  (local $body-len i32)
  (local.set $body-len (call $list-len (local.get $args)))

  (if (i32.eqz (local.get $body-len)) (then 
      (return (global.get $g-nil))))

  (if (i32.eq (local.get $body-len) (i32.const 1)) (then 
      ;; single body element, simply return its evaluation
      (return (call $cont-alloc 
          (%eval-fn) 
          (local.get $env) 
          (%car-l $args) 
          (i32.const 0)))))

  (return (call $cont-alloc
      (%eval-fn) ;; eval
      (local.get $env)
      (%car-l $args)
      (call $cont-alloc
        (%cont-body-list)
        (local.get $env)
        (%cdr-l $args)
        (i32.const 0)))))

;; (cont-body-list val args ...)
(func $cont-body-list (param $env i32) (param $args i32) (result i32)
  (local $val i32)
  (local $stack i32)

  (%pop-l $val $args)

  (if (i32.eq (%get-type $args) (%nil-type))
    ;; Nothing left to evaluate, 
    ;; return the last thing that was evaluated.
    (then (return (local.get $val))))

  (return (call $cont-alloc
      (%eval-fn) ;; eval
      (local.get $env)
      (%car-l $args)
      (call $cont-alloc
        (%cont-body-list)
        (local.get $env)
        (%cdr-l $args)
        (i32.const 0)))))

(func $zip-lambda-args (param $env i32) (param $formals i32) (param $args i32)
  (loop $forever
    ;; if (get-type(formals) == nil-type) {
    (if (i32.eq (%get-type $formals) (%nil-type))
      ;; return g-nil
      (then (return))
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
    (%alloc-error-cons (global.get $g-args) (local.get $args))))

(func $not-implemented-error (param $args i32) (result i32)
  (return
    (%alloc-error-cons (global.get $g-not-impl) (local.get $args))))

(global $g-dump-eval (mut i32) (i32.const 0))
(global $g-dump-eval-indent (mut i32) (i32.const 0))

(func $dump-eval-set! (param $env i32) (param $args i32) (result i32)
  (if (i32.eq (%get-type $args) (%cons-type))
    (then
      (global.set $g-dump-eval (call $is-truthy (%car-l $args)))
      (global.set $g-dump-eval-indent (i32.const 0))))

  (return
    (select
      (global.get $g-true)
      (global.get $g-false)
      (global.get $g-dump-eval))))