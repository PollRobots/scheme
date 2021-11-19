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
)

(func $runtime-cleanup
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

  ;; token-str = reader-read-token(g-reader)
  (local.set $token-str (call $reader-read-token (global.get $g-reader)))

  ;; return string->datum(token-str)
  (return (call $string->datum (local.get $token-str)))
)

(func $string->datum (param $token-str i32) (result i32)
  (local $car-str i32)
  (local $car i32)
  (local $cdr-str i32)
  (local $cdr i32)
  (local $raw-token i32)
  (local $head i32)
  (local $curr i32)

  ;; if (token-str == '(') {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x28) (i32.const 1))
    (then
      ;; malloc-free(token-str)
      (call $malloc-free (local.get $token-str))

      ;; car-str = reader-read-token(g-reader)
      (local.set $car-str (call $reader-read-token (global.get $g-reader)))
      ;; if (car-str == ')') {
      (if (call $short-str-eq (local.get $car-str) (i32.const 0x29) (i32.const 1))
        (then
          ;; malloc-free(car-str)
          (call $malloc-free (local.get $car-str))
          ;; return g-nil
          (return (global.get $g-nil))
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
        ;; if (cdr-str == ')') {
        (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x29) (i32.const 1))
          (then
            ;; malloc-free(cdr-str)
            (call $malloc-free (local.get $cdr-str))
            ;; return head
            (return (local.get $head))
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
            ;; cdr = string->datum(cdr-str)
            (local.set $cdr (call $string->datum (local.get $cdr-str)))
            ;; curr[8] = cdr
            (i32.store
              (i32.add (local.get $curr) (i32.const 8))
              (local.get $cdr)
            )
            ;; cdr-str = reader-read-token(g-reader)
            (local.set $cdr-str (call $reader-read-token (global.get $g-reader)))
            ;; if (cdr-str == ')') {
            (if (call $short-str-eq (local.get $cdr-str) (i32.const 0x29) (i32.const 1))
              (then
                ;; malloc-free(cdr-str)
                (call $malloc-free (local.get $cdr-str))
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
          (i32.add (local.get $curr) (i32.const 8))
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
        (call $heap-alloc
          (global.get $g-heap)
          (%cons-type)
          (call $heap-alloc 
            (global.get $g-heap) 
            (%symbol-type)
            (call $str-from-64 (i32.const 5) (i64.const 0x65746f7571)) ;; 'quote'
            (i32.const 0)
          )
          (call $heap-alloc
            (global.get $g-heap)
            (%cons-type) 
            (call $read)
            (global.get $g-nil)
          )
        )
      )
    )
  ;; }
  )

  ;; raw-token = heap-alloc(6, token-str, 0)
  (local.set $raw-token (call $heap-alloc (global.get $g-heap) (i32.const 7) (local.get $token-str) (i32.const 0)))
  ;; return atom(raw-token);
  (return (call $atom (local.get $raw-token)))
)

(func $atom (param $token i32)  (result i32)
  (local $token-str i32)
  (local $atom i32)

  ;; if ((token[0] & 0xF) != 7 {
  (if (i32.ne (i32.and (i32.load (local.get $token)) (i32.const 0xF)) (i32.const 7))
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

  ;; atom = string->number(token)
  (local.set $atom (call $string->number (local.get $token)))
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

(func $string->number (param $str i32) (result i32)
  (local $str-ptr i32)   ;; pointer to the actual string
  (local $i i32)         ;; code-point-index in the string
  (local $cp i32)        ;; current code-point
  (local $valid i32)     ;; boolean indicating whether this is a valid number
  (local $accum i64)     ;; the currently accumulated unsigned value of the number
  (local $radix i64)     ;; the radix (or base) of the number, valid values are 2 8 10 16
  (local $prefix i32)    ;; boolean indicating whether we are processing the prefix
  (local $mod i32)       ;; boolean indicating whether we have got a # and are waiting for a modifier letter
  (local $negative i32)  ;; boolean indicating whether this is a negative number
  (local $digit i32)     ;; the current digit in the range [0-radix[

  ;; check that it is a string
  ;; if ((*str & 0xF) != 7) {
  (if (i32.ne (i32.and (i32.load (local.get $str)) (i32.const 0xF)) (i32.const 7)) 
    (then (return (global.get $g-false)))
    ;; return #f
  ;; }
  )

  ;; str-ptr = str[4]
  (local.set $str-ptr (i32.load offset=4 (local.get $str)))

  ;; i = 0
  (local.set $i (i32.const 0))
  ;; cp = 0
  (local.set $cp (i32.const 0))
  ;; valid = false
  (local.set $valid (i32.const 0))
  ;; accum = 0
  (local.set $accum (i64.const 0))
  ;; radix = 10
  (local.set $radix (i64.const 10))
  ;; prefix = true
  (local.set $prefix (i32.const 1))
  ;; mod = false
  (local.set $mod (i32.const 0))
  ;; negative = false
  (local.set $negative (i32.const 0))

  ;; while (true) {
  (loop $forever
    ;; cp = str-code-point-at(str-ptr, i)
    (local.set $cp (call $str-code-point-at (local.get $str-ptr) (local.get $i)))
    ;; if (cp == 0) {
    (if (i32.eqz (local.get $cp))
      (then
        ;; if (valid) {
        (if (local.get $valid)
          (then
            ;; if (negative) {
            (if (local.get $negative)
              (then
                ;; accum = 0 - accum
                (local.set $accum (i64.sub (i64.const 0) (local.get $accum)))
              )
            )
            ;; return heap-alloc(4, (i32)accum, (i32)(accum >> 32))
            (return 
              (call $heap-alloc
                (global.get $g-heap) 
                (i32.const 4) 
                (i32.wrap_i64 (local.get $accum))
                (i32.wrap_i64 (i64.shr_u (local.get $accum) (i64.const 32)))
              )
            )
          )
          ;; }
          ;; } else {
          (else 
            ;; return g-false
            (return (global.get $g-false))
            ;; }
          )
        )
        ;; }
      )
    )

    ;; if (prefix) {
    (if (local.get $prefix)
      (then
        ;; if (mod) {
        (if (local.get $mod)
          (then
            (block $b_radix
            ;; if (cp == 'b') 0x62 {
              (if (i32.eq (local.get $cp) (i32.const 0x62))
                (then
                  ;; radix = 2
                  (local.set $radix (i64.const 2))
                  (br $b_radix)
                )
              )
              ;; } else if (cp == 'o') 0x6f {
              (if (i32.eq (local.get $cp) (i32.const 0x6f))
                (then
                  ;; radix = 8
                  (local.set $radix (i64.const 8))
                  (br $b_radix)
                )
              )
              ;; } else if (cp == 'd') 0x64 {
              (if (i32.eq (local.get $cp) (i32.const 0x64))
                (then 
                  ;; radix = 10
                  (local.set $radix (i64.const 10))
                  (br $b_radix)
                )
              )
              ;; } else if (cp == 'x') 0x78 {
              (if (i32.eq (local.get $cp) (i32.const 0x78))
                (then 
                  ;; radix = 16
                  (local.set $radix (i64.const 16))
                  (br $b_radix)
                )
              ;; }
              )

              ;; TODO handle exact/inexact prefixes
              ;; return g-false
              (return (global.get $g-false))
            )
            ;; mod = false
            (local.set $mod (i32.const 0))
          )
          ;; } else {
          (else
            ;; if (cp == '#') (0x23) {
            (if (i32.eq (local.get $cp) (i32.const 0x23))
              (then 
                ;; mod = true
                (local.set $mod (i32.const 1))
              )
              ;; } else {
              (else
                ;; prefix = false
                (local.set $prefix (i32.const 0))
                ;; if (cp == '-') 0x2d {
                (if (i32.eq (local.get $cp) (i32.const 0x2D))
                  (then
                    ;; negative = true
                    (local.set $negative (i32.const 1))
                  )
                  ;; } else if (cp == '+') 0x2B{
                  (else
                    (if (i32.eq (local.get $cp) (i32.const 0x2B))
                      ;; negative = false
                      (then
                        (local.set $negative (i32.const 1))
                      )
                      ;; } else {
                      (else
                        ;; continue
                        (br $forever)
                      )
                    )
                    ;; }
                  )
                )
              )
            ;; }
            )
          ;; }
          )
        )
      )
      ;; } else {
      (else
        (block $b_digit
          ;; if (cp >= 0x30 && cp <= 0x39) '0' - '9' {
          (if (i32.ge_u (local.get $cp) (i32.const 0x30))
            (then
              (if (i32.le_u (local.get $cp) (i32.const 0x39))
                (then
                  ;; digit = cp - 0x30
                  (local.set $digit (i32.sub (local.get $cp) (i32.const 0x30)))
                  (br $b_digit)
                )
              )
            )
          )
          ;; } else if (cp >= 0x61 && cp <= 0x66) 'a' - 'f' {
          (if (i32.ge_u (local.get $cp) (i32.const 0x61))
            (then
              (if (i32.le_u (local.get $cp) (i32.const 0x66))
                (then
                  ;; digit = cp - 0x61 + 10
                  ;; digit = cp - 87
                  (local.set $digit (i32.sub (local.get $cp) (i32.const 87)))
                  (br $b_digit)
                )
              )
            )
          )
          ;; } else if (cp >= 0x41 && cp <= 0x46) 'A' - 'F' {
          (if (i32.ge_u (local.get $cp) (i32.const 0x41))
            (then
              (if (i32.le_u (local.get $cp) (i32.const 0x46))
                (then
                  ;; digit = cp - 0x41 + 10
                  ;; digit = cp - 55
                  (local.set $digit (i32.sub (local.get $cp) (i32.const 55)))
                  (br $b_digit)
                )
              )
            )
          )
          ;; else {
            ;; unknown character in number
            ;; return g-false
            (return (global.get $g-false))
          ;; }
        )

        ;; if (digit < radix) {
        (if (i32.lt_s (local.get $digit) (i32.wrap_i64 (local.get $radix)))
          (then
            ;; accum = accum * radix + digit
            (local.set $accum 
              (i64.add
                (i64.mul (local.get $accum) (local.get $radix)) 
                (i64.extend_i32_u (local.get $digit))
              )
            )
            ;; valid = true
            (local.set $valid (i32.const 1))
          )
          ;; } else { 
          (else
            ;; return g-false
            (return (global.get $g-false))
          )
          ;; }
        )
      )
      ;; }
    )

    ;; i++
    (%inc $i)
    (br $forever)
  ;; }
  )

  unreachable
)

(func $eval (param $env i32) (param $args i32) (result i32)
  (local $type i32)
  (local $op i32)
  (local $cdr i32)


  ;; type = *args & 0xf
  (local.set $type (i32.and (i32.load (local.get $args)) (i32.const 0x0F)))

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
          (call $eval
            (local.get $env)
            (i32.load offset=4 (local.get $args))
          )
          (i32.load offset=8 (local.get $args))
        )
      )
    )
  )
  ;; default:
  ;; return args
  (return (local.get $args))
  ;; }

  (unreachable)
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
    ;; case $builtin-type:
    (if (i32.eq (local.get $op-type) (%builtin-type))
      (then
        ;; args = eval-list(env, args)
        (local.set $args
          (call $eval-list (local.get $env) (local.get $args))
        )
        ;; break
        (br $b_check)
      )
    )
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
    (call $print (call $cons (local.get $op-type) (call $cons (local.get $op) (local.get $args))))
    (unreachable)
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

