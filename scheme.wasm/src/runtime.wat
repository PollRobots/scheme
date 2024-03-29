(global $g-reader           (mut i32) (i32.const 0))
(global $g-heap             (mut i32) (i32.const 0))
(global $g-interned         (mut i32) (i32.const 0))
(global $g-true             (mut i32) (i32.const 0))
(global $g-false            (mut i32) (i32.const 0))
(global $g-nil              (mut i32) (i32.const 0))
(global $g-eof-object       (mut i32) (i32.const 0))
(global $g-one              (mut i32) (i32.const 0))
(global $g-zero             (mut i32) (i32.const 0))
(global $g-newline          (mut i32) (i32.const 0))
(global $g-collect          (mut i32) (i32.const 0))
(global $g-builtin          (mut i32) (i32.const 0))
(global $g-special          (mut i32) (i32.const 0))
(global $g-env              (mut i32) (i32.const 0))

;; symbols
(global $g-apply            (mut i32) (i32.const 0))
(global $g-args             (mut i32) (i32.const 0))
(global $g-arrow            (mut i32) (i32.const 0))
(global $g-close            (mut i32) (i32.const 0))
(global $g-cont-type        (mut i32) (i32.const 0))
(global $g-curr-cont        (mut i32) (i32.const 0))
(global $g-div0             (mut i32) (i32.const 0))
(global $g-dot              (mut i32) (i32.const 0))
(global $g-double-quote     (mut i32) (i32.const 0))
(global $g-ellipsis         (mut i32) (i32.const 0))
(global $g-else             (mut i32) (i32.const 0))
(global $g-eof              (mut i32) (i32.const 0))
(global $g-error            (mut i32) (i32.const 0))
(global $g-eval             (mut i32) (i32.const 0))
(global $g-exp              (mut i32) (i32.const 0))
(global $g-false-str        (mut i32) (i32.const 0))
(global $g-false-str-long   (mut i32) (i32.const 0))
(global $g-fold-case        (mut i32) (i32.const 0))
(global $g-fzero            (mut i32) (i32.const 0))
(global $g-gc-run           (mut i32) (i32.const 0))
(global $g-gt               (mut i32) (i32.const 0))
(global $g-imag             (mut i32) (i32.const 0))
(global $g-inf              (mut i32) (i32.const 0))
(global $g-interned-str     (mut i32) (i32.const 0))
(global $g-lambda           (mut i32) (i32.const 0))
(global $g-let              (mut i32) (i32.const 0))
(global $g-let-star         (mut i32) (i32.const 0))
(global $g-letrec           (mut i32) (i32.const 0))
(global $g-letrec-star      (mut i32) (i32.const 0))
(global $g-lt               (mut i32) (i32.const 0))
(global $g-nan              (mut i32) (i32.const 0))
(global $g-neg              (mut i32) (i32.const 0))
(global $g-neg-inf          (mut i32) (i32.const 0))
(global $g-neg-nan          (mut i32) (i32.const 0))
(global $g-nil-str          (mut i32) (i32.const 0))
(global $g-no-fold-case     (mut i32) (i32.const 0))
(global $g-not-impl         (mut i32) (i32.const 0))
(global $g-open             (mut i32) (i32.const 0))
(global $g-parse            (mut i32) (i32.const 0))
(global $g-plus             (mut i32) (i32.const 0))
(global $g-port-type        (mut i32) (i32.const 0))
(global $g-quote            (mut i32) (i32.const 0))
(global $g-slash            (mut i32) (i32.const 0))
(global $g-space            (mut i32) (i32.const 0))
(global $g-syntax-error     (mut i32) (i32.const 0))
(global $g-syntax-rules     (mut i32) (i32.const 0))
(global $g-true-str         (mut i32) (i32.const 0))
(global $g-true-str-long    (mut i32) (i32.const 0))
(global $g-u8-open          (mut i32) (i32.const 0))
(global $g-u8vec            (mut i32) (i32.const 0))
(global $g-underscore       (mut i32) (i32.const 0))
(global $g-unknown          (mut i32) (i32.const 0))
(global $g-vec-open         (mut i32) (i32.const 0))

(global $g-curr-version     (mut i32) (i32.const 0))
(global $g-debug            (mut i32) (i32.const 0))

(global $g-eval-count (mut i32) (i32.const 0))
(%define %gc-threshold () (i32.const 256))
(%define %gc-small-threshold () (i32.const 16))

(func $runtime-init
  (global.set $g-heap (call $heap-create (i32.const 1024)))
  (global.set $g-interned (call $hashtable-init (i32.const 1024)))
  (global.set $g-true (call $heap-alloc (global.get $g-heap) (%boolean-type) (i32.const 1) (i32.const 0x7423)))
  (global.set $g-false (call $heap-alloc (global.get $g-heap) (%boolean-type) (i32.const 0) (i32.const 0x6623)))
  (global.set $g-nil (call $heap-alloc (global.get $g-heap) (%nil-type) (i32.const 0) (i32.const 0x2928)))
  (global.set $g-eof-object (call $heap-alloc (global.get $g-heap) (%eof-type) (i32.const 0) (i32.const 0x666f65)))
  (global.set $g-one (%alloc-i32 (i32.const 1)))
  (global.set $g-zero (%alloc-i32 (i32.const 0)))

  (global.set $g-true-str (%sym-32 0x7423 2))     ;; '#t'
  (global.set $g-false-str (%sym-32 0x6623 2))    ;; '#f'
  (global.set $g-true-str-long (%str %sym-64 64 "#true"))
  (global.set $g-false-str-long (%str %sym-64 64 "#false"))
  (global.set $g-nil-str (%sym-32 0x2928 2))      ;; '()'
  (global.set $g-newline  (%sym-32 0x0A 1))       ;; '\n'
  (global.set $g-collect (%sym-64 0x207463656c6c6f43 8))  ;; 'collect'
  (global.set $g-builtin  (%sym-64  0x6e69746c697562 7))  ;; 'builtin'
  (global.set $g-special (%sym-64 0x6c616963657073 7))    ;; 'special'
  (global.set $g-env (%sym-32 0x766e65 3))        ;; 'env'
  (global.set $g-lt (%sym-32 0x3c 1))             ;; '<'
  (global.set $g-gt (%sym-32 0x3e 1))             ;; '>'
  (global.set $g-unknown (%sym-64 0x6e776f6e6b6e75 7)) ;; 'unknown'
  (global.set $g-space (%sym-32 0x20 1))          ;; ' '
  (global.set $g-error (%sym-64 0x726f727265 5))  ;; 'error'
  (global.set $g-open (%sym-32 0x28 1))           ;; '('
  (global.set $g-close (%sym-32 0x29 1))          ;; ')'
  (global.set $g-dot (%sym-32 0x202e20 3))        ;; ' . '
  (global.set $g-eof (%sym-32 0x666f65 3))        ;; 'eof'
  (global.set $g-double-quote (%sym-32 0x22 1))   ;; '"'
  (global.set $g-args (%sym-32 0x73677261 4))     ;; 'args'
  (global.set $g-not-impl (%sym-64 0x6c706d692D746f6e 8))     ;; 'not-impl'
  (global.set $g-vec-open (%sym-32 0x2823 2))     ;; '#('
  (global.set $g-u8vec (%sym-64 0x6365763875 5))  ;; 'u8vec'
  (global.set $g-u8-open (%sym-32 0x28387523 4))  ;; '#u8('
  (global.set $g-else (%sym-32 0x65736c65 4))     ;; 'else'
  (global.set $g-arrow (%sym-32 0x3E3D 2))        ;; '=>'
  (global.set $g-apply (%sym-64 0x796c707061 5))  ;; 'apply'
  (global.set $g-interned-str (%sym-128 0x64656e7265746e69 0x203A 10)) ;; 'interned: '
  (global.set $g-eval (%sym-64 0x203A6c617665 6)) ;; 'eval: '
  (global.set $g-gc-run (%sym-32 0x6367 2))       ;; 'gc'
  (global.set $g-div0 (%sym-32 0x30766964 4))     ;; 'div0'
  (global.set $g-inf (%sym-64 0x302e666e692b 6))  ;; '+inf.0'
  (global.set $g-neg-inf (%sym-64 0x302e666e692d 6)) ;; '-inf.0'
  (global.set $g-nan (%sym-64 0x302e6e616e2b 6))  ;; '+nan.0'
  (global.set $g-neg-nan (%sym-64 0x302e6e616e2d 6)) ;; '-nan.0'
  (global.set $g-fzero (%sym-32 0x302e30 3))      ;; '0.0'
  (global.set $g-exp (%sym-32 0x65 1))            ;; 'e'
  (global.set $g-neg (%sym-32 0x2d 1))            ;; '-'
  (global.set $g-cont-type (%sym-32 0x746e6f63 4))  ;; 'cont'
  (global.set $g-syntax-rules (%sym-128 0x722D7861746e7973 0x73656c75 12)) ;; 'syntax-rules'
  (global.set $g-ellipsis (%sym-32 0x2E2E2E 3))   ;; '...'
  (global.set $g-underscore (%sym-32 0x5F 1))     ;; '_'
  (global.set $g-let (%sym-32 0x74656c 3))        ;; 'let'
  (global.set $g-let-star (%sym-32 0x2A74656c 4)) ;; 'let*'
  (global.set $g-letrec (%sym-64 0x63657274656c 6))   ;; 'letrec'
  (global.set $g-letrec-star (%sym-64 0x2A63657274656c 7)) ;; 'letrec*'
  (global.set $g-syntax-error (%sym-128 0x652D7861746e7973 0x726f7272 12)) ;; 'syntax-error'
  (global.set $g-lambda (%sym-64 0x6164626d616c 6)) ;; 'lambda'
  (global.set $g-quote (%sym-64 0x65746f7571 5))  ;; 'quote'
  (global.set $g-slash (%str %sym-32 32 "/"))
  (global.set $g-imag (%str %sym-32 32 "i"))
  (global.set $g-plus (%str %sym-32 32 "+"))
  (global.set $g-parse (%str %sym-64 64 "parse"))
  (global.set $g-fold-case (%str %sym-128 128 "#!fold-case"))
  (global.set $g-no-fold-case (%str %sym-128 128 "#!no-fold-case"))
  (global.set $g-port-type (%str %sym-32 32 "port"))

  (global.set $g-curr-version (%str %sym-192 192 (%version)))

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
  (if (i32.eqz (local.get $token-str)) (then
      (call $reader-commit (local.get $reader))
      (return (i32.const 0))))

  ;; return string->datum(token-str)
  (local.set $result (call $string->datum-with-reader
      (local.get $token-str)
      (local.get $reader)))
  (if (i32.eq (%get-type $result) (%error-type)) (then
      (if (i32.eq (global.get $g-eof) (%car-l $result)) (then
          (call $reader-rollback (local.get $reader))
          (return (local.get $result))))))

  (call $reader-commit (local.get $reader))
  (return (local.get $result)))

(func $string->datum (param $token-str i32) (result i32)
  (return (call $string->datum-with-reader
      (local.get $token-str)
      (global.get $g-reader))))

(func $string->datum-with-reader (param $token-str i32) (param $reader i32) (result i32)
  (local $car-str i32)
  (local $car i32)
  (local $cdr-str i32)
  (local $cdr i32)
  (local $head i32)
  (local $curr i32)
  (local $list-vector i32)

  (%define %check-str (%str) (if (i32.eqz (local.get %str)) (then
        (return (%alloc-error-cons
            (global.get $g-eof)
            (global.get $g-nil))))))

  (%define %check-datum (%datum)
    (if (i32.eq (%get-type %datum) (%error-type)) (then
        (return (local.get %datum)))))

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
            (then (return (call $make-vector-internal (global.get $g-nil) (i32.const 1))))
            (else (return (call $make-byte-vector-internal (global.get $g-nil)))))))

      ;; car = string->datum(car-str)
      (local.set $car (call $string->datum-with-reader
          (local.get $car-str)
          (local.get $reader)))
      (%check-datum $car)
      ;; curr = head = heap-alloc(3, car, g-nil)
      (local.set $curr (local.tee $head (%alloc-cons
            (local.get $car)
            (global.get $g-nil))))
      (%set-flags $curr (%immutable-flag))

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
              (then (return (call $make-vector-internal (local.get $head) (i32.const 1))))
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
            (%check-datum $cdr)
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
              (else (return (%alloc-error
                    (%str %sym-128 128 "expect ) not")
                    (%alloc-str (local.get $cdr-str))))))))

        ;; cdr = string->datum(cdr-str)
        (local.set $cdr (call $string->datum-with-reader
            (local.get $cdr-str)
            (local.get $reader)))
        (%check-datum $cdr)

        ;; curr[8] = heap-alloc(3, cdr, g-nil)
        (i32.store offset=8
          (local.get $curr)
          (%alloc-cons (local.get $cdr) (global.get $g-nil)))

        ;; curr = curr[8]
        (local.set $curr (%cdr-l $curr))
        (%set-flags $curr (%immutable-flag))

        (br $forever))))

  ;; if (token-str == ')') {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x29) (i32.const 1))
    (then (return (%alloc-error
          (%str %sym-128 128 "unexpected")
          (%alloc-str (local.get $token-str))))))

  ;; if (token-str == '.') 0x2E {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x2E) (i32.const 1))
    (then (return (%alloc-error
          (%str %sym-128 128 "unexpected")
          (%alloc-str (local.get $token-str))))))

  ;; }
  ;; if (token-str == "'") 0x27 {
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x27) (i32.const 1))
    (then
      ;; malloc-free(token-str)
      (call $malloc-free (local.get $token-str))
      ;; get the datum to quote, if it is an error then return that,
      (local.set $curr (call $read-with-reader (local.get $reader)))
      (%check-datum $curr)
      ;; otherwise quote it
      ;; return (quote <curr>)
      (return (%alloc-quote (local.get $curr)))))

  ;; if (token-str == "#;")
  (if (call $short-str-eq (local.get $token-str) (i32.const 0x3B23) (i32.const 2))
    (then
      ;; this is a single datum comment, so skip the next datum
      (call $malloc-free (local.get $token-str))
      ;; read this one to drop it on the floor (unless it is an error)
      (local.set $curr (call $read-with-reader (local.get $reader)))
      (%check-datum $curr)
      (return (call $read-with-reader (local.get $reader)))))

  ;; return atom(token);
  (return (call $atom (%alloc-str (local.get $token-str)))))

(func $make-vector-internal (param $list i32) (param $readonly i32) (result i32)
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

  (if (local.get $readonly) (then
      (%set-flags $vec (%immutable-flag))))

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

  (if (i32.ne (%get-type $token) (%str-type)) (then (unreachable)))

  ;; token-str == token[4]
  (local.set $token-str (i32.load offset=4 (local.get $token)))

  ;; if (token-str == '#t') {
  (if (call $short-str-eq
      (local.get $token-str)
      (i32.const 0x7423)
      (i32.const 2))
    (then (return (global.get $g-true))))

  ;; if (token-str == '#true')
  (if (call $str-eq (local.get $token-str) (%car (global.get $g-true-str-long))) (then
    (return (global.get $g-true))))

  ;; if (token-str == '#f') {
  (if (call $short-str-eq
      (local.get $token-str)
      (i32.const 0x6623)
      (i32.const 2))
    (then (return (global.get $g-false))))

  ;; if (token-str == '#false')
  (if (call $str-eq (local.get $token-str) (%car (global.get $g-false-str-long))) (then
    (return (global.get $g-false))))


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
      (local.set $atom (%alloc-str (local.get $atom-str)))
      (%set-flags $atom (%immutable-flag))
      (return (local.get $atom))))

  ;; if (short-str-start-with(token-str, 0, 'tok ', 4)) {
  (if (call $short-str-start-with
      (local.get $token-str)
      (i32.const 0)
      (i32.const 0x206b6f74)
      (i32.const 4))
    (then
      (local.set $str-len (i32.load (local.get $token-str)))
      (i32.store offset=4
        (local.get $token-str)
        (i32.sub (local.get $str-len) (i32.const 4)))
      (local.set $atom-str (call $str-dup (i32.add (local.get $token-str) (i32.const 4))))
      (i32.store offset=4 (local.get $token-str) (i32.const 0x206b6f74))
      (return (%alloc-symbol (local.get $atom-str)))))

  ;; if token.str.startsWith('#\')
  (if (call $short-str-start-with
      (local.get $token-str)
      (i32.const 0)
      (i32.const 0x5c23)
      (i32.const 2))
    (then (return (call $read-char (local.get $token-str)))))

  ;; atom = string->number-impl(token, 10, true)
  (local.set $atom (call $string->number-impl (local.get $token) (i32.const 10)))
  ;; if (is-truthy(atom)) {
  (if (call $is-truthy (local.get $atom)) (then
      (return (local.get $atom))))

  ;; should be an identifier
  (local.set $atom (call $read-identifier (local.get $token-str)))
  (if (call $is-truthy (local.get $atom)) (then
      (return (local.get $atom))))


  ;; return heap-alloc(g-heap, %symbol-type, str-dup(token-str), 0)
  (return (%alloc-error (global.get $g-parse) (local.get $token))))

(func $read-identifier (param $str i32) (result i32)
  (if (call $identifier? (local.get $str)) (then
      (return (%alloc-symbol (call $str-dup (local.get $str))))))
  (return (global.get $g-false)))

(func $identifier? (param $str i32) (result i32)
  (local $offset i32)
  (local $temp64 i64)
  (local $char i32)

  (local.set $offset (i32.const 0))
  (local.set $temp64 (call $str-next-code-point
      (local.get $str)
      (local.get $offset)))
  (if (i64.eqz (local.get $temp64)) (then (return (i32.const 0))))
  (%unpack-64-l $temp64 $offset $char)

  ;; check if this is an initial, or a special initial
  (if (i32.eqz (call $id-initial? (local.get $char))) (then
      (return (call $read-peculiar-identifier (local.get $str)))))

  (block $end (loop $start
      (local.set $temp64 (call $str-next-code-point
          (local.get $str)
          (local.get $offset)))
      (br_if $end (i64.eqz (local.get $temp64)))
      (%unpack-64-l $temp64 $offset $char)

      (block $check
        (br_if $check (call $id-initial? (local.get $char)))
        (br_if $check (call $id-digit? (local.get $char)))
        (br_if $check (call $id-special-subsequent? (local.get $char)))
        (return (i32.const 0)))

      (br $start)))

  (return (i32.const 1)))

(func $read-peculiar-identifier (param $str i32) (result i32)
  (local $offset i32)
  (local $temp64 i64)
  (local $char i32)

  (local.set $offset (i32.const 0))
  (local.set $temp64 (call $str-next-code-point
      (local.get $str)
      (local.get $offset)))
  (if (i64.eqz (local.get $temp64)) (then (return (i32.const 0))))
  (%unpack-64-l $temp64 $offset $char)

  (;
   ;    <peculiar-identifier> --> <explicit-sign>
   ;        | <explicit-sign> <sign-subsequent> <subsequent>*
   ;        | <explicit-sign> . <dot-subsequent> <subsequent>*
   ;        | . <dot-subsequent> <subsequent>*
   ;
   ;    <explicit-sign> --> + -
   ;    <sign-subsequent> --> <initial> | <explicit-sign> | @
   ;    <dot-subsequent> --> <sign-subsequent> | .
   ;)
   (block $check-subsequent
    (block $check-dot
      (block $check-sign
        (br_if $check-sign (i32.eq (local.get $char) (i32.const 0x2B))) ;; +
        (br_if $check-sign (i32.eq (local.get $char) (i32.const 0x2D))) ;; -
        ;; no explicit sign, check for dot
        (br_if $check-dot (i32.eq (local.get $char) (i32.const 0x2E))) ;; .
        ;; else no match
        (return (i32.const 0)))

      ;; we have an explicit sign, get next char
      (local.set $temp64 (call $str-next-code-point
          (local.get $str)
          (local.get $offset)))
      (if (i64.eqz (local.get $temp64)) (then
          ;; just sign is still an identifier (+ or -)
          (return (i32.const 1))))

      (%unpack-64-l $temp64 $offset $char)
      ;; sign then dot, move to check for dot-subsequent
      (br_if $check-dot (i32.eq (local.get $char) (i32.const 0x2E))) ;; /

      ;; check for sign-subsequent
      (br_if $check-subsequent (call $id-initial? (local.get $char)))
      (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x2B))) ;; +
      (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x2D))) ;; -
      (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x40))) ;; @

      ;; no match
      (return (i32.const 0)))

    (local.set $temp64 (call $str-next-code-point
        (local.get $str)
        (local.get $offset)))
    ;; must be a dot-subsequent, othersize no match
    (if (i64.eqz (local.get $temp64)) (then (return (global.get $g-false))))

    (%unpack-64-l $temp64 $offset $char)
    ;; check for dot-subsequent
    (br_if $check-subsequent (call $id-initial? (local.get $char)))
    (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x2B))) ;; +
    (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x2D))) ;; -
    (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x40))) ;; @
    (br_if $check-subsequent (i32.eq (local.get $char) (i32.const 0x2E))) ;; .

    ;; no match
    (return (i32.const 0)))

  ;; check for <subsequent>*
  ;; <subsequent> --> <initial> <digit> <special-subsequent>
  (block $end (loop $start
      (local.set $temp64 (call $str-next-code-point
          (local.get $str)
          (local.get $offset)))
      (br_if $end (i64.eqz (local.get $temp64)))
      (%unpack-64-l $temp64 $offset $char)

      (block $check
        (br_if $check (call $id-initial? (local.get $char)))
        (br_if $check (call $id-digit? (local.get $char)))
        (br_if $check (call $id-special-subsequent? (local.get $char)))
        (return (i32.const 0)))

      (br $start)))

  (return (i32.const 1)))

(func $id-initial? (param $char i32) (result i32)
  ;; letter A-Z
  (if (i32.ge_u (local.get $char) (i32.const 0x41)) (then
      (if (i32.le_u (local.get $char) (i32.const 0x5A)) (then
          (return (i32.const 1))))))

  ;; letter a-z
  (if (i32.ge_u (local.get $char) (i32.const 0x61)) (then
      (if (i32.le_u (local.get $char) (i32.const 0x7A)) (then
          (return (i32.const 1))))))

  ;; special-initial
  ;; ! #x21, $ #x24, % #x25, & #x26, * #x2A, / #x2F, : #x3A, < #x3C, = #x3D, > #x3E, ? #x3F, ^ #x5E, _ #x5F, ~ #x7E
  ;; ! $ % & * / : < = > ? ^ #x5E, _ #x5F, ~ #x7E
  (block $special
    (br_if $special (i32.eq (local.get $char) (i32.const 0x21))) ;; !
    (br_if $special (i32.eq (local.get $char) (i32.const 0x24))) ;; $
    (br_if $special (i32.eq (local.get $char) (i32.const 0x25))) ;; %
    (br_if $special (i32.eq (local.get $char) (i32.const 0x26))) ;; &
    (br_if $special (i32.eq (local.get $char) (i32.const 0x2A))) ;; *
    (br_if $special (i32.eq (local.get $char) (i32.const 0x2F))) ;; /
    (br_if $special (i32.eq (local.get $char) (i32.const 0x3A))) ;; :
    (br_if $special (i32.eq (local.get $char) (i32.const 0x3C))) ;; <
    (br_if $special (i32.eq (local.get $char) (i32.const 0x3D))) ;; =
    (br_if $special (i32.eq (local.get $char) (i32.const 0x3E))) ;; >
    (br_if $special (i32.eq (local.get $char) (i32.const 0x3F))) ;; ?
    (br_if $special (i32.eq (local.get $char) (i32.const 0x5E))) ;; ^
    (br_if $special (i32.eq (local.get $char) (i32.const 0x5F))) ;; _
    (br_if $special (i32.eq (local.get $char) (i32.const 0x7E))) ;; ~

    (return (i32.const 0)))

  (return (i32.const 1)))

(func $id-digit? (param $char i32) (result i32)
  ;; digit 0-9
  (if (i32.ge_u (local.get $char) (i32.const 0x30)) (then
      (if (i32.le_u (local.get $char) (i32.const 0x39)) (then
          (return (i32.const 1))))))
  (return (i32.const 0)))

(func $id-special-subsequent? (param $char i32) (result i32)
  ;; + - . @
  (block $special
    (br_if $special (i32.eq (local.get $char) (i32.const 0x2B))) ;; +
    (br_if $special (i32.eq (local.get $char) (i32.const 0x2D))) ;; -
    (br_if $special (i32.eq (local.get $char) (i32.const 0x2E))) ;; .
    (br_if $special (i32.eq (local.get $char) (i32.const 0x40))) ;; @

    (return (i32.const 0)))

  (return (i32.const 1)))

(func $short-str-eq
  (param $str i32)            ;; a string pointer
  (param $short-str i32)      ;; 32bit value containing utf8 encoded short string
  (param $short-str-len i32)  ;; byte length of the short string
  (result i32)                ;; 1 if strings are the same, 0 otherwise

  ;; if (short-str-len > 4) {
  (if (i32.gt_u (local.get $short-str-len) (i32.const 4))
    ;; trap
    (then (unreachable)))

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

  ;; if (offset + short-str-len > *ptr) {
  (if (i32.gt_u (i32.add (local.get $offset) (local.get $short-str-len))
                (i32.load (local.get $str)))
    (then (return (i32.const 0))))

  (local.set $str (i32.add (local.get $str) (local.get $offset)))

  (block $b-word-len
    (if (i32.eq (local.get $short-str-len) (i32.const 1)) (then
        (local.set $word (i32.load8_u offset=4 (local.get $str)))
        (br $b-word-len)))
    (if (i32.eq (local.get $short-str-len) (i32.const 2)) (then
        (local.set $word (i32.load16_u offset=4 (local.get $str)))
        (br $b-word-len)))
    (if (i32.eq (local.get $short-str-len) (i32.const 3)) (then
        (local.set $word (i32.and
            (i32.load offset=4 (local.get $str))
            (i32.const 0x00FF_FFFF)))
        (br $b-word-len)))
    (if (i32.eq (local.get $short-str-len) (i32.const 4)) (then
        (local.set $word (i32.load offset=4 (local.get $str)))
        (br $b-word-len)))
    (unreachable))

  (return (i32.eq (local.get $word) (local.get $short-str))))

(func $str-index-of-ascii
  (param $str i32)
  (param $offset i32)
  (param $char i32)
  (result i32)

  (local $str-len i32)
  (local $ptr i32)

  (local.set $str-len (i32.load (local.get $str)))

  (if (i32.ge_s (local.get $offset) (local.get $str-len)) (then
      (return (i32.const 0))))

  (local.set $ptr (i32.add
      (i32.add (local.get $str) (local.get $offset))
      (i32.const 4)))

  (block $end (loop $start
      (br_if $end (i32.eq (local.get $offset) (local.get $str-len)))

      (if (i32.eq (local.get $char) (i32.load8_u (local.get $ptr))) (then
          (return (local.get $offset))))

      (%inc $offset)
      (%inc $ptr)
      (br $start)))

  (return (i32.const 0)))

(func $short-str-ends-with
  (param $str i32)
  (param $short-str i32)
  (param $short-str-len i32)
  (result i32)

  (local $str-len i32)

  (local.set $str-len (i32.load (local.get $str)))
  (return (call $short-str-start-with
      (local.get $str)
      (i32.sub (local.get $str-len) (local.get $short-str-len))
      (local.get $short-str)
      (local.get $short-str-len))))

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
  (local $rational i32)
  (local $numerator i32)
  (local $at-index i32)

  (if (i32.ne (%get-type $str) (%str-type)) (then
      (return (global.get $g-false))))

  (local.set $str-ptr (%car-l $str))
  (local.set $str-len (i32.load (local.get $str-ptr)))
  (local.set $offset (i32.const 0))
  (local.set $exact (i32.const 0))
  (local.set $inexact (i32.const 0))
  (local.set $radix-count (i32.const 0))
  (local.set $negative (i32.const 0))
  (local.set $rational (i32.const 0))

  (%define %sssw (%cmp %len) (call $short-str-start-with (local.get $str-ptr) (local.get $offset) (i32.const %cmp) (i32.const %len)))

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

  ;; check to see if this is a rectangular complex number (ends in i)
  (if (call $short-str-ends-with (local.get $str-ptr) (i32.const 0x69) (i32.const 1)) (then
      (return (call $string->complex-rect
          (local.get $str-ptr)
          (local.get $offset)
          (local.get $radix)
          (local.get $inexact)
          (local.get $exact)))))
  (if (local.tee $at-index (call $str-index-of-ascii
        (local.get $str-ptr)
        (local.get $offset)
        (i32.const 0x40))) (then ;; '@'
      (return (call $string->complex-polar
          (local.get $str-ptr)
          (local.get $offset)
          (local.get $at-index)
          (local.get $radix)
          (local.get $inexact)
          (local.get $exact)))))

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

  ;; check for sign
  (if (%sssw 0x2d 1) ;; '-'
    (then
      (local.set $negative (i32.const 1))
      (%inc $offset))
    (else
      (if (%sssw 0x2B 1) ;; '+'
        (then (%inc $offset)))))

  (block $done (loop $l-rational
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

      (local.set $integer-start (local.get $offset))
      (block $b_integer_end (loop $b_integer
          ;; break if offset >= str-len
          (br_if $b_integer_end (i32.ge_u (local.get $offset) (local.get $str-len)))

          (local.set $digit (call $get-radix-digit (local.get $str-ptr) (local.get $offset) (local.get $radix)))
          (br_if $b_integer_end (i32.lt_s (local.get $digit) (i32.const 0)))

          (if (i32.eqz (local.get $integer-overflow)) (then
            ;; integer = integer * radix + digit
            (local.set $integer (i64.add
                (i64.mul (local.get $integer) (i64.extend_i32_u (local.get $radix)))
                (i64.extend_i32_u (local.get $digit))))))

          ;; check for overflow, if it is detected then a big int conversion
          ;; will be done below
          (if (i64.eq
              (i64.and (local.get $integer) (i64.const 0x8000_0000_0000_0000))
              (i64.const 0x8000_0000_0000_0000))
            (then (local.set $integer-overflow (i32.const 1))))

          (%inc $offset)
          (%inc $integer-digits)

          (br $b_integer)))

      (if (i32.eq (local.get $radix) (i32.const 10)) (then
          ;; check for decimal
          (if (%sssw 0x2e 1) (then ;; '.'
              (local.set $decimal (i32.const 1))
              (%inc $offset)

              (local.set $fraction-start (local.get $offset))
              (block $b_fraction_end (loop $b_fraction
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
              (if (%sssw 0x2d 1)  ;; '-'
                (then
                  (local.set $neg-exp (i32.const 1))
                  (%inc $offset))
                (else
                  (if (%sssw 0x2B 1) (then  ;; '+'
                    (%inc $offset)))))

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
                    (then
                      (local.set $exponent-overflow (i32.const 1))))

                  (%inc $exponent-digits)
                  (br $b_exp)))

              (if (i32.eqz (local.get $exponent-digits))
                (then (%dec $offset)))))))

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
              (local.set $num (%alloc-i64 (local.get $integer))))
            (else
              (local.set $big-int (call $mp-string->mp
                  (i32.add (i32.add (local.get $str-ptr) (local.get $integer-start)) (i32.const 4))
                  (local.get $integer-digits)
                  (local.get $radix)))
              (local.set $num (%alloc-big-int (local.get $big-int)))))
          (if (%sssw 0x2F 1) (then  ;; '/'
              (if (local.get $rational) (then
                  (return (global.get $g-false))))

              (%inc $offset)
              (local.set $rational (i32.const 1))
              (local.set $numerator (local.get $num))
              (br $l-rational)))
          (br $done)))

      (if (local.get $rational) (then
          ;; it is not allowed to mix integer and real in a rational
          (return (global.get $g-false))))

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
          (call $mp-free (local.get $big-int-fraction))))

      (if (local.get $exact) (then
          (if (i32.ge_s (local.get $exponent) (i32.const 0)) (then
              ;; apply the exponent
              (if (i32.gt_s (local.get $exponent) (i32.const 0)) (then
                  (local.set $big-int (call $mp-times-eq
                      (local.get $big-int)
                      (call $mp-10-pow (local.get $exponent))))))

              ;; handle sign elsewhere
              ;; (if (local.get $negative) (then
              ;;    (call $mp-neg (local.get $big-int))))

              (local.set $num (%alloc-big-int (local.get $big-int)))
              (br $done)))

          (local.set $rational (i32.const 1))
          (local.set $numerator (%alloc-big-int (local.get $big-int)))
          (local.set $num (%alloc-big-int (call $mp-10-pow (i32.sub
                (i32.const 0)
                (local.get $exponent)))))
          (br $done)))

      (local.set $real (call $mp-algorithm-m
          (local.get $big-int)
          (local.get $exponent)))
      (call $mp-free (local.get $big-int))
      (local.set $num (%alloc-f64 (local.get $real)))
      (br $done)))

  (if (i32.ne (local.get $offset) (local.get $str-len))
      (then (return (global.get $g-false))))

  (if (local.get $rational) (then
      (if (call $num-core-zero? (local.get $num)) (then
          (return (global.get $g-false))))
      (if (call $num-core-zero? (local.get $numerator)) (then
          (return (local.get $numerator))))
      (local.set $num (call $simplify-rational (%alloc-rational
            (local.get $numerator)
            (local.get $num))))))

  (if (local.get $negative) (then
      (local.set $num (call $num-core-neg (local.get $num)))))

  (if (local.get $exact) (then
      (return (call $exact-impl (local.get $num)))))

  (if (local.get $inexact) (then
      (return (call $inexact-impl (local.get $num)))))

  (return (local.get $num)))

;; the format of the string at this point (after offset) is
;; (±?<real>)?±(<imag>)?i
;; strategy is to split before the sign that separates the real and imaginary
;; parts.
;; IF there is no real part (split is the same as offset), THEN
;;  this is just an imaginary number
;; ELSE, parse real part
;; IF imag part is ±i THEN imaginary value is ±1 ELSE parse imag part
(func $string->complex-rect
  (param $str-ptr i32)
  (param $offset i32)
  (param $radix i32)
  (param $inexact i32)
  (param $exact i32)
  (result i32)

  (local $str-len i32)
  (local $split i32)
  (local $before-split i32)
  (local $real i32)
  (local $real-str i32)
  (local $real-str-len i32)
  (local $imag i32)
  (local $imag-str i32)
  (local $imag-str-len i32)
  (local $complex i32)

  (local.set $str-len (i32.load (local.get $str-ptr)))
  ;; start searching at the offset before the i
  (local.set $split (i32.sub (local.get $str-len) (i32.const 2)))
  (local.set $before-split (i32.sub (local.get $split) (i32.const 1)))

  (block $end (loop $start

      ;; checks if the character before the split is an 'e' or 'E'
      (block $no-exp (block $exp
          (br_if $exp (call $short-str-start-with
              (local.get $str-ptr)
              (local.get $before-split)
              (i32.const 0x65) ;; 'e'
              (i32.const 1)))
          (br_if $exp (call $short-str-start-with
              (local.get $str-ptr)
              (local.get $before-split)
              (i32.const 0x45) ;; 'E'
              (i32.const 1)))
          (br $no-exp))
        ;; skip over the exponent
        (%dec $split)
        (%dec $before-split)
        (br $start))

      (br_if $end (call $short-str-start-with
          (local.get $str-ptr)
          (local.get $split)
          (i32.const 0x2D) ;; '-'
          (i32.const 1)))
      (br_if $end (call $short-str-start-with
          (local.get $str-ptr)
          (local.get $split)
          (i32.const 0x2B)  ;; '+'
          (i32.const 1)))
      (if (i32.le_s (local.get $split) (local.get $offset)) (then
          ;; no sign character found, this is invalid
          (return (global.get $g-false))))
      (%dec $split)
      (%dec $before-split)
      (br $start)))

  (if (i32.eq (local.get $split) (local.get $offset))
    (then
      ;; there is no real component
      (local.set $real (global.get $g-zero)))
    (else
      ;; parse the real part
      (local.set $real-str-len (i32.sub (local.get $split) (local.get $offset)))
      (local.set $real-str (call $malloc (i32.add
            (local.get $real-str-len)
            (i32.const 4))))
      (i32.store (local.get $real-str) (local.get $real-str-len))
      (call $memcpy
        (i32.add (local.get $real-str) (i32.const 4))
        (i32.add (i32.add (local.get $str-ptr) (local.get $offset)) (i32.const 4))
        (local.get $real-str-len))

      (local.set $real (call $string->number-impl
          (%alloc-str (local.get $real-str))
          (local.get $radix)))
      (if (i32.eqz (call $numeric? (local.get $real)))
          (then (return (local.get $real))))))

  (if (i32.eq (local.get $split) (i32.sub (local.get $str-len) (i32.const 2)))
    (then
      ;; the imaginary part is ±i
      (if (call $short-str-ends-with
          (local.get $str-ptr)
          (i32.const 0x692d)
          (i32.const 2))
        (then (local.set $imag (%alloc-i64 (i64.const -1))))
        (else (local.set $imag (%alloc-i64 (i64.const 1))))))
    (else
      ;; parse the imag part
      (local.set $imag-str-len (i32.sub
          (i32.sub (local.get $str-len) (local.get $split))
          (i32.const 1)))
      (local.set $imag-str (call $malloc (i32.add
            (local.get $imag-str-len)
            (i32.const 4))))
      (i32.store (local.get $imag-str) (local.get $imag-str-len))
      (call $memcpy
        (i32.add (local.get $imag-str) (i32.const 4))
        (i32.add (i32.add (local.get $str-ptr) (local.get $split)) (i32.const 4))
        (local.get $imag-str-len))

      (local.set $imag (call $string->number-impl
          (%alloc-str (local.get $imag-str))
          (local.get $radix)))
      (if (i32.eqz (call $numeric? (local.get $imag)))
          (then (return (local.get $imag))))))

  (local.set $complex (%alloc-complex (local.get $real) (local.get $imag)))

  (if (local.get $inexact) (then
      (local.set $complex (call $inexact-impl (local.get $complex)))))
  (if (local.get $exact) (then
      (local.set $complex (call $exact-impl (local.get $complex)))))

  (return (local.get $complex)))

(func $string->complex-polar
  (param $str-ptr i32)
  (param $offset i32)
  (param $at-index i32)
  (param $radix i32)
  (param $inexact i32)
  (param $exact i32)
  (result i32)

  (local $str-len i32)
  (local $mag i32)
  (local $mag-str i32)
  (local $mag-str-len i32)
  (local $ang i32)
  (local $ang-str i32)
  (local $ang-str-len i32)
  (local $complex i32)
  (local $cos i32)
  (local $sin i32)

  (local.set $str-len (i32.load (local.get $str-ptr)))

  ;; parse the magnitude part
  (local.set $mag-str-len (i32.sub (local.get $at-index) (local.get $offset)))
  (local.set $mag-str (call $malloc (i32.add
        (local.get $mag-str-len)
        (i32.const 4))))
  (i32.store (local.get $mag-str) (local.get $mag-str-len))
  (call $memcpy
    (i32.add (local.get $mag-str) (i32.const 4))
    (i32.add (i32.add (local.get $str-ptr) (local.get $offset)) (i32.const 4))
    (local.get $mag-str-len))

  (local.set $mag (call $string->number-impl
      (%alloc-str (local.get $mag-str))
      (local.get $radix)))
  (if (i32.eqz (call $numeric? (local.get $mag)))
      (then (return (local.get $mag))))

  ;; parse the angle part
  (%inc $at-index)
  (local.set $ang-str-len (i32.sub (local.get $str-len) (local.get $at-index)))
  (local.set $ang-str (call $malloc (i32.add
        (local.get $ang-str-len)
        (i32.const 4))))
  (i32.store (local.get $ang-str) (local.get $ang-str-len))
  (call $memcpy
    (i32.add (local.get $ang-str) (i32.const 4))
    (i32.add (i32.add (local.get $str-ptr) (local.get $at-index)) (i32.const 4))
    (local.get $ang-str-len))

  (local.set $ang (call $string->number-impl
      (%alloc-str (local.get $ang-str))
      (local.get $radix)))
  (if (i32.eqz (call $numeric? (local.get $ang)))
      (then (return (local.get $ang))))

  (local.set $ang (call $inexact-impl (local.get $ang)))

  (local.set $cos (%alloc-f64 (call $cos-impl (f64.load offset=4 (local.get $ang)))))
  (local.set $sin (%alloc-f64 (call $sin-impl (f64.load offset=4 (local.get $ang)))))

  (local.set $complex (%alloc-complex
      (call $num-core-mul (local.get $mag) (local.get $cos))
      (call $num-core-mul (local.get $mag) (local.get $sin))))

  (if (local.get $inexact) (then
      (local.set $complex (call $inexact-impl (local.get $complex)))))
  (if (local.get $exact) (then
      (local.set $complex (call $exact-impl (local.get $complex)))))

  (return (local.get $complex)))


(func $should-collect (result i32)
  ;; If there is currently a collection, then incremental collection should be
  ;; quite frequent
  (if (global.get $g-gc-collecting?)
    (then
      (if (i32.ge_u (global.get $g-eval-count) (%gc-small-threshold)) (then
        (return (i32.const 1))))
      (return (i32.const 0))))

  ;; collect more often while memory is growing
  (if (i32.ge_u
      (global.get $g-eval-count)
      (i32.shr_u (%gc-threshold) (global.get $g-gc-heap-slabs))) (then
        (return (i32.const 1))))

  (return (i32.const 0)))

(func $eval (param $env i32) (param $args i32) (result i32)
  (local $result i32)
  (local $result-type i32)
  (local $fn i32)
  (local $roots i32)

  (local $cont-stack i32)
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
        (%eval-fn-def) ;; external calls allow define to be set
        (local.get $env)
        (local.get $args)
        (i32.const 0)))
      (local.set $fn (%eval-fn-def))))

  (loop $forever
    ;; set the current continuation (so that call/cc can get it)
    ;; TODO solve this without globals?
    (global.set $g-curr-cont (local.get $cont-stack))

    (block $b_eval_cont
      ;; if this is eval-fn or eval-fn-def
      ;; Note: the difference is that eval-fn-def allows the use of define
      (block $b_not_eval (block $b_is_eval
          (br_if $b_is_eval (i32.eq (local.get $fn) (%eval-fn)))
          (br_if $b_is_eval (i32.eq (local.get $fn) (%eval-fn-def)))
          (br $b_not_eval))

        ;; check if a gc has been indicated
        (if (call $should-collect)
          (then
            ;; root for gc must include cont stack, args, and env. If there is already
            ;; a collection, simply allocating these will add them to the touched set
            ;; otherwise they are passed into the call to gc-run (and thence to
            ;; gc-init)
            (local.set $roots (%alloc-list-3
                (local.get $env)
                (local.get $args)
                (local.get $cont-stack)))

            ;; (call $print-symbol (global.get $g-gc-run))
            (call $gc-run (local.get $roots))
            (global.set $g-eval-count (i32.const 0))))

        ;; this is a call to eval. So pass to eval inner
        (local.set $result (call $eval-inner
            (local.get $fn)
            (local.get $env)
            (local.get $args)))
        (br $b_eval_cont))

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

      ;; This is a debug break, send it to the other side, which will return
      ;; the next continuation frame to continue.
      (if (i32.eq (local.get $fn) (%debug-fn)) (then
        (global.set $g-curr-cont (i32.const 0))
        (if (local.get $result) (then
            (i32.store offset=8 (local.get $curr-cont) (local.get $args))))
        (return (local.get $cont-stack))))

      ;; the debugger paused to look at a result, just move on with that result
      (if (i32.eq (local.get $fn) (%debug-res-fn)) (then
        (local.set $result (%car-l $args))
        (br $b_eval_cont)))

      ;; this is a promise from the "other-side" (the host), so we will return
      ;; this, and resume when the host sends it back to us.
      (if (i32.eq (local.get $fn) (%cont-import-promise)) (then
          (global.set $g-curr-cont (i32.const 0))
          (return (local.get $cont-stack))))

      ;; some other function is the continuation
      (local.set $result (call_indirect $table-builtin (type $builtin-type)
          (local.get $env)
          (local.get $args)
          (local.get $fn))))

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
                  (%cont-apply-internal)
                  (local.get $env)
                  (%alloc-cons (local.get $handler)  (local.get $result))
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

                  (local.set $curr-cont (%car-l $temp-cont))

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
                  (%cont-apply-internal)
                  (local.get $env)
                  (%alloc-cons (local.get $handler)  (local.get $result))
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


(func $eval-inner (param $eval-fn i32) (param $env i32) (param $args i32) (result i32)
  (local $type i32)
  (local $cont i32)

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

      (if (i32.eqz (call $is-list-impl (local.get $args))) (then
          (return (%alloc-raise (%alloc-error
                (%str %sym-128 128 "improper-list")
                (local.get $args))))))

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
      (local.set $cont (call $cont-alloc
          (%eval-fn) ;; eval
          (local.get $env)
          (%car-l $args)
          (call $cont-alloc
            (select (%cont-apply) (%cont-apply-def) (i32.eq (local.get $eval-fn) (%eval-fn)))
            (local.get $env)
            (%cdr-l $args)
            (i32.const 0))))

      (if (global.get $g-debug) (then
        (local.set $cont (call $cont-alloc
          (%debug-fn)
          (i32.const 0)
          (i32.const 0)
          (local.get $cont)))
      ))

      (return (local.get $cont))))

  ;; default:
  ;; return args
  (return (local.get $args)))

(func $cont-apply (param $env i32) (param $args i32) (result i32)
  (return (call $cont-apply-impl
      (i32.const 0)
      (local.get $env)
      (local.get $args))))

(func $cont-apply-def (param $env i32) (param $args i32) (result i32)
  (return (call $cont-apply-impl
      (i32.const 1)
      (local.get $env)
      (local.get $args))))

(func $cont-apply-impl (param $allow-def i32) (param $env i32) (param $args i32) (result i32)
  (local $op i32)
  (local $result i32)

  (%pop-l $op $args)
  (local.set $result (call $apply
      (local.get $allow-def)
      (local.get $env)
      (local.get $op)
      (local.get $args)))

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


(func $apply (param $allow-def i32) (param $env i32) (param $op i32) (param $args i32) (result i32)
  (local $op-type i32)
  (local $fn i32)

  (local.set $op-type (%get-type $op))

  (if (i32.eq (local.get $op-type) (%special-type)) (then
      (local.set $fn (%car-l $op))

      (block $chk-define (block $chk-define-fail
          (br_if $chk-define (local.get $allow-def))
          (br_if $chk-define-fail (i32.eq (local.get $fn) (%special-define)))
          (br_if $chk-define-fail (i32.eq (local.get $fn) (%special-define-values)))
          (br_if $chk-define-fail (i32.eq (local.get $fn) (%special-define-syntax)))
          (br $chk-define))

        (return (%alloc-raise (%alloc-error-cons
              (%cdr-l $op)
              (local.get $args)))))

      (if (i32.eq (local.get $fn) (%special-begin)) (then
          (return (call $eval-body 
              (local.get $allow-def)
              (local.get $env)
              (local.get $args)))))

      (return (call_indirect $table-builtin (type $builtin-type)
          (local.get $env)
          (local.get $args)
          (local.get $fn)))))

  (if (i32.eq (local.get $op-type) (%syntax-rules-type)) (then
      (return (call $apply-syntax-rules
          (local.get $env)
          (local.get $op)
          (local.get $args)))))

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
      (return (call_indirect $table-builtin (type $builtin-type)
        (local.get $env)
        (local.get $args)
        (i32.load offset=4 (local.get $op))))))

  (if (i32.eq (local.get $op-type) (%lambda-type)) (then
      (return (call $apply-lambda
        (local.get $env)
        (local.get $op)
        (local.get $args)))))

  (if (i32.eq (local.get $op-type) (%case-lambda-type)) (then
      (return (call $apply-case-lambda
        (local.get $env)
        (local.get $op)
        (local.get $args)))))

  (if (i32.eq (local.get $op-type) (%cont-proc-type)) (then
      (return (call $apply-cont-proc
          (local.get $op)
          (local.get $args)))))

  (if (i32.eq (local.get $op-type) (%record-method-type)) (then
      (return (call $apply-record-method
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

  (if (i32.eq (%get-type $args) (%nil-type)) (then
    (return (local.get $stack))))

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
  (local $fn i32)

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

    ;; case %case-lambda-type:
    (if (i32.eq (local.get $op-type) (%case-lambda-type)) (then
        (return (call $apply-case-lambda
            (local.get $env)
            (local.get $op)
            (local.get $args)))))

    ;; case %cont-proc-type:
    (if (i32.eq (local.get $op-type) (%cont-proc-type)) (then
        (return (call $apply-cont-proc
            (local.get $op)
            (local.get $args)))))

    (if (i32.eq (local.get $op-type) (%syntax-rules-type)) (then
        (return (call $apply-syntax-rules
            (local.get $env)
            (local.get $op)
            (local.get $args)))))

    (if (i32.eq (local.get $op-type) (%record-method-type)) (then
        (return (call $apply-record-method
            (local.get $op)
            (local.get $args)))))

    ;; default:
    ;;   any other type:
    (return
      (%alloc-raise (%alloc-error-cons
        (global.get $g-apply)
        (%alloc-cons (local.get $op) (local.get $args))))))

  (local.set $fn (%car-l $op))
  (block $chk-define (block $chk-define-fail
      (br_if $chk-define-fail (i32.eq (local.get $fn) (%special-define)))
      (br_if $chk-define-fail (i32.eq (local.get $fn) (%special-define-values)))
      (br_if $chk-define-fail (i32.eq (local.get $fn) (%special-define-syntax)))
      (br $chk-define))

    (return (%alloc-raise (%alloc-error-cons
          (%cdr-l $op)
          (local.get $args)))))

  (return (call_indirect $table-builtin (type $builtin-type)
      (local.get $env)
      (local.get $args)
      (local.get $fn))))

(func $apply-lambda (param $env i32) (param $lambda i32) (param $args i32) (result i32)
  (local $closure i32)
  (local $lambda-args i32)
  (local $formals i32)
  (local $body i32)
  (local $child-env i32)

  ;; closure = (car lambda)
  (local.set $closure (%car-l $lambda))
  ;; lambda-args = lambda[8]
  (local.set $lambda-args (%cdr-l $lambda))

  ;; formals = car(lambda-args)
  (local.set $formals (%car-l $lambda-args))
  ;; body = cdr(lambda-args)
  (local.set $body (%cdr-l $lambda-args))

  ;; child-env = environment-init(gHeap, env)
  (local.set $child-env (call $environment-init (global.get $g-heap) (local.get $closure)))

  ;; zip-lambda-args(child-env, formals, args)
  (call $zip-lambda-args (local.get $child-env) (local.get $formals) (local.get $args))

  (return (call $eval-body (i32.const 1) (local.get $child-env) (local.get $body))))

(func $apply-case-lambda (param $env i32) (param $lambda i32) (param $args i32) (result i32)
  (local $closure i32)
  (local $clauses i32)
  (local $clause i32)
  (local $num-args i32)
  (local $formals i32)
  (local $body i32)
  (local $child-env i32)

  ;; clauses = (cdr lambda)
  (local.set $clauses (%cdr-l $lambda))

  (local.set $num-args (call $list-len (local.get $args)))

  (block $end (loop $start
      (if (i32.eq (local.get $clauses) (global.get $g-nil)) (then
          (return (call $argument-error (local.get $args)))))

      (%pop-l $clause $clauses)

      ;; formals = car(clause)
      (local.set $formals (%car-l $clause))

      (br_if $end (call $formals-match-num-args? 
          (local.get $formals) 
          (local.get $num-args)))

      (br $start)))

  ;; body = cdr(clause)
  (local.set $body (%cdr-l $clause))

  ;; closure = (car lambda)
  (local.set $closure (%car-l $lambda))
  ;; child-env = environment-init(gHeap, closure)
  (local.set $child-env (call $environment-init 
      (global.get $g-heap) 
      (local.get $closure)))

  ;; zip-lambda-args(child-env, formals, args)
  (call $zip-lambda-args (local.get $child-env) (local.get $formals) (local.get $args))

  (return (call $eval-body (i32.const 1) (local.get $child-env) (local.get $body))))

(func $formals-match-num-args? (param $formals i32) (param $count i32) (result i32)
  (local $f-type i32)
  (local $f-val i32)

  (block $end (loop $start
      ;; f-type = get-type(formals)
      (local.set $f-type (%get-type $formals))
      ;; if (f-type == symbol) break
      ;; a trailing symbol matches all remaining args 0 or more
      (br_if $end (i32.eq (local.get $f-type) (%symbol-type)))
      ;; if (f-type == nil) return count == 0
      (if (i32.eq (local.get $f-type) (%nil-type)) (then
          ;; this is the end of the formals list, the number of remaining args 
          ;; must be 0 to match
          (return (i32.eqz (local.get $count)))))
      ;; if (f-type != cons) return 0
      (if (i32.ne (local.get $f-type) (%cons-type)) (then 
          (return (i32.const 0))))

      ;; if (count == 0) return 0
      (if (i32.eqz (local.get $count)) (then
          ;; we have run out of args but still have formals
          (return (i32.const 0))))

      (%pop-l $f-val $formals) 
      (%dec $count)

      (br $start)))

  (return (i32.const 1)))

(func $apply-cont-proc (param $cont-proc i32) (param $args i32) (result i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))
  (return (call $heap-alloc
      (global.get $g-heap)
      (%cont-proc-type)
      (%car-l $cont-proc)
      (local.get $args))))

(func $eval-body (param $allow-def i32) (param $env i32) (param $args i32) (result i32)
  ;; If the argument list (body) is empty, simply return nil
  (if (i32.eq (local.get $args) (global.get $g-nil)) (then
      (return (global.get $g-nil))))

  ;; If the argument list has only one entry, simply evaluate it
  (if (i32.eq (%cdr-l $args) (global.get $g-nil)) (then
      ;; single body element, simply return its evaluation
      (return (call $cont-alloc
          (select (%eval-fn-def) (%eval-fn) (local.get $allow-def))
          (local.get $env)
          (%car-l $args)
          (i32.const 0)))))

  (return (call $cont-alloc
      (select (%eval-fn-def) (%eval-fn) (local.get $allow-def))
      (local.get $env)
      (%car-l $args)
      (call $cont-alloc
        (select (%cont-body-list-def) (%cont-body-list) (local.get $allow-def))
        (local.get $env)
        (%cdr-l $args)
        (i32.const 0)))))

;; (cont-body-list val args ...)
(func $cont-body-list (param $env i32) (param $args i32) (result i32)
  (local $val i32)

  (%pop-l $val $args)

  (if (i32.eq (%get-type $args) (%nil-type))
    ;; Nothing left to evaluate,
    ;; return the last thing that was evaluated.
    (then (return (local.get $val))))

  (return (call $eval-body (i32.const 0) (local.get $env) (local.get $args))))

;; (cont-body-list-def val args ...)
(func $cont-body-list-def (param $env i32) (param $args i32) (result i32)
  (local $val i32)

  (%pop-l $val $args)

  (if (i32.eq (%get-type $args) (%nil-type))
    ;; Nothing left to evaluate,
    ;; return the last thing that was evaluated.
    (then (return (local.get $val))))

  (return (call $eval-body (i32.const 1) (local.get $env) (local.get $args))))

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
      (then (unreachable))
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
  (return (%alloc-raise
    (%alloc-error-cons (global.get $g-args) (local.get $args)))))

(func $not-implemented-error (param $args i32) (result i32)
  (return (%alloc-raise
    (%alloc-error-cons (global.get $g-not-impl) (local.get $args)))))

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
