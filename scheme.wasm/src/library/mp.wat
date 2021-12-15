(;

Multi-precision integer math

numbers are represented as 

sign: u1        -- sign bit, 0 - positive, 1 (2^31) - negative
size: u31       -- number of 32bit words in this number
data: u32[size] -- the words of the number, words themselves are little 
                   endian, but the array is big-endian, sorry.

;)

(%define %mp-len-l (%ptr) (i32.and (i32.load (local.get %ptr)) (i32.const 0x7FFF_FFFF)))
(%define %mp-sign-l (%ptr) (i32.and (i32.load (local.get %ptr)) (i32.const 0x8000_0000)))
(%define %mp-alloc-l (%ptr %len)
  (local.set %ptr (call $calloc (i32.add (local.get %len) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get %ptr) (local.get %len)))
(%define %mp-alloc-sign-l (%ptr %len %neg)
  (local.set %ptr (call $calloc (i32.add (local.get %len) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get %ptr) (i32.or (local.get %len) (local.get %neg))))

;; converts a string to a positive mp integer.
;; string must only include characters in the base
;; valid bases are 2 8 10 and 16
(func $mp-string->mp (param $str i32) (param $str-len i32) (param $base i32) (result i32)
  (local $bit-length i32)
  (local $digit-bits i32)
  (local $digit-mask i32)
  (local $word-length i32)
  (local $buffer i32)
  (local $ptr i32)
  (local $i i32)
  (local $digit i32)
  (local $word i32)
  (local $bit i32)
  (local $temp i64)
  (local $offset i32)
  (local $str-ptr i32)
  (local $neg i32)

  (if (i32.eq (i32.load8_u (local.get $str)) (i32.const 0x2D)) ;; '-' 0x2D
    (then
      (local.set $neg (i32.const 1))
      (%inc $str)
      (%dec $str-len))
    (else (local.set $neg (i32.const 0))))

  (block $b_switch
    (if (i32.eq (local.get $base) (i32.const 10))
      (then 
        (local.set $buffer (call $mp-string10->mp (local.get $str) (local.get $str-len)))
        (if (i32.eq (local.get $buffer) (i32.const -1))
          (then (return (i32.const -1))))
        (if (local.get $neg)
          (then (call $mp-neg (local.get $buffer))))
        (return (local.get $buffer))))

    (if (i32.eq (local.get $base) (i32.const 16)) (then
        (local.set $bit-length (i32.shl (local.get $str-len) (i32.const 2)))
        (local.set $digit-bits (i32.const 4))
        (local.set $digit-mask (i32.const 0xF))
        (br $b_switch)))

    (if (i32.eq (local.get $base) (i32.const 8)) (then
        (local.set $bit-length (i32.mul (local.get $str-len) (i32.const 3)))
        (local.set $digit-bits (i32.const 3))
        (local.set $digit-mask (i32.const 0x7))
        (br $b_switch)))

    (if (i32.eq (local.get $base) (i32.const 2)) (then
        (local.set $bit-length (local.get $str-len))
        (local.set $digit-bits (i32.const 1))
        (local.set $digit-mask (i32.const 0x1))
        (br $b_switch)))
    
    (return (i32.const -1)))

  ;; word-length = (bit-length + 31) / 32
  (local.set $word-length (i32.shr_u (i32.add (local.get $bit-length) (i32.const 31)) (i32.const 5)))

  ;; buffer = calloc(word-length + 1, 4)
  (%mp-alloc-l $buffer $word-length)

  (local.set $i (i32.const 0))
  (local.set $offset (i32.sub
      ;; word-length * 32
      (i32.shl (local.get $word-length) (i32.const 5))
      (local.get $bit-length)))
  (local.set $temp (i64.const 0))
  (local.set $ptr (i32.add (local.get $buffer) (i32.const 4)))
  (local.set $str-ptr (local.get $str))

  (block $b_end 
    (block $b_error
      (loop $b_start 
        (br_if $b_end (i32.ge_u (local.get $i) (local.get $str-len)))

        (local.set $digit (call $mp-char-base-digit 
            (i32.load8_u (local.get $str-ptr)) 
            (local.get $base)))
        (br_if $b_error (i32.eq (local.get $digit) (i32.const -1)))

        ;; temp = (temp << digit-bits) | digit
        (local.set $temp (i64.or 
            (i64.shl (local.get $temp) (i64.extend_i32_u (local.get $digit-bits)))
            (i64.extend_i32_u (local.get $digit))))
        ;; offset += digit-bits
        (local.set $offset (i32.add (local.get $offset) (local.get $digit-bits)))  

        ;; if (offset >= 32)
        (if (i32.ge_u (local.get $offset) (i32.const 32))
          (then
            ;; offset -= 32
            (%minus-eq $offset 32)
            ;; *ptr = (temp >> offset) as i32
            (i32.store 
              (local.get $ptr)
              (i32.wrap_i64 (i64.shr_u 
                  (local.get $temp) 
                  (i64.extend_i32_u (local.get $offset)))))
            ;; ptr += 4
            (%plus-eq $ptr 4)))

        (%inc $i)
        (%inc $str-ptr)
        (br $b_start)))

    (call $malloc-free (local.get $buffer)) 
    (return (i32.const -1)))

  (if (local.get $neg)
    (then (call $mp-neg (local.get $buffer))))
  (return (local.get $buffer)))

(func $mp-mp->string (param $ptr i32) (param $base i32) (result i32)
  (local $str-len i32)
  (local $digit-bits i32)
  (local $digit-mask i32)
  (local $ptr-log2 i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $digit i32)
  (local $src-ptr i32)
  (local $temp i32)

  (local.set $ptr-log2 (call $mp-log2 (local.get $ptr)))

  (if (i32.eqz (local.get $ptr-log2))
    (then
      (return (call $str-from-32 (i32.const 1) (i32.const 0x30)))))
  
  (block $b_switch
    (if (i32.eq (local.get $base) (i32.const 10))
      (then (return (call $mp-mp->string10 (local.get $ptr)))))


    (if (i32.eq (local.get $base) (i32.const 16)) (then
        (local.set $str-len (i32.shr_u 
            (i32.add (local.get $ptr-log2) (i32.const 3)) 
            (i32.const 2)))
        (local.set $digit-bits (i32.const 4))
        (local.set $digit-mask (i32.const 0xF))
        (br $b_switch)))

    (if (i32.eq (local.get $base) (i32.const 8)) (then
        (local.set $str-len (i32.div_u 
            (i32.add (local.get $ptr-log2) (i32.const 2)) 
            (i32.const 3)))
        (local.set $digit-bits (i32.const 3))
        (local.set $digit-mask (i32.const 0x7))
        (br $b_switch)))

    (if (i32.eq (local.get $base) (i32.const 2)) (then
        (local.set $str-len (local.get $ptr-log2))
        (local.set $digit-bits (i32.const 1))
        (local.set $digit-mask (i32.const 0x1))
        (br $b_switch)))
    
    (return (i32.const 0)))

  (local.set $temp (call $mp-copy (local.get $ptr)))

  (if (%mp-sign-l $ptr)
    (then
      (local.set $str (call $malloc (i32.add (local.get $str-len) (i32.const 5))))
      (i32.store (local.get $str) (i32.add (local.get $str-len) (i32.const 1)))
      (i32.store8 offset=4 (local.get $str) (i32.const 0x2D)) ;; '-' 0x2D
      (local.set $str-ptr (i32.add 
          (i32.add 
            (local.get $str)
            (i32.const 4))
          (local.get $str-len)))
      (call $mp-neg (local.get $temp)))
    (else
      (local.set $str (call $malloc (i32.add (local.get $str-len) (i32.const 4))))
      (i32.store (local.get $str) (local.get $str-len))
      (local.set $str-ptr (i32.add 
          (i32.add 
            (local.get $str)
            (i32.const 3))
          (local.get $str-len)))))

  (local.set $src-ptr (i32.add (local.get $temp) (%word-size (%mp-len-l $temp))))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $str-len)))

      (local.set $digit (i32.and 
          (i32.load (local.get $src-ptr)) 
          (local.get $digit-mask)))

      (if (i32.lt_u (local.get $digit) (i32.const 10))
        (then (%plus-eq $digit 0x30))
        (else (%plus-eq $digit 0x37))) ;; hex digit
      
      (i32.store8 (local.get $str-ptr) (local.get $digit))
      (call $mp-shr-ip (local.get $temp) (local.get $digit-bits))

      (%dec $str-len)
      (%dec $str-ptr)
      (br $b_start)))

  (call $mp-free (local.get $temp))

  (return (local.get $str)))

(func $mp-mp->string10 (param $ptr i32) (result i32)
  (local $log2 i32)
  (local $sign i32)
  (local $max-digits i32)
  (local $char-buffer i32)
  (local $char-ptr i32)
  (local $ten i32)
  (local $quot i32)
  (local $rem i32)
  (local $div-res i64)
  (local $digit i32)
  (local $actual-len i32)
  (local $res i32)

  (local.set $log2 (call $mp-log2 (local.get $ptr)))
  (local.set $sign (%mp-sign-l $ptr))
  ;; log_2(10) is 3.3219, so we're going to assume that every 3 bits is another digit
  ;; and 1 for a possible minus sign
  (local.set $max-digits (i32.add 
      (i32.div_u
        (i32.add (local.get $log2) (i32.const 2))
        (i32.const 3))
      (i32.const 1)))
  (local.set $char-buffer (call $malloc (local.get $max-digits)))
  (local.set $char-ptr (i32.sub 
      (i32.add (local.get $char-buffer) (local.get $max-digits))
      (i32.const 1)))

  (local.set $quot (call $mp-copy (local.get $ptr)))
  (local.set $ten (call $mp-10-pow (i32.const 1)))
  (local.set $actual-len (i32.const 0))
  (if (local.get $sign) 
    (then (call $mp-neg (local.get $quot))))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (call $mp-log2 (local.get $quot))))

      (local.set $div-res (call $mp-div (local.get $quot) (local.get $ten)))
      ;; quotient is in the high word, remainder in the low word
      (call $mp-free (local.get $quot))
      (local.set $quot (i32.wrap_i64 (i64.shr_u (local.get $div-res) (i64.const 32))))
      (local.set $rem (i32.wrap_i64 (local.get $div-res)))
      (local.set $digit (i32.wrap_i64 (call $mp-to-u64 (local.get $rem))))
      (call $mp-free (local.get $rem))

      (i32.store8 (local.get $char-ptr) (i32.add (local.get $digit) (i32.const 0x30)))

      (%dec $char-ptr)
      (%inc $actual-len)
      (br $b_start)))

  (if (i32.ne (local.get $quot) (global.get $g-mp-zero))
    (then (call $mp-free (local.get $quot))))
  (call $mp-free (local.get $ten))

  (if (local.get $sign)
    (then
      (i32.store8 (local.get $char-ptr) (i32.const 0x2D)) ;; '-' 0x2D
      (%dec $char-ptr)
      (%inc $actual-len)))

  (local.set $res (call $malloc (i32.add (local.get $actual-len) (i32.const 4))))
  (i32.store (local.get $res) (local.get $actual-len))
  (call $memcpy 
      (i32.add (local.get $res) (i32.const 4))
      (i32.add (local.get $char-ptr) (i32.const 1))
      (local.get $actual-len))
  (call $malloc-free (local.get $char-buffer))

  (return (local.get $res)))

(func $mp-char-base-digit (param $char i32) (param $base i32) (result i32)
  (local $digit i32)
  
  (block $b_digit
    (if (i32.ge_u (local.get $char) (i32.const 0x30))     ;; '0' 0x30
      (then
        (if (i32.le_u (local.get $char) (i32.const 0x39)) ;; '9' 0x39
          (then
            (local.set $digit (i32.sub (local.get $char) (i32.const 0x30)))
            (br $b_digit)))))

    (if (i32.ge_u (local.get $char) (i32.const 0x41))     ;; 'A' 0x41
      (then
        (if (i32.le_u (local.get $char) (i32.const 0x46)) ;; 'F' 0x46
          (then
            ;; digit = char - 0x41 + 0xA
            ;; digit = char - 0x37
            (local.set $digit (i32.sub (local.get $char) (i32.const 0x37)))
            (br $b_digit)))))

    (if (i32.ge_u (local.get $char) (i32.const 0x61))     ;; 'a' 0x61
      (then
        (if (i32.le_u (local.get $char) (i32.const 0x66)) ;; 'f' 0x66
          (then
            ;; digit = char - 0x61 + 0xA
            ;; digit = char - 0x57
            (local.set $digit (i32.sub (local.get $char) (i32.const 0x57)))
            (br $b_digit)))))
    
    ;; not a digit
    (return (i32.const -1)))

  (if (i32.lt_u (local.get $digit) (local.get $base))
    (then (return (local.get $digit))))

  (return (i32.const -1)))

(func $mp-string10->mp (param $str i32) (param $str-len i32) (result i32)
  (local $split i32)
  (local $low-split i32)
  (local $low i32)
  (local $high i32)
  (local $scaled-high i32)
  (local $res i32)

  (if (i32.le_u (local.get $str-len) (i32.const 9))
    (then (return (call $mp-string10-short->mp (local.get $str) (local.get $str-len)))))

  ;; split = str-len / 2
  (local.set $split (i32.shr_u (local.get $str-len) (i32.const 1)))
  (local.set $low-split (i32.sub (local.get $str-len) (local.get $split)))

  (local.set $low (call $mp-string10->mp 
      (i32.add (local.get $str) (local.get $split))
      (local.get $low-split)))
  (if (i32.eq (local.get $low) (i32.const -1))
    (then (return (i32.const -1))))

  (local.set $high (call $mp-string10->mp 
      (local.get $str)
      (local.get $split)))
  (if (i32.eq (local.get $high) (i32.const -1))
    (then 
      (call $mp-free (local.get $low))
      (return (i32.const -1))))

  (local.set $scaled-high (call $mp-mul
      (local.get $high)
      (call $mp-10-pow (local.get $low-split))))

  (call $mp-free (local.get $high))

  (local.set $res (call $mp-add (local.get $low) (local.get $scaled-high)))
  (call $mp-free (local.get $scaled-high))
  (call $mp-free (local.get $low))

  (return (local.get $res)))

(func $mp-string10-short->mp (param $str i32) (param $str-len i32) (result i32)
  (local $buffer i32)
  (local $accum i32)
  (local $digit i32)
  (local $i i32)

  (local.set $i (i32.const 0))
  (local.set $accum (i32.const 0))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.ge_u (local.get $i) (local.get $str-len)))

      (local.set $digit (call $mp-char-base-digit
          (i32.load8_u (local.get $str))
          (i32.const 10)))

      (if (i32.eq (local.get $digit) (i32.const -1))
        (then (return (i32.const -1))))

      (local.set $accum (i32.add
        (i32.mul (local.get $accum) (i32.const 10))
        (local.get $digit)))

      (%inc $i)
      (%inc $str)
      (br $b_start)))

  (local.set $buffer (call $malloc (i32.const 8)))
  (i32.store (local.get $buffer) (i32.const 1))
  (i32.store offset=4 (local.get $buffer) (local.get $accum))
  (return (local.get $buffer)))

(func $mp-add (param $left i32) (param $right i32) (result i32)
  (local $left-length i32)
  (local $right-length i32)
  (local $res i32)

  (local.set $left-length (i32.load (local.get $left)))
  (local.set $right-length (i32.load (local.get $right)))

  ;; check if signs differ 
  (if (i32.and 
      (i32.xor (local.get $left-length) (local.get $right-length))
      (i32.const 0x8000_0000))
    (then
      ;; xor of sign-bits is 1, so one must be negative, compute subtraction instead
      (if (i32.and (local.get $left-length) (i32.const 0x8000_0000))
        (then
          ;; left is negative. temporarily switch sign
          (call $mp-neg (local.get $left))
          ;; then compute right - left
          (local.set $res (call $mp-sub (local.get $right) (local.get $left)))
          ;; then switch sign back and return result
          (call $mp-neg (local.get $left))
          (return (local.get $res)))
        (else
          ;; right is negative. temporarily switch sign
          (call $mp-neg (local.get $right))
          ;; then compute left - right
          (local.set $res (call $mp-sub (local.get $left) (local.get $right)))
          ;; then switch sign back and return result
          (call $mp-neg (local.get $right))
          (return (local.get $res))))))

  ;; Either both are positive, or both are negative.
  ;; Compute addition and fix sign at the end.
  (if (i32.ge_u (call $mp-log2 (local.get $left)) (call $mp-log2 (local.get $right)))
    (then (return (call $mp-add-helper (local.get $left) (local.get $left-length) (local.get $right) (local.get $right-length))))
    (else (return (call $mp-add-helper (local.get $right) (local.get $right-length) (local.get $left) (local.get $left-length)))))

  (unreachable))

;; internal helper.
;; This is called with left known to be ge right, and the sign of both
;; numbers are the same
(func $mp-add-helper (param $left i32) (param $left-length i32) (param $right i32) (param $right-length i32) (result i32)
  (local $left-log2 i32)
  (local $right-log2 i32)
  (local $final-length i32)
  (local $neg i32)
  (local $left-offset i32)
  (local $right-offset i32)
  (local $dest-offset i32)
  (local $temp-res i64)
  (local $res i32)

  (local.set $neg (i32.and (local.get $left-length) (i32.const 0x8000_0000)))
  (local.set $left-length (i32.and (local.get $left-length) (i32.const 0x7FFF_FFFF)))
  (local.set $right-length (i32.and (local.get $right-length) (i32.const 0x7FFF_FFFF)))

  (local.set $left-log2 (call $mp-log2 (local.get $left)))
  (local.set $right-log2 (call $mp-log2 (local.get $right)))

  ;; result of addition can be at most 1 bit longer, so compute the number of words needed
  (local.set $final-length (i32.shr_u (i32.add (local.get $left-log2) (i32.const 1)) (i32.const 32)))

  (%mp-alloc-sign-l $res $final-length $neg)

  (local.set $left-offset (i32.shl (local.get $left-length) (i32.const 2)))
  (local.set $right-offset (i32.shl (local.get $right-length) (i32.const 2)))
  (local.set $dest-offset (i32.shl (local.get $final-length) (i32.const 2)))

  (local.set $temp-res (i64.const 0))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $right-offset)))
      (br_if $b_end (i32.eqz (local.get $left-offset)))
      (br_if $b_end (i32.eqz (local.get $dest-offset)))

      (local.set $temp-res (i64.add (i64.add
            (i64.load32_u (i32.add (local.get $left) (local.get $left-offset))) ;; left word
            (i64.load32_u (i32.add (local.get $right) (local.get $right-offset)))) ;; right word
          (local.get $temp-res))) ;; carry bit

      (i64.store32 
        (i32.add (local.get $res) (local.get $dest-offset))
        (local.get $temp-res))
      (local.set $temp-res (i64.shr_u (local.get $temp-res) (i64.const 32)))

      (%minus-eq $left-offset 4)
      (%minus-eq $right-offset 4)
      (%minus-eq $dest-offset 4)
      (br $b_start)))

  ;; finish up any words that are just in the left, this could carry a bit all the way 
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $left-offset)))
      (br_if $b_end (i32.eqz (local.get $dest-offset)))

      (local.set $temp-res (i64.add 
          (i64.load32_u (i32.add (local.get $left) (local.get $left-offset))) ;; left word
          (local.get $temp-res))) ;; carry bit

      (i64.store32 
        (i32.add (local.get $res) (local.get $dest-offset))
        (local.get $temp-res))
      (local.set $temp-res (i64.shr_u (local.get $temp-res) (i64.const 32)))

      (%minus-eq $left-offset 4)
      (%minus-eq $dest-offset 4)
      (br $b_start)))

  (if (i64.ne (local.get $temp-res) (i64.const 0))
    (then
      ;; there is still a carry bit to handle
      (i64.store32 
        (i32.add (local.get $res) (local.get $dest-offset))
        (local.get $temp-res))))

  (return (local.get $res)))

(func $mp-neg (param $ptr i32)
  (i32.store
    (local.get $ptr)
    (i32.xor 
      (i32.load (local.get $ptr))
      (i32.const 0x8000_0000))))

(func $mp-abs (param $ptr i32)
  (i32.store
    (local.get $ptr)
    (i32.and 
      (i32.load (local.get $ptr))
      (i32.const 0x7FFF_FFFF))))

;; returns the highest bit set in the abs number 
(func $mp-log2 (param $ptr i32) (result i32)
  (local $length i32)
  (local $clz i32)

  (local.set $length (%mp-len-l $ptr))

  (%plus-eq $ptr 4)
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $length)))

      (local.set $clz (i32.clz (i32.load (local.get $ptr))))
      (if (i32.lt_u (local.get $clz) (i32.const 32))
        (then
          ;; total bits are 32 * length - clz
          (return (i32.sub
              (i32.shl (local.get $length) (i32.const 5))
              (local.get $clz)))))

      (%dec $length)
      (%plus-eq $ptr 4)
      (br $b_start)))

  (return (i32.const 0)))


(func $mp-abs-gt? (param $left i32) (param $right i32) (result i32)
  (local $left-log2 i32)
  (local $right-log2 i32)
  (local $left-length i32)
  (local $right-length i32)
  (local $left-norm-length i32)
  (local $right-norm-length i32)
  (local $left-word i32)
  (local $right-word i32)

  (local.set $left-log2 (call $mp-log2 (local.get $left)))
  (local.set $right-log2 (call $mp-log2 (local.get $right)))

  (if (i32.gt_u (local.get $left-log2) (local.get $right-log2))
    (then (return (i32.const 1))))
  (if (i32.lt_u (local.get $left-log2) (local.get $right-log2))
    (then (return (i32.const 0))))

  (local.set $left-length (%mp-len-l $left))
  (local.set $right-length (%mp-len-l $right))

  (local.set $left-norm-length (i32.shr_u 
      (i32.add (local.get $left-log2) (i32.const 31)) 
      (i32.const 5)))
  (local.set $left (i32.add 
      (local.get $left)
      (%word-size (i32.sub (local.get $left-length) (local.get $left-norm-length)))))

  (local.set $right-norm-length (i32.shr_u 
      (i32.add (local.get $right-log2) (i32.const 31)) 
      (i32.const 5)))
  (local.set $right (i32.add 
      (local.get $right)
      (%word-size (i32.sub (local.get $right-length) (local.get $right-norm-length)))))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $left-norm-length)))

      (%plus-eq $left 4)
      (%plus-eq $right 4)

      (local.set $left-word (i32.load (local.get $left)))
      (local.set $right-word (i32.load (local.get $right)))

      (if (i32.gt_u (local.get $left-word) (local.get $right-word))
        (then (return (i32.const 1))))
      (if (i32.lt_u (local.get $left-word) (local.get $right-word))
        (then (return (i32.const 0))))

      (%dec $left-norm-length)
      (br $b_start)))

  (return (i32.const 0)))

(func $mp-sub-eq (param $left i32) (param $right i32) (result i32)
  (local $temp i32)
  (local.set $temp (call $mp-sub (local.get $left) (local.get $right)))
  (call $mp-free (local.get $left))
  (return (local.get $temp)))

(func $mp-sub (param $left i32) (param $right i32) (result i32)
  (local $left-length i32)
  (local $right-length i32)
  (local $final-length i32)
  (local $left-offset i32)
  (local $right-offset i32)
  (local $dest-offset i32)
  (local $temp-res i64)
  (local $res i32)
  (local $neg i32)

  (local.set $left-length (i32.load (local.get $left)))
  (local.set $right-length (i32.load (local.get $right)))

  ;; check if signs differ 
  (if (i32.and 
      (i32.xor (local.get $left-length) (local.get $right-length))
      (i32.const 0x8000_0000))
    (then
      ;; xor of sign-bits is 1, so one must be negative, compute addition instead
      (if (i32.and (local.get $left-length) (i32.const 0x8000_0000))
        (then
          ;; -left - right = -(left + right)
          ;; left is negative. temporarily switch sign
          (call $mp-neg (local.get $left))
          ;; then compute -(left + right)
          (local.set $res (call $mp-add (local.get $left) (local.get $right)))
          ;; then switch sign back and return result
          (call $mp-neg (local.get $left))
          (call $mp-neg (local.get $res))
          (return (local.get $res)))
        (else
          ;; left - -right = left + right
          ;; right is negative. temporarily switch sign
          (call $mp-neg (local.get $right))
          ;; then compute (left + right)
          (local.set $res (call $mp-add (local.get $left) (local.get $right)))
          ;; then switch sign back and return result
          (call $mp-neg (local.get $right))
          (return (local.get $res))))))

  ;; Either both are positive, or both are negative.
  ;; Compute subtraction and fix sign at the end.
  (local.set $neg (i32.and (local.get $left-length) (i32.const 0x8000_0000)))
  (local.set $left-length (i32.and (local.get $left-length) (i32.const 0x7FFF_FFFF)))
  (local.set $right-length (i32.and (local.get $right-length) (i32.const 0x7FFF_FFFF)))

  (if (call $mp-abs-gt? (local.get $right) (local.get $left))
    (then
      ;; right > left, right - left = -(left - right)
      (local.set $res (call $mp-sub (local.get $right) (local.get $left)))
      (call $mp-neg (local.get $res))
      (return (local.get $res))))

  ;; now we know that left >= right, and that both are the same sign.
  ;; time to actually do some subtracting

  (local.set $left-offset (%word-size-l $left-length))
  (local.set $right-offset (%word-size-l $right-length))
  (%mp-alloc-sign-l $res $left-length $neg)

  (local.set $temp-res (i64.const 0))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $left-offset)))
      (br_if $b_end (i32.eqz (local.get $right-offset)))

      ;; temp-res = left[left-offset] - right[right-offset] - carry
      (local.set $temp-res (i64.sub
          (i64.sub 
            (i64.load32_u (i32.add (local.get $left) (local.get $left-offset)))
            (i64.load32_u (i32.add (local.get $right) (local.get $right-offset))))
          (local.get $temp-res)))

      (i64.store32 
        (i32.add (local.get $res) (local.get $left-offset))
        (local.get $temp-res))

      (local.set $temp-res (i64.and 
          (i64.shr_u (local.get $temp-res) (i64.const 32)) 
          (i64.const 1)))

      (%minus-eq $left-offset 4)
      (%minus-eq $right-offset 4)
      (br $b_start)))

  ;; continue carrying through the remainder of the left item
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $left-offset)))

      ;; temp-res = left[left-offset] - right[right-offset] - carry
      (local.set $temp-res (i64.sub
          (i64.load32_u (i32.add (local.get $left) (local.get $left-offset)))
          (local.get $temp-res)))

      (i64.store32 
        (i32.add (local.get $res) (local.get $left-offset))
        (local.get $temp-res))

      (local.set $temp-res (i64.and 
          (i64.shr_u (local.get $temp-res) (i64.const 32)) 
          (i64.const 1)))

      (%minus-eq $left-offset 4)
      (br $b_start)))

  (return (call $mp-normalize (local.get $res))))

;; This creates a copy of the input number that uses the minimum storage.
;; it frees the input ptr (or returns it if it is already normalized)
(func $mp-normalize (param $ptr i32) (result i32)
  (return (call $mp-copy-impl (local.get $ptr) (i32.const 1))))

;; This creates a copy of the input number that uses the minimum storage.
;; Unlike mp-normalize, this doesn't free the input ptr.
(func $mp-copy (param $ptr i32) (result i32)
  (return (call $mp-copy-impl (local.get $ptr) (i32.const 0))))

;; internal implementation of both mp-copy and mp-normalize
(func $mp-copy-impl (param $ptr i32) (param $free i32) (result i32)
  (local $log2 i32)
  (local $len i32)
  (local $sign i32)
  (local $norm-len i32)
  (local $norm i32)

  ;; len = *ptr
  (local.set $len (i32.load (local.get $ptr)))
  ;; sign = 0x8000_0000 & len
  (local.set $sign (i32.and (local.get $len) (i32.const 0x8000_0000)))
  ;; len = len & 0x7FFF_FFFF
  (local.set $len (i32.and (local.get $len) (i32.const 0x7FFF_FFFF)))

  ;; log2 = mp-log2(ptr)
  (local.set $log2 (call $mp-log2 (local.get $ptr)))
  ;; norm-len = (log2 + 0x1F) >>> 5
  (local.set $norm-len (i32.shr_u (i32.add (local.get $log2) (i32.const 0x1F)) (i32.const 5)))

  (if (local.get $free)
    (then
      ;; if (norm-len == len)
      (if (i32.eq (local.get $norm-len) (local.get $len))
        ;; already normalized so don't create a copy
        (then (return (local.get $ptr))))))

  ;; norm = calloc(norm-len + 1, 4)
  ;; *norm = norm-len | sign
  (%mp-alloc-sign-l $norm $norm-len $sign)

  (call $memcpy
    (i32.add (local.get $norm) (i32.const 4))
    (i32.add 
      (local.get $ptr) 
      (%word-size (i32.add (i32.sub (local.get $len) (local.get $norm-len)) (i32.const 1))))
    (%word-size-l $norm-len))

  (if (local.get $free)
    (then (call $mp-free (local.get $ptr))))

  (return (local.get $norm)))

;; creates a copy of ptr that uses a power of 2 words (this makes karatsuba much easier)
;; this doesn't free the input
(func $mp-denormalize-po2 (param $ptr i32) (result i32)
  (local $sign i32)
  (local $len i32)
  (local $denorm-len i32)
  (local $denorm i32)
  (local $src-ptr i32)
  (local $dest-ptr i32)

  (local.set $len (i32.load (local.get $ptr)))
  (local.set $sign (i32.and (local.get $len) (i32.const 0x8000_0000)))
  (local.set $len (i32.and (local.get $len) (i32.const 0x7FFF_FFFF)))

  (if (i32.le_u (i32.popcnt (local.get $len)) (i32.const 1))
    ;; if popcnt is 1 or 0 then this is a power of 2 (or zero)
    (then (return (local.get $ptr))))

  ;; norm-len = 0x80000000 >> (clz(len) - 1)
  (local.set $denorm-len (i32.shr_u 
      (i32.const 0x8000_0000) 
      (i32.sub (i32.clz (local.get $len)) (i32.const 1))))

  (%mp-alloc-sign-l $denorm $denorm-len $sign)

  (local.set $src-ptr (i32.add (local.get $ptr) (%word-size-l $len)))
  (local.set $dest-ptr (i32.add (local.get $denorm) (%word-size-l $denorm-len)))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (local.get $src-ptr) (local.get $ptr)))

      (i32.store (local.get $dest-ptr) (i32.load (local.get $src-ptr)))

      (%minus-eq $dest-ptr 4)
      (%minus-eq $src-ptr 4)

      (br $b_start)))

  (return (local.get $denorm)))

;; multiply two numbers.
;; for now use the simplest O(n²) algorithm, consider karatsuba for longer inputs (maybe word len 20 or so)
(;
multiply(a[1..p], b[1..q], base)                            // Operands containing rightmost digits at index 1
  product = [1..p+q]                                        // Allocate space for result
  for b_i = 1 to q                                          // for all digits in b
    carry = 0
    for a_i = 1 to p                                        // for all digits in a
      product[a_i + b_i - 1] += carry + a[a_i] * b[b_i]
      carry = product[a_i + b_i - 1] / base
      product[a_i + b_i - 1] = product[a_i + b_i - 1] mod base
    product[b_i + p] = carry                               // last digit comes from final carry
  return product
;)

(func $mp-mul (param $left i32) (param $right i32) (result i32)
  (local $left-len i32)
  (local $right-len i32)
  (local $left-sign i32)
  (local $right-sign i32)
  (local $res-len i32)
  (local $left-ptr i32)
  (local $right-ptr i32)
  (local $res i32)
  (local $carry i64)
  (local $temp i64)
  (local $right-word i64)
  (local $left-word i64)
  (local $res-word i64)
  (local $res-ptr i32)
  (local $res-end-ptr i32)

  (local.set $left-len (i32.load (local.get $left)))
  (local.set $left-sign (i32.and (local.get $left-len) (i32.const 0x8000_0000)))
  (local.set $left-len (i32.and (local.get $left-len) (i32.const 0x7FFF_FFFF)))

  (local.set $right-len (i32.load (local.get $right)))
  (local.set $right-sign (i32.and (local.get $right-len) (i32.const 0x8000_0000)))
  (local.set $right-len (i32.and (local.get $right-len) (i32.const 0x7FFF_FFFF)))

  (local.set $res-len (i32.add 
      (i32.add (local.get $left-len) (local.get $right-len)) 
      (i32.const 1)))
  (local.set $res (call $calloc (i32.add (local.get $res-len) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get $res) (i32.or 
      (local.get $res-len) 
      (i32.xor (local.get $left-sign) (local.get $right-sign))))
  (local.set $res-end-ptr (i32.add (local.get $res) (%word-size-l $res-len)))

  (local.set $right-ptr (i32.add (local.get $right) (%word-size-l $right-len)))

  (block $right_end (loop $right_start 
      (br_if $right_end (i32.eq (local.get $right-ptr) (local.get $right)))

      (local.set $carry (i64.const 0))
      (local.set $right-word (i64.load32_u (local.get $right-ptr)))
      (local.set $left-ptr (i32.add (local.get $left) (%word-size-l $left-len)))
      (local.set $res-ptr (local.get $res-end-ptr))

      (block $left_end (loop $left_start
          (br_if $left_end (i32.eq (local.get $left-ptr) (local.get $left)))

          (local.set $res-word (i64.load32_u (local.get $res-ptr)))
          (local.set $left-word (i64.load32_u (local.get $left-ptr)))
          (local.set $temp (i64.add 
              (i64.add
                (local.get $carry)
                (local.get $res-word))
              (i64.mul
                (local.get $left-word)
                (local.get $right-word))))
          (local.set $carry (i64.shr_u (local.get $temp) (i64.const 32)))
          (local.set $res-word (i64.and (local.get $temp) (i64.const 0xFFFF_FFFF)))
          
          (i64.store32 (local.get $res-ptr) (local.get $res-word))

          (%minus-eq $left-ptr 4)
          (%minus-eq $res-ptr 4)
          (br $left_start)))

      (i64.store32 (local.get $res-ptr) (local.get $carry))
      
      (%minus-eq $right-ptr 4)
      (%minus-eq $res-end-ptr 4)
      (br $right_start)))
  
  (return (call $mp-normalize (local.get $res))))

(func $mp-10-pow (param $pow i32) (result i32)
  (local $split i32)
  (local $val i32)
  (local $bits i32)
  (local $curr i32)
  (local $accum i32)
  (local $smallest i32)
  (local $temp i32)

  ;; check the cache
  (if (local.tee $val (call $mp-10-cache-get (local.get $pow)))
    (then (return (local.get $val))))

  ;; if the power of 10 will be less than 2^64 (i.e. <= 10^19 ≌ 2^63.1 )
  (if (i32.le_u (local.get $pow) (i32.const 19))
    (then (return (call $mp-10-pow-short (local.get $pow)))))


  ;; if pow is a power of 2
  (if (i32.eq (i32.shl (i32.const 1) (i32.ctz (local.get $pow))) (local.get $pow))
    (then
      ;; temp = 10^{n/2}
      (local.set $temp (call $mp-10-pow (i32.shr_u (local.get $pow) (i32.const 1))))
      ;; 10^n = temp·temp
      (return (call $mp-10-cache-set! 
          (local.get $pow)
          (call $mp-mul (local.get $temp) (local.get $temp))))))

  (local.set $bits (local.get $pow)) 
  (local.set $curr (i32.const 0))
  (local.set $accum (i32.const 0))

  ;; set accum to 1 (10^0)
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $bits)))
      ;; find the smallest power of 2 in bits
      (local.set $smallest (i32.shl (i32.const 1) (i32.ctz (local.get $bits))))
      ;; remove current bit from the bit pattern
      (local.set $bits (i32.xor (local.get $bits) (local.get $smallest)))
      ;; add it to the current pow value
      (local.set $curr (i32.or (local.get $curr) (local.get $smallest)))

      ;; check if we already have this in the cache
      (if (local.tee $temp (call $mp-10-cache-get (local.get $curr)))
        (then 
          (local.set $accum (local.get $temp))
          (br $b_start)))

      (local.set $temp (call $mp-10-pow (local.get $smallest)))
      (if (local.get $accum)
        (then
          (local.set $accum (call $mp-mul (local.get $accum) (local.get $temp)))
          (drop (call $mp-10-cache-set! (local.get $curr) (local.get $accum))))
        (else
          (local.set $accum (local.get $temp))
          (local.set $curr (local.get $smallest))))
      
      (br $b_start)))

  (return (local.get $accum)))

(func $mp-10-pow-short (param $pow i32) (result i32)
  (local $count i32)
  (local $accum i64)
  (local $high i32)
  (local $low i32)
  (local $res i32)

  (local.set $accum (i64.const 1))
  (local.set $count (local.get $pow))
  (block $b_end (loop $b_start 
      (br_if $b_end (i32.eqz (local.get $count)))
      (local.set $accum (i64.mul (local.get $accum) (i64.const 10)))
      (%dec $count)
      (br $b_start)))

  (local.set $low (i32.wrap_i64 (local.get $accum))) 
  (local.set $high (i32.wrap_i64 (i64.shr_u (local.get $accum) (i64.const 32))))

  (if (local.get $high)
    (then
      (local.set $res (call $malloc (i32.const 12)))
      (i32.store (local.get $res) (i32.const 2))
      (i32.store offset=4 (local.get $res) (local.get $high))
      (i32.store offset=8 (local.get $res) (local.get $low)))
    (else
      (local.set $res (call $malloc (i32.const 8)))
      (i32.store (local.get $res) (i32.const 1))
      (i32.store offset=4 (local.get $res) (local.get $low))))
  
  (return (call $mp-10-cache-set! (local.get $pow) (local.get $res))))

(global $g-mp-10-cache (mut i32) (i32.const 0))
(global $g-mp-10-cache-len (mut i32) (i32.const 0))

(func $mp-10-cache-get (param $pow i32) (result i32)
  (if (i32.lt_u (local.get $pow) (global.get $g-mp-10-cache-len))
    (then
      (return (i32.load (i32.add
            (global.get $g-mp-10-cache) 
            (%word-size-l $pow))))))

  (return (i32.const 0)))

(func $mp-10-cache-set! (param $pow i32) (param $val i32) (result i32)
  (local $new-len i32)
  (local $cache i32)

  (if (i32.ge_u (local.get $pow) (global.get $g-mp-10-cache-len))
    (then
      ;; we need to resize the cache. make it pow, rounded up to next 32
      (local.set $new-len (i32.and 
          (i32.add (local.get $pow) (i32.const 0x20)) 
          (i32.const 0xFFFF_FFE0)))
      (local.set $cache (call $calloc 
          (local.get $new-len) 
          (i32.const 4)))
      
      (if (global.get $g-mp-10-cache)
        (then
          ;; copy values from old cache, then delete it
          (call $memcpy 
            (local.get $cache) 
            (global.get $g-mp-10-cache) 
            (%word-size (global.get $g-mp-10-cache-len)))
          (call $malloc-free (global.get $g-mp-10-cache))
        ))
      (global.set $g-mp-10-cache (local.get $cache))
      (global.set $g-mp-10-cache-len (local.get $new-len))))

  (i32.store 
    (i32.add (global.get $g-mp-10-cache) (%word-size-l $pow))
    (local.get $val))

  (return (local.get $val)))

(func $mp-cleanup
  (local $num i32)
  (local $ptr i32)
  (local $count i32)

  (if (global.get $g-mp-zero) (then
      (call $malloc-free (global.get $g-mp-zero))
      (global.set $g-mp-zero (i32.const 0))))

  (local.set $ptr (global.get $g-mp-10-cache))
  (if (i32.eqz (local.get $ptr))
    (then (return)))

  (local.set $count (global.get $g-mp-10-cache-len))
  (block $b_end (loop $b_start 
      (br_if $b_end (i32.eqz (local.get $count)))

      (if (local.tee $num (i32.load (local.get $ptr)))
        (then (call $malloc-free (local.get $num))))

      (%dec $count)
      (%plus-eq $ptr 4)
      (br $b_start)))

  (call $malloc-free (global.get $g-mp-10-cache))
  (global.set $g-mp-10-cache (i32.const 0))
  (global.set $g-mp-10-cache-len (i32.const 0)))

(func $mp-is-10-pow (param $ptr i32) (result i32)
  (local $cache-ptr i32)
  (local $count i32)

  (if (i32.eqz (local.tee $cache-ptr (global.get $g-mp-10-cache))) (then
    (return (i32.const 0))))
  (local.set $count (global.get $g-mp-10-cache-len))
  
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $count)))

      (if (i32.eq (i32.load (local.get $cache-ptr)) (local.get $ptr)) (then
        (return (i32.const 1))))

      (%dec $count)
      (%plus-eq $cache-ptr 4)
      (br $b_start)))

  (return (i32.const 0)))

(func $mp-zero? (param $ptr i32) (result i32)
  (return (select
    (i32.const 1)
    (i32.const 0)
    (i32.eqz (call $mp-log2 (local.get $ptr))))))

(global $g-mp-zero (mut i32) (i32.const 0))

(func $mp-zero (result i32)
  (local $ptr i32)
  (if (i32.eqz (global.get $g-mp-zero))
    (then
      (local.set $ptr (call $malloc (i32.const 8)))
      (i32.store (local.get $ptr) (i32.const 1))
      (i32.store offset=4 (local.get $ptr) (i32.const 0))
      (global.set $g-mp-zero (local.get $ptr))))

  (return (global.get $g-mp-zero)))

(global $g-mp-one (mut i32) (i32.const 0))

(func $mp-one (result i32)
  (local $ptr i32)
  (if (i32.eqz (global.get $g-mp-one))
    (then
      (local.set $ptr (call $malloc (i32.const 8)))
      (i32.store (local.get $ptr) (i32.const 1))
      (i32.store offset=4 (local.get $ptr) (i32.const 1))
      (global.set $g-mp-one (local.get $ptr))))

  (return (global.get $g-mp-one)))

(func $mp-free (param $ptr i32)
  (block $b_check
    (br_if $b_check (i32.eq (local.get $ptr) (global.get $g-mp-zero)))
    (br_if $b_check (i32.eq (local.get $ptr) (global.get $g-mp-one)))
    (br_if $b_check (call $mp-is-10-pow (local.get $ptr)))

    (call $malloc-free (local.get $ptr))))

;; Integer division.
;;
;; Returns the quotient as the high word in an i64 and the remainder 
;; as the low word
(func $mp-div (param $dividend i32) (param $divisor i32) (result i64)
  (local $quot i32)
  (local $temp i32)
  (local $rem i32)
  (local $res-len i32)
  (local $dvnd-sig i32) ;; number of significant words in the dividend
  (local $dvsr-sig i32) ;; number of significant words in the divisor
  (local $dvnd-sign i32) ;; sign of the divident
  (local $dvsr-sign i32) ;; sign of the divisor
  (local $shift i32)
  (local $temp-high i64)
  (local $temp-low i64)
  (local $testor i32)

  ;; handle simple cases first
  (if (call $mp-zero? (local.get $divisor))
    ;; cannot divide by zero
    (then (return (i64.const 0))))

  (%define %div-result (%quot %rem) (i64.or
      (i64.shl (i64.extend_i32_u %quot) (i64.const 32))
      (i64.extend_i32_u %rem)))

  (if (call $mp-abs-gt? (local.get $divisor) (local.get $dividend))
    ;; if dividend is greater than the divisor, then quotient is 0, 
    ;; and the remainder is the dividend
    (then (return (%div-result 
          (call $mp-zero) 
          (call $mp-copy (local.get $dividend))))))


  ;; initial res-len is the same as dividend normalized len
  (local.set $res-len (i32.shr_u 
     (i32.add 
        (call $mp-log2 (local.get $dividend)) 
        (i32.const 0x1f)) 
      (i32.const 5)))


  ;; find number of significant words in dividend and divisor
  (local.set $dvnd-sig (i32.shr_u 
      (i32.add (call $mp-log2 (local.get $dividend)) (i32.const 0x1f))
      (i32.const 5)))
  (local.set $dvnd-sign (%mp-sign-l $dividend))
  (local.set $dvsr-sig 
      (i32.shr_u (i32.add (call $mp-log2 (local.get $divisor)) (i32.const 0x1f))
      (i32.const 5)))
  (local.set $dvsr-sign (%mp-sign-l $divisor))

  (block $d_done
    (if (i32.le_u (local.get $dvnd-sig) (i32.const 2))
      (then
        ;; this is just a 64-bit division
        (local.set $temp-high (call $mp-to-u64 (local.get $dividend)))
        (local.set $temp-low (call $mp-to-u64 (local.get $divisor)))

        (local.set $quot (call $mp-from-u64 
            (i64.div_u 
              (local.get $temp-high) 
              (local.get $temp-low))))
        
        (local.set $rem (call $mp-from-u64
            (i64.rem_u
              (local.get $temp-high) 
              (local.get $temp-low))))
        
        (br $d_done)))

    (local.set $rem (call $mp-copy (local.get $dividend)))
    (if (local.get $dvnd-sign)
      (then (call $mp-neg (local.get $rem))))
    (%mp-alloc-l $quot $res-len)

    (local.set $shift (i32.sub 
        (call $mp-log2 (local.get $dividend)) 
        (call $mp-log2 (local.get $divisor))))
    (if (local.get $dvsr-sign)
      (then
        (call $mp-neg (local.get $divisor))
        (local.set $testor (call $mp-shl (local.get $divisor) (local.get $shift)))
        (call $mp-neg (local.get $divisor)))
      (else
        (local.set $testor (call $mp-shl (local.get $divisor) (local.get $shift)))))

    (block $b_end (loop $b_start
        (if (call $mp-abs-gt? (local.get $testor) (local.get $rem))
          (then nop)
          (else
            ;; subtract testor from rem
            (local.set $rem (call $mp-sub-eq (local.get $rem) (local.get $testor)))
            ;; set a bit in the quotient
            (call $mp-bit-set (local.get $quot) (local.get $shift))
            (br_if $b_end (call $mp-zero? (local.get $rem)))))

        (br_if $b_end (i32.eqz (local.get $shift)))
        (%dec $shift)
        (call $mp-shr-ip (local.get $testor) (i32.const 1))
        (br $b_start)))

    (call $mp-free (local.get $testor)))

  ;; The quotient has the xor of the signs (i.e if one is negative, 
  ;; the quotient is negative, otherwise positive).
  (if (i32.xor (local.get $dvnd-sign) (local.get $dvsr-sign))
    (then (call $mp-neg (local.get $quot))))
  ;; The remainder always has the same sign as the dividend.
  (if (local.get $dvnd-sign)
    (then (call $mp-neg (local.get $rem))))

  (return (%div-result 
      (call $mp-normalize (local.get $quot))
      (call $mp-normalize (local.get $rem)))))

(func $mp-bit-set (param $ptr i32) (param $bit i32)
  (local $word-offset i32)
  (local $bit-offset i32)
  (local $dest-ptr i32)

  (local.set $word-offset (i32.shr_u (local.get $bit) (i32.const 5)))
  (local.set $bit-offset (i32.and (local.get $bit) (i32.const 0x1f)))

  (local.set $dest-ptr (i32.add 
      (local.get $ptr)
      (%word-size (i32.sub (%mp-len-l $ptr) (local.get $word-offset)))))

  (i32.store 
    (local.get $dest-ptr)
    (i32.or
      (i32.load (local.get $dest-ptr))
      (i32.shl (i32.const 1) (local.get $bit-offset)))))

(func $mp-bit-get (param $ptr i32) (param $bit i32) (result i32)
  (local $word-offset i32)
  (local $bit-offset i32)
  (local $src-ptr i32)

  (local.set $word-offset (i32.shr_u (local.get $bit) (i32.const 5)))
  (local.set $bit-offset (i32.and (local.get $bit) (i32.const 0x1f)))

  (local.set $src-ptr (i32.add 
      (local.get $ptr)
      (%word-size (i32.sub (%mp-len-l $ptr) (local.get $word-offset)))))

  (if (i32.le_u (local.get $src-ptr) (local.get $ptr))
    (then (return (i32.const 0))))

  (return (i32.and 
      (i32.shr_u 
        (i32.load (local.get $src-ptr))
        (local.get $bit-offset))
      (i32.const 1))))

(func $mp-shr-ip (param $ptr i32) (param $shift i32)
  (local $word-offset i32)
  (local $small-shift i32)
  (local $small-shift-inv i32)
  (local $src-ptr i32)
  (local $dest-ptr i32)

  (if (i32.eqz (local.get $shift))
    (then (return)))

  (local.set $word-offset (i32.shr_u (local.get $shift) (i32.const 5)))
  (local.set $small-shift (i32.and (local.get $shift) (i32.const 0x1f)))
  (local.set $small-shift-inv (i32.sub (i32.const 0x20) (local.get $small-shift)))

  ;; dest-ptr = (add ptr (word-size (len ptr)))
  (local.set $dest-ptr (i32.add 
      (local.get $ptr) 
      (%word-size (%mp-len-l $ptr))))
  ;; src-ptr = (sub dest-ptr (word-size (add word-offset 1)))
  (local.set $src-ptr (i32.sub 
      (local.get $dest-ptr)
      (%word-size (i32.add (local.get $word-offset) (i32.const 1)))))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (local.get $dest-ptr) (local.get $ptr)))

      (block $b_shifted
        (if (i32.gt_u (local.get $src-ptr) (local.get $ptr))
          (then
            (i32.store (local.get $dest-ptr) (i32.or
                (i32.shl (i32.load offset=0 (local.get $src-ptr)) (local.get $small-shift-inv))
                (i32.shr_u (i32.load offset=4 (local.get $src-ptr)) (local.get $small-shift))))
            (br $b_shifted)))

        (if (i32.eq (local.get $src-ptr) (local.get $ptr))
          (then
            (i32.store (local.get $dest-ptr)
              (i32.shr_u (i32.load offset=4 (local.get $src-ptr)) (local.get $small-shift)))
            (br $b_shifted)))

        (i32.store (local.get $dest-ptr) (i32.const 0)))

      (%minus-eq $dest-ptr 4)
      (%minus-eq $src-ptr 4)
      (br $b_start))))

(func $mp-shl (param $ptr i32) (param $shift i32) (result i32)
  (local $init-log i32)
  (local $init-sign i32)
  (local $res-log i32)
  (local $init-len i32)
  (local $init-norm-len i32)
  (local $res-len i32)
  (local $partial i32)
  (local $res i32)
  (local $dest-idx i32)
  (local $src-idx i32)
  (local $dest-ptr i32)
  (local $src-ptr i32)
  (local $small-shift i32)
  (local $small-shift-inv i32)

  (if (i32.eqz(local.get $shift))
    (then (return (call $mp-copy (local.get $ptr)))))

  ;; small-shift = shift & 0x1F
  (local.set $small-shift (i32.and (local.get $shift) (i32.const 0x1F)))

  (if (i32.ge_u (local.get $shift) (i32.const 32))
    (then
      (local.set $partial (call $mp-shl-32 
          (local.get $ptr) 
          (i32.shr_u (local.get $shift) (i32.const 5))))
      (if (i32.eqz (local.get $small-shift))
        (then (return (local.get $partial)))
        (else 
          (local.set $res (call $mp-shl 
              (local.get $partial) 
              (local.get $small-shift)))
          (call $mp-free (local.get $partial))
          (return (local.get $res))))))

  ;; small-shift-inv = 32 - small-shift
  (local.set $small-shift-inv (i32.sub (i32.const 0x20) (local.get $small-shift)))

  (local.set $init-log (call $mp-log2 (local.get $ptr)))
  (local.set $init-len (%mp-len-l $ptr))
  (local.set $init-norm-len (i32.shr_u 
      (i32.add (local.get $init-log) (i32.const 0x1F)) 
      (i32.const 5)))

  (local.set $res-log (i32.add (local.get $init-log) (local.get $shift)))
  (local.set $res-len (i32.shr_u 
      (i32.add (local.get $res-log) (i32.const 0x1F)) 
      (i32.const 5)))

  (local.set $init-sign (%mp-sign-l $ptr))
  (%mp-alloc-sign-l $res $res-len $init-sign)

  (local.set $dest-idx (i32.const 0))
  (local.set $src-idx (i32.sub (i32.const 0) (local.get $shift)))

  (local.set $dest-ptr (i32.add (local.get $res) (%word-size-l $res-len)))
  (local.set $src-ptr (i32.sub
      (i32.add 
        (local.get $ptr)
        (%word-size-l $init-len))
      (%word-size (i32.sub 
          (i32.shr_u (i32.add (local.get $shift) (i32.const 0x1F)) (i32.const 5))
          (i32.const 1)))))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq (local.get $dest-ptr) (local.get $res)))

      (block $b_shifted
        (if (i32.lt_u (local.get $src-ptr) (local.get $ptr))
          (then unreachable))

        (if (i32.eq (local.get $src-ptr) (local.get $ptr))
          (then
            (i32.store (local.get $dest-ptr)
                (i32.shr_u (i32.load offset=4 (local.get $src-ptr)) (local.get $small-shift-inv)))
            (br $b_shifted)))

        (if (i32.gt_s (local.get $src-idx) (i32.const 0))
          (then
            (i32.store (local.get $dest-ptr) (i32.or
                (i32.shl (i32.load offset=0 (local.get $src-ptr)) (local.get $small-shift))
                (i32.shr_u (i32.load offset=4 (local.get $src-ptr)) (local.get $small-shift-inv))))
            (br $b_shifted)))

        (if (i32.gt_s (local.get $src-idx) (i32.const -32))
          (then
            (i32.store (local.get $dest-ptr)
                (i32.shl (i32.load offset=0 (local.get $src-ptr)) (local.get $small-shift)))
            (br $b_shifted))))

      (%minus-eq $src-ptr 4) 
      (%minus-eq $dest-ptr 4)
      (%plus-eq $dest-idx 32)
      (%plus-eq $src-idx 32)
      (br $b_start)))


  (return (local.get $res)))

(func $mp-from-u64 (param $value i64) (result i32)
  (local $low i32)
  (local $high i32)
  (local $ptr i32)

  (local.set $low (i32.wrap_i64 (local.get $value)))
  (local.set $high (i32.wrap_i64 (i64.shr_u (local.get $value) (i64.const 32))))
  (if (local.get $high)
    (then
      (local.set $ptr (call $malloc (i32.const 12)))
      (i32.store offset=0 (local.get $ptr) (i32.const 2))
      (i32.store offset=4 (local.get $ptr) (local.get $high))
      (i32.store offset=8 (local.get $ptr) (local.get $low)))
    (else
      (local.set $ptr (call $malloc (i32.const 8)))
      (i32.store offset=0 (local.get $ptr) (i32.const 1))
      (i32.store offset=4 (local.get $ptr) (local.get $low))))

  (return (local.get $ptr)))

(func $mp-from-i64 (param $value i64) (result i32)
  (local $mp i32)
  
  (if (i64.ge_s (local.get $value) (i64.const 0))
    (then (return (call $mp-from-u64 (local.get $value)))))

  (call $mp-neg (local.tee $mp 
      (call $mp-from-u64 (i64.sub (i64.const 0) (local.get $value)))))
  (return (local.get $mp)))

(func $mp-to-u64 (param $ptr i32) (result i64)
  (local $len i32)
  (local $value i64)

  (local.set $len (%mp-len-l $ptr))
  (local.set $value (i64.load32_u (i32.add 
        (local.get $ptr)
        (%word-size (local.get $len)))))
  (%dec $len)
  (if (local.get $len)
    (then 
      (local.set $value (i64.or 
        (local.get $value)
        (i64.shl 
          (i64.load32_u (i32.add
              (local.get $ptr)
              (%word-size (local.get $len))))
          (i64.const 32))))))

  (return (local.get $value)))

;; shifts a number left by `shift` number of 32 bit words, used by division
(func $mp-shl-32 (param $ptr i32) (param $shift i32) (result i32)
  (local $ptr-len i32)
  (local $min-ptr-len i32)
  (local $ptr-sign i32)
  (local $ptr-log2 i32)
  (local $dest-log2 i32)
  (local $dest-len i32)
  (local $dest i32)

  (local.set $ptr-len (%mp-len-l $ptr))
  (local.set $ptr-sign (%mp-sign-l $ptr))
  (local.set $ptr-log2 (call $mp-log2 (local.get $ptr)))
  (local.set $dest-log2 (i32.add 
      (local.get $ptr-log2)
      (i32.shl (local.get $shift) (i32.const 5))))

  (local.set $dest-len (i32.shr_u 
      (i32.add (local.get $dest-log2) (i32.const 0x1f)) 
      (i32.const 5)))
  (local.set $min-ptr-len (i32.shr_u 
      (i32.add (local.get $ptr-log2) (i32.const 0x1f)) 
      (i32.const 5)))
  
  (%mp-alloc-sign-l $dest $dest-len $ptr-sign)

  (call $memcpy
    (i32.add (local.get $dest) (i32.const 4))
    (i32.add 
      (i32.add (local.get $ptr) (i32.const 4))
      (%word-size (i32.sub (local.get $ptr-len) (local.get $min-ptr-len))))
    (%word-size-l $min-ptr-len))

  (return (local.get $dest)))

(func $mp-eq? (param $a i32) (param $b i32) (result i32)
  (return (i32.eqz (call $mp-cmp (local.get $a) (local.get $b)))))

(func $mp-gt? (param $a i32) (param $b i32) (result i32)
  (return (i32.gt_s 
      (call $mp-cmp (local.get $a) (local.get $b))
      (i32.const 0))))

(func $mp-ge? (param $a i32) (param $b i32) (result i32)
  (return (i32.ge_s 
      (call $mp-cmp (local.get $a) (local.get $b))
      (i32.const 0))))

(func $mp-lt? (param $a i32) (param $b i32) (result i32)
  (return (i32.lt_s 
      (call $mp-cmp (local.get $a) (local.get $b))
      (i32.const 0))))

(func $mp-le? (param $a i32) (param $b i32) (result i32)
  (return (i32.le_s 
      (call $mp-cmp (local.get $a) (local.get $b))
      (i32.const 0))))

(func $mp-cmp (param $left i32) (param $right i32) (result i32)
  (local $sign-left i32)
  (local $sign-right i32)
  (local $len-left i32)
  (local $len-right i32)
  (local $ptr-left i32)
  (local $ptr-right i32)
  (local $temp i32)
  (local $word-left i32)
  (local $word-right i32)

  (if (i32.eq (local.get $left) (local.get $right))
    (then (return (i32.const 0))))

  (local.set $sign-left (%mp-sign-l $left))
  (local.set $sign-right (%mp-sign-l $right))
  (if (i32.ne (local.get $sign-left) (local.get $sign-right))
    (then ;; signs are not equal
      ;; return -1 if left is -ve, +1 otherwise
      (return (select
        (i32.const -1)
        (i32.const 1)
        (local.get $sign-left)))))

  (if (local.get $sign-left)
    (then ;; both are negative, negate and invert the comparison
      (call $mp-neg (local.get $left))
      (call $mp-neg (local.get $right))
      (local.set $temp (call $mp-cmp (local.get $right) (local.get $left)))
      (call $mp-neg (local.get $left))
      (call $mp-neg (local.get $right))
      (return (local.get $temp))))


  (local.set $len-left (%mp-len-l $left))
  (local.set $len-right (%mp-len-l $right))

  ;; loop until ptr-left points to some data, or until len-left is 0
  (local.set $ptr-left (i32.add (local.get $left) (i32.const 4)))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $len-left)))
      (br_if $b_end (i32.load (local.get $ptr-left)))
      (%plus-eq $ptr-left 4)
      (%dec $len-left)
      (br $b_start)))

  ;; loop until ptr-right points to some data, or until len-right is 0
  (local.set $ptr-right (i32.add (local.get $right) (i32.const 4)))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $len-right)))
      (br_if $b_end (i32.load (local.get $ptr-right)))
      (%plus-eq $ptr-right 4)
      (%dec $len-right)
      (br $b_start)))

  ;; if left and right lengths differ
  (if (i32.ne (local.get $len-left) (local.get $len-right))
    ;; if left len is less, then -1, otherwise +1
    (then (return (select
      (i32.const -1)
      (i32.const 1)
      (i32.lt_u (local.get $len-left) (local.get $len-right))))))

  (if (i32.eqz (local.get $len-left))
    ;; both are zero
    (then (return (i32.const 0))))

  ;; only case where we have work to do, same number of words, remaining

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $len-left)))

      (local.set $word-left (i32.load (local.get $ptr-left)))
      (local.set $word-right (i32.load (local.get $ptr-right)))

      (if (i32.ne (local.get $word-left) (local.get $word-right))
        ;; temp is not zero
        ;; return -1 if left is less than right (temp is < 0), 1 otherwise
        (then (return (select
              (i32.const -1)  
              (i32.const 1)
              (i32.lt_u (local.get $word-left) (local.get $word-right))))))

      (%plus-eq $ptr-left 4)
      (%plus-eq $ptr-right 4)
      (%dec $len-left)
      (br $b_start)))

  (return (i32.const 0)))

(func $mp-plus-eq (param $left i32) (param $right i32) (result i32)
  (local $res i32)

  (local.set $res (call $mp-add (local.get $left) (local.get $right)))
  (call $mp-free (local.get $left))

  (return (local.get $res)))

(func $mp-times-eq (param $left i32) (param $right i32) (result i32)
  (local $res i32)

  (local.set $res (call $mp-mul (local.get $left) (local.get $right)))
  (call $mp-free (local.get $left))

  (return (local.get $res)))

(func $mp-minus-eq (param $left i32) (param $right i32) (result i32)
  (local $res i32)

  (local.set $res (call $mp-sub (local.get $left) (local.get $right)))
  (call $mp-free (local.get $left))

  (return (local.get $res)))

(func $mp-shl-eq (param $left i32) (param $shift i32) (result i32)
  (local $res i32)

  (local.set $res (call $mp-shl (local.get $left) (local.get $shift)))
  (call $mp-free (local.get $left))

  (return (local.get $res)))

(func $mp-abs-inc (param $ptr i32) (result i32)
  (local $dest-ptr i32)
  (local $len i32)
  (local $sign i32)
  (local $carry i32)
  (local $temp i64)
  (local $copy i32)

  (local.set $len (%mp-len-l $ptr))
  (local.set $dest-ptr (i32.add (local.get $ptr) (%word-size-l $len))) 

  (local.set $carry (i32.const 1))
  (block $b_end (loop $b_start
    (br_if $b_end (i32.eq (local.get $dest-ptr) (local.get $ptr)))
    
    (local.set $temp (i64.add 
        (i64.load32_u (local.get $dest-ptr))
        (i64.extend_i32_u (local.get $carry))))
    (i64.store32 (local.get $dest-ptr) (local.get $temp))

    (local.set $carry (i32.wrap_i64 (i64.shr_u (local.get $temp) (i64.const 32))))
    (if (i32.eqz (local.get $carry))
      (then (return (local.get $ptr))))

    (%minus-eq $dest-ptr 4)
    (br $b_start)))

  (%inc $len)
  (local.set $sign (%mp-sign-l $sign))
  (%mp-alloc-sign-l $copy $len $sign)

  (i32.store offset=4 (local.get $copy) (local.get $carry))
  (call $memcpy 
    (i32.add (local.get $copy) (i32.const 8))
    (i32.add (local.get $ptr) (i32.const 4))
    (%word-size (i32.sub (local.get $len) (i32.const 1))))

  (return (local.get $copy)))

(;

  From: Clinger, William D. "How to read floating point numbers accurately." 
        ACM SIGPLAN Notices 25.6 (1990): 92-101.

  https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.4152&rep=rep1&type=pdf

  ; Given exact integers f and e, with f nonnegative,
  ; returns the floating point number closest to 
  ; f * delta^e

  (define (AlgorithmM (f e))

    ; f * delta^e = u/v * beta^k

    (define (loop (u v k))
      (let ((x (quotient u v)))
        (cond ((and (<= beta^n-1 x) (< x beta^n))
               (ratio->float u v k))
              ((< x beta^n-1)
               (loop (* beta u) v (- k 1)))
              ((<= beta^n x)
               (loop u (* beta v) (+ k 1))))))

    (if (negative? e)
        (loop f (expt 10 (- e)) 0)
        (loop (* f (expt 10 e)) 1 0)))

  ; Given exact positive integers u and v with
  ; beta^(n-1) <= u/v beta^n, and exact integer k,
  ; returns the float closest to u/v * beta^k.

  (define (ratio->float (u v k))
    (let * ((q   (quotient u v))
            (r   (- u (* q v)))
            (v-r (- v r))
            (z   (make-float q k)))
      (cond ((< r v-r) z)
            ((> r v-r) (nextfloat z))
            ((even? q) z)
            (else (nextfloat z)))))

  (define delta 10)
  (define beta 2)
  (define n 53)
  (define beta^n (expt beta n))
  (define beta^n-1 (expt beta (- n 1)))

  ; Given a normalized floating point number
  ; z = m & beta^k, returns the normalized floating
  ; point number whose value is (m+1) * beta^k

  (define (nextfloat (z))
    (let ((m (float-significand z))
          (k (float-exponent z)))
      (if (= m (- beta^n 1))
          (make-float beta^n-1 (+ k 1))
          (make-float (+ m 1) k))))

  ; Given a normalized floating point number
  ; z = m * beta^k, returns the greatest normalized
  ; floating point number less than z. Note that the
  ; value returned may be greater than (m-1) * beta^k

  (define (prevfloat (z)
    (let ((m (float-significand z))
          (k (float-exponent z)))
      (if (= m beta^n-1)
          (make-float (- beta^n 1) (- k 1))
          (make-float (- m 1) k))))

;)

(func $mp-algorithm-m (param $f i32) (param $e i32) (result f64)
  (local $u i32)
  (local $v i32)
  (local $k i32)
  (local $quot-rem i64)
  (local $quot i32)
  (local $rem i32)
  (local $log2-quot i32)
  (local $v-rem i32)
  (local $z f64)
  (local $rem-cmp i32)
  (local $v-free i32)
  (local $kdiff i32)
  (local $sign i32)

  (if (call $mp-zero? (local.get $f)) (then
      (return (f64.const 0.0))))

  (if (local.tee $sign (%mp-sign-l $f))
    (then (call $mp-neg (local.get $f))))

  (if (i32.le_s (local.get $e) (i32.const 0))
    (then 
      (local.set $u (call $mp-copy (local.get $f)))
      (local.set $v (call $mp-10-pow (i32.sub (i32.const 0) (local.get $e)))))
    (else
      (local.set $u (call $mp-mul 
          (local.get $f)
          (call $mp-10-pow (local.get $e))))
      (local.set $v (call $mp-10-pow (i32.const 0)))))
  (local.set $v-free (i32.const 0))
  (local.set $k (i32.const 52))

  (if (local.get $sign) (then (call $mp-neg (local.get $f))))

  (loop $b_forever
    (local.set $quot-rem (call $mp-div (local.get $u) (local.get $v)))
    (if (i64.eqz (local.get $quot-rem))
      (then (unreachable)))
    (local.set $quot (i32.wrap_i64 (i64.shr_u (local.get $quot-rem) (i64.const 32))))
    (local.set $rem (i32.wrap_i64 (local.get $quot-rem)))

    (local.set $log2-quot (i32.sub
        (call $mp-log2 (local.get $quot)) 
        (i32.const 1)))

    (if (i32.eq (local.get $log2-quot) (i32.const 52)) (then
        (block $b-ratio->float
          (local.set $v-rem (call $mp-sub (local.get $v) (local.get $rem)))
          (local.set $z (call $mp-make-float (local.get $quot) (local.get $k)))
          (local.set $rem-cmp (call $mp-cmp (local.get $rem) (local.get $v-rem)))
          (call $mp-free (local.get $v-rem))
        
          (br_if $b-ratio->float (i32.lt_s (local.get $rem-cmp) (i32.const 0)))
          (if (i32.gt_s (local.get $rem-cmp) (i32.const 0))
            (then
              (local.set $z (call $mp-next-float (local.get $z)))
              (br $b-ratio->float)))
          (br_if $b-ratio->float (i32.eqz (call $mp-bit-get 
                (local.get $quot) 
                (i32.const 0))))
          (local.set $z (call $mp-next-float (local.get $z))))

        (call $mp-free (local.get $rem))
        (call $mp-free (local.get $quot))
        (call $mp-free (local.get $u))
        (call $mp-free (local.get $v))
        (if (local.get $sign) (then 
            (local.set $z (f64.neg (local.get $z)))))
        (return (local.get $z))))
    
    (if (i32.lt_s (local.get $log2-quot) (i32.const 52)) 
      (then
        (local.set $kdiff (i32.sub (i32.const 52) (local.get $log2-quot)))
        (local.set $u (call $mp-shl-eq (local.get $u) (local.get $kdiff)))
        (local.set $k (i32.sub (local.get $k) (local.get $kdiff))))
      (else
        (local.set $kdiff (i32.sub (local.get $log2-quot) (i32.const 52)))
        (if (local.get $v-free)
          (then
            (local.set $v (call $mp-shl-eq (local.get $v) (local.get $kdiff))))
          (else
            (local.set $v (call $mp-shl (local.get $v) (local.get $kdiff)))
            (local.set $v-free (i32.const 1))))
        (local.set $k (i32.add (local.get $k) (local.get $kdiff)))))

    (call $mp-free (local.get $rem))
    (call $mp-free (local.get $quot))
    (br $b_forever))

  (unreachable))

(func $mp-make-float (param $num i32) (param $exp i32) (result f64)
  (return (call $mp-make-float-64 
      (call $mp-to-u64 (local.get $num))
      (local.get $exp))))

(func $mp-make-float-64 (param $int64 i64) (param $exp i32) (result f64)
  (%plus-eq $exp 0x3FF)
  (if (i32.ge_s (local.get $exp) (i32.const 0x7FF)) (then 
      (return (f64.const inf))))
  (if (i32.lt_s (local.get $exp) (i32.const 0)) (then 
      (return (f64.const 0))))

  (return (f64.reinterpret_i64 (i64.or
      (i64.shl (i64.extend_i32_u (local.get $exp)) (i64.const 52))
      (i64.and (local.get $int64) (i64.const 0x000F_FFFF_FFFF_FFFF))))))

(func $mp-next-float (param $z f64) (result f64)
  (local $m i64)
  (local $k i32)

  (local.set $m (i64.or 
      (i64.and
        (i64.reinterpret_f64 (local.get $z))
        (i64.const 0x000F_FFFF_FFFF_FFFF))
      (i64.const 0x0010_0000_0000_0000)))

  (local.set $k (i32.sub
      (i32.wrap_i64 (i64.shr_u
          (i64.reinterpret_f64 (f64.abs (local.get $z)))
          (i64.const 52)))
      (i32.const 0x3FF)))

  (if (i64.eq (local.get $m) (i64.const 0x001F_FFFF_FFFF_FFFF)) (then 
      (return (call $mp-make-float-64
          (i64.const 0x0010_0000_0000_0000)
          (i32.add (local.get $k) (i32.const 1))))))
  
  (return (call $mp-make-float-64
      (i64.add (local.get $m) (i64.const 1))
      (local.get $k))))
