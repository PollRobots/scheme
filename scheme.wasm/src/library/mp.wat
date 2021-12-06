(;

Multi-precision integer math

numbers are represented as 

size: u31       -- number of 32bit words in this number
sign: u1        -- sign bit, 0 - positive, 1 - negative
data: u32[size] -- the words of the number

;)

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
  (local.set $buffer (call $calloc (i32.add (local.get $word-length) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get $buffer) (local.get $word-length))

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
      (call $malloc-free (local.get $low))
      (return (i32.const -1))))

  (local.set $scaled-high (call $mp-mul
      (local.get $high)
      (call $mp-10-pow (local.get $low-split))))

  (call $malloc-free (local.get $high))

  (local.set $res (call $mp-add (local.get $low) (local.get $scaled-high)))
  (call $malloc-free (local.get $scaled-high))
  (call $malloc-free (local.get $low))

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

  (local.set $res (call $calloc (i32.add (local.get $final-length) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get $res) (i32.or (local.get $final-length) (local.get $neg)))

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

  (local.set $length (i32.and (i32.load (local.get $ptr)) (i32.const 0x7FFF_FFFF)))

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


(func $mp-abs-gt (param $left i32) (param $right i32) (result i32)
  (local $left-log2 i32)
  (local $right-log2 i32)
  (local $left-length i32)
  (local $right-length i32)
  (local $left-word i32)
  (local $right-word i32)

  (local.set $left-log2 (call $mp-log2 (local.get $left)))
  (local.set $right-log2 (call $mp-log2 (local.get $right)))

  (if (i32.gt_u (local.get $left-log2) (local.get $right-log2))
    (then (return (i32.const 1))))
  (if (i32.lt_u (local.get $left-log2) (local.get $right-log2))
    (then (return (i32.const 0))))

  (local.set $left-length (i32.and (i32.load (local.get $left)) (i32.const 0x7FFF_FFFF)))
  (local.set $right-length (i32.and (i32.load (local.get $right)) (i32.const 0x7FFF_FFFF)))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq 
          (local.get $left-length) 
          (i32.shr_u (i32.add (local.get $left-log2) (i32.const 31)) (i32.const 5))))
      (%dec $left-length)
      (%plus-eq $left 4)
      (br $b_start)))
  (block $b_end (loop $b_start
      (br_if $b_end (i32.eq 
          (local.get $right-length) 
          (i32.shr_u (i32.add (local.get $right-log2) (i32.const 31)) (i32.const 5))))
      (%dec $right-length)
      (%plus-eq $right 4)
      (br $b_start)))

  (if (i32.ne (local.get $left-length) (local.get $right-length))
    (then unreachable))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $left-length)))

      (%plus-eq $left 4)
      (%plus-eq $right 4)

      (local.set $left-word (i32.load (local.get $left)))
      (local.set $right-word (i32.load (local.get $right)))

      (if (i32.gt_u (local.get $left-word) (local.get $right-word))
        (then (return (i32.const 1))))
      (if (i32.lt_u (local.get $left-word) (local.get $right-word))
        (then (return (i32.const 0))))

      (%dec $left-length)
      (br $b_start)))

  (return (i32.const 0)))

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

  (if (call $mp-abs-gt (local.get $right) (local.get $left))
    (then
      ;; right > left, right - left = -(left - right)
      (local.set $res (call $mp-sub (local.get $right) (local.get $left)))
      (call $mp-neg (local.get $res))
      (return (local.get $res))))

  ;; now we know that left >= right, and that both are the same sign.
  ;; time to actually do some subtracting

  (local.set $left-offset (%word-size-l $left-length))
  (local.set $right-offset (%word-size-l $right-length))
  (local.set $res (call $calloc (i32.add (local.get $left-length) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get $res) (i32.or (local.get $left-length) (local.get $neg)))

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

;; this creates a copy of the input number that uses the minimum storage.
;; it frees the input ptr (or returns it)
(func $mp-normalize (param $ptr i32) (result i32)
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

  ;; if (norm-len == len)
  (if (i32.eq (local.get $norm-len) (local.get $len))
    ;; already normalized
    (then (return (local.get $ptr))))

  ;; norm = calloc(norm-len + 1, 4)
  (local.set $norm (call $calloc (i32.add (local.get $norm-len) (i32.const 1)) (i32.const 4)))
  ;; *norm = norm-len | sign
  (i32.store (local.get $norm) (i32.or (local.get $norm-len) (local.get $sign)))

  (call $memcpy
    (i32.add (local.get $norm) (i32.const 4))
    (i32.add 
      (local.get $ptr) 
      (%word-size (i32.add (i32.sub (local.get $len) (local.get $norm-len)) (i32.const 1))))
    (%word-size-l $norm-len))
  (call $malloc-free (local.get $ptr))

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

  (local.set $denorm (call $calloc (i32.add (local.get $denorm-len) (i32.const 1)) (i32.const 4)))
  (i32.store (local.get $denorm) (i32.or (local.get $denorm-len) (local.get $sign)))

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
