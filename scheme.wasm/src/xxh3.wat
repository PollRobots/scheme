  ;; #define XXH_PRIME64_1  0x9E3779B185EBCA87ULL  /*!< 0b1001111000110111011110011011000110000101111010111100101010000111 */
  (global $xxh_prime_1 i64 (i64.const 0x9E3779B185EBCA87))
  ;; #define XXH_PRIME64_2  0xC2B2AE3D27D4EB4FULL  /*!< 0b1100001010110010101011100011110100100111110101001110101101001111 */
  (global $xxh_prime_2 i64 (i64.const 0xC2B2AE3D27D4EB4F))
  ;; #define XXH_PRIME64_3  0x165667B19E3779F9ULL  /*!< 0b0001011001010110011001111011000110011110001101110111100111111001 */
  (global $xxh_prime_3 i64 (i64.const 0x165667B19E3779F9))
  ;; #define XXH_PRIME64_4  0x85EBCA77C2B2AE63ULL  /*!< 0b1000010111101011110010100111011111000010101100101010111001100011 */
  (global $xxh_prime_4 i64 (i64.const 0x85EBCA77C2B2AE63))
  ;; #define XXH_PRIME64_5  0x27D4EB2F165667C5ULL  /*!< 0b0010011111010100111010110010111100010110010101100110011111000101 */
  (global $xxh_prime_5 i64 (i64.const 0x27D4EB2F165667C5))

  ;; static xxh_u64 XXH64_round(xxh_u64 acc, xxh_u64 input)
  ;; {
  ;;     acc += input * XXH_PRIME64_2;
  ;;     acc  = XXH_rotl64(acc, 31);
  ;;     acc *= XXH_PRIME64_1;
  ;;     return acc;
  ;; }
  (func $xxh64_round (param $acc i64) (param $input i64) (result i64)
  ;;     acc += input * XXH_PRIME64_2;
    (local.set $acc (i64.add (local.get $acc) (i64.mul (local.get $input) (global.get $xxh_prime_2))))
  ;;     acc  = XXH_rotl64(acc, 31);
    (local.set $acc (i64.rotl (local.get $acc) (i64.const 31)))
  ;;     acc *= XXH_PRIME64_1;
  ;;     return acc;
    (return (i64.mul (local.get $acc) (global.get $xxh_prime_1)))
  )

  ;; static xxh_u64 XXH64_mergeRound(xxh_u64 acc, xxh_u64 val)
  ;; {
  ;;     val  = XXH64_round(0, val);
  ;;     acc ^= val;
  ;;     acc  = acc * XXH_PRIME64_1 + XXH_PRIME64_4;
  ;;     return acc;
  ;; }
  (func $xxh64_mergeRound (param $acc i64) (param $val i64) (result i64)
    ;; val  = XXH64_round(0, val);
    (local.set $val (call $xxh64_round (i64.const 0) (local.get $val)))
    ;; acc ^= val;
    (local.set $acc (i64.xor (local.get $acc) (local.get $val)))
    ;; acc  = acc * XXH_PRIME64_1 + XXH_PRIME64_4;
    ;; return acc;
    (return (i64.add (i64.mul (local.get $acc) (global.get $xxh_prime_1)) (global.get $xxh_prime_4)))
  )

  ;; static xxh_u64 XXH64_avalanche(xxh_u64 h64)
  ;; {
  ;;     h64 ^= h64 >> 33;
  ;;     h64 *= XXH_PRIME64_2;
  ;;     h64 ^= h64 >> 29;
  ;;     h64 *= XXH_PRIME64_3;
  ;;     h64 ^= h64 >> 32;
  ;;     return h64;
  ;; }
  (func $xxh64_avalanche (param $h64 i64) (result i64)
    ;; h64 ^= h64 >> 33;
    (local.set $h64 (i64.xor (local.get $h64) (i64.shr_u (local.get $h64) (i64.const 33))))
    ;; h64 *= XXH_PRIME64_2;
    (local.set $h64 (i64.mul (local.get $h64) (global.get $xxh_prime_2)))
    ;; h64 ^= h64 >> 29;
    (local.set $h64 (i64.xor (local.get $h64) (i64.shr_u (local.get $h64) (i64.const 29))))
    ;; h64 *= XXH_PRIME64_3;
    (local.set $h64 (i64.mul (local.get $h64) (global.get $xxh_prime_3)))
    ;; h64 ^= h64 >> 32;
    ;; return h64;
    (return (i64.xor (local.get $h64) (i64.shr_u (local.get $h64) (i64.const 32))))
  )


  ;; static xxh_u64
  ;; XXH64_finalize(xxh_u64 h64, const xxh_u8* ptr, size_t len, XXH_alignment align)
  ;; {
  ;;     len &= 31;
  ;;     while (len >= 8) {
  ;;         xxh_u64 const k1 = XXH64_round(0, XXH_get64bits(ptr));
  ;;         ptr += 8;
  ;;         h64 ^= k1;
  ;;         h64  = XXH_rotl64(h64,27) * XXH_PRIME64_1 + XXH_PRIME64_4;
  ;;         len -= 8;
  ;;     }
  ;;     if (len >= 4) {
  ;;         h64 ^= (xxh_u64)(XXH_get32bits(ptr)) * XXH_PRIME64_1;
  ;;         ptr += 4;
  ;;         h64 = XXH_rotl64(h64, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
  ;;         len -= 4;
  ;;     }
  ;;     while (len > 0) {
  ;;         h64 ^= (*ptr++) * XXH_PRIME64_5;
  ;;         h64 = XXH_rotl64(h64, 11) * XXH_PRIME64_1;
  ;;         --len;
  ;;     }
  ;;     return  XXH64_avalanche(h64);
  ;; }
  (func $xxh64_finalize (param $h64 i64) (param $ptr i32) (param $len i32) (result i64)
    (local $k1 i64)
    ;; len &= 31;
    (local.set $len (i32.and (local.get $len) (i32.const 31)))

    ;; while (len >= 8) {
    (block $w8_end
      (loop $w8_start
        (br_if $w8_end (i32.lt_u (local.get $len) (i32.const 8)))
        ;; xxh_u64 const k1 = XXH64_round(0, XXH_get64bits(ptr));
        (local.set $k1 (call $xxh64_round (i64.const 0) (i64.load (local.get $ptr))))
        ;; ptr += 8;
        (%plus-eq $ptr 8)
        ;; h64 ^= k1;
        (local.set $h64 (i64.xor (local.get $h64) (local.get $k1)))
        ;; h64  = XXH_rotl64(h64,27) * XXH_PRIME64_1 + XXH_PRIME64_4;
        (local.set $h64 (i64.add
                          (i64.mul
                            (i64.rotl (local.get $h64) (i64.const 27))
                            (global.get $xxh_prime_1)
                          )
                          (global.get $xxh_prime_4)
                        )
        )
        ;; len -= 8;
        (local.set $len (i32.sub (local.get $len) (i32.const 8)))
        (br $w8_start)
    ;; }
      )
    )
    ;; if (len >= 4) {
    (if (i32.ge_u (local.get $len) (i32.const 4))
      (then
        ;; h64 ^= (xxh_u64)(XXH_get32bits(ptr)) * XXH_PRIME64_1;
        (local.set $h64 (i64.xor
                          (local.get $h64)
                          (i64.mul
                            (i64.load32_u (local.get $ptr))
                            (global.get $xxh_prime_1)
                          )
                        )
        )
        ;; ptr += 4;
        (%plus-eq $ptr 4)
        ;; h64 = XXH_rotl64(h64, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
        (local.set $h64 (i64.add
                          (i64.mul
                            (i64.rotl (local.get $h64) (i64.const 23))
                            (global.get $xxh_prime_2)
                          )
                          (global.get $xxh_prime_3)
                        )
        )
        ;; len -= 4;
        (local.set $len (i32.sub (local.get $len) (i32.const 4)))
      )
    ;; }
    )
    ;; while (len > 0) {
    (block $wlen_end
      (loop $wlen_start
        (br_if $wlen_end (i32.eqz (local.get $len)))
    ;;     h64 ^= (*ptr++) * XXH_PRIME64_5;
        (local.set $h64 (i64.xor
                          (local.get $h64)
                          (i64.mul
                            (i64.load8_u (local.get $ptr))
                            (global.get $xxh_prime_5)
                          )
                        )
        )
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
    ;;     h64 = XXH_rotl64(h64, 11) * XXH_PRIME64_1;
        (local.set $h64 (i64.mul
                          (i64.rotl (local.get $h64) (i64.const 11))
                          (global.get $xxh_prime_1)
                        )
        )
    ;;     --len;
        (local.set $len (i32.sub (local.get $len) (i32.const 1)))
    ;; }
        (br $wlen_start)
      )
    )

    ;; return  XXH64_avalanche(h64);
    (return (call $xxh64_avalanche (local.get $h64)))
  )


  (func $xxh64 (param $input i32) (param $len i32) (param $seed i64) (result i64)
    (local $bEnd i32) ;; u8*
    (local $h64 i64)
    (local $limit i32) ;; u8*
    (local $v1 i64)
    (local $v2 i64)
    (local $v3 i64)
    (local $v4 i64)

    ;; bEnd = input + len;
    (local.set $bEnd (i32.add (local.get $input) (local.get $len)))

    ;; if (len >= 32)
    (if (i32.ge_u (local.get $len) (i32.const 32))
      (then
        ;; const xxh_u8* const limit = bEnd - 32;
        (local.set $limit (i32.sub (local.get $bEnd) (i32.const 32)))

        ;; xxh_u64 v1 = seed + XXH_PRIME64_1 + XXH_PRIME64_2;
        (local.set $v1 (i64.add (local.get $seed) (i64.add (global.get $xxh_prime_1) (global.get $xxh_prime_2))))
        ;; xxh_u64 v2 = seed + XXH_PRIME64_2;
        (local.set $v2 (i64.add (local.get $seed) (global.get $xxh_prime_2)))
        ;; xxh_u64 v3 = seed + 0;
        (local.set $v3 (local.get $seed))
        ;; xxh_u64 v4 = seed - XXH_PRIME64_1;
        (local.set $v4 (i64.sub (local.get $seed) (global.get $xxh_prime_1)))

        ;; do {
        (block $do_end
          (loop $do_start
            ;; v1 = XXH64_round(v1, XXH_get64bits(input)); input+=8;
            (local.set $v1 (call $xxh64_round (local.get $v1) (i64.load (local.get $input))))
            (%plus-eq $input 8)

            ;; v2 = XXH64_round(v2, XXH_get64bits(input)); input+=8;
            (local.set $v2 (call $xxh64_round (local.get $v2) (i64.load (local.get $input))))
            (%plus-eq $input 8)

            ;; v3 = XXH64_round(v3, XXH_get64bits(input)); input+=8;
            (local.set $v3 (call $xxh64_round (local.get $v3) (i64.load (local.get $input))))
            (%plus-eq $input 8)

            ;; v4 = XXH64_round(v4, XXH_get64bits(input)); input+=8;
            (local.set $v4 (call $xxh64_round (local.get $v4) (i64.load (local.get $input))))
            (%plus-eq $input 8)

            ;; } while (input<=limit);
            (br_if $do_start (i32.le_u (local.get $input) (local.get $limit)))
            (br $do_end)
          )
        )

        ;; h64 = XXH_rotl64(v1, 1) + XXH_rotl64(v2, 7) + XXH_rotl64(v3, 12) + XXH_rotl64(v4, 18);
        (local.set $h64
          (i64.add 
            (i64.add
              (i64.add
                (i64.rotl (local.get $v1) (i64.const 1))
                (i64.rotl (local.get $v2) (i64.const 7))
              )
              (i64.rotl (local.get $v3) (i64.const 12))
            )
            (i64.rotl (local.get $v4) (i64.const 18))
          )
        )
        ;; h64 = XXH64_mergeRound(h64, v1);
        (local.set $h64 (call $xxh64_mergeRound (local.get $h64) (local.get $v1)))
        ;; h64 = XXH64_mergeRound(h64, v2);
        (local.set $h64 (call $xxh64_mergeRound (local.get $h64) (local.get $v2)))
        ;; h64 = XXH64_mergeRound(h64, v3);
        (local.set $h64 (call $xxh64_mergeRound (local.get $h64) (local.get $v3)))
        ;; h64 = XXH64_mergeRound(h64, v4);
        (local.set $h64 (call $xxh64_mergeRound (local.get $h64) (local.get $v4)))

      )
      (else
        ;; h64 = seed + PRIME_5
        (local.set $h64 (i64.add (local.get $seed) (global.get $xxh_prime_5)))
      )
    )

    ;; h64 += len
    (local.set $h64 (i64.add (local.get $h64) (i64.extend_i32_u (local.get $len))))

    ;; return XXH64_finalize(h64, input, len, align)
    (return (call $xxh64_finalize (local.get $h64) (local.get $input) (local.get $len)))
  )