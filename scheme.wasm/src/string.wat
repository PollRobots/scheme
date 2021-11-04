;; strings are represented as an i32 length followed by length bytes of utf-8 encoded string

(func $str-from-32 (param $len i32) (param $data i32) (result i32)
  (local $ptr i32)  ;; the return value and pointer to the string

  ;; if (len > 4) {
  (if (i32.gt_u (local.get $len) (i32.const 4))
  ;;   trap
    (then unreachable)
  )
  ;; }

  ;; ptr = malloc(8)
  (local.set $ptr (call $malloc (i32.const 8)))

  ;; *ptr = len
  (i32.store (local.get $ptr) (local.get $len))
  ;; *(ptr + 4) = data
  (i32.store (i32.add (local.get $ptr) (i32.const 4)) (local.get $data))

  ;; return ptr
  (return (local.get $ptr))
)

(func $str-from-64 (param $len i32) (param $data i64) (result i32)
  (local $ptr i32)  ;; the return value and pointer to the string

  ;; if (len > 8) {
  (if (i32.gt_u (local.get $len) (i32.const 8))
  ;;   trap
    (then unreachable)
  )
  ;; }

  ;; ptr = malloc(12)
  (local.set $ptr (call $malloc (i32.const 12)))

  ;; *ptr = len
  (i32.store (local.get $ptr) (local.get $len))
  ;; *(ptr + 4) = data
  (i64.store (i32.add (local.get $ptr) (i32.const 4)) (local.get $data))

  ;; return ptr
  (return (local.get $ptr))
)

(func $str-from-128 (param $len i32) (param $data1 i64) (param $data2 i64) (result i32)
  (local $ptr i32)  ;; the return value and pointer to the string

  ;; if (len > 16) {
  (if (i32.gt_u (local.get $len) (i32.const 16))
  ;;   trap
    (then unreachable)
  )
  ;; }

  ;; if (len <= 8) {
  (if (i32.le_s (local.get $len) (i32.const 8))
    (then
  ;;    return str-from-64(len, data1)
      (return (call $str-from-64 (local.get $len) (local.get $data1)))
  ;; }
    )
  )

  ;; ptr = malloc(20)
  (local.set $ptr (call $malloc (i32.const 20)))

  ;; *ptr = len
  (i32.store (local.get $ptr) (local.get $len))
  ;; *(ptr + 4) = data1
  (i64.store (i32.add (local.get $ptr) (i32.const 4)) (local.get $data1))
  ;; *(ptr + 12) = data2
  (i64.store (i32.add (local.get $ptr) (i32.const 12)) (local.get $data2))

  ;; return ptr
  (return (local.get $ptr))
)

(func $str-byte-len (param $ptr i32) (result i32)
  ;; if (ptr == 0) {
  (if (i32.eqz (local.get $ptr))
    ;; trap
    (then unreachable)
  )
  ;; }

  ;; return *ptr
  (return (i32.load (local.get $ptr)))
)

(func $str-code-point-len (param $ptr i32) (result i32)
  (local $byte-len i32) ;; the length of the string in bytes
  (local $cp-len i32) ;; the length of the string in code points
  (local $offset i32) ;; the bit offset within the current 32-bit word
  (local $word i32) ;; the current 32-bit word
  (local $byte i32) ;; the current byte

  ;; if (ptr == 0) {
  (if (i32.eqz (local.get $ptr))
    ;; trap
    (then unreachable)
  )
  ;; }

  ;; byte-len = *ptr
  (local.set $byte-len (i32.load (local.get $ptr)))
  ;; cp-len = 0
  (local.set $cp-len (i32.const 0))
  ;; offset = 0
  (local.set $offset (i32.const 0))

  ;; ptr = ptr += 4
  (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
  ;; word = *ptr
  (local.set $word (i32.load (local.get $ptr)))
  ;; while (byte-len > 0) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.le_s (local.get $byte-len) (i32.const 0)))
  ;;   byte = (word >> offset)
      (local.set $byte (i32.shr_u (local.get $word) (local.get $offset)))

  ;;   if ((byte & 0x80) == 0) {
      (if (i32.eqz (i32.and (local.get $byte) (i32.const 0x80)))
        (then
  ;;     // single byte
  ;;     offset += 8
          (local.set $offset (i32.add (local.get $offset) (i32.const 8)))
  ;;     byte-len -= 1
          (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 1)))
        )
        (else
  ;;   } else if ((byte & 0xE0) == 0xC0) {
          (if (i32.eq (i32.and (local.get $byte) (i32.const 0xE0)) (i32.const 0xC0))
            (then
  ;;     // double byte
  ;;     offset += 16
              (local.set $offset (i32.add (local.get $offset) (i32.const 16)))
  ;;     byte-len -= 2
              (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 2)))
            )
            (else
  ;;   } else if ((byte & 0xF0) == 0xE0) {
              (if (i32.eq (i32.and (local.get $byte) (i32.const 0xF0)) (i32.const 0xE0))
                (then
  ;;     // triple byte
  ;;     offset += 24
                  (local.set $offset (i32.add (local.get $offset) (i32.const 24)))
  ;;     byte-len -= 3
                  (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 3)))
                )
                (else
  ;;   } else if ((byte & 0xF8) == 0xF0) {
                  (if (i32.eq (i32.and (local.get $byte) (i32.const 0xF8)) (i32.const 0xF0))
                    (then
  ;;     offset += 32
                      (local.set $offset (i32.add (local.get $offset) (i32.const 32)))
  ;;     byte-len -= 4
                      (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 4)))
                    )
                    (else
  ;;   } else {
  ;;     trap
                      unreachable
  ;;   }
                    )
                  )
                )
              )
            )
          )
        )
      )

  ;;   cp-len++
      (local.set $cp-len (i32.add (local.get $cp-len) (i32.const 1)))

  ;;   if (offset >= 32) {
      (if (i32.ge_u (local.get $offset) (i32.const 32))
        (then
  ;;     ptr += 4
          (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
  ;;     word = *ptr
          (local.set $word (i32.load (local.get $ptr)))
  ;;     offset -= 32
          (local.set $offset (i32.sub (local.get $offset) (i32.const 32)))
        )
  ;;   }
      )
      
      (br $b_start)
    )
  ;; }
  )

  ;; return cp-len
  (return (local.get $cp-len))
)

(func $get-bytes (param $ptr i32) (param $end i32) (param $offset i32) (param $get-len i32) (result i32)
  (local $quad i64)
  ;; quad = *ptr
  ;; if (end - ptr > 4) {
  (if (i32.gt_s (i32.sub (local.get $end) (local.get $ptr)) (i32.const 4))
    (then
      ;; quad = (i64*)ptr
      (local.set $quad (i64.load (local.get $ptr)))
    )
  ;; } else {
    (else
      ;; quad = (i64)(i64*)ptr
      (local.set $quad (i64.extend_i32_u (i32.load (local.get $ptr))))
    )
  ;; }
  )

  ;; return (quad >> offset) & (-1 >> ((8-len) * 8))
  (return (i32.wrap_i64 (i64.and
    (i64.shr_u (local.get $quad) (i64.extend_i32_u (local.get $offset)))
    (i64.shr_u 
      (i64.const -1)
      (i64.extend_i32_u (i32.mul
        (i32.sub (i32.const 8) (local.get $get-len))
        (i32.const 8)
      ))
    )
  )))
)

(func $str-code-point-at (param $ptr i32) (param $at i32) (result i32)
  (local $byte-len i32) ;; the byte length of the string
  (local $cp-len i32)   ;; the length of the string in code points
  (local $offset i32)   ;; the bit offset within the current word
  (local $word i32)     ;; the current 32-bit word
  (local $byte i32)     ;; the current byte
  (local $char i32)     ;; the current encoded character
  (local $end i32)      ;; ptr to end of string
  ;; if (ptr == 0) {
  (if (i32.eqz (local.get $ptr))
    ;; trap
    (then unreachable)
  )
  ;; }

  ;; byte-len = *ptr
  (local.set $byte-len (i32.load (local.get $ptr)))
  ;; cp-len = 0
  (local.set $cp-len (i32.const 0))
  ;; offset = 0
  (local.set $offset (i32.const 0))

  ;; ptr = ptr += 4
  (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
  (local.set $end (i32.add (local.get $ptr) (local.get $byte-len)))
  ;; word = *ptr
  (local.set $word (i32.load (local.get $ptr)))
  ;; while (byte-len > 0) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.le_s (local.get $byte-len) (i32.const 0)))
      ;; byte = (word >> offset)
      (local.set $byte (i32.shr_u (local.get $word) (local.get $offset)))

      ;; if ((byte & 0x80) == 0) {
      (if (i32.eqz (i32.and (local.get $byte) (i32.const 0x80)))
        (then
          ;; single byte
          (if (i32.eq (local.get $cp-len) (local.get $at))
          ;;   return (byte & 0x7F)
            (return (i32.and (local.get $byte) (i32.const 0x7F)))
          ;; }
          )
          ;; offset += 8
          (local.set $offset (i32.add (local.get $offset) (i32.const 8)))
          ;; byte-len -= 1
          (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 1)))
          ;; if (cp-len == at) {
        )
        (else
          ;; } else if ((byte & 0xE0) == 0xC0) {
          (if (i32.eq (i32.and (local.get $byte) (i32.const 0xE0)) (i32.const 0xC0))
            (then
              ;; double byte
              ;; if (cp-len == at) {
              (if (i32.eq (local.get $cp-len) (local.get $at))
                (then
                  ;; char = get-bytes(ptr, end, offset, 2) 
                  (local.set $char (call $get-bytes
                    (local.get $ptr) 
                    (local.get $end) 
                    (local.get $offset) 
                    (i32.const 2)
                  ))
                  ;; 0b110xxxxx 0b10xxxxxx
                  ;; return ((char & 0x1f) << 6) | ((char & 0x3F00) >> 8)
                  (return (i32.or
                    (i32.shl (i32.and (local.get $char) (i32.const 0x1F)) (i32.const 6))
                    (i32.shr_u (i32.and (local.get $char) (i32.const 0x3F00)) (i32.const 8))
                  ))
                )
              ;; }
              )
              ;; offset += 16
              (local.set $offset (i32.add (local.get $offset) (i32.const 16)))
              ;; byte-len -= 2
              (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 2)))
            )
            (else
              ;; } else if ((byte & 0xF0) == 0xE0) {
              (if (i32.eq (i32.and (local.get $byte) (i32.const 0xF0)) (i32.const 0xE0))
                (then
                  ;; triple byte
                  ;; if (cp-len == at) {
                  (if (i32.eq (local.get $cp-len) (local.get $at))
                    (then
                      ;; char = get-bytes(ptr, end, offset, 3) 
                      (local.set $char (call $get-bytes
                        (local.get $ptr) 
                        (local.get $end) 
                        (local.get $offset) 
                        (i32.const 3)
                      ))
                      ;; 0b1110xxxx 0b10xxxxxx 0b10xxxxxx
                      ;; return ((char & 0xF) << 12) | ((char & 0x3f00) >> 2) | ((char & 0x3f0000) >> 16)
                      (return 
                        (i32.or 
                          (i32.or
                            (i32.shl (i32.and (local.get $char) (i32.const 0xF)) (i32.const 12))
                            (i32.shr_u (i32.and (local.get $char) (i32.const 0x3f00)) (i32.const 2)) ;; >> 8 then << 6
                          )
                          (i32.shr_u (i32.and (local.get $char) (i32.const 0x3f0000)) (i32.const 16))
                        )
                      )
                    )
                  ;; }
                  )
                  ;; offset += 24
                  (local.set $offset (i32.add (local.get $offset) (i32.const 24)))
                  ;; byte-len -= 3
                  (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 3)))
                )
                (else
                  ;; } else if ((byte & 0xF8) == 0xF0) {
                  (if (i32.eq (i32.and (local.get $byte) (i32.const 0xF8)) (i32.const 0xF0))
                    (then
                      ;; quad byte
                      ;; if (cp-len == at) {
                      (if (i32.eq (local.get $cp-len) (local.get $at))
                        (then
                          ;; char = get-bytes(ptr, end, offset, 4) 
                          (local.set $char (call $get-bytes
                            (local.get $ptr) 
                            (local.get $end) 
                            (local.get $offset) 
                            (i32.const 4)
                          ))
                          ;; 0b11110xxx 0b10xxxxxx 0b10xxxxxx 0b10xxxxxx
                          ;; return ((char & 0x7) << 18) | ((char & 0x3f00) << 4) | ((char & 0x3f0000) >> 10) | ((char & 0x3f000000) >> 24)
                          (return
                            (i32.or
                              (i32.or
                                (i32.or
                                  (i32.shl (i32.and (local.get $char) (i32.const 0x7)) (i32.const 18))
                                  (i32.shl (i32.and (local.get $char) (i32.const 0x3F00)) (i32.const 4)) ;; >> 8 then << 12
                                )
                                (i32.shr_u (i32.and (local.get $char) (i32.const 0x3f0000)) (i32.const 10)) ;; >> 16 then << 6
                              )
                              (i32.shr_u (i32.and (local.get $char) (i32.const 0x3f000000)) (i32.const 24))
                            )
                          )
                        )
                      ;; }
                      )
                      ;; offset += 32
                      (local.set $offset (i32.add (local.get $offset) (i32.const 32)))
                      ;; byte-len -= 4
                      (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 4)))
                    )
                    (else
                      ;; } else {
                      ;;   trap
                      unreachable
                      ;; }
                    )
                  )
                )
              )
            )
          )
        )
      )

      ;; cp-len++
      (local.set $cp-len (i32.add (local.get $cp-len) (i32.const 1)))

      ;; if (offset >= 32) {
      (if (i32.ge_u (local.get $offset) (i32.const 32))
        (then
          ;; ptr += 4
          (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
          ;; word = *ptr
          (local.set $word (i32.load (local.get $ptr)))
          ;; offset -= 32
          (local.set $offset (i32.sub (local.get $offset) (i32.const 32)))
        )
      ;; }
      )

      (br $b_start)
    )
  ;; }
  )

  ;; return 0
  (return (i32.const 0))
)

(func $str-is-valid (param $ptr i32) (result i32)
  (local $byte-len i32) ;; the byte length of the string
  (local $offset i32)   ;; the bit offset within the current word
  (local $word i32)     ;; the current 32-bit word
  (local $byte i32)     ;; the current byte
  (local $char i32)     ;; the current encoded character
  (local $end i32)      ;; ptr to the end of the string
  ;; if (ptr == 0) {
  (if (i32.eqz (local.get $ptr))
    ;; trap
    (then unreachable)
  )
  ;; }

  ;; byte-len = *ptr
  (local.set $byte-len (i32.load (local.get $ptr)))
  ;; offset = 0
  (local.set $offset (i32.const 0))

  ;; ptr = ptr += 4
  (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
  ;; end = ptr + byte-len
  (local.set $end (i32.add (local.get $ptr) (local.get $byte-len)))

  ;; word = *ptr
  (local.set $word (i32.load (local.get $ptr)))
  ;; while (byte-len > 0) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.le_s (local.get $byte-len) (i32.const 0)))

      ;; if (offset >= 32) {
      (if (i32.ge_u (local.get $offset) (i32.const 32))
        (then
          ;; ptr += 4
          (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
          ;; word = *ptr
          (local.set $word (i32.load (local.get $ptr)))
          ;; offset -= 32
          (local.set $offset (i32.sub (local.get $offset) (i32.const 32)))
        )
      ;; }
      )
      ;; byte = (word >> offset) & 0xFF
      (local.set $byte (i32.and (i32.shr_u (local.get $word) (local.get $offset)) (i32.const 0xff)))

      ;; if ((byte & 0x80) == 0) {
      (if (i32.eqz (i32.and (local.get $byte) (i32.const 0x80)))
        (then
         ;; single byte
         ;; no special validation needed
         ;; offset += 8
          (local.set $offset (i32.add (local.get $offset) (i32.const 8)))
          ;; byte-len -= 1
          (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 1)))
        )
        (else
          ;; } else if ((byte & 0xE0) == 0xC0) {
          (if (i32.eq (i32.and (local.get $byte) (i32.const 0xE0)) (i32.const 0xC0))
            (then
              ;; double byte
              ;; if (byte-len < 2) {
              (if (i32.lt_s (local.get $byte-len) (i32.const 2))
                (then
                  ;; return 0
                  (return (i32.const 0))
                )
              ;; }
              )
              ;; char = get-bytes(ptr, end, offset, 2) 
              (local.set $char (call $get-bytes 
                (local.get $ptr) 
                (local.get $end) 
                (local.get $offset) 
                (i32.const 2)
              ))
              ;; 0b110xxxxx 0b10xxxxxx
              ;; if ((char & 0xFFFFC0E0) != 0x80C0) {
              (if (i32.ne (i32.and (local.get $char) (i32.const 0xFFFFC0E0)) (i32.const 0x80C0))
                ;; return 0
                (return (i32.const 0))
              ;;}
              )
              ;; offset += 16
              (local.set $offset (i32.add (local.get $offset) (i32.const 16)))
              ;; byte-len -= 2
              (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 2)))
            )
            (else
              ;; } else if ((byte & 0xF0) == 0xE0) {
              (if (i32.eq (i32.and (local.get $byte) (i32.const 0xF0)) (i32.const 0xE0))
                (then
                  ;; triple byte
                  ;; if (byte-len < 3) {
                  (if (i32.lt_s (local.get $byte-len) (i32.const 3))
                    (then
                      ;; return 0
                      (return (i32.const 0))
                    )
                  ;; }
                  )
                  ;; char = get-bytes(ptr, end, offset, 3) 
                  (local.set $char (call $get-bytes 
                    (local.get $ptr) 
                    (local.get $end) 
                    (local.get $offset) 
                    (i32.const 3)
                  ))
                  ;; 0b1110xxxx 0b10xxxxxx 0b10xxxxxx
                  ;; if ((char & 0xFFC0C0F0) != 0x80l80E0) {
                  (if (i32.ne (i32.and (local.get $char) (i32.const 0xFFC0C0F0)) (i32.const 0x8080E0))
                    ;; return 0
                    (return (i32.const 0))
                  ;;}
                  )
                  ;; offset += 24
                  (local.set $offset (i32.add (local.get $offset) (i32.const 24)))
                  ;; byte-len -= 3
                  (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 3)))
                )
                (else
                  ;; } else if ((byte & 0xF8) == 0xF0) {
                  (if (i32.eq (i32.and (local.get $byte) (i32.const 0xF8)) (i32.const 0xF0))
                    (then
                      ;; quad byte
                      ;; if (byte-len < 4) {
                      (if (i32.lt_s (local.get $byte-len) (i32.const 4))
                        (then
                          ;; return 0
                          (return (i32.const 0))
                        )
                      ;; }
                      )
                      ;; char = get-bytes(ptr, end, offset, 4) 
                      (local.set $char (call $get-bytes 
                        (local.get $ptr) 
                        (local.get $end) 
                        (local.get $offset) 
                        (i32.const 4)
                      ))
                      ;; 0b11110xxx 0b10xxxxxx 0b10xxxxxx 0b10xxxxxx
                      ;; if ((char & 0xC0C0C0F8) != 0x808080F0) {
                      (if (i32.ne (i32.and (local.get $char) (i32.const 0xC0C0C0F8)) (i32.const 0x808080F0))
                        (then
                          ;; return 0
                          (return (i32.const 0))
                        )
                      ;; }
                      )
                      ;; top 5 bits should be lte 0x10
                      ;; if (((char & 0x7) << 2) | ((char & 0x3000)>>12)> 0x10)
                      (if (i32.gt_u
                            (i32.or 
                              (i32.shl (i32.and (local.get $char) (i32.const 0x7)) (i32.const 2))
                              (i32.shr_u (i32.and (local.get $char) (i32.const 0x3000)) (i32.const 12))
                            )
                            (i32.const 0x10)
                          )
                        (then
                          ;; return 0
                          (return (i32.const 0))
                        )
                      ;; }
                      )
                      
                      ;; offset += 32
                      (local.set $offset (i32.add (local.get $offset) (i32.const 32)))
                      ;; byte-len -= 4
                      (local.set $byte-len (i32.sub (local.get $byte-len) (i32.const 4)))
                    )
                    (else
                      ;; } else {
                      ;;   return 0
                      (return (i32.const 0))
                      ;; }
                    )
                  )
                )
              )
            )
          )
        )
      )

     (br $b_start)
    )
  ;; }
  )

  ;; if (byte-len < 0) {
  (if (i32.lt_s (local.get $byte-len) (i32.const 0))
    (then
      (return (i32.const 0))
    ;; return 0;
    )
  ;;}
  )

  ;; return 1
  (return (i32.const 1))
)