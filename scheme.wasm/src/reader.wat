;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the reader can get the next token from an input stream
;; when a token is requested 
;;   it reads a character at a time,
;;     reading from the io module (using $io-read():i32) as necessary
;;   characters are accumulated in an input buffer until a space or delimiter is reached
;;     which is grown as necessary
;;   the token is put into a string and returned
;; The characters are stored in the accum buffer as unencoded code-points (1 word per code-point)


;; reader contents
;;   type name    offset  Description
;;   i32  input   0       The current input string
;;   i32  in-off  4       The current code-point offset in the input string
;;   i32  accum   8       The accumulation buffer
;;   i32  size    12      The size of the accumulation buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a new reader
(func $reader-init (result i32)
  (local $reader i32) ;; the reader pointer
  (local $accum i32)  ;; the accumulation buffer ptr

  ;; allocate a buffer for 16 characters
  ;; accum = malloc(16 * 4)
  (local.set $accum (call $malloc (i32.const 64))) 

  ;; reader = malloc(16)
  (local.set $reader (call $malloc (i32.const 16))) 

  ;; clear input and in-off to 0
  ;; reader[0] = 0
  (i64.store (local.get $reader) (i64.const 0))

  ;; reader[8] = accum
  (i32.store (i32.add (local.get $reader) (i32.const 8)) (local.get $accum))
  ;; reader[12] = 16
  (i32.store (i32.add (local.get $reader) (i32.const 12)) (i32.const 16))
  
  ;; return reader
  (return (local.get $reader))
)

;; free a reader
(func $reader-free (param $reader i32)
  (local $input i32)   ;; the input string
  (local $accum i32)   ;; the accum buffer

  ;; input = reader[0]
  (local.set $input (i32.load (local.get $reader)))
  ;; if (input != 0) malloc-free(input)
  (if (local.get $input)
    (then (call $malloc-free (local.get $input)))
  )

  ;; accum = reader[8]
  (local.set $accum (i32.load offset=8 (local.get $reader)))
  ;; malloc-free(accum)
  (call $malloc-free (local.get $accum))

  ;; malloc-free(reader)
  (call $malloc-free (local.get $reader))
)

(func $reader-read-token (param $reader i32) (result i32)
  (local $input i32)  ;; input string
  (local $in-off i32) ;; code point offset within input string
  (local $in-off-ptr i32) ;; code point offset ptr
  (local $char i32)   ;; code point of the current character
  (local $first i32)  ;; is this the first character
  (local $acc-off i32) ;; accumulator offset
  (local $accum i32)
  (local $size i32)

  ;; input = reader[0];
  (local.set $input (i32.load (local.get $reader)))
  ;; in-off-ptr = reader + 4
  (local.set $in-off-ptr (i32.add (local.get $reader) (i32.const 4)))
  ;; acuum = reader[8];
  (local.set $accum (i32.load offset=8 (local.get $reader)))
  ;; size = reader[12]
  (local.set $size (i32.load offset=12 (local.get $reader)))

  ;; if (input == 0) {
  (if (i32.eqz (local.get $input))
    (then
      ;; reader[0] = input = io-read();
      (i32.store
        (local.get $reader)
        (local.tee $input (call $io-read))
      )
      ;; if (input == 0) return 0;
      (if (i32.eqz (local.get $input))
        (then (return (i32.const 0)))
      )
      ;; *in-off-ptr = in-off = 0;
      (i32.store
        (local.get $in-off-ptr)
        (local.tee $in-off (i32.const 0))
      )
    )
    ;; } else {
    (else
      ;; in-off = *in-off-ptr
      (local.set $in-off (i32.load (local.get $in-off-ptr)))
    )
  ;; }
  )

  ;; first = 1
  (local.set $first (i32.const 1))
  ;; acc-off = 0
  (local.set $acc-off (i32.const 0))

  ;; while (true) {
  (loop $forever
    ;; char = str-code-point-at
    (local.set $char (call $str-code-point-at (local.get $input) (local.get $in-off)))
    ;; if (char == 0)
    (if (i32.eqz (local.get $char))
      ;; {
      (then
        ;; free existing input string
        ;; malloc-free(input)
        (call $malloc-free (local.get $input))

        ;; read next input string
        ;; reader[0] = input = io-read()
        (i32.store 
          (local.get $reader)
          (local.tee $input (call $io-read))
        )

        ;; if (input == 0) return 0;
        (if (i32.eqz (local.get $input))
          (then (return (i32.const 0)))
        )
        ;; $in-off-ptr = in-off = 0
        (i32.store
          (local.get $in-off-ptr)
          (local.tee $in-off (i32.const 0))
        )
        ;; Continue here rather than trying to read the first character,
        ;; just in case this is an empty string.
        ;; continue
        (br $forever)
      )
      ;; } else {
      (else
        ;; *in-off-ptr = in-off++
        (i32.store
          (local.get $in-off-ptr)
          (local.tee $in-off (i32.add (local.get $in-off) (i32.const 1)))
        )
      )
      ;; }
    )

    ;; if (first) {
    (if (local.get $first)
      (then
        ;; if this is the first character then skip whitespace, return for delimiters, accum for anything else
        ;; if (is-whitespace($char)) continue
        (br_if $forever (call $is-whitespace (local.get $char)))

        ;; if (is-delimiter($char)) {
        (if (call $is-delimiter (local.get $char))
          (then
            ;; return str-from-32(utf8-code-point-size($char), utf8-from-code-point($char)))
            (return
              (call $str-from-32
                (call $utf8-code-point-size (local.get $char))
                (call $utf8-from-code-point (local.get $char))
              )
            )
          )
        )
        ;; }

        ;; add to accumulator
        ;; assert(acc-off == 0)
        (if (local.get $acc-off) (then unreachable))

        ;; *accum = $char
        (i32.store (local.get $accum) (local.get $char))
        ;; acc-off = 1
        (local.set $acc-off (i32.const 1))
        ;; first = 0
        (local.set $first (i32.const 0))
      )
      ;; } else {
      (else
        ;; * whitespace ends the string
        ;; * a delimiter ends the token, and needs to be pushed back
        ;; anything else is accumulated

        ;; if (is-delimiter(char)) {
        (if (call $is-delimiter (local.get $char))
          (then
            ;; in-off-ptr = in-off = in-off - 1
            (i32.store
              (local.get $in-off-ptr)
              (local.tee $in-off (i32.sub (local.get $in-off) (i32.const 1)))
            )
          )
        ;; } else if (!is-whitespace(char)) {
          (else
            (if (i32.eqz (call $is-whitespace (local.get $char)))
              (then
                ;; if (acc-off >= size) {
                (if (i32.ge_u (local.get $acc-off) (local.get $size))
                  (then
                    ;; TODO grow accum'
                    unreachable
                  )
                ;; }
                )
                ;; accum[acc-off] = char
                (i32.store
                  (i32.add (local.get $accum) (i32.shl (local.get $acc-off) (i32.const 2)))
                  (local.get $char)
                )
                ;; acc-off++
                (%inc $acc-off)
                ;; continue
                (br $forever)
              )
            )
          )
        ;; }
        )
        ;; TODO end the string here.
        ;; return str-from-code-points(accum, acc-off)
        (return (call $str-from-code-points
          (local.get $accum)
          (local.get $acc-off)
        ))
      )
      ;; }
    )

    (br $forever)
  )
  ;; }

  ;; trap
  unreachable
)

(func $is-whitespace (param $char i32) (result i32)
  ;; char == ' ' 0x20
  (if (i32.eq (local.get $char) (i32.const 0x20))
    (then (return (i32.const 1)))
  )
  ;; char == '\t' 0x9
  (if (i32.eq (local.get $char) (i32.const 0x09))
    (then (return (i32.const 1)))
  )
  ;; char == '\n' 0xa
  (if (i32.eq (local.get $char) (i32.const 0x0a))
    (then (return (i32.const 1)))
  )
  ;; char == '\r' 0xd
  (if (i32.eq (local.get $char) (i32.const 0x0d))
    (then (return (i32.const 1)))
  )
  (return (i32.const 0))
)

(func $is-delimiter (param $char i32) (result i32)
  ;; char == '(' 0x28
  (if (i32.eq (local.get $char) (i32.const 0x28))
    (then (return (i32.const 1)))
  )
  ;; char == ')' 0x29
  (if (i32.eq (local.get $char) (i32.const 0x29))
    (then (return (i32.const 1)))
  )
  ;; char == '.' 0x2E
  (if (i32.eq (local.get $char) (i32.const 0x2E))
    (then (return (i32.const 1)))
  )
  ;; char == "'" 0x27
  (if (i32.eq (local.get $char) (i32.const 0x27))
    (then (return (i32.const 1)))
  )
  (return (i32.const 0))
)