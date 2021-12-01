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
;;   type name        offset  Description
;;   i32  input       0       The current input string
;;   i32  in-off      4       The current code-point offset in the input string
;;   i32  accum       8       The accumulation buffer
;;   i32  size        12      The size of the accumulation buffer
;;   i32  cache-read  16      The head of the read cache
;;   i32  cache-write 20      The tail of the write cache
;;
;;   SIZE             24

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a new reader
(func $reader-init (result i32)
  (local $reader i32) ;; the reader pointer
  (local $accum i32)  ;; the accumulation buffer ptr

  ;; allocate a buffer for 16 characters
  ;; accum = malloc(16 * 4)
  (local.set $accum (call $malloc (i32.const 64))) 

  ;; reader = malloc(24)
  (local.set $reader (call $malloc (i32.const 24))) 

  ;; clear input and in-off to 0
  ;; reader[0] = 0
  (i64.store (local.get $reader) (i64.const 0))

  ;; reader[8] = accum
  (i32.store offset=8 (local.get $reader) (local.get $accum))
  ;; reader[12] = 16
  (i32.store offset=12 (local.get $reader) (i32.const 16))

  ;; clear read and write caches
  (i32.store offset=16 (local.get $reader) (global.get $g-nil))
  (i32.store offset=20 (local.get $reader) (global.get $g-nil))
  
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
  (local $input i32)      ;; input string
  (local $in-off i32)     ;; code point offset within input string
  (local $char i32)       ;; code point of the current character
  (local $first i32)      ;; is this the first character
  (local $is-string i32)  ;; is this a string
  (local $acc-off i32)    ;; accumulator offset
  (local $accum i32)
  (local $size i32)
  (local $escaping i32)
  (local $hex-escape i32)
  (local $hex-value i32)
  (local $ws-escape i32)
  (local $digit i32)

  ;; input = reader[0];
  (local.set $input (i32.load (local.get $reader)))
  ;; acuum = reader[8];
  (local.set $accum (i32.load offset=8 (local.get $reader)))
  ;; size = reader[12]
  (local.set $size (i32.load offset=12 (local.get $reader)))

  ;; if (input == 0) {
  (if (i32.eqz (local.get $input))
    (then
      (local.set $input (call $primitive-read (local.get $reader)))

      ;; if (input == 0) return 0;
      (if (i32.eqz (local.get $input))
        (then (return (i32.const 0)))
      )
    )
    ;; }
  )
  ;; in-off = reader.in-off;
  (local.set $in-off (i32.load offset=4 (local.get $reader)))

  ;; first = 1
  (local.set $first (i32.const 1))
  ;; acc-off = 0
  (local.set $acc-off (i32.const 0))

  ;; while (true) {
  (loop $forever
    ;; char = str-code-point-at
    (local.set $char (call $str-code-point-at (local.get $input) (local.get $in-off)))
    ;; if (char == 1)
    (if (i32.eq (local.get $char) (i32.const -1))
      ;; {
      (then
        (local.set $input (call $primitive-read (local.get $reader)))

        ;; if (input == 0) return 0;
        (if (i32.eqz (local.get $input))
          (then (return (i32.const 0)))
        )
        ;; in_off = reader.in_off
        (local.set $in-off (i32.load offset=4 (local.get $reader)))
        ;; Continue here rather than trying to read the first character,
        ;; just in case this is an empty string.
        ;; continue
        (br $forever)
      )
      ;; } else {
      (else
        ;; reader.in-off = in-off++
        (i32.store
          offset=4
          (local.get $reader)
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

        (if (i32.eq (local.get $char) (i32.const 0x22)) ;; quote
          (then
            (local.set $is-string (i32.const 1))
            (local.set $escaping (i32.const 0))
            (local.set $hex-escape (i32.const 0))
            (local.set $hex-value (i32.const 0))
            (local.set $ws-escape (i32.const 0))

            ;; start token with 'str '
            (i32.store (local.get $accum) (i32.const 0x73))
            (i32.store offset=4 (local.get $accum) (i32.const 0x74))
            (i32.store offset=8 (local.get $accum) (i32.const 0x72))
            (i32.store offset=12 (local.get $accum) (i32.const 0x20))
            (local.set $acc-off (i32.const 4))
          )
          (else
            ;; *accum = $char
            (i32.store (local.get $accum) (local.get $char))
            ;; acc-off = 1
            (local.set $acc-off (i32.const 1))
            (local.set $is-string (i32.const 0))
          )
        )

        ;; first = 0
        (local.set $first (i32.const 0))
        (br $forever)
      )
    )

    ;; if (acc-off >= size) {
    (if (i32.ge_u (local.get $acc-off) (local.get $size))
      (then
        ;; TODO grow accum'
        (local.set $accum (call $reader-grow-accum (local.get $reader)))
        (local.set $size (i32.load offset=12 (local.get $reader)))
      )
    ;; }
    )

    ;; if (is-string)
    (if (local.get $is-string)
      (then
        (if (local.get $escaping)
          (then
            (if (local.get $ws-escape)
              (then
                (if (i32.eq (local.get $char) (i32.const 0x0A))
                  (then
                    (local.set $escaping (i32.const 0))
                    (local.set $ws-escape (i32.const 0))
                  )
                )
                (br $forever)
              )
            )
            (if (local.get $hex-escape)
              (then
                (if (i32.eq (local.get $char) (i32.const 0x3b)) ;; ';'
                  (then
                    (i32.store 
                      (i32.add (local.get $accum) (%word-size-l $acc-off))
                      (local.get $hex-value)
                    )
                    (%inc $acc-off)
                    (local.set $hex-escape (i32.const 0))
                    (local.set $escaping (i32.const 0))
                    (br $forever)
                  )
                )

                (block $b_digit
                  ;; check if digit is in range 0x30-0x39
                  (local.set $digit (i32.sub (local.get $char) (i32.const 0x30)))
                  (br_if $b_digit (i32.lt_u (local.get $digit) (i32.const 10)))
                  ;; check if digit is in range 0x41-46 A-F
                  (%minus-eq $digit 0x11)
                  (if (i32.lt_u (local.get $digit) (i32.const 6))
                    (then
                      (%plus-eq $digit 10)
                      (br $b_digit) 
                    )
                  )
                  ;; check if digit is in range 0x61-66 a-f
                  (%minus-eq $digit 0x20)
                  (if (i32.lt_u (local.get $digit) (i32.const 6))
                    (then
                      (%plus-eq $digit 10)
                      (br $b_digit) 
                    )
                  )

                  ;; not a hex digit
                  (i32.store 
                    (i32.add (local.get $accum) (%word-size-l $acc-off))
                    (i32.const 0xFFFD)
                  )
                  (%inc $acc-off)
                  (local.set $hex-escape (i32.const 0))
                  (local.set $escaping (i32.const 0))
                  (br $forever)
                )
                ;; hex-value = hex-value << 4 + digit
                (local.set $hex-value
                  (i32.add
                    (i32.shl (local.get $hex-value) (i32.const 4))
                    (local.get $digit)
                  )
                )
                (br $forever)
              )
            )


            (block $b_switch
              (if (i32.eq (local.get $char) (i32.const 0x61)) ;; '\a'
                (then
                  (local.set $char (i32.const 0x07)) ;; alarm
                  (br $b_switch)
                )
              )
              (if (i32.eq (local.get $char) (i32.const 0x62)) ;; '\b'
                (then
                  (local.set $char (i32.const 0x07)) ;; backspace
                  (br $b_switch)
                )
              )
              (if (i32.eq (local.get $char) (i32.const 0x74)) ;; '\t'
                (then
                  (local.set $char (i32.const 0x09)) ;; tab
                  (br $b_switch)
                )
              )
              (if (i32.eq (local.get $char) (i32.const 0x6e)) ;; '\n'
                (then
                  (local.set $char (i32.const 0x0A)) ;; linefeed
                  (br $b_switch)
                )
              )
              (if (i32.eq (local.get $char) (i32.const 0x72)) ;; '\r'
                (then
                  (local.set $char (i32.const 0x0D)) ;; carriage return
                  (br $b_switch)
                )
              )
              (br_if $b_switch (i32.eq (local.get $char) (i32.const 0x22))) ;; '\"'
              (br_if $b_switch (i32.eq (local.get $char) (i32.const 0x5c))) ;; '\\'
              (br_if $b_switch (i32.eq (local.get $char) (i32.const 0x7c))) ;; '\|'
              (if (i32.eq (local.get $char) (i32.const 0x78)) ;; '\x'
                (then
                  (local.set $hex-escape (i32.const 1))
                  (local.set $hex-value (i32.const 0))
                  (br $forever)
                )
              )
              (if (call $is-whitespace (local.get $char))
                (then
                  (if (i32.eq (local.get $char) (i32.const 0x0A))
                    (then
                      (local.set $escaping (i32.const 0))
                      (local.set $ws-escape (i32.const 0))
                    )
                    (else
                      (local.set $ws-escape (i32.const 1))
                    )
                  )
                  (br $forever)
                )
              )
            )

            (i32.store 
              (i32.add (local.get $accum) (%word-size-l $acc-off))
              (local.get $char)
            )
            (%inc $acc-off)
            (local.set $escaping (i32.const 0))
            (br $forever)
          )
          (else
            ;; if (char == '"')
            (if (i32.eq (local.get $char) (i32.const 0x22))
              (then
                ;; end string
                (return (call $str-from-code-points
                  (local.get $accum)
                  (local.get $acc-off)
                ))
              )
            )
            ;; if (char == '\')
            (if (i32.eq (local.get $char) (i32.const 0x5c)) ;; \
              (then
                ;; escape
                (local.set $escaping (i32.const 1))
                (br $forever)
              )
            )
            (i32.store 
              (i32.add (local.get $accum) (%word-size-l $acc-off))
              (local.get $char)
            )
            (%inc $acc-off)
            (br $forever)
          )
        )
      )
      ;; } else {
      (else
        ;; * whitespace ends the string
        ;; * a delimiter ends the token, and needs to be pushed back
        ;; anything else is accumulated

        (block $b_done
          ;; if this is open-paren, check 
          ;;    if accum buffer is ['#'], then this is a vector start
          ;;    if accum buffer is ['#', 'u', '8'], then this is a bytevector start
          (if (i32.eq (local.get $char) (i32.const 0x28)) ;; open-paren 0x28
            (then
              ;; checking for #()
              (block $b_v
                (br_if $b_v (i32.ne (local.get $acc-off) (i32.const 1)))
                (br_if $b_v (i32.ne (i32.load (local.get $accum)) (i32.const 0x23))) ;; # 0x23

                (return (call $str-from-32 (i32.const 2) (i32.const 0x2823))))

              ;; checking #u8(
              (block $b_bv
                (br_if $b_bv (i32.ne (local.get $acc-off) (i32.const 3)))
                (br_if $b_bv (i32.ne (i32.load offset=0 (local.get $accum)) (i32.const 0x23))) ;; # 0x23
                (br_if $b_bv (i32.ne (i32.load offset=4 (local.get $accum)) (i32.const 0x75))) ;; u 0x75
                (br_if $b_bv (i32.ne (i32.load offset=8 (local.get $accum)) (i32.const 0x38))) ;; 8 0x38

                (return (call $str-from-32 (i32.const 4) (i32.const 0x28387523))))
            )
          )

          ;; if (is-delimiter(char)) {
          (if (call $is-delimiter (local.get $char))
            (then
              ;; reader.in_off= in-off = in-off - 1
              (i32.store
                offset=4
                (local.get $reader)
                (local.tee $in-off (i32.sub (local.get $in-off) (i32.const 1)))
              )
              (br $b_done)
            )
          )

          ;; } else if (!is-whitespace(char)) {
          (if (i32.eqz (call $is-whitespace (local.get $char)))
            (then
              ;; accum[acc-off] = char
              (i32.store
                (i32.add (local.get $accum) (%word-size-l $acc-off))
                (local.get $char)
              )
              ;; acc-off++
              (%inc $acc-off)
              ;; continue
              (br $forever)
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

(func $reader-grow-accum (param $reader i32) (result i32)
  (local $old-accum i32)
  (local $old-size i32)
  (local $new-accum i32)
  (local $new-size i32)
  (local $src-ptr i32)
  (local $dest-ptr i32)

  (local.set $src-ptr (local.tee $old-accum (i32.load offset=8 (local.get $reader))))
  (local.set $old-size (i32.load offset=12 (local.get $reader)))

  (local.set $new-size (i32.shl (local.get $old-size) (i32.const 1)))
  (local.set $dest-ptr (local.tee $new-accum (call $malloc (%word-size-l $new-size))))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $old-size)))

      (i32.store 
        (local.get $dest-ptr) 
        (i32.load (local.get $src-ptr))
      )

      (%plus-eq $src-ptr 4)
      (%plus-eq $dest-ptr 4)
      (%dec $old-size)
      (br $b_start)
    )
  )

  (i32.store offset=8 (local.get $reader) (local.get $new-accum))
  (i32.store offset=12 (local.get $reader) (local.get $new-size))
  (return (local.get $new-accum))
)

(func $primitive-read (param $reader i32) (result i32)
  (local $input i32)
  (local $cache-read i32)
  (local $cache-head i32)
  (local $cache-write i32)

  ;; input = reader.input
  (local.set $input (i32.load (local.get $reader)))
  ;; if (input) {
  (if (local.get $input)
    (then
      ;; malloc-free(input)
      (call $malloc-free (local.get $input))
      ;; reader.input = 0
      (i32.store (local.get $reader) (i32.const 0))
    )
  ;; }
  )

  ;; reader.in-off = 0
  (i32.store offset=4 (local.get $reader) (i32.const 0))

  ;; cache-read = reader.cache-read
  (local.set $cache-read (i32.load offset=16 (local.get $reader)))
  ;; if (get-type(cache-read) == cons-type) {
  (if (i32.eq (%get-type $cache-read) (%cons-type))
    (then
      ;; there are data on the read cache. So pop the top element
      ;; cache-head = car(cache-read)
      (local.set $cache-head (%car-l $cache-read))
      ;; assert(cache-head is string)
      (%assert-str $cache-head)
      ;; reader.cache-read = cdr(cache-read)
      (i32.store offset=16 (local.get $reader) (%cdr-l $cache-read))
      ;; input = car(cache-head)
      (local.set $input (%car-l $cache-head))
      ;; heap-free(g-heap, cache-head)
      (call $heap-free (global.get $g-heap) (local.get $cache-head))
    )
    ;; } else {
    (else
      ;; input = io-read()
      (local.set $input (call $io-read))
    )
    ;; }
  )

  (if (local.get $input)
    (then
      ;; cache-write = reader.cache-write
      (local.set $cache-write (i32.load offset=20 (local.get $reader)))
      ;; reader.cache-write = cons(heap-alloc(g-heap, str-type, str-dup(input), 0), cache-write)
      (i32.store
        offset=20
        (local.get $reader)
        (%alloc-cons
          (%alloc-str (call $str-dup (local.get $input)))
          (local.get $cache-write)
        )
      )
    )
  )

  ;; reader.input = input 
  (i32.store (local.get $reader) (local.get $input))
  ;; return input
  (return (local.get $input))
)

(func $reader-commit (param $reader i32)
  ;; reader.cache-write = g-nil
  (i32.store offset=20 (local.get $reader) (global.get $g-nil))
)

(func $reader-rollback (param $reader i32)
  (local $reversed i32)
  (local $temp i32)
  (local $tail i32)
  (local $head i32)

  ;; reversed = g-nil
  (local.set $reversed (global.get $g-nil))

  ;; head = reader.cache-write 
  (local.set $head (i32.load offset=20 (local.get $reader)))
  ;; while (get-type(head) != nil-type) {
  (block $w_end
    (loop $w_start
      (br_if $w_end (i32.eq (%get-type $head) (%nil-type)))
      ;; temp = head
      (local.set $temp (local.get $head))
      ;; assert(temp is cons)
      (%assert-cons $temp)
      ;; head = cdr(temp)
      (local.set $head (%cdr-l $temp))
      ;; set-cdr!(temp, reversed)
      (%set-cdr!-l $temp $reversed)
      ;; reversed = temp
      (local.set $reversed (local.get $temp))

      (br $w_start)
    )
    ;; }
  )
  ;; reader.cache-write = g-nil
  (i32.store offset=16 (local.get $reader) (global.get $g-nil))

  ;; tail = reader.cache-read
  (local.set $tail (i32.load offset=16 (local.get $reader)))
  ;; if (get-type(tail) == nil-type) {
  (if (i32.eq (%get-type $tail) (%nil-type))
    (then
      ;; reader.cache-read = reversed
      (i32.store offset=16 (local.get $reader) (local.get $reversed))
    )
    ;; } else {
    (else
      ;; while (true) {
      (loop $forever
        ;; assert(tail is cons)
        (%assert-cons $tail)
        ;; temp = cdr(tail)
        (local.set $temp (%cdr-l $tail))
        ;; if (get-type(temp) == nil-type) {
        (if (i32.eq (%get-type $temp) (%nil-type))
          (then
            ;; set-cdr!(tail, reversed)
            (%set-cdr!-l $tail $reversed)
            ;; return;
            (return)
          )
          ;; } else {
          (else
            ;; tail = temp
            (local.set $tail (local.get $temp))
          )
          ;; }
        )

        (br $forever)
      )
      ;; }
    )
    ;; }
  )
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
  ;; char == "'" 0x27
  (if (i32.eq (local.get $char) (i32.const 0x27))
    (then (return (i32.const 1)))
  )
  (return (i32.const 0))
)