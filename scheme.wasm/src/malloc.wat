(global $malloc-hdr-offset i32 (i32.const 32))

;; malloc header size = 8

;; malloc free list entry
;;   next: i32 (ptr)
;;   size: i32 (size) -- size of the entry not including this header

%( malloc

;; these macros should only be accessible from within malloc.wat

(%define %malloc-store-list-l (%ptr %next %size)
  (i32.store (local.get %ptr) (local.get %next))
  (i32.store offset=4 (local.get %ptr) (local.get %size)))

(%define %malloc-store-bad-food-l (%ptr)
  (i32.store (local.get %ptr) (i32.const 0x0df0adab))
  (i32.store offset=4 (local.get %ptr) (i32.const 0x0df0adab)))

(%define %malloc-store-list (%ptr %next %size)
  (i32.store %ptr %next)
  (i32.store offset=4 %ptr %size))

(%define %malloc-load-list-l (%ptr %next %size)
  (local.set %next (i32.load (local.get %ptr)))
  (local.set %size (i32.load offset=4 (local.get %ptr))))

;; initializes malloc to point to all of available memory
(func $malloc-init
  (local $size i32)
  (local $first i32)

  ;; size = memory.size << 16
  (local.set $size (i32.shl (memory.size) (i32.const 16)))

  ;; first = malloc-hdr_offset + malloc-hdr_size
  (local.set $first (i32.add (global.get $malloc-hdr-offset) (i32.const 8)))

  ;; free list entry
  ;; malloc-store-list(first, 0, size - first - 8)
  (%malloc-store-list (local.get $first) (i32.const 0) (i32.sub
      (i32.sub
        (local.get $size)
        (local.get $first))
      (i32.const 8)))

  ;; set free pointer to first address after the header
  (i32.store (global.get $malloc-hdr-offset) (local.get $first))
  ;; set memory size (this is the total size of the memory in bytes)
  (i32.store offset=4 (global.get $malloc-hdr-offset) (local.get $size))
)

(func $malloc (param $size i32) (result i32)
  ;; previous free ptr
  (local $prev i32)
  ;; a current free ptr
  (local $free i32)
  ;; free entry next
  (local $f_next i32)
  ;; free entry size
  (local $f_size i32)
  (local $rem i32)
  (local $needed i32)
  (local $oldsize i32)
  (local $memsize i32)

  ;; prev = malloc-hdr_offset
  (local.set $prev (global.get $malloc-hdr-offset))
  ;; free = *malloc-hdr_offset
  (local.set $free (i32.load (local.get $prev)))

  ;; grow size to the next multiple of 8
  (local.set $size (i32.and (i32.add (local.get $size) (i32.const 7)) (i32.const 0xFFFFFFF8)))

  (block $bf_end
    (loop $bf_start
      ;; break if free == 0
      (br_if $bf_end (i32.eqz (local.get $free)))

      (%malloc-load-list-l $free $f_next $f_size)

      ;; if (f_size >= $size)
      (if (i32.ge_u (local.get $f_size) (local.get $size))
        (then
          ;; if (size + 32 >= f_size)
          (if (i32.ge_u (i32.add (local.get $size) (i32.const 32)) (local.get $f_size))
            (then
              ;; use entire block
              ;; *prev = f_next
              (i32.store (local.get $prev) (local.get $f_next))
              ;; *f_next = free
              (i32.store (local.get $free) (local.get $free))
            )
            (else
              ;; use partial block
              ;; use the end of the block,
              ;; rem = f_size - size - 8
              (local.set $rem (i32.sub (i32.sub (local.get $f_size) (local.get $size)) (i32.const 8)))

              ;; malloc-store-list(free, f_next, rem)
              (%malloc-store-list-l $free $f_next $rem)

              ;; free += rem + 8
              (local.set $free (i32.add (i32.add (local.get $free) (local.get $rem)) (i32.const 8)))
              ;; malloc-store-list(free, free, size)
              (%malloc-store-list-l $free $free $size)
            )
          )
          ;; return free + 8
          (return (i32.add (local.get $free) (i32.const 8)))
        )
      )

      ;; prev = first
      (local.set $prev (local.get $free))
      ;; free = f_next
      (local.set $free (local.get $f_next))
      (br $bf_start)
    )
  )

  ;; nothing big enough in the free list, so let's grow memory
  ;; needed = (size + 0xffff) >> 16
  (local.set $needed (i32.shr_u (i32.add (local.get $size) (i32.const 0xFFFF)) (i32.const 16)))
  ;; res = memory.grow(needed)
  (local.set $oldsize (memory.grow (local.get $needed)))
  ;; if (res < 0)
  (if (i32.lt_s (local.get $oldsize) (i32.const 0))
    ;; trap
    (then (unreachable))
  )

  ;; memsize = memory.size << 16
  (local.set $memsize (i32.shl (memory.size) (i32.const 16)))

  ;; convert from pages to address
  (local.set $oldsize (i32.shl (local.get $oldsize) (i32.const 16)))

  ;; new free list item (marked as an inuse item)
  ;; malloc-store-list(oldsize, oldsize, memsize - oldsize - 8)
  (%malloc-store-list
    (local.get $oldsize)
    (local.get $oldsize)
    (i32.sub (i32.sub (local.get $memsize) (local.get $oldsize)) (i32.const 8)))

  ;; give the new memory to malloc by 'free'ing it
  ;; *(malloc-hdr_offset + 4) = memsize
  (i32.store offset=4 (global.get $malloc-hdr-offset) (local.get $memsize))
  ;; malloc-free(oldsize + 8)
  (call $malloc-free (i32.add (local.get $oldsize) (i32.const 8)))

  ;; call malloc again now there is space
  (return (call $malloc (local.get $size)))
)

(func $malloc-free (param $ptr i32)
  (local $hdr i32)
  (local $h_next i32)
  (local $h_size i32)
  (local $fend i32)
  (local $prev i32)
  (local $curr i32)
  (local $c_next i32)
  (local $c_size i32)
  (local $n_next i32)
  (local $n_size i32)

  ;; if (ptr < malloc-hdr_offset + 8)
  (if (i32.lt_u (local.get $ptr) (i32.add (global.get $malloc-hdr-offset) (i32.const 8)))
    (then
      local.get $ptr
      global.get $malloc-hdr-offset
      (unreachable)
    )
  )

  ;; hdr = ptr - 8
  (local.set $hdr (i32.sub (local.get $ptr) (i32.const 8)))

  ;; h_next, h_size = malloc-load-list(hdr)
  (%malloc-load-list-l $hdr $h_next $h_size)
  ;; malloc-store-list(hdr, 0xbaadf00d, 0xbaadf00d)
  (%malloc-store-bad-food-l $hdr)

  ;; fend = ptr + h_size
  (local.set $fend (i32.add (local.get $ptr) (local.get $h_size)))

  ;; if (h_next != hdr)
  (if (i32.ne (local.get $h_next) (local.get $hdr))
    ;; trap
    (then
      local.get $h_next
      local.get $hdr
      (unreachable)
    )
  )

  ;; prev = malloc-hdr_offset
  (local.set $prev (global.get $malloc-hdr-offset))
  ;; curr = *prev
  (local.set $curr (i32.load (local.get $prev)))

  (block $w_end
    (loop $w_start
      ;; break if (curr == 0)
      (br_if $w_end (i32.eqz (local.get $curr)))

      ;; if (hdr == curr)
      (if (i32.eq (local.get $hdr) (local.get $curr))
        ;; trap, double free
        (then
          local.get $hdr
          local.get $curr
          (unreachable)
        )
      )

      ;; c_next, c_size = malloc-load-list(curr)
      (%malloc-load-list-l $curr $c_next $c_size)

      ;; if (hdr < curr)
      (if (i32.lt_u (local.get $hdr) (local.get $curr))
        (then
          ;; if (fend > curr)
          (if (i32.gt_u (local.get $fend) (local.get $curr))
            ;; trap, blocks overlap
            (then
              local.get $fend
              local.get $curr
              (unreachable)
            )
          )

          ;; if (fend == curr)
          (if (i32.eq (local.get $fend) (local.get $curr))
            (then
              ;; merge blocks
              ;; i.e. extend current block at front
              ;; malloc-store-list(hdr, c_next, h_size + c_size + 8)
              (%malloc-store-list
                (local.get $hdr)
                (local.get $c_next)
                (i32.add (i32.add (local.get $h_size) (local.get $c_size)) (i32.const 8))
              )
              ;; malloc-store-list(curr, ABADF00D, ABADF00D)
              (%malloc-store-bad-food-l $curr)

              ;; *prev = hdr
              (i32.store (local.get $prev) (local.get $hdr))
              (return)
            )
            (else
              ;; insert before block
              ;; malloc-store-list(hdr, curr, h_size)
              (%malloc-store-list-l $hdr $curr $h_size)
              ;; *prev = hdr
              (i32.store (local.get $prev) (local.get $hdr))
              (return)
            )
          )
        )
      )

      ;; if (curr + c_size + 8 == hdr)
      (if (i32.eq
          (i32.add (i32.add (local.get $curr) (local.get $c_size)) (i32.const 8))
          (local.get $hdr)
        )
        (then
          ;; the free block is immediately adjacent (after) to the current block

          ;; if (c_next != 0 && fend > c_next)
          (if (i32.ne (local.get $c_next) (i32.const 0))
            (then
              (if (i32.gt_u (local.get $fend) (local.get $c_next))
                ;; trap, blocks overlap
                (then
                  local.get $fend
                  local.get $c_next
                  (unreachable)
                )
              )
            )
          )

          ;; merge blocks
          ;; if (fend == c_next)
          (if (i32.eq (local.get $fend) (local.get $c_next)
            )
            (then
              ;; merge with current and next
              ;; n_next, n_size = malloc-load-list(c_next)
              (%malloc-load-list-l $c_next $n_next $n_size)
              ;; malloc-store-list(curr, n_next, csize + h_size + n_size + 16)
              (%malloc-store-list
                (local.get $curr)
                (local.get $n_next)
                (i32.add (i32.add (i32.add (local.get $c_size) (local.get $h_size)) (local.get $n_size)) (i32.const 16))
              )
              ;; return
              (return)
            )
            (else
              ;; merge with current block (i.e. extend current block)
              ;; malloc-store-list(curr, c_next, c_size + h_size + 8)
              (%malloc-store-list
                (local.get $curr)
                (local.get $c_next)
                (i32.add (i32.add (local.get $c_size) (local.get $h_size)) (i32.const 8)))
              ;; return
              (return)
            )
          )
        )
        (else
          ;; if (c_next == 0)
          (if (i32.eqz (local.get $c_next))
            (then
              ;; this is the last block, so make the free block the last block
              ;; malloc-store-list(hdr, c_next, h_size)
              (%malloc-store-list (local.get $hdr) (i32.const 0) (local.get $h_size))
              ;; malloc-store-list(curr, hdr, c_size)
              (%malloc-store-list-l $curr $hdr $c_size)

              (return)
            )
            ;; otherwise keep going to the next free block
          )
        )
      )

      ;; prev = curr
      (local.set $prev (local.get $curr))
      ;; curr = c_next
      (local.set $curr (local.get $c_next))
      (br $w_start)
    )
  )

  ;; trap
  (i32.const 42)
  (i32.const 42)
  (unreachable)
)

(func $malloc-zero (param $ptr i32) (param $size i32)
  (local $rounded i32)
  (local $end i32)
  (local $rem i32)
  (local $temp i64)

  ;; end = ptr + size
  (local.set $end (i32.add (local.get $ptr) (local.get $size)))
  ;; rounded = (ptr + 7) & 0xFFFFFFF8
  (local.set $rounded (i32.and
      (i32.add (local.get $ptr) (i32.const 7))
      (i32.const 0xFFFF_FFF8)))

  (block $p_end (loop $p_start
      ;; break if (ptr >= rounded || ptr >= end)
      (br_if $p_end (i32.ge_u (local.get $ptr) (local.get $rounded)))
      (br_if $p_end (i32.ge_u (local.get $ptr) (local.get $end)))

      ;; *ptr = 0
      (i32.store8 (local.get $ptr) (i32.const 0))

      ;; ptr++
      (%inc $ptr)
      (br $p_start)))

  (block $b_end (loop $b_start
      ;; break if ptr + 8 >= end
      (br_if $b_end (i32.ge_u
          (i32.add (local.get $ptr) (i32.const 8))
          (local.get $end)))

      (i64.store (local.get $ptr) (i64.const 0))

      ;; ptr += 8
      (%plus-eq $ptr 8)
      (br $b_start)))

  (block $s_end (loop $s_start
      ;; break if (ptr >= end)
      (br_if $s_end (i32.ge_u (local.get $ptr) (local.get $end)))

      ;; *ptr = 0
      (i32.store8 (local.get $ptr) (i32.const 0))

      ;; ptr++
      (%inc $ptr)
      (br $s_start))))

(func $calloc (param $nmemb i32) (param $size i32) (result i32)
  (local $ptr i32)

  ;; size *= nmemb
  (local.set $size (i32.mul (local.get $size) (local.get $nmemb)))

  ;; ptr = malloc(size)
  (local.set $ptr (call $malloc (local.get $size)))

  (call $malloc-zero (local.get $ptr) (local.get $size))

  (return (local.get $ptr))
)

(func $memcpy (param $dest i32) (param $src i32) (param $len i32)
  (block $b_end (loop $b_start
      (br_if $b_end (i32.lt_u (local.get $len) (i32.const 32)))

      (i64.store (local.get $dest) (i64.load (local.get $src)))
      (i64.store offset=8 (local.get $dest) (i64.load offset=8 (local.get $src)))
      (i64.store offset=16 (local.get $dest) (i64.load offset=16 (local.get $src)))
      (i64.store offset=24 (local.get $dest) (i64.load offset=24 (local.get $src)))

      (%plus-eq $dest 32)
      (%plus-eq $src 32)
      (%minus-eq $len 32)
      (br $b_start)))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.lt_u (local.get $len) (i32.const 4)))

      (i32.store (local.get $dest) (i32.load (local.get $src)))

      (%plus-eq $dest 4)
      (%plus-eq $src 4)
      (%minus-eq $len 4)
      (br $b_start)))

  (block $b_one
    (br_if $b_one (i32.eqz (local.get $len)))
    (i32.store8 (local.get $dest) (i32.load8_u (local.get $src)))
    (%dec $len)
    (br_if $b_one (i32.eqz (local.get $len)))
    (%inc $dest)
    (%inc $src)
    (i32.store8 (local.get $dest) (i32.load8_u (local.get $src)))
    (%dec $len)
    (br_if $b_one (i32.eqz (local.get $len)))
    (%inc $dest)
    (%inc $src)
    (i32.store8 (local.get $dest) (i32.load8_u (local.get $src))))
)

(func $memmove (param $dest i32) (param $src i32) (param $len i32)
  (local $src-ptr i32)
  (local $dest-ptr i32)

  ;; if the src is at a higher memory address than the destination, then
  ;; memcpy is safe
  (if (i32.gt_u (local.get $src) (local.get $dest)) (then
      (call $memcpy (local.get $dest) (local.get $src) (local.get $len))
      (return)))

  ;; if the src and dest don't overlap, then memcpy is safe
  (if (i32.le_u
      (i32.add (local.get $src) (local.get $len))
      (local.get $dest)) (then
      (call $memcpy (local.get $dest) (local.get $src) (local.get $len))
      (return)))

  ;; copy backwards...
  (local.set $src-ptr (i32.add (local.get $src) (local.get $len)))
  (local.set $dest-ptr (i32.add (local.get $dest) (local.get $len)))

  ;; TODO single byte copies until ptrs are aligned (obvi only if possible)

  (block $b_end (loop $b_start
      (br_if $b_end (i32.lt_u (local.get $len) (i32.const 32)))

      (%minus-eq $src-ptr 32)
      (%minus-eq $dest-ptr 32)

      (i64.store offset=24 (local.get $dest-ptr) (i64.load offset=24 (local.get $src-ptr)))
      (i64.store offset=16 (local.get $dest-ptr) (i64.load offset=16 (local.get $src-ptr)))
      (i64.store offset=8 (local.get $dest-ptr) (i64.load offset=8 (local.get $src-ptr)))
      (i64.store (local.get $dest-ptr) (i64.load (local.get $src-ptr)))

      (%minus-eq $len 32)
      (br $b_start)))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.lt_u (local.get $len) (i32.const 4)))

      (%minus-eq $dest-ptr 4)
      (%minus-eq $src-ptr 4)

      (i32.store (local.get $dest-ptr) (i32.load (local.get $src-ptr)))

      (%minus-eq $len 4)
      (br $b_start)))

  (block $b_one
    (br_if $b_one (i32.eqz (local.get $len)))
    (%dec $dest-ptr)
    (%dec $src-ptr)
    (i32.store8 (local.get $dest-ptr) (i32.load8_u (local.get $src-ptr)))
    (%dec $len)
    (br_if $b_one (i32.eqz (local.get $len)))
    (%dec $dest-ptr)
    (%dec $src-ptr)
    (i32.store8 (local.get $dest-ptr) (i32.load8_u (local.get $src-ptr)))
    (%dec $len)
    (br_if $b_one (i32.eqz (local.get $len)))
    (%dec $dest-ptr)
    (%dec $src-ptr)
    (i32.store8 (local.get $dest-ptr) (i32.load8_u (local.get $src-ptr)))))



(func $memset (param $dest i32) (param $byte i32) (param $len i32)
  (local $ptr i32)
  (local $word i32)

  (local.set $ptr (local.get $dest))
  (local.set $byte (i32.and (local.get $byte) (i32.const 0xFF)))

  (if (i32.eqz (i32.and (local.get $ptr) (i32.const 0x3))) (then
    ;; ptr is 32-bit aligned, set words at a time
    (local.set $word (i32.mul (i32.const 0x01010101) (local.get $byte)))

    (block $b_end (loop $b_start
        (br_if $b_end (i32.lt_u (local.get $len) (i32.const 4)))

        (i32.store (local.get $ptr) (local.get $word))

        (%plus-eq $ptr 4)
        (%minus-eq $len 4)
        (br $b_start)))))

  (block $b_end (loop $b_start
      (br_if $b_end (i32.eqz (local.get $len)))

      (i32.store8 (local.get $ptr) (local.get $byte))

      (%inc $ptr)
      (%dec $len)
      (br $b_start))))

)%
