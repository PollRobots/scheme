(global $malloc-hdr-offset i32 (i32.const 32))

;; malloc header size = 8

;; malloc free list entry
;;   next: i32 (ptr)
;;   size: i32 (size) -- size of the entry not including this header 

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
  (call $malloc-store-list (local.get $first) (i32.const 0) (i32.sub (i32.sub (local.get $size) (local.get $first)) (i32.const 8)))

  ;; set free pointer to first address after the header
  (i32.store (global.get $malloc-hdr-offset) (local.get $first))
  ;; set memory size (this is the total size of the memory in bytes)
  (i32.store (i32.add (global.get $malloc-hdr-offset) (i32.const 4)) (local.get $size))
)

(func $malloc-store-list (param $ptr i32) (param $next i32) (param $size i32)
  (i32.store (local.get $ptr) (local.get $next))
  (i32.store (i32.add (local.get $ptr) (i32.const 4)) (local.get $size))
)

(func $malloc-load-list (param $ptr i32) (result i64)
  (return (i64.or
    (i64.shl (i64.extend_i32_u (i32.load (local.get $ptr))) (i64.const 32)) ;; next
    (i64.extend_i32_u (i32.load offset=4 (local.get $ptr))) ;; size
  ))
)

(func $malloc (export "malloc") (param $size i32) (result i32)
  ;; previous free ptr
  (local $prev i32)
  ;; a current free ptr
  (local $free i32)
  (local $entry i64)
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

      (local.set $entry (call $malloc-load-list (local.get $free)))
      (local.set $f_next (i32.wrap_i64 (i64.shr_u (local.get $entry) (i64.const 32))))
      (local.set $f_size (i32.wrap_i64 (local.get $entry)))

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
              (call $malloc-store-list (local.get $free) (local.get $f_next) (local.get $rem))

              ;; free += rem + 8
              (local.set $free (i32.add (i32.add (local.get $free) (local.get $rem)) (i32.const 8)))
              ;; malloc-store-list(free, free, size)
              (call $malloc-store-list (local.get $free) (local.get $free) (local.get $size))
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
    (then unreachable)
  )

  ;; memsize = memory.size << 16
  (local.set $memsize (i32.shl (memory.size) (i32.const 16)))

  ;; new free list item (marked as an inuse item)
  ;; malloc-store-list(oldsize, oldsize, memsize - oldsize - 8)
  (call $malloc-store-list
    (local.get $oldsize)
    (local.get $oldsize)
    (i32.sub (i32.sub (local.get $memsize) (local.get $oldsize)) (i32.const 8))
  )

  ;; give the new memory to malloc by 'free'ing it
  ;; *(malloc-hdr_offset + 4) = memsize
  (i32.store (i32.add (global.get $malloc-hdr-offset) (i32.const 4)) (local.get $memsize))
  ;; malloc-free(oldsize + 8)
  (call $malloc-free (i32.add (local.get $oldsize) (i32.const 8)))

  ;; call malloc again now there is space
  (return (call $malloc (local.get $size)))
)

(func $malloc-free (export "free") (param $ptr i32)
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
  (local $entry i64)

  ;; if (ptr < malloc-hdr_offset + 8)
  (if (i32.lt_u (local.get $ptr) (i32.add (global.get $malloc-hdr-offset) (i32.const 8)))
    (then 
      local.get $ptr
      global.get $malloc-hdr-offset
      unreachable
    )
  )

  ;; hdr = ptr - 8
  (local.set $hdr (i32.sub (local.get $ptr) (i32.const 8)))

  ;; h_next, h_size = malloc-load-list(hdr)
  (local.set $entry (call $malloc-load-list (local.get $hdr)))
  (local.set $h_next (i32.wrap_i64 (i64.shr_u (local.get $entry) (i64.const 32))))
  (local.set $h_size (i32.wrap_i64 (local.get $entry)))
  ;; malloc-store-list(hdr, 0xbaadf00d, 0xbaadf00d)
  (call $malloc-store-list (local.get $hdr) (i32.const 0x0df0adba) (i32.const 0x0df0adba))

  ;; fend = ptr + h_size
  (local.set $fend (i32.add (local.get $ptr) (local.get $h_size)))

  ;; if (h_next != hdr)
  (if (i32.ne (local.get $h_next) (local.get $hdr))
    ;; trap
    (then 
      local.get $h_next
      local.get $hdr
      unreachable
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
          unreachable
        )
      )

      ;; c_next, c_size = malloc-load-list(curr)
      (local.set $entry (call $malloc-load-list (local.get $curr)))
      (local.set $c_next (i32.wrap_i64 (i64.shr_u (local.get $entry) (i64.const 32))))
      (local.set $c_size (i32.wrap_i64 (local.get $entry)))

      ;; if (hdr < curr)
      (if (i32.lt_u (local.get $hdr) (local.get $curr))
        (then
          ;; if (fend > curr)
          (if (i32.gt_u (local.get $fend) (local.get $curr))
            ;; trap, blocks overlap
            (then 
              local.get $fend
              local.get $curr
              unreachable
            )
          )

          ;; if (fend == curr) 
          (if (i32.eq (local.get $fend) (local.get $curr))
            (then
              ;; merge blocks
              ;; i.e. extend current block at front
              ;; malloc-store-list(hdr, c_next, h_size + c_size + 8)
              (call $malloc-store-list
                (local.get $hdr) 
                (local.get $c_next) 
                (i32.add (i32.add (local.get $h_size) (local.get $c_size)) (i32.const 8))
              )
              ;; malloc-store-list(curr, BAADF00D, BAADF00D)
              (call $malloc-store-list
                (local.get $curr)
                (i32.const 0x0df0adba)
                (i32.const 0x0df0adba)
              )
              ;; *prev = hdr
              (i32.store (local.get $prev) (local.get $hdr))
              (return)
            )
            (else
              ;; insert before block
              ;; malloc-store-list(hdr, curr, h_size)
              (call $malloc-store-list (local.get $hdr) (local.get $curr) (local.get $h_size))
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
                  unreachable
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
              (local.set $entry (call $malloc-load-list (local.get $c_next)))
              (local.set $n_next (i32.wrap_i64 (i64.shr_u (local.get $entry) (i64.const 32))))
              (local.set $n_size (i32.wrap_i64 (local.get $entry)))
              ;; malloc-store-list(curr, n_next, csize + h_size + n_size + 16)
              (call $malloc-store-list
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
              (call $malloc-store-list
                (local.get $curr)
                (local.get $c_next)
                (i32.add (i32.add (local.get $c_size) (local.get $h_size)) (i32.const 8))
              )
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
              (call $malloc-store-list (local.get $hdr) (i32.const 0) (local.get $h_size))
              ;; malloc-store-list(curr, hdr, c_size)
              (call $malloc-store-list (local.get $curr) (local.get $hdr) (local.get $c_size))

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
  unreachable
)

(func $malloc-zero (param $ptr i32) (param $size i32)
  (local $rounded i32)
  (local $end i32)
  (local $rem i32)
  (local $temp i64)

  ;; end = ptr + size
  (local.set $end (i32.add (local.get $ptr) (local.get $size)))
  ;; rounded = (ptr + 7) & 0xFFFFFFF8
  (local.set $rounded (i32.and (i32.add (local.get $ptr) (i32.const 7)) (i32.const 0xFFFFFFF8)))

  (block $p_end
    (loop $p_start
      ;; break if (ptr >= rounded || ptr >= end)
      (br_if $p_end (i32.ge_u (local.get $ptr) (local.get $rounded)))
      (br_if $p_end (i32.ge_u (local.get $ptr) (local.get $end)))

      ;; *ptr = 0
      (i32.store8 (local.get $ptr) (i32.const 0))

      ;; ptr++
      (%inc $ptr)
      (br $p_start)
    )
  )

  (block $b_end
    (loop $b_start
      ;; break if ptr + 8 >= end
      (br_if $b_end (i32.ge_u (i32.add (local.get $ptr) (i32.const 8)) (local.get $end)))

      (i64.store (local.get $ptr) (i64.const 0))

      ;; ptr += 8
      (%plus-eq $ptr 8)
      (br $b_start)
    )
  )

  (block $s_end
    (loop $s_start
      ;; break if (ptr >= end)
      (br_if $s_end (i32.ge_u (local.get $ptr) (local.get $end)))

      ;; *ptr = 0
      (i32.store8 (local.get $ptr) (i32.const 0))

      ;; ptr++
      (%inc $ptr)
      (br $s_start)
    )
  )
)

(func $calloc (param $nmemb i32) (param $size i32) (result i32)
  (local $ptr i32)

  ;; size *= nmemb
  (local.set $size (i32.mul (local.get $size) (local.get $nmemb)))

  ;; ptr = malloc(size)
  (local.get $ptr (call $malloc (local.get $size)))

  (call $malloc-zero (local.get $ptr) (local.get $size))

  (return (local.get $ptr))
)