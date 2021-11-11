;; defines the gc heap and the types that can be stored on it

;; HeapEntry
;;  type: i8      type enum
;;  gc-flags: i8  gc flags 0 -- white, 0x01 -- gray, 0x10 -- black 
;;  reserved: i16
;;  data: i64     type specific value


;; Type - enum
;;   empty = 0
;;   nil = 1
;;   boolean = 2
;;   cons = 3
;;   i64 = 4
;;   f64 = 5
;;   symbol = 6
;;   str = 7
;;   char = 8
;;   env = 9
;;   special = 10
;;   builtin = 11
;;   lambda = 12
;; kMaxType = 12

;;  Empty cell
;;    next-empty: i32 ptr

;;  Cons
;;    car: i32 ptr
;;    cdr: 132 ptr

;;  Symbol
;;    sym: i32 ptr

;;  String
;;    ptr: i32 ptr

;;  Char
;;    code-point: i32

;;  Env
;;    hashtable: i32
;;    parent: i32

;; Heap
;;   Size:      i32
;;   Free:      i32 ptr -- pointer to an empty cell
;;   next-heap: i32 ptr -- pointer to the next heap
;;   Entryies:  HeapEntry[Size]


(func $heap-create (param $size i32) (result i32)
  (local $heap-size i32)
  (local $heap-ptr i32)
  (local $entry-ptr i32)
  (local $next i32)

  ;; heap-size = size * 12 + 12
  (local.set $heap-size (i32.add (i32.mul (local.get $size) (i32.const 12)) (i32.const 12)))

  ;; heap-ptr = malloc(heap-size)
  (local.set $heap-ptr (call $malloc (local.get $heap-size)))

  ;; *heap-ptr = heap-size
  (i32.store (local.get $heap-ptr) (local.get $size))
  ;; heap-ptr[8] = 0
  (i32.store (i32.add (local.get $heap-ptr) (i32.const 8)) (i32.const 0))

  ;; set all entries to empty
  ;; entry-ptr = heap-ptr + 12
  (local.set $entry-ptr (i32.add (local.get $heap-ptr) (i32.const 12)))
  ;; set free ptr to first empty entry
  ;; heap-ptr[4] = entry-ptr
  (i32.store (i32.add (local.get $heap-ptr) (i32.const 4)) (local.get $entry-ptr))

  ;; next = entry-ptr + 12
  (local.set $next (i32.add (local.get $entry-ptr) (i32.const 12)))

  ;; while (true) {
  (loop $forever
    ;; *entry-ptr = 0 (type 0: empty)
    (i32.store (local.get $entry-ptr) (i32.const 0))

    ;; if (size == 1)
    (if (i32.eq (local.get $size) (i32.const 1))
      (then
        ;; last slot has an next-ptr of 0
        ;; *(entry-ptr + 4) = 0
        (i32.store (i32.add (local.get $entry-ptr) (i32.const 4)) (i32.const 0))
        ;; return heap-ptr
        (return (local.get $heap-ptr))
      )
      (else
        ;; *(entry-ptr + 4) = next
        (i32.store (i32.add (local.get $entry-ptr) (i32.const 4)) (local.get $next))
      )
    )

    ;; entry-ptr = next
    (local.set $entry-ptr (local.get $next))
    ;; next += 12
    (local.set $next (i32.add (local.get $next) (i32.const 12)))
    ;; size--;
    (local.set $size (i32.sub (local.get $size) (i32.const 1)))

    (br $forever)
  )
  ;; }

  ;; return heap-ptr
  (unreachable)
)

(func $heap-destroy (param $heap i32)
  (local $next-heap-ptr i32)
  (local $size i32)
  (local $entry-ptr i32)
  (local $type i32)

  ;; next-heap-ptr = heap[8]
  (local.set $next-heap-ptr (i32.load (i32.add (local.get $heap) (i32.const 8))))
  ;; if (next-heap-ptr != 0) {
  (if (local.get $next-heap-ptr)
    (then
      ;; heap-destroy(next-heap-ptr)
      (call $heap-destroy (local.get $next-heap-ptr))
    )
  ;; }
  )

  ;; size = heap[0]
  (local.set $size (i32.load (local.get $heap)))
  ;; entry-ptr = heap + 12
  (local.set $entry-ptr (i32.add (local.get $heap) (i32.const 12)))

  ;; while (size) {
  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $size)))

      ;; type = entry-ptr[0] & 0xF
      (local.set $type (i32.and (i32.load (local.get $entry-ptr)) (i32.const 0xF)))

      ;; if this is a string or a symbol
      ;; if (type == 6 || type == 7) {
      (block $b_str_or_sym_else
        (block $b_str_or_sym
          (if (i32.eq (local.get $type) (i32.const 6))
            (then (br $b_str_or_sym))
            (else
              (if (i32.eq (local.get $type) (i32.const 7))
                (then (br $b_str_or_sym))
                (else (br $b_str_or_sym_else))
              )
            )
          )
        )
        ;; free the underlying string
        ;; malloc-free(entry-ptr[4])
        (call $malloc-free (i32.load (i32.add (local.get $entry-ptr) (i32.const 4))))
        ;; }
      )

      ;; if this is an environment
      ;; if (type == 9) {
      (if (i32.eq (local.get $type) (i32.const 9))
        (then
          ;; destroy the environment (but not any outer environment)
          ;; environment-destroy(entry-ptr, false)
          (call $environment-destroy (local.get $entry-ptr) (i32.const 0))
          ;; free the environment hashtable
          ;; malloc-free(entry-ptr[4])
          (call $malloc-free (i32.load (i32.add (local.get $entry-ptr) (i32.const 4))))
        )
      )
      ;; }

      ;; entry-ptr += 12
      (local.set $entry-ptr (i32.add (local.get $entry-ptr) (i32.const 12)))
      ;; size--;
      (local.set $size (i32.sub (local.get $size) (i32.const 1)))
      (br $b_start)
    ;; }
    )
  )
  
  ;; malloc-free(heap)
  (call $malloc-free (local.get $heap))
)

(func $heap-alloc (param $heap i32) (param $type i32) (param $data1 i32) (param $data2 i32) (result i32)
  (local $empty-ptr i32)
  (local $next-heap-ptr i32)

  ;; empty-ptr = heap[4]
  (local.set $empty-ptr (i32.load (i32.add (local.get $heap) (i32.const 4))))
  ;; if (empty-ptr == 0) {
  (if (i32.eqz (local.get $empty-ptr))
    (then
      ;; no space in this heap, alloc in the next
      ;; next-heap-ptr = heap[8]
      (local.set $next-heap-ptr (i32.load (i32.add (local.get $heap) (i32.const 8))))

      ;; if (next-heap-ptr == 0) {
      (if (i32.eqz (local.get $next-heap-ptr))
        (then
          ;; there is no next heap, create a new one
          ;; heap[8] = next-heap-ptr = heap-create(heap[0])
          (i32.store
            (i32.add (local.get $heap) (i32.const 8))
            (local.tee $next-heap-ptr (call $heap-create (i32.load (local.get $heap))))
          )
        )
      ;; }
      )
      ;; return heap-alloc(next-heap-ptr, type, data1, data2);
      (return (call $heap-alloc
        (local.get $next-heap-ptr) 
        (local.get $type) 
        (local.get $data1) 
        (local.get $data2)
      ))
    )
  ;; }
  )

  ;; if (type == 0) {
  (if (i32.eqz (local.get $type))
    ;; cannot allocate an empty cell
    ;; trap
    (then unreachable)
  ;; }
  )
  ;; if (type > kMaxType(11) ) {
  (if (i32.gt_u (local.get $type) (i32.const 11))
    ;; trap
    (then unreachable)
  ;; }
  )

  ;; heap[4] = empty-ptr[4]
  (i32.store
    (i32.add (local.get $heap) (i32.const 4))
    (i32.load (i32.add (local.get $empty-ptr) (i32.const 4)))
  )
  ;; empty-ptr[0] = type
  (i32.store
    (local.get $empty-ptr)
    (local.get $type)
  )
  ;; empty-ptr[4] = data1
  (i32.store
    (i32.add (local.get $empty-ptr) (i32.const 4))
    (local.get $data1)
  )
  ;; empty-ptr[8] = data2
  (i32.store
    (i32.add (local.get $empty-ptr) (i32.const 8))
    (local.get $data2)
  )

  ;; return empty-ptr
  (return (local.get $empty-ptr))
)

(func $heap-free (param $heap i32) (param $ptr i32)
  (local $size i32)
  (local $next-ptr i32)

  ;; size = heap[0]
  (local.set $size (i32.load (local.get $heap)))
  ;; if (ptr >= heap + 12 && ptr <= heap + 12 + size * 12) {
  (block $b_if
    (br_if $b_if (i32.lt_u
      (local.get $ptr)
      (i32.add (local.get $heap) (i32.const 12))
    ))
    (br_if $b_if (i32.gt_u
      (local.get $ptr)
      (i32.add
        (i32.add
          (local.get $heap)
          (i32.const 12)
        )
        (i32.mul (local.get $size) (i32.const 12))
      )
    ))

    ;; ptr is within this heap
    ;; if ((ptr - heap) % 12) {
    (if (i32.rem_u (i32.sub (local.get $ptr) (local.get $heap)) (i32.const 12))
      ;; ptr doesn't point to a slot
      ;; trap
      (then unreachable)
      ;; }
    )

    ;; ptr[0] = 0 ;; mark as empty
    (i32.store (local.get $ptr) (i32.const 0))
    ;; ptr[4] = heap[4] ;; set next to the free ptr
    (i32.store
      (i32.add (local.get $ptr) (i32.const 4))
      (i32.load (i32.add (local.get $heap) (i32.const 4)))
    )
    ;; heap[4] = ptr ;; set free ptr to be this ptr
    (i32.store
      (i32.add (local.get $ptr) (i32.const 4))
      (local.get $ptr)
    )
  
    ;; return;
    (return)
  ;; }
  )

  ;; next-ptr = heap[8]
  (local.set $next-ptr (i32.load (i32.add (local.get $heap) (i32.const 8))))
  ;; if (next-ptr == 0) {
  (if (i32.eqz (local.get $next-ptr))
    ;; can't find a heap that owns this ptr
    ;; trap
    (then unreachable)
  ;; }
  )

  ;; heap-free(next-ptr, ptr)
  (call $heap-free (local.get $next-ptr) (local.get $ptr))
)
