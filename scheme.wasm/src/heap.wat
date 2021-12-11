;; defines the gc heap and the types that can be stored on it

;; HeapEntry
;;  type: i8        type enum
;;  gc-flags: i8    gc flags  flag | color | during         | after 
;;                            -----+-------+----------------+---------------
;;                            0b00 | white | unvisited      |  can be freed
;;                            0b01 | gray  | to be checked  |  error
;;                            0b10 | black | in-use         |  must be kept 
;;  reserved: i16
;;  data: i64       type specific value


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
;;   error = 13
;;   values = 14
;;   vector = 15
;;   bytevector = 16
;;   cont(inuation) = 17 
;;   big-int = 18
;; kMaxType = 18

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

;; Lambda
;;    env : i32
;;    cons (formals, body): i32

;; Error
;;    symbol: i32
;;    data: i32

;;  Values
;;    car: i32 ptr
;;    cdr: 132 ptr

;;  Vector
;;    car: i32 ptr
;;    cdr: i32 count

;;  Bytevector
;;    car: i32 ptr
;;    cdr: i32 len

;;  Continuation
;;    car: i32 ptr

;;  BigInt
;;    car; i32 ptr

;; Heap
;;   Size:      i32
;;   Free:      i32 ptr -- pointer to an empty cell
;;   next-heap: i32 ptr -- pointer to the next heap
;;   entries:   HeapEntry[Size]


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
    (%plus-eq $next 12)
    ;; size--;
    (%dec $size)

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
  (local.set $next-heap-ptr (i32.load offset=8 (local.get $heap)))
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
      (local.set $type (%get-type $entry-ptr))

      ;; switch (type)
      (block $b_switch

        ;; if this is a string or a symbol or a big-ing
        ;; case %symbol-type:
        ;; case %string-type:
        ;; case %big-int-type:
        (block $b_str_or_sym_else
          (block $b_str_or_sym
            (if (i32.eq (local.get $type) (%symbol-type))
              (then (br $b_str_or_sym)))
            (if (i32.eq (local.get $type) (%str-type))
              (then (br $b_str_or_sym))
            (if (i32.eq (local.get $type) (%big-int-type))
              (then (br $b_str_or_sym))
            (br $b_str_or_sym_else))))
          ;; free the underlying string
          ;; malloc-free(entry-ptr[4])
          (call $malloc-free (%car-l $entry-ptr))
          ;; }
          (br $b_switch))

        ;; if this is an environment
        ;; if (type == 9) {
        (if (i32.eq (local.get $type) (%env-type))
          (then
            ;; destroy the environment (but not any outer environment)
            ;; environment-destroy(entry-ptr, false)
            (call $environment-destroy (local.get $entry-ptr) (i32.const 0))
            ;; free the environment hashtable
            ;; malloc-free(entry-ptr[4])
            (call $malloc-free (i32.load offset=4 (local.get $entry-ptr)))
            (br $b_switch))))
        ;; }


      ;; entry-ptr += 12
      (%plus-eq $entry-ptr 12)
      ;; size--;
      (%dec $size)
      (br $b_start)))
  
  ;; malloc-free(heap)
  (call $malloc-free (local.get $heap)))

(func $heap-alloc (param $heap i32) (param $type i32) (param $data1 i32) (param $data2 i32) (result i32)
  (local $empty-ptr i32)
  (local $next-heap-ptr i32)
  (local $interned i32)

  ;; if (type == symbol-type) {
  (if (i32.eq (local.get $type) (%symbol-type))
    (then
      ;; interned = get-interned-symbol(data1)
      ;; if (interned) {
      ;; if (interned = get-interned-symbol(data1)) {
      (if (local.tee $interned (call $get-interned-symbol (local.get $data1)))
        (then
          ;; free the string which is no longer needed
          (call $malloc-free (local.get $data1))
          ;; malloc-free(data1)
          ;; return interned
          ;; (if (global.get $g-interned-str)
          ;;   (then
          ;;     (call $print (global.get $g-interned-str))
          ;;     (call $print (local.get $interned))
          ;;     (call $print (global.get $g-newline))))
          (return (local.get $interned))))))

  ;; empty-ptr = heap[4]
  (local.set $empty-ptr (i32.load offset=4 (local.get $heap)))
  ;; if (empty-ptr == 0) {
  (if (i32.eqz (local.get $empty-ptr))
    (then
      ;; no space in this heap, alloc in the next
      ;; next-heap-ptr = heap[8]
      (local.set $next-heap-ptr (i32.load offset=8 (local.get $heap)))

      ;; if (next-heap-ptr == 0) {
      (if (i32.eqz (local.get $next-heap-ptr))
        (then
          ;; there is no next heap, create a new one
          ;; heap[8] = next-heap-ptr = heap-create(heap[0])
          (i32.store
            (i32.add (local.get $heap) (i32.const 8))
            (local.tee $next-heap-ptr (call $heap-create (i32.load (local.get $heap)))))))


      ;; return heap-alloc(next-heap-ptr, type, data1, data2);
      (return (call $heap-alloc
        (local.get $next-heap-ptr) 
        (local.get $type) 
        (local.get $data1) 
        (local.get $data2)))))

  ;; if (type == 0) {
  (if (i32.eqz (local.get $type))
    ;; cannot allocate an empty cell
    ;; trap
    (then unreachable))

  ;; if (type > kMaxType) ) {
  (if (i32.gt_u (local.get $type) (%max-heap-type))
    ;; trap
    (then unreachable))

  ;; heap[4] = empty-ptr[4]
  (i32.store offset=4
    (local.get $heap)
    (i32.load offset=4 (local.get $empty-ptr)))
  ;; empty-ptr[0] = type
  (i32.store
    (local.get $empty-ptr)
    (local.get $type))
  ;; empty-ptr[4] = data1
  (i32.store offset=4
    (local.get $empty-ptr)
    (local.get $data1))
  ;; empty-ptr[8] = data2
  (i32.store offset=8
    (local.get $empty-ptr)
    (local.get $data2))

  ;; if (type == symbol-type) {
  (if (i32.eq (local.get $type) (%symbol-type))
    ;; intern-symbol(data1, empty-ptr)
    (then (call $intern-symbol (local.get $data1) (local.get $empty-ptr))))

  (if (global.get $g-gc-collecting?)
    ;; items allocated while gc is collecting are considered gray
    (then (call $gc-maybe-gray-enqueue (local.get $empty-ptr))))

  ;; return empty-ptr
  (return (local.get $empty-ptr)))

(func $get-interned-symbol (param $str i32) (result i32)
  (return
    (call $hashtable-get
      (global.get $g-interned)
      (local.get $str)
    )
  )
)

(func $intern-symbol (param $str i32) (param $heap-ptr i32)
  (local $new-interned i32)

  (local.set $new-interned
    (call $hashtable-add
      (global.get $g-interned)
      (local.get $str)
      (local.get $heap-ptr)
    )
  )
  (if (i32.ne (local.get $new-interned) (global.get $g-interned))
    (global.set $g-interned (local.get $new-interned))
  )
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
      (i32.load offset=4 (local.get $heap))
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
  (local.set $next-ptr (i32.load offset=8 (local.get $heap)))
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
