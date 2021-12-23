(;
GC process

All objects are considered to start as unrooted 
the working set is initialized with the known rooted objects
  this includes:
    - root environment
while the working set is non-empty:
  an item is selected from the working set (the current item)
  for each reference in the current item:
    - IF the reference is rooted, OR in the working set THEN do nothing
    - ELSE IF the reference is a value type THEN mark it as rooted
    - ELSE add the reference to the working set
  the current item is marked as rooted and removed from the working set

;)

;; Indicates whether there is on ongoing collection. 
;; While this is true all new allocations must be added to the working set.
(global $g-gc-collecting? (mut i32) (i32.const 0))
;; The number of items to process from the working set on a single cycle
(global $g-gc-work-cycle-size (mut i32) (i32.const 1024))
;; The number of collections run
(global $g-gc-collection-count (mut i32) (i32.const 0))
;; The number of items collected ever
(global $g-gc-total-collected-count (mut i32) (i32.const 0))
;; The number of items not-collected ever
(global $g-gc-total-not-collected-count (mut i32) (i32.const 0))
;; The number of items collected in the last/current collection
(global $g-gc-collected-count (mut i32) (i32.const 0))
;; The number of items not-collected in the last/current collection
(global $g-gc-not-collected-count (mut i32) (i32.const 0))

;; The state of any ongoing collection, this is essentially the working set
(global $g-gc-state (mut i32) (i32.const 0))
(;
  GC state object

  head-ptr  i32 ptr
  head-idx  i32 
  tail-ptr  i32 ptr
  tail-idx  i32
  count     i32

  The working set is stored as a linked list of sub-arrays
  items added at the end, removed from the start with marching pointers

  Sub array structure
  next    i32 ptr   pointer to the next sub-array or 0 if no prev
  count   i32 val   the number of values stored in this sub-array
  data    i32[256]  the values in the sub-array

  sizeof sub-array = (256 + 2) * 4 = 0x102 * 4 = 0x408  (1032)
;)


(func $gc-run (param $gray-init i32)
  (local $ptr i32)
  (local $i i32)
  (local $curr i32)
  (local $type i32)

  ;; if (!g-gc-collecting?) {
  (if (i32.eqz (global.get $g-gc-collecting?))
    ;; gc-init(gray-init);
    (then (call $gc-init (local.get $gray-init)))
  ;; }
  )

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))
  ;; i = g-gc-work-cycle-size
  (local.set $i (global.get $g-gc-work-cycle-size))

  ;; while (true) {
  (loop $forever
    ;; if (ptr.count == 0) {
    ;; if (ptr[16] == 0) {
    (if (i32.eqz (i32.load offset=16 (local.get $ptr)))
      (then
        ;; malloc-free(ptr.head-ptr)
        ;; malloc-free(ptr[0])
        (call $malloc-free (i32.load offset=0 (local.get $ptr)))
        ;; malloc-free(ptr)
        (call $malloc-free (local.get $ptr))
        ;; g-gc-state = 0
        (global.set $g-gc-state (i32.const 0))
        ;; g-gc-collecting? = 0
        (global.set $g-gc-collecting? (i32.const 0))

        ;; gc-finalize(g-heap)
        (call $gc-finalize (global.get $g-heap))
        ;; return;
        (return)
        ;; }
      )
    )
    ;; if (i == 0) {
    (if (i32.eqz (local.get $i))
      ;; return
      (then (return))
      ;; }
    )

    ;; curr = gc-gray-dequeue()
    (local.set $curr (call $gc-gray-dequeue))
    ;; assert(curr)
    (if (i32.eqz (local.get $curr)) (then
      (call $print-symbol (global.get $g-gc-run))
      (call $print-integer 
        (i64.extend_i32_s (i32.load offset=16 (local.get $ptr)))
        (i32.const 10))
        (i32.store offset=16 (local.get $ptr) (i32.const 0))
        (br $forever)))
    (%assert (local.get $curr))
    ;; type = get-type(curr)
    (local.set $type (%get-type $curr))

    (block $b_switch
      ;; switch (type) {
      ;; case cons-type:
      (if (i32.eq (local.get $type) (%cons-type)) (then
          ;; gc-maybe-gray-enqueue(car(curr))
          (call $gc-maybe-gray-enqueue (%car-l $curr))
          ;; gc-maybe-gray-enqueue(cdr(curr))
          (call $gc-maybe-gray-enqueue (%cdr-l $curr))
          ;; break
          (br $b_switch)))

      ;; case lambda-type
      (if (i32.eq (local.get $type) (%lambda-type)) (then
          ;; gc-maybe-gray-enqueue(car(curr))
          (call $gc-maybe-gray-enqueue (%car-l $curr))
          ;; gc-maybe-gray-enqueue(cdr(curr))
          (call $gc-maybe-gray-enqueue (%cdr-l $curr))
          ;; break
          (br $b_switch)))

      ;; case env-type
      (if (i32.eq (local.get $type) (%env-type)) (then
          ;; gc-enqueue-env-items(env)
          (call $gc-enqueue-env-items (local.get $curr))
          ;; break
          (br $b_switch)))

      ;; case error-type
      (if (i32.eq (local.get $type) (%error-type)) (then
          ;; gc-maybe-gray-enqueue(car(curr))
          (call $gc-maybe-gray-enqueue (%car-l $curr))
          ;; gc-maybe-gray-enqueue(cdr(curr))
          (call $gc-maybe-gray-enqueue (%cdr-l $curr))
          (br $b_switch)))

      ;; case values-type
      (if (i32.eq (local.get $type) (%values-type)) (then
          ;; gc-maybe-gray-enqueue(car(curr))
          (call $gc-maybe-gray-enqueue (%car-l $curr))
          ;; gc-maybe-gray-enqueue(cdr(curr))
          (call $gc-maybe-gray-enqueue (%cdr-l $curr))
          (br $b_switch)))

      ;; case vector-type
      (if (i32.eq (local.get $type) (%vector-type)) (then
          (call $gc-gray-vector (local.get $curr))
          (br $b_switch)))

      (if (i32.eq (local.get $type) (%cont-type)) (then
          (call $gc-gray-continuation (%car-l $curr))
          (br $b_switch)))
          
      ;; }
    )

    ;; mark-black(curr)
    (call $gc-mark-black (local.get $curr) (local.get $type))

    ;; i--
    (%dec $i)
    ;; }
    (br $forever)
  )
  (unreachable)
)

(func $gc-gray-vector (param $vec i32)
  (local $ptr i32)
  (local $count i32)

  (local.set $ptr (%car-l $vec))
  (local.set $count (%cdr-l $vec))

  (loop $forever
    (if $b_end (i32.eqz (local.get $count))
      (then (return)))
    
    (call $gc-maybe-gray-enqueue (i32.load (local.get $ptr)))
    
    (%plus-eq $ptr 4)
    (%dec $count)
    (br $forever)))

(func $gc-gray-continuation (param $cont i32)
  (loop $forever
    ;; continuation env is gray
    (call $gc-maybe-gray-enqueue (i32.load offset=4 (local.get $cont)))
    ;; continuation args are gray
    (call $gc-maybe-gray-enqueue (i32.load offset=8 (local.get $cont)))

    ;; if there is a next continuation, process it also 
    (br_if $forever (local.tee $cont (i32.load offset=12 (local.get $cont))))))

(func $gc-finalize (param $heap i32)
  (local $size i32)
  (local $next i32)
  (local $ptr i32)
  (local $type i32)

  (loop $tail-call
    ;; size = heap.size
    ;; size = heap[0]
    (local.set $size (i32.load offset=0 (local.get $heap)))
    ;; next = heap.next
    ;; next = heap[8]
    (local.set $next (i32.load offset=8 (local.get $heap)))

    ;; ptr = heap.data
    ;; ptr = heap + 12
    (local.set $ptr (i32.add (local.get $heap) (i32.const 12)))

    ;; while (size) {
    (block $w_end
      (loop $w_start 
        (br_if $w_end (i32.eqz (local.get $size)))

        ;; type = get-type(ptr)
        (local.set $type (%get-type $ptr))

        (block $if
          ;; if (type != empty) {
          (br_if $if (i32.eqz (local.get $type)))
          ;; if (gc-is-black(ptr) || type == (%symbol-type)) {
          (block $if_b_or_s_done
            (block $if_b_or_s_else
              (block $if_b_or_s_then
                ;; if (gc-is-black(ptr) || 
                (br_if $if_b_or_s_then (call $gc-is-black (local.get $ptr)))
                ;; type == (%symbol-type)) {
                (br_if $if_b_or_s_then (i32.eq (local.get $type) (%symbol-type)))
                (br $if_b_or_s_else)
              )
              ;; gc-mark-white(ptr, type)
              (call $gc-mark-white (local.get $ptr) (local.get $type))
              (%ginc $g-gc-not-collected-count)
              (%ginc $g-gc-total-not-collected-count)
              (br $if_b_or_s_done)
            )
            ;; } else {
            ;; assert(gc-is-white(ptr))
            (if (i32.eqz (call $gc-is-white (local.get $ptr))) (then
              (call $print-symbol (global.get $g-newline))
              (call $print-symbol (global.get $g-error))
              (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 10))
              (call $print-symbol (global.get $g-space))
              (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 16))
              ;; oops, this should be black or white, but is gray
              ;; treat as black
              (br $if_b_or_s_done)))
            (%assert (call $gc-is-white (local.get $ptr)))
            ;; if (type == %str-type) {
            ;; (call $print (global.get $g-collect))
            ;; (call $print-integer (i64.extend_i32_u (local.get $type)))
            ;; (call $print (global.get $g-space))
            ;; (call $print (local.get $ptr))
            ;; (call $print (global.get $g-newline))
            (if (i32.eq (local.get $type) (%str-type))
              ;; malloc-free(car(ptr))
              (then (call $malloc-free (%car-l $ptr)))
            )
            ;; } else if (type == %env-type) {
            (if (i32.eq (local.get $type) (%env-type))
              ;; environment-destroy(ptr, 0)
              (then (call $environment-destroy (local.get $ptr) (i32.const 0)))
            ;; }
            )
            ;; } else if (type == %vector-type) {
            (if (i32.eq (local.get $type) (%vector-type))
              (then (call $malloc-free (%car-l $ptr)))
            )
            ;; }
            ;; heap-free(ptr)
            (call $heap-free (local.get $heap) (local.get $ptr))
            (%ginc $g-gc-collected-count)
            (%ginc $g-gc-total-collected-count)
            ;; }
          )
          ;; }
        )

        ;; ptr += 12
        (%plus-eq $ptr 12)
        ;; size--
        (%dec $size)

        (br $w_start)
      )
      ;; }
    )

    (if (i32.eqz (local.get $next))
      (then (return)))

    (local.set $heap (local.get $next))
    (br $tail-call)
  )
  (unreachable)
)

(func $gc-enqueue-env-items (param $env i32)
  (local $parent i32)
  (local $hashtable i32)
  (local $capacity i32)
  (local $slot i32)
  (local $digest i64)

  ;; parent = cdr(env)
  ;; if (parent) {
  (if (local.tee $parent (%cdr-l $env))
    (then
      ;; gc-maybe-gray-enqueue(parent)
      (call $gc-maybe-gray-enqueue (local.get $parent))
    )
  ;; }
  )

  ;; hashtable = car(env)
  (local.set $hashtable (%car-l $env))
  ;; capacity = hashtable.capacity
  ;; capacity = hashtable[0]
  (local.set $capacity (i32.load offset=0 (local.get $hashtable)))
  ;; slot = hashtable.data
  ;; slot = hashtable+8
  (local.set $slot (i32.add (local.get $hashtable) (i32.const 8)))
  ;; while (capacity) {
  (block $w_end
    (loop $w_start
      (br_if $w_end (i32.eqz (local.get $capacity)))
      ;; digest = slot.digest
      ;; digest = slot[0]
      (local.set $digest (i64.load offset=0 (local.get $slot)))
      ;; if (digest != 0 && ··· ) {
      (if (i64.ne (local.get $digest) (i64.const 0))
        (then
          ;; if ( ··· && digest != tombstone (-1)) {
          (if (i64.ne (local.get $digest) (i64.const -1))
            (then
              ;; gc-maybe-gray-enqueue(slot.data)
              ;; gc-maybe-gray-enqueue(slot[12])
              (call $gc-maybe-gray-enqueue 
                (i32.load offset=12 (local.get $slot))
              )
            )
          )
        )
      ;; }
      )

      ;; slot += 16
      (%plus-eq $slot 16)
      ;; capacity--
      (%dec $capacity)

      (br $w_start)
    )
    ;; }
  )
)

(func $gc-mark-gray (param $item i32) (param $type i32)
  (i32.store (local.get $item) (i32.or (local.get $type) (i32.const 0x100)))
)

(func $gc-is-white (param $item i32) (result i32)
  (local $color i32)
  (local.set $color (i32.and (i32.load (local.get $item)) (i32.const 0xF00)))
  (return (i32.eqz (local.get $color)))
)

(func $gc-is-gray (param $item i32) (result i32)
  (local $color i32)
  (local.set $color (i32.and (i32.load (local.get $item)) (i32.const 0xF00)))
  (return (i32.eq (local.get $color) (i32.const 0x100)))
)

(func $gc-is-black (param $item i32) (result i32)
  (local $color i32)
  (local.set $color (i32.and (i32.load (local.get $item)) (i32.const 0xF00)))
  (return (i32.eq (local.get $color) (i32.const 0x200)))
)


(func $gc-mark-black (param $item i32) (param $type i32)
  (i32.store (local.get $item) (i32.or (local.get $type) (i32.const 0x200)))
)

(func $gc-mark-white (param $item i32) (param $type i32)
  (i32.store (local.get $item) (i32.and (local.get $type) (i32.const 0xF)))
)

(func $gc-maybe-gray-enqueue (param $item i32)
  (local $type i32)

  ;; if (!is-white(item)) {
  (if (i32.eqz (call $gc-is-white (local.get $item))) (then
    (if (call $gc-is-gray (local.get $item)) (then
      (%assert (call $in-gray-queue? (local.get $item)))))
    ;; return
    (return)))

  ;; type = get-type(item)
  (local.set $type (%get-type $item))
  (block $b_if
    (block $b_else
      ;; if (type == cons-type || type == lambda-type || type == env-type) {
      (block $b_then
        (br_if $b_then (i32.eq (local.get $type) (%cons-type)))
        (br_if $b_then (i32.eq (local.get $type) (%lambda-type)))
        (br_if $b_then (i32.eq (local.get $type) (%env-type)))
        (br_if $b_then (i32.eq (local.get $type) (%error-type)))
        (br_if $b_then (i32.eq (local.get $type) (%values-type)))
        (br_if $b_then (i32.eq (local.get $type) (%vector-type)))
        (br_if $b_then (i32.eq (local.get $type) (%cont-type)))
        (br $b_else)
      )
      ;; mark-gray(item)
      (call $gc-mark-gray (local.get $item) (local.get $type))
      ;; gc-gray-enqueue(item)
      (call $gc-gray-enqueue (local.get $item))
      ;;
      (br $b_if)
    )
    ;; } else {
    ;; mark-black(item)
    (call $gc-mark-black (local.get $item) (local.get $type))
    ;; }
  )
)

(func $gc-init (param $gray-init i32)
  (local $array i32)

  ;; array = malloc(0x408)
  (local.set $array (call $malloc (i32.const 0x408)))
  ;; array.next = 0
  ;; array[0] = 0
  (i32.store offset=0 (local.get $array) (i32.const 0))
  ;; array.count = 1
  ;; array[4] = 1
  (i32.store offset=4 (local.get $array) (i32.const 1))
  ;; array.data[0] = env
  ;; array[8] = env
  (i32.store offset=8 (local.get $array) (local.get $gray-init))
  (call $gc-mark-gray (local.get $gray-init) (%get-type $gray-init))

  ;; g-gc-state = malloc(16)
  (global.set $g-gc-state (call $malloc (i32.const 20)))
  ;; g-gc-state.head-ptr = array
  (i32.store offset=0 (global.get $g-gc-state) (local.get $array))
  ;; g-gc-state.head-idx = 0
  (i32.store offset=4 (global.get $g-gc-state) (i32.const 0))
  ;; g-gc-state.tail-ptr = array
  (i32.store offset=8 (global.get $g-gc-state) (local.get $array))
  ;; g-gc-state.tail-idx = 1
  (i32.store offset=12 (global.get $g-gc-state) (i32.const 1))
  ;; g-gc-state.count = 1
  (i32.store offset=16 (global.get $g-gc-state) (i32.const 1))

  (call $gc-mark-black (global.get $g-nil) (%nil-type))
  (call $gc-mark-black (global.get $g-true) (%boolean-type))
  (call $gc-mark-black (global.get $g-false) (%boolean-type))
  (call $gc-maybe-gray-enqueue (i32.load offset=16 (global.get $g-reader)))
  (call $gc-maybe-gray-enqueue (i32.load offset=20 (global.get $g-reader)))
  (call $gc-maybe-gray-enqueue (global.get $g-char-env))

  ;; g-gc-collecting? = true
  (global.set $g-gc-collecting? (i32.const 1))
  (%ginc $g-gc-collection-count)
  (global.set $g-gc-collected-count (i32.const 0))
  (global.set $g-gc-not-collected-count (i32.const 0))
)

(func $gc-gray-dequeue (result i32)
  (local $ptr i32)
  (local $head-array i32)
  (local $idx i32)
  (local $val i32)
  (local $count i32)
  (local $next i32)

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))

  ;; head-array = ptr.head-ptr
  ;; head-array = ptr[0]
  (local.set $head-array (i32.load offset=0 (local.get $ptr)))
  ;; idx = ptr.head-idx
  ;; idx = ptr[4]
  (local.set $idx (i32.load offset=4 (local.get $ptr)))

  ;; if (idx == -1) {
  (if (i32.eq (local.get $idx) (i32.const -1))
    (then
      ;; // gray set is empty
      ;; assert(ptr.head-ptr == ptr.tail-ptr)
      ;; assert(ptr[0] == ptr[8])
      (%assert (i32.eq (local.get $head-array) (i32.load offset=8 (local.get $ptr))))
      ;; return 0
      (return (i32.const 0))
    )
    ;; }
  )

  ;; val = head-array.data[idx * 4]
  ;; val = head-array[8 + idx * 4]
  ;; val = head-array[8 + idx << 2]
  (local.set $val 
    (i32.load offset=8 
      (i32.add 
        (local.get $head-array)
        (%word-size-l $idx)
      )
    )
  )
  ;; count = head-array.count - 1
  ;; count = head-array[4] - 1
  (local.set $count 
    (i32.sub 
      (i32.load offset=4 (local.get $head-array)) 
      (i32.const 1)
    )
  )
  ;; if (count == 0) {
  (if (i32.eqz (local.get $count))
    (then
      ;; next = head-array.next
      ;; next = head-array[0]
      (local.set $next (i32.load (local.get $head-array)))
      ;; if (next) {
      (if (local.get $next)
        (then
          ;; ptr.head-ptr = next
          ;; ptr[0] = next
          (i32.store (local.get $ptr) (local.get $next))
          ;; ptr.head-idx = 0
          ;; ptr[4] = 0
          (i32.store offset=4 (local.get $ptr) (i32.const 0))
          ;; malloc-free(head-array)
          (call $malloc-free (local.get $head-array))
        )
        ;; } else {
        (else
          ;; assert(ptr.head-ptr == ptr.tail-ptr)
          ;; assert(ptr[0] == ptr[8])
          (%assert (i32.eq (i32.load (local.get $ptr)) (i32.load offset=8 (local.get $ptr))))
          ;; ptr.head-idx = -1
          ;; ptr[4] = -1
          (i32.store offset=4 (local.get $ptr) (i32.const -1))
          ;; ptr.tail-idx
          ;; ptr[12] = 0
          (i32.store offset=12 (local.get $ptr) (i32.const 0))
          ;; head-array.count = 0
          ;; head-array[4] = 0
          (i32.store offset=4 (local.get $head-array) (i32.const 0))
        )
        ;; }
      )
    )
    ;; } else {
    (else
      ;; ptr.head-idx = idx + 1
      ;; ptr[4] = idx + 1
      (i32.store offset=4 (local.get $ptr) (i32.add (local.get $idx) (i32.const 1)))
      ;; head-array.count = count
      ;; head-array[4] = count
      (i32.store offset=4 (local.get $head-array) (local.get $count))
    )
    ;; }
  )

  ;; ptr.count--
  ;; ptr[16] = ptr[16] - 1
  (i32.store
    offset=16 (local.get $ptr)
    (i32.sub
      (i32.load offset=16 (local.get $ptr))
      (i32.const 1)
    )
  )
  ;; return val
  (return (local.get $val))
)

(func $gc-gray-enqueue (param $val i32)
  (local $ptr i32)
  (local $tail-array i32)
  (local $idx i32)
  (local $next i32)

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))
  ;; tail-array = ptr.tail-ptr
  ;; tail-array = ptr[8]
  (local.set $tail-array (i32.load offset=8 (local.get $ptr)))
  ;; idx = ptr.tail-ptr
  ;; idx = ptr[12]
  (local.set $idx (i32.load offset=12 (local.get $ptr)))

  ;; tail-array.data[idx * 4] = val
  ;; tail-array[8 + idx * 4] = val
  ;; tail-array[8 + idx << 2] = val
  (i32.store offset=8 
    (i32.add 
      (local.get $tail-array)
      (%word-size-l $idx)
    )
    (local.get $val)
  )
  ;; ptr.count++
  ;; ptr[16] = ptr[16] + 1
  (i32.store
    offset=16 (local.get $ptr)
    (i32.add
      (i32.load offset=16 (local.get $ptr))
      (i32.const 1)
    )
  )
  ;; idx++
  (%inc $idx)
  ;; if (idx < 255) {
  (if (i32.lt_u (local.get $idx) (i32.const 255))
    (then
      ;; tail-array.count = tail-array.count + 1
      ;; tail-array[4] = tail-array[4] + 1;
      (i32.store offset=4
        (local.get $tail-array)
        (i32.add
          (i32.load offset=4 (local.get $tail-array))
          (i32.const 1)
        )
      )
      ;; ptr.tail-idx = idx
      ;; ptr[12] = idx
      (i32.store offset=12 (local.get $ptr) (local.get $idx))
      ;; if (ptr.head-idx == -1)
      (if (i32.eq (i32.load offset=4 (local.get $ptr)) (i32.const -1)) (then
        ;; grey queue was completely empty, so need to set the head-idx to point
        ;; here
        (i32.store offset=4 
          (local.get $ptr) 
          (i32.sub (local.get $idx) (i32.const 1)))))
    )
    ;; } else {
    (else
      ;; assert(tail-array.next == 0)
      ;; assert(tail-array[0] == 0)
      (%assert (i32.eqz (i32.load (local.get $tail-array))))
      ;; next = malloc(0x408)
      (local.set $next (call $malloc (i32.const 0x408)))
      ;; next.next =0
      ;; next[0] = 0
      (i32.store offset=0 (local.get $next) (i32.const 0))
      ;; next.count = 0
      ;; next[4] = 0
      (i32.store offset=4 (local.get $next) (i32.const 0))
      ;; tail-arry.next = next
      ;; tail-array[0] = next;
      (i32.store offset=0 (local.get $tail-array) (local.get $next))
      ;; ptr.tail-ptr = next
      ;; ptr[8] = next
      (i32.store offset=8 (local.get $ptr) (local.get $next))
      ;; ptr.tail-idx = 0
      ;; ptr[12] = 0
      (i32.store offset=12 (local.get $ptr) (i32.const 0))
    )
    ;; }
  )
)

(func $in-gray-queue? (param $val i32) (result i32)
  (local $ptr i32)
  (local $array i32)
  (local $idx i32)
  (local $tail-array i32)
  (local $tail-idx i32)
  (local $array-count i32)

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))
  ;; array = ptr.head-ptr
  ;; tail-array = ptr[0]
  (local.set $array (i32.load offset=0 (local.get $ptr)))
  ;; idx = ptr.head-idx
  ;; idx = ptr[4]
  (local.set $idx (i32.load offset=4 (local.get $ptr)))
  ;; array = ptr.head-ptr
  ;; tail-array = ptr[0]
  (local.set $tail-array (i32.load offset=8 (local.get $ptr)))
  ;; idx = ptr.head-idx
  ;; idx = ptr[4]
  (local.set $tail-idx (i32.load offset=12 (local.get $ptr)))

  (block $end (loop $start
      (local.set $array-count (i32.load offset=4 (local.get $array)))

      (block $inner_end (loop $inner_start
          (br_if $inner_end (i32.eqz (local.get $array-count)))
          (%assert (i32.le_u (local.get $idx) (i32.const 256)))

          (if (i32.eq
              (local.get $val) 
              (i32.load offset=8 (i32.add 
                  (local.get $array)
                  (%word-size-l $idx)))) (then
              (return (i32.const 1))))

          (%dec $array-count)
          (%inc $idx)
          (br $inner_start)))


      (local.set $array (i32.load (local.get $array)))
      (br_if $end (i32.eqz (local.get $array)))
      (local.set $idx (i32.const 0))

      (br $start)))

  (return (i32.const 0)))