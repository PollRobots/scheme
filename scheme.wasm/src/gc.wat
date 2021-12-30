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
(global $g-gc-work-cycle-size (mut i32) (i32.const 0x1_0000))
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

;; The number of heap slabs created, used to guide how large/frequent gc's 
;; should be.
(global $g-gc-heap-slabs (mut i32) (i32.const 0))

;; The state of any ongoing collection, this is essentially the working set
(global $g-gc-state (mut i32) (i32.const 0))
(;
  GC state object

  head-ptr  i32 ptr
  count     i32

  The working set is stored as a linked list of sub-arrays
  items used as a stack

  Sub array structure
  next    i32 ptr   pointer to the next sub-array or 0 if no prev
  count   i32 val   the number of values stored in this sub-array
  data    i32[256]  the values in the sub-array

  sizeof sub-array = (256 + 2) * 4 = 0x102 * 4 = 0x408  (1032)
;)


(func $gc-run (param $touched-init i32)
  (local $ptr i32)
  (local $i i32)
  (local $curr i32)
  (local $type i32)

  ;; if (!g-gc-collecting?) {
  (if (i32.eqz (global.get $g-gc-collecting?)) (then
    (if (global.get $g-gc-heap-slabs) (then
      ;; Decrement the number of heap-slabs, while memory is growing collections
      ;; are more frequent.
      (%gdec $g-gc-heap-slabs)))
    ;; gc-init(touched-init);
    (call $gc-init (local.get $touched-init))))

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))
  ;; i = g-gc-work-cycle-size
  (local.set $i (global.get $g-gc-work-cycle-size))

  ;; while (true) {
  (loop $forever
    ;; if (ptr.count == 0) {
    ;; if (ptr[4] == 0) {
    (if (i32.eqz (i32.load offset=4 (local.get $ptr)))
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

    ;; curr = gc-touched-pop()
    (local.set $curr (call $gc-touched-pop))
    ;; assert(curr)
    (if (i32.eqz (local.get $curr)) (then
      (call $print-symbol (global.get $g-gc-run))
      (call $print-symbol (global.get $g-space))
      (call $print-integer 
        (i64.extend_i32_s (i32.load offset=16 (local.get $ptr)))
        (i32.const 10))
      (call $print-symbol (global.get $g-newline))
      (i32.store offset=16 (local.get $ptr) (i32.const 0))
      (br $forever)))
    (%assert (local.get $curr))
    ;; type = get-type(curr)
    (local.set $type (%get-type $curr))

    (block $b_switch
      ;; switch (type) {
      ;; case cons-type:
      (if (i32.eq (local.get $type) (%cons-type)) (then
          ;; gc-maybe-touched-push(car(curr))
          (call $gc-maybe-touched-push (%car-l $curr))
          ;; gc-maybe-touched-push(cdr(curr))
          (call $gc-maybe-touched-push (%cdr-l $curr))
          ;; break
          (br $b_switch)))

      ;; case lambda-type
      (if (i32.eq (local.get $type) (%lambda-type)) (then
          ;; gc-maybe-touched-push(car(curr))
          (call $gc-maybe-touched-push (%car-l $curr))
          ;; gc-maybe-touched-push(cdr(curr))
          (call $gc-maybe-touched-push (%cdr-l $curr))
          ;; break
          (br $b_switch)))

      ;; case env-type
      (if (i32.eq (local.get $type) (%env-type)) (then
          ;; gc-push-env-items(env)
          (call $gc-push-env-items (local.get $curr))
          ;; break
          (br $b_switch)))

      ;; case error-type
      (if (i32.eq (local.get $type) (%error-type)) (then
          ;; gc-maybe-touched-push(car(curr))
          (call $gc-maybe-touched-push (%car-l $curr))
          ;; gc-maybe-touched-push(cdr(curr))
          (call $gc-maybe-touched-push (%cdr-l $curr))
          (br $b_switch)))

      ;; case values-type
      (if (i32.eq (local.get $type) (%values-type)) (then
          ;; gc-maybe-touched-push(car(curr))
          (call $gc-maybe-touched-push (%car-l $curr))
          ;; gc-maybe-touched-push(cdr(curr))
          (call $gc-maybe-touched-push (%cdr-l $curr))
          (br $b_switch)))

      ;; case vector-type
      (if (i32.eq (local.get $type) (%vector-type)) (then
          (call $gc-touched-vector (local.get $curr))
          (br $b_switch)))

      ;; case cont-type
      (if (i32.eq (local.get $type) (%cont-type)) (then
          (call $gc-touched-continuation (%car-l $curr))
          (call $gc-maybe-touched-push (%cdr-l $curr))
          (br $b_switch)))

      ;; case except-type
      (if (i32.eq (local.get $type) (%except-type)) (then
          (call $gc-maybe-touched-push (%car-l $curr))
          (br $b_switch)))

      ;; case cont-proc-type
      (if (i32.eq (local.get $type) (%cont-proc-type)) (then
          (call $gc-maybe-touched-push (%car-l $curr))
          (call $gc-maybe-touched-push (%cdr-l $curr))
          (br $b_switch)))
          
      ;; }
    )

    ;; mark-safe(curr)
    (call $gc-mark-safe (local.get $curr))

    ;; i--
    (%dec $i)
    ;; }
    (br $forever)
  )
  (unreachable)
)

(func $gc-touched-vector (param $vec i32)
  (local $ptr i32)
  (local $count i32)

  (local.set $ptr (%car-l $vec))
  (local.set $count (%cdr-l $vec))

  (loop $forever
    (if (i32.eqz (local.get $count))
      (then (return)))
    
    (call $gc-maybe-touched-push (i32.load (local.get $ptr)))
    
    (%plus-eq $ptr 4)
    (%dec $count)
    (br $forever)))

(func $gc-touched-continuation (param $cont i32)
  (loop $forever
    ;; continuation env is touched
    (call $gc-maybe-touched-push (i32.load offset=4 (local.get $cont)))
    ;; continuation args are touched
    (call $gc-maybe-touched-push (i32.load offset=8 (local.get $cont)))))

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
          ;; if (gc-is-safe(ptr) || type == (%symbol-type)) {
          (block $if_b_or_s_done
            (block $if_b_or_s_else
              (block $if_b_or_s_then
                ;; if (gc-is-safe(ptr) || 
                (br_if $if_b_or_s_then (call $gc-is-safe (local.get $ptr)))
                ;; type == (%symbol-type)) {
                (br_if $if_b_or_s_then (i32.eq (local.get $type) (%symbol-type)))
                (br $if_b_or_s_else)
              )
              ;; gc-mark-untouched(ptr, type)
              (call $gc-mark-untouched (local.get $ptr))
              (%ginc $g-gc-not-collected-count)
              (%ginc $g-gc-total-not-collected-count)
              (br $if_b_or_s_done)
            )
            ;; } else {
            ;; assert(gc-is-untouched(ptr))
            (if (i32.eqz (call $gc-is-untouched (local.get $ptr))) (then
              (call $print-symbol (global.get $g-error))
              (call $print-symbol (global.get $g-space))
              (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 10))
              (call $print-symbol (global.get $g-space))
              (call $print-integer (i64.extend_i32_u (local.get $ptr)) (i32.const 16))
              (call $print-symbol (global.get $g-space))
              (call $print (local.get $ptr))
              (call $print-symbol (global.get $g-newline))
              ;; oops, this should be safe or untouched, but is touched
              ;; treat as safe
              (br $if_b_or_s_done)))
            (%assert (call $gc-is-untouched (local.get $ptr)))
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
            (if (i32.eq (local.get $type) (%cont-type)) (then
                (call $cont-free (%car-l $ptr))))

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

(func $gc-push-env-items (param $env i32)
  (local $parent i32)
  (local $hashtable i32)
  (local $capacity i32)
  (local $slot i32)
  (local $digest i64)

  ;; parent = cdr(env)
  ;; if (parent) {
  (if (local.tee $parent (%cdr-l $env))
    (then
      ;; gc-maybe-touched-push(parent)
      (call $gc-maybe-touched-push (local.get $parent))
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
              ;; gc-maybe-touched-push(slot.data)
              ;; gc-maybe-touched-push(slot[12])
              (call $gc-maybe-touched-push 
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


(func $gc-is-untouched (param $item i32) (result i32)
  (return (i32.eqz (%get-gc-flags $item))))

(func $gc-is-touched (param $item i32) (result i32)
  (return (i32.eq (%get-gc-flags $item) (i32.const 1))))

(func $gc-is-safe (param $item i32) (result i32)
  (return (i32.eq (%get-gc-flags $item) (i32.const 2))))

(func $gc-mark-untouched (param $item i32)
  (%set-gc-flags $item (i32.const 0)))

(func $gc-mark-touched (param $item i32)
  (%set-gc-flags $item (i32.const 1)))

(func $gc-mark-safe (param $item i32)
  (%set-gc-flags $item (i32.const 2)))

(func $gc-maybe-touched-push (param $item i32)
  (local $type i32)

  (if (i32.eqz (local.get $item)) (then (return)))

  ;; if (!is-untouched(item)) {
  (if (i32.eqz (call $gc-is-untouched (local.get $item))) (then
    (if (call $gc-is-touched (local.get $item)) (then
      (if (i32.eqz (call $in-touched-queue? (local.get $item))) (then
        ;; item is marked as touched, but isn't in the touched queue. This is a 
        ;; bug, but we'll simply mark it as untouched and try again
        (call $gc-mark-untouched (local.get $item))
        (call $gc-maybe-touched-push (local.get $item))))))
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
        (br_if $b_then (i32.eq (local.get $type) (%except-type)))
        (br $b_else)
      )
      ;; mark-touched(item)
      (call $gc-mark-touched (local.get $item))
      ;; gc-touched-push(item)
      (call $gc-touched-push (local.get $item))
      ;;
      (br $b_if)
    )
    ;; } else {
    ;; mark-safe(item)
    (call $gc-mark-safe (local.get $item))
    ;; }
  )
)

(func $gc-init (param $touched-init i32)
  (local $array i32)

  ;; array = malloc(0x408)
  (local.set $array (call $malloc (i32.const 0x408)))
  ;; array.next = 0
  ;; array[0] = 0
  (i32.store offset=0 (local.get $array) (i32.const 0))
  ;; array.count = 0
  ;; array[4] = 0
  (i32.store offset=4 (local.get $array) (i32.const 0))

  ;; g-gc-state = malloc(8)
  (global.set $g-gc-state (call $malloc (i32.const 8)))
  ;; g-gc-state.head-ptr = array
  (i32.store offset=0 (global.get $g-gc-state) (local.get $array))
  ;; g-gc-state.count = 0
  (i32.store offset=4 (global.get $g-gc-state) (i32.const 0))

  (call $gc-mark-safe (global.get $g-nil))
  (call $gc-mark-safe (global.get $g-true))
  (call $gc-mark-safe (global.get $g-false))
  ;; Add passed in roots to the touched set
  (call $gc-maybe-touched-push (local.get $touched-init))
  ;; Add the read and write caches from the global reader to the touched set
  (call $gc-maybe-touched-push (i32.load offset=16 (global.get $g-reader)))
  (call $gc-maybe-touched-push (i32.load offset=20 (global.get $g-reader)))
  ;; Add the named character set to the touched set
  (call $gc-maybe-touched-push (global.get $g-char-env))

  ;; g-gc-collecting? = true
  (global.set $g-gc-collecting? (i32.const 1))
  (%ginc $g-gc-collection-count)
  (global.set $g-gc-collected-count (i32.const 0))
  (global.set $g-gc-not-collected-count (i32.const 0))
)

(func $gc-touched-pop (result i32)
  (local $ptr i32)
  (local $head-array i32)
  (local $array i32)
  (local $val i32)
  (local $count i32)
  (local $next i32)
  (local $prev i32)

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))

  (local.set $array (local.tee $head-array (i32.load (local.get $ptr))))
  (local.set $prev (i32.const 0))
  ;; loop until we find the final array
  (block $end (loop $start
      (local.set $next (i32.load (local.get $array)))
      (br_if $end (i32.eqz (local.get $next)))
      (local.set $prev (local.get $array))
      (local.set $array (local.get $next))
      (br $start)))

  (local.set $count (i32.load offset=4 (local.get $array)))
  ;; there are no items left in the touched set
  (if (i32.eqz (local.get $count)) (then
      ;; assert that there is no previous array block
      (%assert (i32.eqz (local.get $prev)))
      (return (i32.const 0))))

  (local.set $val (i32.load offset=4 (i32.add 
        (local.get $array) 
        (%word-size-l $count))))

  (%dec $count)
  (i32.store offset=4 (local.get $array) (local.get $count))

  ;; decrement the total count
  (i32.store offset=4 
    (local.get $ptr)
    (i32.sub (i32.load offset=4 (local.get $ptr)) (i32.const 1)))

  ;; If the current array is empty
  (if (i32.eqz (local.get $count)) (then
      ;; If there is a previous array (i.e. this isn't the first array)
      (if (local.get $prev) (then
        (i32.store (local.get $prev) (i32.const 0))
        (call $malloc-free (local.get $array))))))

  (return (local.get $val)))

(func $gc-touched-push (param $val i32)
  (local $ptr i32)
  (local $head-array i32)
  (local $array i32)
  (local $next i32)
  (local $count i32)

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))

  ;; array = head-array = *ptr
  (local.set $array (local.tee $head-array (i32.load (local.get $ptr))))

  ;; loop until we find the final array
  (block $end (loop $start
      (local.set $next (i32.load (local.get $array)))
      (br_if $end (i32.eqz (local.get $next)))
      (local.set $array (local.get $next))
      (br $start)))

  (local.set $count (i32.load offset=4 (local.get $array)))
  (if (i32.eq (local.get $count) (i32.const 256)) (then
      (local.set $next (call $malloc (i32.const 0x408)))
      (i32.store offset=0 (local.get $next) (i32.const 0))
      (i32.store offset=4 (local.get $next) (i32.const 0))
      (local.set $count (i32.const 0))
      (i32.store offset=0 (local.get $array) (local.get $next))
      (local.set $array (local.get $next))))

  (i32.store offset=8 
    (i32.add (local.get $array) (%word-size-l $count))
    (local.get $val))
  (%inc $count)
  (i32.store offset=4 (local.get $array) (local.get $count))
  (i32.store offset=4
    (local.get $ptr)
    (i32.add (i32.load offset=4 (local.get $ptr)) (i32.const 1))))

(func $in-touched-queue? (param $val i32) (result i32)
  (local $ptr i32)
  (local $array i32)
  (local $idx i32)
  (local $count i32)
  (local $array-ptr i32)

  ;; ptr = g-gc-state
  (local.set $ptr (global.get $g-gc-state))
  ;; array = ptr.head-ptr
  ;; tail-array = ptr[0]
  (local.set $array (i32.load offset=0 (local.get $ptr)))

  (block $end (loop $start
      (br_if $end (i32.eqz (local.get $array)))

      (local.set $count (i32.load offset=4 (local.get $array)))
      (local.set $array-ptr (i32.add (local.get $array) (i32.const 8)))
      (local.set $idx (i32.const 0))
      (block $inner_end (loop $inner_start
          (br_if $inner_end (i32.eq (local.get $idx) (local.get $count)))

          (if (i32.eq (local.get $val) (i32.load (local.get $array-ptr))) (then
              (return (i32.const 1))))

          (%inc $idx)
          (%plus-eq $array-ptr 4)
          (br $inner_start)))

      (local.set $array (i32.load (local.get $array)))
      (br $start)))

  (return (i32.const 0)))