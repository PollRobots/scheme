;; hashtable data structure

;; HashTable :=
;;   HashTableHeader header
;;   HashTableEntry[capacity] -- entries
;;
;; HashTableHeader := (size 8)
;;   i32 capacity (offset 0) -- number of slots total
;;   i32 count    (offset 4) -- number of entries
;;
;; HashTableEntry := (size 16)
;;    i64 digest (offset 0)  -- digest of str
;;    i32 str    (offset 8)  -- key
;;    i32 data   (offset 12) -- data value 

(func $hashtable-init (param $capacity i32) (result i32)
  (local $size i32) ;; total size of the hashtable
  (local $ptr i32)  ;; ptr to the hashtable

  ;; if (capacity < 32) capacity = 32
  (if (i32.le_s (local.get $capacity) (i32.const 32))
    (local.set $capacity (i32.const 32))
  )

  ;; size = sizeof HashTableHeader + capacity * (sizeof HashTableEntry)
  ;; size = 8 + capacity * 16
  (local.set $size (i32.add (i32.const 8) (i32.mul (local.get $capacity) (i32.const 16))))

  ;; ptr = malloc(size)
  (local.set $ptr (call $malloc (local.get $size)))

  ;; mallocZero(ptr, size);
  (call $malloc-zero (local.get $ptr) (local.get $size))
  
  ;; ptr[0] = capacity
  (i32.store (local.get $ptr) (local.get $capacity))

  (return (local.get $ptr))
)

(func $hashtable-free-keys (param $hashtable i32)
  (local $capacity i32)   ;; capacity of the hashtable
  (local $count i32)      ;; number of entries in the hashtable
  (local $slot-ptr i32)
  (local $slot i32)
  (local $digest i64)

  ;; capacity = hashtable[0]
  (local.set $capacity (i32.load (local.get $hashtable)))
  ;; count = hashtable[4]
  (local.set $count (i32.load (i32.add (local.get $hashtable) (i32.const 4))))
  

  ;; slot = 0
  (local.set $slot (i32.const 0))
  ;; slot-ptr = hashtable + 8
  (local.set $slot-ptr (i32.add (local.get $hashtable) (i32.const 8)))
  
  ;; while (slot != capacity) {
  (block $w_end
    (loop $w_start
      (br_if $w_end (i32.eq (local.get $slot) (local.get $capacity)))

      ;; digest = *slot-ptr
      (local.set $digest (i64.load (local.get $slot-ptr)))
      ;; if (digest != 0 && digest != -1) {
      (if (i64.ne (local.get $digest) (i64.const 0))
        (then
          (if (i64.ne (local.get $digest) (i64.const -1))
            (then
              ;; key = slot-ptr[8]
              ;; malloc-free(key)
              ;;  as point-free
              ;; malloc-free(slot-ptr[8])
              (call $malloc-free
                (i32.load (i32.add (local.get $slot-ptr) (i32.const 8)))
              )

              ;; count--
              (local.set $count (i32.sub (local.get $count) (i32.const 1)))
            )
          )
        )
      )
      ;; }

      ;; slot++
      (%inc $slot)
      ;; slot-ptr += 16
      (%plus-eq $slot-ptr 16)

      (br $w_start)
    )
  )
  ;; }

  ;; assert(count == 0)
  ;; if (count) {
  (if (local.get $count)
    ;; trap
    (then unreachable)
  ;; }
  )
)

(func $hashtable-digest (param $key i32) (result i64)
  (local $len i32)

  ;; len = *key
  (local.set $len (i32.load (local.get $key)))
  ;; return xxh64(key + 4, len, 0)
  (return (call $xxh64 (i32.add (local.get $key) (i32.const 4)) (local.get $len) (i64.const 0)))
)

(func $hashtable-add
  (param $hash i32) ;; pointer to the hash table
  (param $key i32) ;; key of the entry to store
  (param $value i32) ;; value to store
  (result i32 ) ;; pointer to the hashtable (this will change if the hashtable grows)

  (local $ptr i32)
  (local $capacity i32)   ;; capacity of the hashtable
  (local $count i32)      ;; number of entries in the hashtable
  (local $threshold i32)  ;; threshold beyond which we will grow
  (local $digest i64)     ;; the digest of the string
  (local $slot i32)       ;; the slot to store in
  (local $slot-ptr i32)   ;; the address of the slot
  (local $slot-digest i64);; the digets value of the slot

  ;; ptr = hash
  (local.set $ptr (local.get $hash))

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))
  ;; count = ptr[1]
  (local.set $count (i32.load (i32.add (local.get $ptr) (i32.const 4))))
  ;; threshold = 3 * capacity / 4
  ;; threshold = (3 * capacity) >> 2
  (local.set $threshold (i32.shr_u (i32.mul (i32.const 3) (local.get $capacity)) (i32.const 2)))

  ;; if (count >= threshold)
  (if (i32.ge_u (local.get $count) (local.get $threshold))
  ;; {
    (then
      ;; ptr = hashtable-grow(ptr)
      (local.set $ptr (call $hashtable-grow (local.get $hash)))
      ;; return hashtable-add(ptr, key, value)
      (return (call $hashtable-add (local.get $ptr) (local.get $key) (local.get $value)))
    )
  ;; }
  )

  ;; digest = hashtable-digest(key)
  (local.set $digest (call $hashtable-digest (local.get $key)))

  ;; slot = (i32)(digest % (i64)capacity)
  (local.set $slot (i32.wrap_i64 (i64.rem_u (local.get $digest) (i64.extend_i32_u (local.get $capacity)))))

  ;; while (true)
  (loop $forever
    (if (i32.ge_u (local.get $slot) (local.get $capacity))
      (then unreachable)
    )
  ;; {
    ;; slot-ptr = ptr + 8 + 16 * slot
    (local.set $slot-ptr
      (i32.add 
        (i32.add 
          (local.get $ptr)
          (i32.const 8)
        )
        (i32.shl
          (local.get $slot)
          (i32.const 4)
        )
      )
    )
    ;; slot-digest = *slot-ptr
    (local.set $slot-digest (i64.load (local.get $slot-ptr)))

    ;; if the slot is empty or marked with a tombstone (-1)
    ;; if (slot-digest == 0 || slot-digest == -1) {
    (block $b_end
      (block $b_or
        ;; skip next check if slot-digest == empty(0)
        (br_if $b_or (i64.eqz (local.get $slot-digest)))
        ;; if slot-digest != tombstone(-1) then skip 
        (br_if $b_end (i64.ne (local.get $slot-digest) (i64.const -1)))
      )

      ;; use this slot
      ;; *slot-ptr = digest
      (i64.store (local.get $slot-ptr) (local.get $digest))
      ;; *(slot-ptr + 8) = key
      (i32.store (i32.add (local.get $slot-ptr) (i32.const 8)) (local.get $key))
      ;; *(slot-ptr + 12) = value
      (i32.store (i32.add (local.get $slot-ptr) (i32.const 12)) (local.get $value))
      ;; *(ptr + 4) = count + 1
      (i32.store 
        (i32.add (local.get $ptr) (i32.const 4))
        (i32.add (local.get $count) (i32.const 1))
      )
      ;; return ptr
      (return (local.get $ptr))
    )
    ;; } else {
    ;; slot = (slot + 1 ) % capacity 
    (local.set $slot (i32.rem_u
      (i32.add (local.get $slot) (i32.const 1))
      (local.get $capacity))
    )
    ;; }

    (br $forever)
  )
  unreachable
  ;; }
)

(func $hashtable-grow
  (param $ptr i32)
  (result i32 )

  (local $old i32)
  (local $capacity i32)
  (local $count i32)
  (local $new i32)
  (local $i i32)
  (local $digest i64)
  (local $str i32)
  (local $value i32)

  ;; old = ptr;
  (local.set $old (local.get $ptr))

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))
  ;; count = ptr[1]
  (local.set $count (i32.load (i32.add (local.get $ptr) (i32.const 4))))
  ;; new = hashtable-init(capacity * 2)
  (local.set $new (call $hashtable-init (i32.shl (local.get $capacity) (i32.const 1))))

  ;; ptr += 8
  (%plus-eq $ptr 8)

  ;; while (i < capacity) {
  (block $b_end
    (loop $b_start
      ;; break if i >= capacity
      (br_if $b_end (i32.ge_u (local.get $i) (local.get $capacity))) 

      ;; digest = *ptr
      (local.set $digest (i64.load (local.get $ptr)))

      ;; skip slots that have a zero digest or tombstone digest
      ;; if (digest && digest != -1) {
      (if (i64.ne (local.get $digest) (i64.const 0))
        (then 
          (if (i64.ne (local.get $digest) (i64.const -1))
            (then
              ;; str = *(ptr + 8)
              (local.set $str (i32.load (i32.add (local.get $ptr) (i32.const 8))))
              ;; value = *(ptr + 12)
              (local.set $value (i32.load (i32.add (local.get $ptr) (i32.const 12))))

              ;; hashtable-add(new, str, value)
              (drop (call $hashtable-add (local.get $new) (local.get $str) (local.get $value)))
            )
          )
        )
      ;; }
      )

      ;; i++
      (%inc $i)
      ;; ptr += 16
      (%plus-eq $ptr 16)
      (br $b_start)
    )
  ;; }
  )

  ;; malloc-free(ptr)
  (call $malloc-free (local.get $old))

  ;; return new
  (return (local.get $new))
)

(func $hashtable-get-internal
  (param $ptr i32)  ;; the hashtable
  (param $key i32)  ;; the key to lookup
  (result i32 )     ;; the slot number containing the entry

  (local $capacity i32)
  (local $count i32)
  (local $digest i64)
  (local $slot i32)
  (local $slot-ptr i32)
  (local $slot-digest i64)
  (local $slot-key i32)

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))
  ;; count = ptr[1]
  (local.set $count (i32.load (i32.add (local.get $ptr) (i32.const 4))))

  ;; digest = hashtable-digest(key)
  (local.set $digest (call $hashtable-digest (local.get $key)))

  ;; slot = (i32)(digest % (i64)capacity)
  (local.set $slot (i32.wrap_i64 (i64.rem_u (local.get $digest) (i64.extend_i32_u (local.get $capacity)))))

  ;; while (count) {
  (block $b_end
    (loop $b_start
      ;; break if count == 0
      (br_if $b_end (i32.eqz (local.get $count)))

      ;; slot-ptr = ptr + 8 + slot-ptr * 16
      (local.set $slot-ptr 
        (i32.add
          (i32.add
            (local.get $ptr)
            (i32.const 8)
          )
          (i32.mul (local.get $slot) (i32.const 16))
        )
      )

      ;; slot-digest = *slot-ptr
      (local.set $slot-digest (i64.load (local.get $slot-ptr)))

      ;; if (slot-digest == 0) return 0;
      (if (i64.eqz (local.get $slot-digest))
        (then (return (local.get $capacity)))
      )

      ;; if (slot-digest == digest) {
      (if (i64.eq (local.get $slot-digest) (local.get $digest))
        (then
          ;; slot-key = *(slot-ptr + 8)
          (local.set $slot-key (i32.load (i32.add (local.get $slot-ptr) (i32.const 8))))
          ;; if (str-eq(slot-key, key)) {
          (if (call $str-eq (local.get $slot-key) (local.get $key))
            (then
              ;; return slot
              (return (local.get $slot))
            )
          )
          ;;}
        )
      )
      ;; }


      ;; slot = (slot + 1) % capacity
      (local.set $slot (i32.rem_u (i32.add (local.get $slot) (i32.const 1)) (local.get $capacity)))
      ;; count--;
      (local.set $count (i32.sub (local.get $count) (i32.const 1)))
      ;; }
      (br $b_start)
    )
  )
  
  ;; return 0
  (return (local.get $capacity))
)


(func $hashtable-get
  (param $ptr i32)  ;; the hashtable
  (param $key i32)  ;; the key to lookup
  (result i32 )     ;; the value stored in the hashtable, or 0 if not found

  (local $capacity i32)
  (local $slot i32)
  (local $slot-ptr i32)
  (local $slot-value i32)

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))

  ;; slot = hashtable-get-internal(ptr, key)
  (local.set $slot (call $hashtable-get-internal (local.get $ptr) (local.get $key)))

  ;; if (slot >= capacity) { return 0; }
  (if (i32.ge_u (local.get $slot) (local.get $capacity))
    (then
      (return (i32.const 0))
    )
  )
  
  ;; slot-ptr = ptr + 8 + slot-ptr * 16
  ;; slot-ptr = ptr + 8 + slot-ptr << 4
  (local.set $slot-ptr 
    (i32.add
      (i32.add (local.get $ptr) (i32.const 8))
      (i32.shl (local.get $slot) (i32.const 4))
    )
  )

  ;; slot-value = #(slot-ptr + 12)
  (local.set $slot-value (i32.load (i32.add (local.get $slot-ptr) (i32.const 12))))
  ;; return slot-value
  (return (local.get $slot-value))
)

(func $hashtable-has
  (param $ptr i32)  ;; the hashtable
  (param $key i32)  ;; the key to lookup
  (result i32 )     ;; indicates whether the hashtable has a value for this key (1)

  (local $capacity i32)
  (local $slot i32)

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))

  ;; slot = hashtable-get-internal(ptr, key)
  (local.set $slot (call $hashtable-get-internal (local.get $ptr) (local.get $key)))

  ;; return slot < capacity
  (return (i32.lt_u (local.get $slot) (local.get $capacity)))
)

(func $hashtable-remove
  (param $ptr i32)  ;; the hashtable
  (param $key i32)  ;; the key to lookup
  (result i32 )     ;; 1 if removed, 0 otherwise

  (local $capacity i32)
  (local $count i32)
  (local $slot i32)
  (local $slot-ptr i32)
  (local $slot-value i32)

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))

  ;; slot = hashtable-get-internal(ptr, key)
  (local.set $slot (call $hashtable-get-internal (local.get $ptr) (local.get $key)))

  ;; if (slot >= capacity) { return 0; }
  (if (i32.ge_u (local.get $slot) (local.get $capacity))
    (then
      (return (i32.const 0))
    )
  )

  ;; slot-ptr = ptr + 8 + slot * 16
  ;; slot-ptr = ptr + 8 + slot << 4
  (local.set $slot-ptr 
    (i32.add
      (i32.add (local.get $ptr) (i32.const 8))
      (i32.shl (local.get $slot) (i32.const 4))
    )
  )

  ;; *slot-ptr = tombstone(-1)
  (i64.store (local.get $slot-ptr) (i64.const -1))
  ;; *(slot-ptr + 8) = 0
  (i64.store (i32.add (local.get $slot-ptr) (i32.const 8)) (i64.const 0))

  ;; reduce count
  ;; count = ptr[4]
  (local.set $count (i32.load (i32.add (local.get $ptr) (i32.const 4))))
  ;; ptr[4] = count - 1
  (i32.store
    (i32.add (local.get $ptr) (i32.const 4)) 
    (i32.sub (local.get $count) (i32.const 1))
  )

  ;; return 1
  (return (i32.const 1))
)

(func $hashtable-replace
  (param $ptr i32)    ;; the hashtable
  (param $key i32)    ;; the key to lookup
  (param $value i32)  ;; the new value
  (result i32 )       ;; 1 if replaced, 0 otherwise

  (local $capacity i32)
  (local $count i32)
  (local $slot i32)
  (local $slot-ptr i32)
  (local $slot-value i32)

  ;; capacity = ptr[0]
  (local.set $capacity (i32.load (local.get $ptr)))

  ;; slot = hashtable-get-internal(ptr, key)
  (local.set $slot (call $hashtable-get-internal (local.get $ptr) (local.get $key)))

  ;; if (slot >= capacity) { return 0; }
  (if (i32.ge_u (local.get $slot) (local.get $capacity))
    (then
      (return (i32.const 0))
    )
  )

  ;; slot-ptr = ptr + 8 + slot * 16
  ;; slot-ptr = ptr + 8 + slot << 4
  (local.set $slot-ptr 
    (i32.add
      (i32.add (local.get $ptr) (i32.const 8))
      (i32.shl (local.get $slot) (i32.const 4))
    )
  )

  ;; slot-ptr[12] = value
  (i32.store
    (i32.add (local.get $slot-ptr) (i32.const 12))
    (local.get $value)
  )

  ;; return 1
  (return (i32.const 1))
)
