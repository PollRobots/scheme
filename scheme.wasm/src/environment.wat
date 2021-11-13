;; An environment is a hashtable and a pointer to a (heap allocated) outer 
;; environment, conveniently this fits within a heap object
;; 
;; Environment
;;  hashtable i32  -- ptr to a hashtable allocated with $hashtable-init
;;  outer i32      -- heap-ptr to the outer hashtable (0 if none)


(func $environment-init (param $heap i32) (param $outer i32) (result i32)
  (local $hashtable i32)

  ;; hashtable = hashtable-init(0)
  ;; return heap-alloc(heap, 9, hashtable, outer)
  (return
    (call $heap-alloc
      (local.get $heap)
      (i32.const 9)
      (call $hashtable-init (i32.const 0))
      (local.get $outer)
    )
  )
)

(func $environment-destroy (param $env i32) (param $destroy-outer i32)
  (local $hashtable i32)
  (local $outer i32)

  ;; hashtable = env[4]
  ;; hashtable-free-keys(hashtable)
  ;;  as point-free
  ;; hashtable-free-keys(env[4])
  (call $hashtable-free-keys 
    (i32.load offset=4 (local.get $env))
  )

  ;; if (destroy-outer) {
  (if (local.get $destroy-outer)
    (then
      ;; outer = env[8]
      (local.set $outer (i32.load offset=8 (local.get $env)))
      ;; if (outer) {
      (if (local.get $outer)
        (then
          ;; environment-destroy(outer, destroy-outer)
          (call $environment-destroy (local.get $outer) (local.get $destroy-outer))
        )
        ;; }
      )
    )
    ;; }
  )
)

(func $environment-add (param $env i32) (param $key i32) (param $value i32)
  (local $key-str i32)
  (local $hashtable i32)
  (local $new-hash i32)

  ;; check that key is a symbol
  ;; if (*key & 0xF != 6) {
  (if (i32.ne (i32.and (i32.load (local.get $key)) (i32.const 0xF)) (%symbol-type))
    ;; trap
    ;; TODO: return error
    (then unreachable)
  ;; }
  )

  ;; key-str = key[4]
  (local.set $key-str (i32.load offset=4 (local.get $key)))
  ;; hashtable = env[4]
  (local.set $hashtable (i32.load offset=4 (local.get $env)))

  ;; check that there isn't an existing entry in the hashtable
  ;; if (hashtable-has(hashtable, key-str)) {
  (if (call $hashtable-has (local.get $hashtable) (local.get $key-str))
    ;; trap
    ;; TODO: return error
    (then unreachable)
  ;; }
  )

  ;; place value in hashtable
  ;; new-hash = hashtable-add(hashtable, str-dup(key-str), value)
  (local.set $new-hash 
    (call $hashtable-add
      (local.get $hashtable)
      (call $str-dup (local.get $key-str))
      (local.get $value)
    )
  )

  ;; if (new-hash != hashtable) {
  (if (i32.ne (local.get $new-hash) (local.get $hashtable))
    (then
      ;; env[4] = new-hash
      (i32.store
        (i32.add (local.get $env) (i32.const 4))
        (local.get $new-hash)
      )
    )
  ;; }
  )
)

(func $environment-get (param $env i32) (param $key i32) (result i32)
  (local $key-str i32)
  (local $hashtable i32)
  (local $value i32)

  ;; check that key is a string
  ;; if (*key & 0xF != 6) {
  (if (i32.ne (i32.and (i32.load (local.get $key)) (i32.const 0xF)) (%symbol-type))
    (then
      ;; return g-nil
      (return (global.get $g-nil))
    )
  ;; }
  )
  ;; key-str = key[4]
  (local.set $key-str (i32.load offset=4 (local.get $key)))

  (loop $forever
    ;; hashtable = env[4]
    (local.set $hashtable (i32.load offset=4 (local.get $env)))
    ;; value = hashtable-get(hashtable, key-str)
    (local.set $value 
      (call $hashtable-get 
        (local.get $hashtable) 
        (local.get $key-str)
      )
    )
    ;; if (value != 0) {
    (if (local.get $value)
      (then
        ;; return value
        (return (local.get $value))
      )
    ;; }
    )

    ;; env = env[8]
    (local.set $env (i32.load offset=8 (local.get $env)))

    ;; if (env) continue;
    (br_if $forever (local.get $env))
  )

  ;; return g-nil;
  (return (global.get $g-nil))
)

(func $environment-set! (param $env i32) (param $key i32) (param $value i32)
  (local $key-str i32)
  (local $hashtable i32)
  (local $new-hashtable i32)

  ;; check that key is a string
  ;; if (*key & 0xF != symbol) {
  (if (i32.ne (i32.and (i32.load (local.get $key)) (i32.const 0xF)) (%symbol-type))
    (then
      ;; trap
      (unreachable)
    )
  ;; }
  )
  ;; key-str = key[4]
  (local.set $key-str (i32.load offset=4 (local.get $key)))

  ;; while (true) {
  (loop $forever
    ;; hashtable = env[4]
    (local.set $hashtable (i32.load offset=4 (local.get $env)))

    ;; Note: cannot remove then add, because the duplicated key-string in the hashtable will leak.
    ;; if (hashtable-replace(hashtable, key-str, value)) {
    (if (call $hashtable-replace (local.get $hashtable) (local.get $key-str) (local.get $value))
      ;; return
      (then return)
    ;; }
    )

    ;; env = env[8]
    (local.set $env (i32.load offset=8 (local.get $env)))

    ;; if (env) continue;
    (br_if $forever (local.get $env))
  )
  ;; }

  ;; trap
  (unreachable) 
)