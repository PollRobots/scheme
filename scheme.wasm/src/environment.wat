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
  (block $done (loop $forever
      ;; we don't free the keys here because symbols are interned
      (call $malloc-free (%car-l $env))

      ;; if (!destroy-outer) break
      (br_if $done (i32.eqz (local.get $destroy-outer)))

      ;; if (env = env[8]) continue
      (br_if $forever (local.tee $env (%cdr-l $env))))))

(func $environment-add (param $env i32) (param $key i32) (param $value i32)
  (local $key-str i32)
  (local $hashtable i32)
  (local $new-hash i32)

  ;; check that key is a symbol
  ;; if (*key & 0xF != 6) {
  (if (i32.ne (%get-type $key) (%symbol-type))
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
      (local.get $key-str)
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

(func $environment-has (param $env i32) (param $key i32) (result i32)
  ;; check that key is a symbol
  (if (i32.ne (%get-type $key) (%symbol-type)) (then 
    (return (i32.const 0))))

  ;; check that there isn't an existing entry in the hashtable
  ;; return hashtable-has(hashtable, key-str)
  (return (call $hashtable-has (%car-l $env) (%car-l $key))))

(func $environment-get (param $env i32) (param $key i32) (result i32)
  (local $key-str i32)
  (local $hashtable i32)
  (local $value i32)

  ;; check that key is a string
  ;; if (*key & 0xF != 6) {
  (if (i32.ne (%get-type $key) (%symbol-type)) (then
      ;; return g-nil
      (return (global.get $g-nil))))

  ;; key-str = key[4]
  (local.set $key-str (i32.load offset=4 (local.get $key)))

  (loop $forever
    ;; hashtable = env[4]
    (local.set $hashtable (i32.load offset=4 (local.get $env)))
    ;; value = hashtable-get(hashtable, key-str)
    (local.set $value (call $hashtable-get 
        (local.get $hashtable) 
        (local.get $key-str)))
    ;; if (value != 0) {
    (if (local.get $value) (then
        ;; return value
        (return (local.get $value))))

    ;; env = env[8]
    (local.set $env (i32.load offset=8 (local.get $env)))

    ;; if (env) continue;
    (br_if $forever (local.get $env)))

  ;; return g-nil;
  (return (%alloc-error (global.get $g-unknown) (local.get $key))))

(func $environment-set! (param $env i32) (param $key i32) (param $value i32) (result i32)
  (local $key-str i32)
  (local $hashtable i32)
  (local $new-hashtable i32)

  ;; check that key is a string
  ;; if (*key & 0xF != symbol) {
  (if (i32.ne (%get-type $key) (%symbol-type)) (then 
      (return (call $argument-error (%alloc-list-2 
            (local.get $key) 
            (local.get $value))))))

  ;; key-str = key[4]
  (local.set $key-str (%car-l $key))

  ;; while (true) {
  (loop $forever
    ;; hashtable = car(env)
    (local.set $hashtable (%car-l $env))

    ;; Note: cannot remove then add, because the duplicated key-string in the hashtable will leak.
    ;; if (hashtable-replace(hashtable, key-str, value)) {
    (if (call $hashtable-replace 
        (local.get $hashtable) 
        (local.get $key-str) 
        (local.get $value))
      ;; return
      (then (return (global.get $g-nil))))


    ;; if (env = cdr(env)) continue;
    (br_if $forever (local.tee $env (%cdr-l $env))))
  ;; }

  (return (call $argument-error (%alloc-list-2 
        (local.get $key) 
        (local.get $value)))))