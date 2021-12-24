(;

Continuations.

Continuations are stored in a separate slab. 

Continuation Block
  size: i32       -- Size of the block
  count: i32      -- number of continuations allocated in this block
  free: i32 ptr   -- ptr to the free list (if any)
  next: i32 ptr   -- ptr to the next continuation block (if any)

Continuation:
  fn: i32         -- function index in the builtins-table, 
  env: i32 env    -- environment
  args: i32 list  -- arguments to the continuation

  if cont.fn is 0 then it is an eval

;)

(%define %continuation-block-count () (i32.const 32))

(global $g-continuations (mut i32) (i32.const 0))

(func $cont-init
  (global.set $g-continuations (call $cont-alloc-block)))

(func $cont-cleanup
  (local $block i32)
  (local $next i32)

  (local.set $block (global.get $g-continuations))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $block)))

      (local.set $next (i32.load offset=12 (local.get $block)))
      (call $malloc-free (local.get $block))
      (local.set $block (local.get $next))
      (br $b_start))))

(func $cont-alloc-block (result i32)
  (local $block i32)
  (local $ptr i32)
  (local $next-ptr i32)
  (local $count i32)

  (local.set $count (%continuation-block-count))
  (local.set $block (call $malloc 
      (i32.add 
        (i32.const 16) ;; header
        (i32.mul (local.get $count) (i32.const 12))))) ;; number of continuations * 12

  (local.set $ptr (i32.add (local.get $block) (i32.const 16))) 

  (i32.store offset=0 (local.get $block) (local.get $count))  ;; size
  (i32.store offset=4 (local.get $block) (i32.const 0))       ;; count
  (i32.store offset=8 (local.get $block) (local.get $ptr))    ;; free
  (i32.store offset=12 (local.get $block) (i32.const 0))      ;; next

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $count)))

      (local.set $next-ptr (i32.add (local.get $ptr) (i32.const 12)))
      (i32.store 
        (local.get $ptr)
        (select
          (local.get $next-ptr)
          (i32.const 0)
          (i32.gt_u (local.get $count) (i32.const 1))))

      (%dec $count)
      (local.set $ptr (local.get $next-ptr))
      (br $b_start)))

  (return (local.get $block)))

(func $cont-alloc (param $fn i32) (param $env i32) (param $args i32) (param $next i32) (result i32)
  (return (call $heap-alloc
      (global.get $g-heap)
      (%cont-type)
      (call $cont-alloc-inner 
        (local.get $fn) 
        (local.get $env) 
        (local.get $args))
      (local.get $next))))

(func $cont-alloc-inner (param $fn i32) (param $env i32) (param $args i32) (result i32)
  (local $block i32)
  (local $next-block i32)
  (local $ptr i32)
  (local $count i32)
  (local $size i32)

  (local.set $block (global.get $g-continuations))

  (block $b_end
    (loop $b_start
      (local.set $size (i32.load offset=0 (local.get $block)))
      (local.set $count (i32.load offset=4 (local.get $block)))
      (if (i32.lt_u (local.get $count) (local.get $size))
        (then
          (local.set $ptr (i32.load offset=8 (local.get $block)))
          ;; increment count
          (i32.store offset=4 (local.get $block) (i32.add (local.get $count) (i32.const 1)))
          ;; set free ptr to this continuations next ptr
          (i32.store offset=8 (local.get $block) (i32.load (local.get $ptr)))

          (i32.store offset=0 (local.get $ptr) (local.get $fn))
          (i32.store offset=4 (local.get $ptr) (local.get $env))
          (i32.store offset=8 (local.get $ptr) (local.get $args))

          (return (local.get $ptr))
        ))

      (local.set $next-block (i32.load offset=12 (local.get $block)))
      (if (i32.eqz (local.get $next-block))
        (then
          (i32.store offset=12
            (local.get $block)
            (local.tee $next-block (call $cont-alloc-block)))))
      (local.set $block (local.get $next-block))
      (br $b_start)))

  (unreachable))

(func $cont-free (param $ptr i32)
  (local $block i32)
  (local $size i32)
  (local $start i32)
  (local $end i32)

  (local.set $block (global.get $g-continuations))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eqz (local.get $block)))
      (local.set $size (i32.load offset=0 (local.get $block)))
      (local.set $start (i32.add (local.get $block) (i32.const 16)))

      (if (i32.ge_u (local.get $ptr) (local.get $start))
        (then
          (local.set $end (i32.add 
              (local.get $start) 
              (i32.mul (local.get $size) (i32.const 12))))
          (if (i32.lt_u (local.get $ptr) (local.get $end))
            (then
              ;; ptr is within the block
              ;; set ptr to free
              (i32.store (local.get $ptr) (i32.load offset=8 (local.get $block)))
              ;; set free to ptr
              (i32.store offset=8 (local.get $block) (local.get $ptr))
              ;; decrement count
              (i32.store offset=4 
                (local.get $block)
                (i32.sub
                  (i32.load offset=4 (local.get $block))
                  (i32.const 1)))
              (return)))))

      (local.set $block (i32.load offset=12 (local.get $block)))
      (br $b_start)))

  ;; TODO message that attempted to free invalid continuation
  (unreachable))

