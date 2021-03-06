(func $current-second (param $env i32) (param $args i32) (result i32)
  (if (call $list-len (local.get $args)) (then
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-f64 (call $time-current-second))))

(func $current-jiffy (param $env i32) (param $args i32) (result i32)
  (if (call $list-len (local.get $args)) (then
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-f64 (call $time-current-jiffy))))

(func $jiffies-per-second (param $env i32) (param $args i32) (result i32)
  (if (call $list-len (local.get $args)) (then
      (return (call $argument-error (local.get $args)))))

  (return (%alloc-f64 (call $time-jiffies-per-second))))
