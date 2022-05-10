%( port

(%define %open-input-file ()          (i32.const 5))  ;; input (1) + text (4)
(%define %open-binary-input-file ()   (i32.const 1))  ;; input (1) 
(%define %open-input-string ()        (i32.const 21)) ;; input (1) + text (4) + string (16)
(%define %open-input-bytevector ()    (i32.const 33)) ;; input (1) + bytevec (32)
(%define %open-output-file ()         (i32.const 6))  ;; output (2) + text (4)
(%define %open-binary-output-file ()  (i32.const 2))  ;; output (2) 
(%define %open-output-string ()       (i32.const 22)) ;; output (2) + text (4) + string (16)
(%define %open-output-bytevector ()   (i32.const 34)) ;; output (2) + bytevec (32)

;; (port? <obj>)
(func $port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $obj) (%port-type)))))

;; (input-port? <obj>)
(func $input-port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (%cdr-l $obj) (%port-input-flag)))))

;; (output-port? <obj>)
(func $output-port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (%cdr-l $obj) (%port-output-flag)))))

;; (textual-port? <obj>)
(func $textual-port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (%cdr-l $obj) (%port-text-flag)))))

;; (binary-port? <obj>)
(func $binary-port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-false)
      (global.get $g-true)
      (i32.and (%cdr-l $obj) (%port-text-flag)))))

;; (string-port? <obj>)
(func $string-port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (%cdr-l $obj) (%port-string-flag)))))

;; (string-port-obj <port>)
(func $string-port-obj (param $env i32) (param $args i32) (result i32)
  (local $port i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $port (%car-l $args))
      (%chk-type $fail $port %port-type)
      (br_if $fail (i32.eqz (i32.and (%cdr-l $port) (%port-string-flag))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%car-l $port)))

;; (bytevector-port? <obj>)
(func $bytevector-port? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (%cdr-l $obj) (%port-bytevec-flag)))))

;; (bytevector-port-obj <port>)
(func $bytevector-port-obj (param $env i32) (param $args i32) (result i32)
  (local $port i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $port (%car-l $args))
      (%chk-type $fail $port %port-type)
      (br_if $fail (i32.eqz (i32.and (%cdr-l $port) (%port-bytevec-flag))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%car-l $port)))

;; (input-port-open? <obj>)
(func $input-port-open? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $flags i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (local.set $flags (%cdr-l $obj))

  (if (i32.eqz (i32.and (local.get $flags) (%port-input-flag))) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (local.get $flags) (%port-open-flag)))))

;; (output-port-open? <obj>)
(func $output-port-open? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (local $flags i32)
  
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))
  (if (i32.ne (%get-type $obj) (%port-type)) (then
      (return (global.get $g-false))))

  (local.set $flags (%cdr-l $obj))

  (if (i32.eqz (i32.and (local.get $flags) (%port-output-flag))) (then
      (return (global.get $g-false))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.and (local.get $flags) (%port-open-flag)))))

;; (close-port <port>)
(func $close-port (param $env i32) (param $args i32) (result i32)
  (local $port i32)
  (local $flags i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $port (%car-l $args))
      (%chk-type $fail $port %port-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (local.set $flags (%cdr-l $port))
  ;; check if port is open
  (if (i32.and (local.get $flags) (%port-open-flag)) (then

    ;; different behavior for string ports
    (if (i32.and 
        (local.get $flags) 
        (i32.or (%port-string-flag) (%port-bytevec-flag)))
      (then
        ;; simply release the reference to the string/bytevector
        (%set-car! (local.get $port) (global.get $g-nil)))
      (else
        (call $port-close (%car-l $port))))
    ;; set port to no longer open
    (local.set $flags (i32.sub (local.get $flags) (%port-open-flag)))
    (%set-cdr!-l $port $flags)))

  (return (global.get $g-nil)))

;; (open-input-file <string>)
(func $open-input-file (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $fd i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $name (%car-l $args))
      (%chk-type $fail $name %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (local.set $fd (call $port-open (%car-l $name) (%open-input-file)))

  (if (i32.lt_s (local.get $fd) (i32.const 0)) (then
    ;; TODO(pacaro) add error message.
    (unreachable)))

  (return (%alloc-port 
      (local.get $fd) 
      (i32.add (%open-input-string) (%port-open-flag)))))

;; (open-binary-input-file <string>)
(func $open-binary-input-file (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $fd i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $name (%car-l $args))
      (%chk-type $fail $name %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (local.set $fd (call $port-open (%car-l $name) (%open-binary-input-file)))

  (if (i32.lt_s (local.get $fd) (i32.const 0)) (then
    ;; TODO(pacaro) add error message.
    (unreachable)))

  (return (%alloc-port 
      (local.get $fd) 
      (i32.add (%open-binary-input-file) (%port-open-flag)))))

;; (open-input-string <string>)
(func $open-input-string (param $env i32) (param $args i32) (result i32)
  (local $str i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $str (%car-l $args))
      (%chk-type $fail $str %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (return (%alloc-port 
      (%alloc-cons (global.get $g-zero) (local.get $str))
      (i32.add (%open-input-string) (%port-open-flag)))))

;; (open-input-bytevector <bytevector>)
(func $open-input-bytevector (param $env i32) (param $args i32) (result i32)
  (local $bytevec i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $bytevec (%car-l $args))
      (%chk-type $fail $bytevec %bytevector-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (return (%alloc-port 
      (%alloc-cons (global.get $g-zero) (local.get $bytevec))
      (i32.add (%open-input-bytevector) (%port-open-flag)))))

;; (open-output-file <string>)
(func $open-output-file (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $fd i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $name (%car-l $args))
      (%chk-type $fail $name %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (local.set $fd (call $port-open (%car-l $name) (%open-output-file)))

  (if (i32.lt_s (local.get $fd) (i32.const 0)) (then
    ;; TODO(pacaro) add error message.
    (unreachable)))

  (return (%alloc-port 
      (local.get $fd) 
      (i32.add (%open-output-file) (%port-open-flag)))))

;; (open-binary-output-file <string>)
(func $open-binary-output-file (param $env i32) (param $args i32) (result i32)
  (local $name i32)
  (local $fd i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (local.set $name (%car-l $args))
      (%chk-type $fail $name %str-type)
      (br $check))
    (return (call $argument-error (local.get $args))))

  (local.set $fd (call $port-open (%car-l $name) (%open-binary-output-file)))

  (if (i32.lt_s (local.get $fd) (i32.const 0)) (then
    ;; TODO(pacaro) add error message.
    (unreachable)))

  (return (%alloc-port 
      (local.get $fd) 
      (i32.add (%open-binary-output-file) (%port-open-flag)))))

;; (open-output-string)
(func $open-output-string (param $env i32) (param $args i32) (result i32)
  (if (call $list-len (local.get $args)) (then
    (return (call $argument-error (local.get $args)))))

  (return (%alloc-port 
      (%alloc-cons (global.get $g-nil) (global.get $g-nil))
      (i32.add (%open-output-string) (%port-open-flag)))))

;; (open-output-bytevector)
(func $open-output-bytevector (param $env i32) (param $args i32) (result i32)
  (if (call $list-len (local.get $args)) (then
    (return (call $argument-error (local.get $args)))))

  (return (%alloc-port 
      (%alloc-cons (global.get $g-nil) (global.get $g-nil))
      (i32.add (%open-output-bytevector) (%port-open-flag)))))

(func $eof-object (param $env i32) (param $args i32) (result i32)
  (if (call $list-len (local.get $args)) (then
      (return (call $argument-error (local.get $args)))))

  (return (global.get $g-eof-object)))

(func $eof-object? (param $env i32) (param $args i32) (result i32)
  (local $obj i32)
  (if (i32.ne (call $list-len (local.get $args)) (i32.const 1)) (then
      (return (call $argument-error (local.get $args)))))

  (local.set $obj (%car-l $args))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (i32.eq (%get-type $obj) (%eof-type)))))


)%

