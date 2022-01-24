;; (make-rectangular <x_1> <x_2>)
;; Creates a complex number with real part <x_1> and imaginary part <x_2>
;; i.e., <x_1>+<x_2>i
(func $make-rectangular (param $env i32) (param $args i32) (result i32)
  (local $real i32)
  (local $imag i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (br_if $fail (i32.eqz (call $all-numeric (local.get $args))))

      (local.set $real (%car-l $args))
      (%chk-type-ne $fail $real %complex-type)

      (local.set $imag (%car (%cdr-l $args)))
      (%chk-type-ne $fail $imag %complex-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (if (call $num-core-zero? (local.get $imag)) (then
      (return (local.get $real))))

  (return (%alloc-complex (local.get $real) (local.get $imag))))

;; (real-part <z>)
(func $real-part (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $fail (i32.eqz (call $all-numeric (local.get $args))))

      (local.set $num (%car-l $args))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (if (i32.eq (%get-type $num) (%complex-type)) (then
      (return (%car-l $num))))

  (return (local.get $num)))

;; (imag-part <z>)
(func $imag-part (param $env i32) (param $args i32) (result i32)
  (local $num i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br_if $fail (i32.eqz (call $all-numeric (local.get $args))))

      (local.set $num (%car-l $args))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (if (i32.eq (%get-type $num) (%complex-type)) (then
      (return (%cdr-l $num))))

  (return (global.get $g-zero)))

;; sin x = (e^ix - e^-ix) / 2i
;; sin (a + bi) =  sin a · cosh b + i cos a · sinh b
(func $complex-sin (param $num i32) (result i32)
  (local $real i32)
  (local $imag i32)

  (local.set $real (call $inexact-impl (%car-l $num)))
  (local.set $imag (call $inexact-impl (%cdr-l $num)))

  (return (%alloc-complex
      (%alloc-f64 (f64.mul
          (call $sin-impl (f64.load offset=4 (local.get $real)))
          (call $cosh-impl (local.get $imag))))
      (%alloc-f64 (f64.mul
          (call $cos-impl (f64.load offset=4 (local.get $real)))
          (call $sinh-impl (local.get $imag)))))))

;; cos x = (e^ix + e^-ix) / 2i
;; cos (a + bi) =  cos a · cosh b - i sin a · sinh b
(func $complex-cos (param $num i32) (result i32)
  (local $real i32)
  (local $imag i32)

  (local.set $real (call $inexact-impl (%car-l $num)))
  (local.set $imag (call $inexact-impl (%cdr-l $num)))

  (return (%alloc-complex
      (%alloc-f64 (f64.mul
          (call $cos-impl (f64.load offset=4 (local.get $real)))
          (call $cosh-impl (local.get $imag))))
      (%alloc-f64 (f64.neg (f64.mul
            (call $sin-impl (f64.load offset=4 (local.get $real)))
            (call $sinh-impl (local.get $imag))))))))

;; tan (a + bi) = (sin 2a + i sinh 2b) / (cos 2a + cosh 2b)
(func $complex-tan (param $num i32) (result i32)
  (local $real i32)
  (local $imag i32)
  (local $scale f64)

  (local.set $real (call $inexact-impl (%car-l $num)))
  (local.set $imag (call $inexact-impl (%cdr-l $num)))

  (local.set $real (call $num-core-add (local.get $real) (local.get $real)))
  (local.set $imag (call $num-core-add (local.get $imag) (local.get $imag)))

  (local.set $scale (f64.div
      (f64.const 1)
      (f64.add
        (call $cos-impl (f64.load offset=4 (local.get $real)))
        (call $cosh-impl (local.get $imag)))))

  (return (%alloc-complex
      (%alloc-f64 (f64.mul
          (local.get $scale)
          (call $sin-impl (f64.load offset=4 (local.get $real)))))
      (%alloc-f64 (f64.mul
          (local.get $scale)
          (call $sinh-impl (local.get $imag)))))))

;; ln re^iP = ln r + iP
;; where r = sqrt(a² + b²)
;;       P = atan2(b, a)
(func $complex-logn (param $num i32) (result i32)
  (local $real f64)
  (local $imag f64)
  (local $mag i32)
  (local $angle f64)

  (%define %kPIbyTwo ()   (f64.const 1.57079632679489661923))

  (if (call $num-core-zero? (%cdr-l $num)) (then
      (return (call $logn-impl (%car-l $num)))))

  (local.set $real (f64.load offset=4 (call $inexact-impl (%car-l $num))))
  (local.set $imag (f64.load offset=4 (call $inexact-impl (%cdr-l $num))))

  (local.set $mag (%alloc-f64 (f64.sqrt (f64.add
          (f64.mul (local.get $real) (local.get $real))
          (f64.mul (local.get $imag) (local.get $imag))))))

  (local.set $angle (call $atan2-impl (local.get $imag) (local.get $real)))

  (return (%alloc-complex
      (call $logn-impl (local.get $mag))
      (%alloc-f64 (local.get $angle)))))

(func $complex-log-base (param $num i32) (param $base i32) (result i32)
  (local $ln-num i32)
  (local $ln-base i32)

  (if (i32.eq (%get-type $num) (%complex-type))
    (then
      (local.set $ln-num (call $complex-logn (local.get $num))))
    (else
      (local.set $ln-num (call $logn-impl (local.get $num)))))

  (if (i32.eq (%get-type $base) (%complex-type))
    (then
      (local.set $ln-base (call $complex-logn (local.get $base))))
    (else
      (local.set $ln-base (call $logn-impl (local.get $base)))))

  (return (call $num-core-div (local.get $ln-num) (local.get $ln-base))))

;; e^(a+bi) = e^a(cos b + i sin b)
(func $complex-exp (param $num i32) (result i32)
  (local $real i32)
  (local $imag f64)
  (local $scale f64)

  (local.set $real (%car-l $num))
  (local.set $imag (f64.load offset=4 (call $inexact-impl (%cdr-l $num))))

  (local.set $scale (call $exp-impl (local.get $real)))

  (return (%alloc-complex
      (%alloc-f64 (f64.mul
          (local.get $scale)
          (call $cos-impl (local.get $imag))))
      (%alloc-f64 (f64.mul
          (local.get $scale)
          (call $sin-impl (local.get $imag)))))))

;; atan x = -i/2 · ln((i - x) / (i + x))
;; atan x = -i/2 · (ln(i - x) - ln(i + x))
(func $complex-atan (param $num i32) (result i32)
  (local $real i32)
  (local $imag i32)
  (local $r64 f64)
  (local $i64 f64)

  (local.set $real (call $inexact-impl (%car-l $num)))
  (local.set $imag (call $inexact-impl (%cdr-l $num)))
  (local.set $r64 (f64.load offset=4 (local.get $real)))
  (local.set $i64 (f64.load offset=4 (local.get $imag)))

  (return (call $num-core-mul
        (%alloc-complex (global.get $g-zero) (%alloc-f64 (f64.const -0.5))) ;; -i/2
        (call $complex-logn
          (call $num-core-div
            (%alloc-complex   ;; i - x ==> (-a + (1 - b)i)
              (%alloc-f64 (f64.neg (local.get $r64)))
              (%alloc-f64 (f64.sub (f64.const 1) (local.get $i64))))
            (%alloc-complex   ;; i + x ==> (a + (b + 1)i)
              (local.get $real)
              (%alloc-f64 (f64.add (local.get $i64) (f64.const 1)))))))))

;; asin x = -i ln(ix + sqrt(1 - x²))
(func $complex-asin (param $num i32) (result i32)
  (local $one-minus-x2 i32)
  (local $ix i32)

  (local.set $one-minus-x2 (call $num-core-sub
      (global.get $g-one)
      (call $complex-mul (local.get $num) (local.get $num))))

  (local.set $ix (%alloc-complex (call $num-core-neg
        (%cdr-l $num))
        (%car-l $num)))

  (return (call $complex-mul
      (%alloc-complex (global.get $g-zero) (%alloc-i64 (i64.const -1)))
      (call $complex-logn
        (call $num-core-add
          (local.get $ix)
          (call $complex-exp (call $complexify (call $num-core-mul ;; sqrt p = e^ (0.5·ln p)
                (%alloc-f64 (f64.const 0.5))
                (call $complex-logn (call $complexify
                    (local.get $one-minus-x2)))))))))))

(func $complexify (param $num i32) (result i32)
  (if (i32.eq (%get-type $num) (%complex-type)) (then
      (return (local.get $num))))
  (return (%alloc-complex (local.get $num) (global.get $g-zero))))

;; sinh x = ½(e^x - e^-x)
(func $sinh-impl (param $x i32) (result f64)
  (local $e-to-x f64)

  (local.set $e-to-x (call $exp-impl (local.get $x)))
  (return (f64.mul
    (f64.const 0.5)
    (f64.sub
      (local.get $e-to-x)
      (f64.div (f64.const 1) (local.get $e-to-x))))))

;; cosh x = ½(e^x + e^-x)
(func $cosh-impl (param $x i32) (result f64)
  (local $e-to-x f64)

  (local.set $e-to-x (call $exp-impl (local.get $x)))
  (return (f64.mul
    (f64.const 0.5)
    (f64.add
      (local.get $e-to-x)
      (f64.div (f64.const 1) (local.get $e-to-x))))))
