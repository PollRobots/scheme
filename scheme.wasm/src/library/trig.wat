(func $sin (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $num i32)

  (block $b_check (block $b_fail
      (local.set $num-args (call $list-len (local.get $args)))
      (br_if $b_fail (i32.ne (local.get $num-args) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (local.set $num (%car-l $args))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (call $inexact-impl (local.get $num)))

  (return (%alloc-f64 (call $sin-impl (f64.load offset=4 (local.get $num))))))

(func $cos (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $num i32)

  (block $b_check (block $b_fail
      (local.set $num-args (call $list-len (local.get $args)))
      (br_if $b_fail (i32.ne (local.get $num-args) (i32.const 1)))
      (br_if $b_fail (i32.eqz (call $all-numeric (local.get $args))))
      (local.set $num (%car-l $args))
      (br $b_check))

    (return (call $argument-error (local.get $args))))

  (local.set $num (call $inexact-impl (local.get $num)))

  (return (%alloc-f64 (call $cos-impl (f64.load offset=4 (local.get $num))))))

(%define %kPI ()        (f64.const 3.14159265358979323846))
(%define %kPIbyTwo ()   (f64.const 1.57079632679489661923))
(%define %kPIbyFour ()  (f64.const 0.78539816339744830962))
(%define %kTwoPI ()     (f64.const 6.28318530717958647693))

(func $sin-impl (param $v f64) (result f64)
  (local $t f64)

  (if (i32.or
      (call $ieee-inf? (local.get $v))
      (call $ieee-nan? (local.get $v))) (then
      (return (f64.const nan))))

  ;; if |x| <= π/4 --> call inner impl
  (if (f64.le (f64.abs (local.get $v)) (%kPIbyFour)) (then
    (return (call $sin-inner-impl (local.get $v)))))

  ;; sin(-x) = -sin(x)
  (if (f64.lt (local.get $v) (f64.const 0)) (then
    (return (f64.neg (call $sin-impl (f64.neg (local.get $v)))))))

  ;; large numbers will necessarily lose accuracy...
  ;; scale anything larger than 2π
  (if (f64.ge (local.get $v) (%kTwoPI)) (then
    ;; t = v / 2π
    (local.set $t (f64.div (local.get $v) (%kTwoPI)))
    ;; t = t - ⌊t⌋
    (local.set $t (f64.sub (local.get $t) (f64.floor (local.get $t))))
    ;; v = t·2π
    (return (call $sin-impl (f64.mul (local.get $t) (%kTwoPI))))))

  ;; sin(π + x) = -sin(x)
  (if (f64.gt (local.get $v) (%kPI)) (then
    (return (f64.neg (call $sin-impl (f64.sub (local.get $v) (%kPI)))))))

  ;; sin(π/2 + x) = sin(x)
  (if (f64.gt (local.get $v) (%kPIbyTwo)) (then
    (return (call $sin-impl (f64.sub (%kPI) (local.get $v))))))

  ;; sin(π/4 + x) = cos(π/2 - x)
  (if (f64.gt (local.get $v) (%kPIbyFour)) (then
    (return (call $cos-inner-impl (f64.sub (%kPIbyTwo) (local.get $v))))))

  ;; shouldn't be possible to get here
  (unreachable))

(func $cos-impl (param $v f64) (result f64)
  (local $t f64)

  (if (i32.or
      (call $ieee-inf? (local.get $v))
      (call $ieee-nan? (local.get $v))) (then
      (return (f64.const nan))))

  ;; if |x| <= π/4 --> call inner impl
  (if (f64.le (f64.abs (local.get $v)) (%kPIbyFour)) (then
    (return (call $cos-inner-impl (local.get $v)))))

  ;; cos(-x) = cos(x)
  (if (f64.lt (local.get $v) (f64.const 0)) (then
    (local.set $v (f64.neg (local.get $v)))))

  ;; large numbers will necessarily lose accuracy...
  ;; scale anything larger than 2π
  (if (f64.ge (local.get $v) (%kTwoPI)) (then
    ;; t = v / 2π
    (local.set $t (f64.div (local.get $v) (%kTwoPI)))
    ;; t = t - ⌊t⌋
    (local.set $t (f64.sub (local.get $t) (f64.floor (local.get $t))))
    ;; v = t·2π
    (return (call $cos-impl (f64.mul (local.get $t) (%kTwoPI))))))

  ;; cos(π + x) = -cos(x)
  (if (f64.gt (local.get $v) (%kPI)) (then
    (return (f64.neg (call $cos-impl (f64.sub (local.get $v) (%kPI)))))))

  ;; cos(π/2 + x) = -cos(π/2 - x)
  (if (f64.gt (local.get $v) (%kPIbyTwo)) (then
    (return (f64.neg (call $cos-impl (f64.sub (%kPI) (local.get $v)))))))

  ;; cos(π/4 + x) = sin(π/2 - x)
  (if (f64.gt (local.get $v) (%kPIbyFour)) (then
    (return (call $sin-inner-impl (f64.sub (%kPIbyTwo) (local.get $v))))))

  ;; shouldn't be possible to get here
  (unreachable))

(func $sin-inner-impl (param $x f64) (result f64)
  (local $x2 f64)
  (local $x3 f64)
  (local $x4 f64)
  (local $p f64)

  ;; sin(ε) = ε
  (if (f64.le (f64.abs (local.get $x)) (f64.const 1e-8)) (then
      (return (local.get $x))))

  ;; -1/3!
  (%define %kF3 ()  (f64.const -0.16666666666666666))
  ;; 1/5!
  (%define %kF5 ()  (f64.const 0.008333333333333333))
  ;; -1/7!
  (%define %kF7 ()  (f64.const -0.0001984126984126984))
  ;; 1/9!
  (%define %kF9 ()  (f64.const 0.0000027557319223985893))
  ;; -1/11!
  (%define %kF11 ()  (f64.const -2.505210838544172e-8))
  ;; 1/13!
  (%define %kF13 ()  (f64.const 1.6059043836821613e-10))
  ;; -1/15!
  (%define %kF15 ()  (f64.const -7.647163731819816e-13))

  ;; x²
  (local.set $x2 (f64.mul (local.get $x) (local.get $x)))
  ;; x³
  (local.set $x3 (f64.mul (local.get $x2) (local.get $x)))
  ;; x⁴
  (local.set $x4 (f64.mul (local.get $x2) (local.get $x2)))

  ;; sin(x) ≅ x - x³/3! + x⁵/5! - x⁷/7! + x⁹/9! - x¹¹/11! + x¹³/13!

  ;; p = 1/5! + x²(-1/7! + x²/9!) + x⁶(-1/11! + x²/13!)
  ;;   = 1/5! - x²/7! + x⁴/9! - x⁶/11! + x⁸/13!
  (local.set $p (f64.add
    (f64.add
      (%kF5) ;; 1/5
      (f64.mul
        (local.get $x2) ;; x²
        (f64.add
          (%kF7) ;; -1/7!
          (f64.mul (local.get $x2) (%kF9))))) ;; x²/9!
    (f64.mul
      (f64.mul (local.get $x2) (local.get $x4)) ;; x⁶
      (f64.add
        (%kF11) ;; -1/11
        (f64.mul (local.get $x2) (%kF13)))))) ;; x²/13!

  ;; <-- x - x³/3! + x⁵/5! - x⁷/7! + x⁹/9! - x¹¹/11! + x¹³/13!
  ;;     x - x³/3! + x⁵p
  ;;     x + x³(-1/3! + x²p)
  (return (f64.add
    (local.get $x) ;; x
    (f64.mul
      (local.get $x3) ;; x³
      (f64.add
        (%kF3) ;; -1/3!
        (f64.mul (local.get $x2) (local.get $p))))))) ;; x²p


(func $cos-inner-impl (param $x f64) (result f64)
  (local $x2 f64)
  (local $x4 f64)
  (local $x6 f64)
  (local $p f64)

  ;; -1/2!
  (%define %kF2 ()  (f64.const -0.5))
  ;; 1/4!
  (%define %kF4 ()  (f64.const 0.041666666666666664))
  ;; -1/6!
  (%define %kF6 ()  (f64.const -0.001388888888888889))
  ;; 1/8!
  (%define %kF8 ()  (f64.const 0.0000248015873015873))
  ;; -1/10!
  (%define %kF10 ()  (f64.const -2.755731922398589e-7))
  ;; 1/12!
  (%define %kF12 ()  (f64.const 2.08767569878681e-9))
  ;; -1/14!
  (%define %kF14 ()  (f64.const -1.147074559772972e-11))
  ;; 1/16!
  (%define %kF16 ()  (f64.const 4.779477332387385e-14))

  ;; cos(x) ≅ 1 - x²/2! + x⁴/4! - x⁶/6! + x⁸/8! - x¹⁰/10! + x¹²/12! - x¹⁴/14!

  ;; x²
  (local.set $x2 (f64.mul (local.get $x) (local.get $x)))
  ;; x⁴
  (local.set $x4 (f64.mul (local.get $x2) (local.get $x2)))
  ;; x6
  (local.set $x6 (f64.mul (local.get $x2) (local.get $x4)))

  ;; p = -1/6! + x²(1/8! - x²/10!) + x⁶(1/12 - x²/14!)
  (local.set $p (f64.add
      (f64.add
        (%kF6)                                    ;; -1/6!
        (f64.mul
          (local.get $x2)                         ;; x²
          (f64.add
            (%kF8)                                ;; 1/8!
            (f64.mul (local.get $x2) (%kF10)))))  ;; -x²/10!
      (f64.mul
        (local.get $x6)                           ;; x⁶
        (f64.add
          (%kF12)                                 ;; 1/12!
          (f64.mul (local.get $x2) (%kF14))))))   ;; -x²/14!

  ;; <-- 1 - x²/2! + x²(1/4! + x²p)
  ;; <-- 1 + x²(-1/2! + x²/4!) + x⁶p
  (return (f64.add
      (f64.add
        (f64.const 1)                                     ;; 1
        (f64.mul
          (local.get $x2)                                 ;; x²
          (f64.add
            (%kF2)                                        ;; -1/2!
            (f64.mul (local.get $x2) (%kF4)))))           ;; x²/4!
      (f64.mul (local.get $x6) (local.get $p)))))         ;; x⁶p
