(;
  Implements Grisu2

  http://florian.loitsch.com/publications (bench.tar.gz)

  Copyright (c) 2009 Florian Loitsch

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use,
    copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following
    conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
    OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.

;)

(%define %kPrecision () (i32.const 53))
(%define %kPrecisionMinus1 () (i64.const 52))
(%define %kExponentBias () (i32.const 0x3FF))
(%define %kHiddenBit () (i64.const 0x0010_0000_0000_0000)) ;; 1 << (kPrecision - 1)
(%define %kSignMask () (i64.const 0x8000_0000_0000_0000)) ;; 1 << 63
(%define %kInvSignMask () (i64.const 0x7FFF_FFFF_FFFF_FFFF)) ;; 1 << 63
(%define %kExponentMask () (i64.const 0x7FF0_0000_0000_0000)) ;; 0x755 << (kPrecision - 1)
(%define %kSignificandMask () (i64.const 0x000F_FFFF_FFFF_FFFF)) ;; kHiddenBit - 1
(%define %ieee-bits (%num) (i64.reinterpret_f64 (local.get %num)))

(func $ieee-exponent-bits (param $f f64) (result i64)
  ;; return (bits & kExponentMask) >> (kPrecision - 1);
  (i64.shr_u
    (i64.and (%ieee-bits $f) (%kExponentMask))
    (%kPrecisionMinus1)))

(func $ieee-significand-bits (param $f f64) (result i64)
  ;; return (bits & kSignificandMask);
  (i64.and (%ieee-bits $f) (%kSignificandMask)))

;; Returns true if the sign-bit is set.
(func $ieee-negative? (param $f f64) (result i32)
  ;; return (bits & kSignMask) != 0;
  (i64.ne (i64.and (%ieee-bits $f) (%kSignMask)) (i64.const 0)))

;; Returns true if this value is -0 or +0.
(func $ieee-zero? (param $f f64) (result i32)
  ;; return (bits & ~kSignMask) == 0;
  (i64.eqz (i64.and (%ieee-bits $f) (%kInvSignMask))))

;; Returns true if this value is denormal or 0.
(func $ieee-denormal? (param $f f64) (result i32)
  ;; return (bits & kExponentMask) == 0;
  (i64.eqz (i64.and (%ieee-bits $f) (%kExponentMask))))

;; Returns true if this value is NaN
(func $ieee-nan? (param $f f64) (result i32)
  (local $bits i64)
  (local.set $bits (%ieee-bits $f))
  ;; return (bits & kExponentMask) == kExponentMask && (bits & kSignificandMask) != 0;
  (i32.and
    (i64.eq
      (i64.and (local.get $bits) (%kExponentMask))
      (%kExponentMask))
    (i64.ne
      (i64.and (local.get $bits) (%kSignificandMask))
      (i64.const 0))))

;; Returns true if this value is -Inf or +Inf.
(func $ieee-inf? (param $f f64) (result i32)
  (local $bits i64)
  (local.set $bits (%ieee-bits $f))
  ;; return (bits & kExponentMask) == kExponentMask && (bits & kSignificandMask) == 0;
  (i32.and
    (i64.eq
      (i64.and (local.get $bits) (%kExponentMask))
      (%kExponentMask))
    (i64.eq
      (i64.and (local.get $bits) (%kSignificandMask))
      (i64.const 0))))

;; Returns this value with the sign-bit cleared.
(func $ieee-abs (param $f f64) (result f64)
  ;; return IEEEFloat(bits & ~kSignMask).value;
  (f64.abs (local.get $f)))

(;
struct Fp // f * 2^e
{
    static constexpr int const kPrecision = 64; // = q

    uint64_t f;
    int e;

    constexpr Fp() : f(0), e(0) {}
    constexpr Fp(uint64_t f_, int e_) : f(f_), e(e_) {}

    // Returns x - y.
    // Requires: x.e == y.e and x.f >= y.f
    static Fp Sub(Fp x, Fp y);

    // Returns x * y.
    // The result is rounded. (Only the upper q bits are returned.)
    static Fp Mul(Fp x, Fp y);

    // Normalize x such that the significand is >= 2^(q-1).
    // Requires: x.f != 0
    static Fp Normalize(Fp x);

    // Normalize x such that the result has the exponent E.
    // Requires: e >= x.e and the upper e - x.e bits of x.f must be zero.
    static Fp NormalizeTo(Fp x, int e);
};

an FP is
  f i64 -- unsigned fractional word
  e i32 -- signed exponent
;)

(%define %fp-f (%fp) (i64.load (local.get %fp)))
(%define %fp-e (%fp) (i32.load offset=8 (local.get %fp)))

;; constexpr Fp() : f(0), e(0) {}
(func $fp-new/0 (result i32)
  (local $fp i32)
  (local.set $fp (call $malloc (i32.const 12)))
  (i64.store (local.get $fp) (i64.const 0))
  (i32.store offset=8 (local.get $fp) (i32.const 0))
  (return (local.get $fp)))

;; constexpr Fp(uint64_t f_, int e_) : f(f_), e(e_) {}
(func $fp-new (param $f i64) (param $e i32) (result i32)
  (local $fp i32)
  (local.set $fp (call $malloc (i32.const 12)))
  (i64.store (local.get $fp) (local.get $f))
  (i32.store offset=8 (local.get $fp) (local.get $e))
  (return (local.get $fp)))

(func $fp-delete (param $fp i32)
  (call $malloc-free (local.get $fp)))

(func $fp-sub (param $x i32) (param $y i32) (result i32)
  ;; assert(x.e == y.e);
  (%assert (i32.eq (%fp-e $x) (%fp-e $y)))
  ;; assert(x.f >= y.f);
  (%assert (i64.ge_u (%fp-f $x) (%fp-f $y)))

  ;; return Fp(x.f - y.f, x.e);
  (return (call $fp-new
      (i64.sub (%fp-f $x) (%fp-f $y))
      (%fp-e $x))))

(func $fp-mul (param $x i32) (param $y i32) (result i32)
  ;;  Computes:
  ;;   f = round((x.f * y.f) / 2^q)
  ;;   e = x.e + y.e + q

  ;;  Emulate the 64-bit * 64-bit multiplication:
  ;;
  ;;  p = u * v
  ;;    = (u_lo + 2^32 u_hi) (v_lo + 2^32 v_hi)
  ;;    = (u_lo v_lo         ) + 2^32 ((u_lo v_hi         ) + (u_hi v_lo         )) + 2^64 (u_hi v_hi         )
  ;;    = (p0                ) + 2^32 ((p1                ) + (p2                )) + 2^64 (p3                )
  ;;    = (p0_lo + 2^32 p0_hi) + 2^32 ((p1_lo + 2^32 p1_hi) + (p2_lo + 2^32 p2_hi)) + 2^64 (p3                )
  ;;    = (p0_lo             ) + 2^32 (p0_hi + p1_lo + p2_lo                      ) + 2^64 (p1_hi + p2_hi + p3)
  ;;    = (p0_lo             ) + 2^32 (Q                                          ) + 2^64 (H                 )
  ;;    = (p0_lo             ) + 2^32 (Q_lo + 2^32 Q_hi                           ) + 2^64 (H                 )
  ;;
  ;;  (Since Q might be larger than 2^32 - 1)
  ;;
  ;;    = (p0_lo + 2^32 Q_lo) + 2^64 (Q_hi + H)
  ;;
  ;;  (Q_hi + H does not overflow a 64-bit int)
  ;;
  ;;    = p_lo + 2^64 p_hi
  (local $u_lo i64)
  (local $u_hi i64)
  (local $v_lo i64)
  (local $v_hi i64)
  (local $p0 i64)
  (local $p1 i64)
  (local $p2 i64)
  (local $p3 i64)
  (local $p0_hi i64)
  (local $p1_lo i64)
  (local $p1_hi i64)
  (local $p2_lo i64)
  (local $p2_hi i64)
  (local $Q i64)
  (local $h i64)

  ;; uint64_t const u_lo = x.f & 0xFFFFFFFF;
  (local.set $u_lo (i64.and (%fp-f $x) (i64.const 0xFFFF_FFFF)))
  ;; uint64_t const u_hi = x.f >> 32;
  (local.set $u_hi (i64.shr_u (%fp-f $x) (i64.const 32)))
  ;; uint64_t const v_lo = y.f & 0xFFFFFFFF;
  (local.set $v_lo (i64.and (%fp-f $y) (i64.const 0xFFFF_FFFF)))
  ;; uint64_t const v_hi = y.f >> 32;
  (local.set $v_hi (i64.shr_u (%fp-f $y) (i64.const 32)))

  ;; uint64_t const p0 = u_lo * v_lo;
  (local.set $p0 (i64.mul (local.get $u_lo) (local.get $v_lo)))
  ;; uint64_t const p1 = u_lo * v_hi;
  (local.set $p1 (i64.mul (local.get $u_lo) (local.get $v_hi)))
  ;; uint64_t const p2 = u_hi * v_lo;
  (local.set $p2 (i64.mul (local.get $u_hi) (local.get $v_lo)))
  ;; uint64_t const p3 = u_hi * v_hi;
  (local.set $p3 (i64.mul (local.get $u_hi) (local.get $v_hi)))

  ;; uint64_t const p0_hi = p0 >> 32;
  (local.set $p0_hi (i64.shr_u (local.get $p0) (i64.const 32)))
  ;; uint64_t const p1_lo = p1 & 0xFFFFFFFF;
  (local.set $p1_lo (i64.and (local.get $p1) (i64.const 0xFFFF_FFFF)))
  ;; uint64_t const p1_hi = p1 >> 32;
  (local.set $p1_hi (i64.shr_u (local.get $p1) (i64.const 32)))
  ;; uint64_t const p2_lo = p2 & 0xFFFFFFFF;
  (local.set $p2_lo (i64.and (local.get $p2) (i64.const 0xFFFF_FFFF)))
  ;; uint64_t const p2_hi = p2 >> 32;
  (local.set $p2_hi (i64.shr_u (local.get $p2) (i64.const 32)))

  ;; uint64_t Q = p0_hi + p1_lo + p2_lo;
  (local.set $Q (i64.add
      (i64.add
        (local.get $p0_hi)
        (local.get $p1_lo))
      (local.get $p2_lo)))

  (;
    The full product might now be computed as

    p_hi = p3 + p2_hi + p1_hi + (Q >> 32)
    p_lo = p0_lo + (Q << 32)

    But in this particular case here, the full p_lo is not required.
    Effectively we only need to add the highest bit in p_lo to p_hi (and
    Q_hi + 1 does not overflow).
  ;)

  ;; Q += uint64_t{1} << (63 - 32); // round, ties up
  (local.set $Q (i64.add
    (local.get $Q)
    (i64.shl (i64.const 1) (i64.const 31))))

  ;; uint64_t const h = p3 + p2_hi + p1_hi + (Q >> 32);
  (local.set $h (i64.add
      (i64.add
        (i64.add
          (local.get $p3)
          (local.get $p2_hi))
        (local.get $p1_hi))
      (i64.shr_u (local.get $Q) (i64.const 32))))

  ;; return Fp(h, x.e + y.e + 64);
  (return (call $fp-new
      (local.get $h)
      (i32.add
        (i32.add (%fp-e $x) (%fp-e $y))
        (i32.const 64)))))

(func $fp-normalize (param $x i32) (result i32)
  (local $leading-zeros i64)
  ;; int const leading_zeros = CountLeadingZeros_64(x.f);
  (local.set $leading-zeros (i64.clz (%fp-f $x)))
  ;; return Fp(x.f << leading_zeros, x.e - leading_zeros);
  (return (call $fp-new
      (i64.shl (%fp-f $x) (local.get $leading-zeros))
      (i32.sub (%fp-e $x) (i32.wrap_i64 (local.get $leading-zeros))))))

(func $fp-normalize-to (param $x i32) (param $e i32) (result i32)
  (local $delta i64)
  ;; int const delta = x.e - e;
  (local.set $delta (i64.extend_i32_s (i32.sub (%fp-e $x) (local.get $e))))

  ;; assert(delta >= 0);
  (%assert (i64.ge_s (local.get $delta) (i64.const 0)))
  ;; assert(((x.f << delta) >> delta) == x.f);
  (%assert (i64.eq
      (i64.shr_u
        (i64.shl (%fp-f $x) (local.get $delta))
        (local.get $delta))
      (%fp-f $x)))

  ;; return Fp(x.f << delta, e);
  (return (call $fp-new
      (i64.shl (%fp-f $x) (local.get $delta))
      (local.get $e))))

(;
struct BoundedFp {
    Fp w;
    Fp minus;
    Fp plus;
};
;)

(%define %bfp-w (%bfp) (local.get %bfp))
(%define %bfp-minus (%bfp) (i32.add (local.get %bfp) (i32.const 12)))
(%define %bfp-plus (%bfp) (i32.add (local.get %bfp) (i32.const 24)))

;; copies and packs the values of w minus and plus into one 36 byte allocation
(func $bfp-new (param $w i32) (param $minus i32) (param $plus i32) (result i32)
  (local $bfp i32)

  (local.set $bfp (call $malloc (i32.const 36)))
  (i64.store offset=0 (local.get $bfp) (%fp-f $w))
  (i32.store offset=8 (local.get $bfp) (%fp-e $w))
  (i64.store offset=12 (local.get $bfp) (%fp-f $minus))
  (i32.store offset=20 (local.get $bfp) (%fp-e $minus))
  (i64.store offset=24 (local.get $bfp) (%fp-f $plus))
  (i32.store offset=32 (local.get $bfp) (%fp-e $plus))

  (return (local.get $bfp)))

(func $bfp-delete (param $bfp i32)
  (call $malloc-free (local.get $bfp)))

;;
;; Computes the boundaries m- and m+ of the floating-point value v.
;;
;; Determine v- and v+, the floating-point predecessor and successor if v,
;; respectively.
;;
;;      v- = v - 2^e        if f != 2^(p-1) or e != e_min                    (A)
;;         = v - 2^(e-1)    if f == 2^(p-1) and e > e_min                    (B)
;;
;;      v+ = v + 2^e
;;
;; Let m- = (v- + v) / 2 and m+ = (v + v+) / 2. All real numbers _strictly_
;; between m- and m+ round to v, regardless of how the input rounding algorithm
;; breaks ties.
;;
;;      ---+-------------+-------------+-------------+-------------+---      (A)
;;         v-            m-            v             m+            v+
;;
;;      -----------------+------+------+-------------+-------------+---      (B)
;;                       v-     m-     v             m+            v+
;;
;; Note that m- and m+ are (by definition) not representable with precision p
;; and we therefore need some extra bits of precision.
;;
(func $compute-bounded-fp (param $v-ieee f64) (result i32)
  (local $E i64)
  (local $F i64)
  (local $v i32)
  (local $v-norm i32)
  (local $m_plus i32)
  (local $m_minus i32)
  (local $plus i32)
  (local $minus i32)
  (local $res i32)

  (;
    Convert the IEEE representation into a DiyFp.

    If v is denormal:
         value = 0.F * 2^(1 - E_bias) = (          F) * 2^(1 - E_bias - (p-1))
    If v is normalized:
         value = 1.F * 2^(E - E_bias) = (2^(p-1) + F) * 2^(E - E_bias - (p-1))
  ;)

  ;; IEEEType const v_ieee_bits(v_ieee);
  ;; uint64_t const E = v_ieee_bits.ExponentBits(); // biased exponent
  (local.set $E (call $ieee-exponent-bits (local.get $v-ieee)))
  ;; uint64_t const F = v_ieee_bits.SignificandBits();
  (local.set $F (call $ieee-significand-bits (local.get $v-ieee)))

  ;; constexpr int const kBias = IEEEType::kExponentBias + (IEEEType::kPrecision - 1);
  (%define %kBias () (i32.const 0x433)) ;; 0x3FF + 53 - 1

  ;; Fp const v = (E == 0) // denormal?
  ;;     ? Fp(F, 1 - kBias)
  ;;     : Fp(IEEEType::kHiddenBit + F, static_cast<int>(E) - kBias);
  (if (i64.eqz (local.get $E))
      (then (local.set $v (call $fp-new (local.get $F) (i32.const -0x432))))
      (else (local.set $v (call $fp-new
            (i64.add (%kHiddenBit) (local.get $F))
            (i32.sub (i32.wrap_i64 (local.get $E)) (%kBias))))))


  (;
    v+ = v + 2^e = (f + 1) * 2^e and therefore

         m+ = (v + v+) / 2
            = (2*f + 1) * 2^(e-1)
  ;)
  ;; Fp const m_plus = Fp(2*v.f + 1, v.e - 1);
  (local.set $m_plus (call $fp-new
      (i64.or (i64.shl (%fp-f $v) (i64.const 1)) (i64.const 1))
      (i32.sub (%fp-e $v) (i32.const 1))))

  (;
    If f != 2^(p-1), then v- = v - 2^e = (f - 1) * 2^e and

         m- = (v- + v) / 2
            = (2*f - 1) * 2^(e-1)

    If f = 2^(p-1), then the next smaller _normalized_ floating-point number
    is actually v- = v - 2^(e-1) = (2^p - 1) * 2^(e-1) and therefore

         m- = (4*f - 1) * 2^(e-2)

    The exception is the smallest normalized floating-point number
    v = 2^(p-1) * 2^e_min. In this case the predecessor is the largest
    denormalized floating-point number: v- = (2^(p-1) - 1) * 2^e_min and then

         m- = (2*f - 1) * 2^(e-1)

    If v is denormal, v = f * 2^e_min and v- = v - 2^e = (f - 1) * 2^e and
    again

         m- = (2*f - 1) * 2^(e-1)

    Note: 0 is not a valid input for Grisu and in case v is denormal:
    f != 2^(p-1).

    For IEEE floating-point numbers not equal to 0, the condition f = 2^(p-1)
    is equivalent to F = 0, and for the smallest normalized number E = 1.
    For denormals E = 0 (and F != 0).
  ;)
  ;; Fp const m_minus = (F == 0 && E > 1)
  ;;     ? Fp(4*v.f - 1, v.e - 2)
  ;;     : Fp(2*v.f - 1, v.e - 1);
  (if (i32.and
        (i64.eqz (local.get $F))
        (i64.gt_u (local.get $E) (i64.const 1)))
      (then (local.set $m_minus (call $fp-new
          (i64.sub (i64.shl (%fp-f $v) (i64.const 2)) (i64.const 1))
          (i32.sub (%fp-e $v) (i32.const 2)))))
      (else (local.set $m_minus (call $fp-new
          (i64.sub (i64.shl (%fp-f $v) (i64.const 1)) (i64.const 1))
          (i32.sub (%fp-e $v) (i32.const 1))))))

  (;
    Determine the normalized w+ = m+.
  ;)
  ;; Fp const plus = Fp::Normalize(m_plus);
  (local.set $plus (call $fp-normalize (local.get $m_plus)))

  (;
    Determine w- = m- such that e_(w-) = e_(w+).
  ;)
  ;; Fp const minus = Fp::NormalizeTo(m_minus, plus.e);
  (local.set $minus (call $fp-normalize-to (local.get $m_minus) (%fp-e $plus)))

  ;; //assert(plus.f > minus.f);
  ;; //assert(plus.f - minus.f >= 3 * (uint64_t{1} << (Fp::kPrecision - IEEEType::kPrecision - 2)));

  ;; return {Fp::Normalize(v), minus, plus};
  (local.set $v-norm (call $fp-normalize (local.get $v)))
  (local.set $res (call $bfp-new
      (local.get $v-norm)
      (local.get $minus)
      (local.get $plus)))

  (call $fp-delete (local.get $v))
  (call $fp-delete (local.get $v-norm))
  (call $fp-delete (local.get $plus))
  (call $fp-delete (local.get $minus))
  (call $fp-delete (local.get $m_plus))
  (call $fp-delete (local.get $m_minus))

  (return (local.get $res)))

(;
  Given a (normalized) floating-point number v and its neighbors m- and m+

       ---+---------------------------+---------------------------+---
          m-                          v                           m+

  Grisu first scales the input number w, and its boundaries w- and w+, by an
  approximate power-of-ten c ~= 10^-k (which needs to be precomputed using
  high-precision arithmetic and stored in a table) such that the exponent of
  the products lies within a certain range [alpha, gamma]. It then remains to
  produce the decimal digits of a DiyFp number M = f * 2^e, where
  alpha <= e <= gamma.

  The choice of alpha and gamma determines the digit generation procedure and
  the size of the look-up table (or vice versa...) and depends on the
  extended precision q.

  In other words, given normalized w, Grisu needs to find a (normalized) cached
  power-of-ten c, such that the exponent of the product c * w = f * 2^e
  satisfies (Definition 3.2)

       alpha <= e = e_c + e_w + q <= gamma

  or

       f_c * f_w * 2^alpha <= f_c 2^(e_c) * f_w 2^(e_w) * 2^q
                           <= f_c * f_w * 2^gamma

  Since c and w are normalized, i.e. 2^(q-1) <= f < 2^q, this implies

       2^(q-1) * 2^(q-1) * 2^alpha <= c * w * 2^q < 2^q * 2^q * 2^gamma

  or

       2^(q - 2 + alpha) <= c * w < 2^(q + gamma)

  The distance (gamma - alpha) should be as large as possible in order to make
  the table as small as possible, but the digit generation procedure should
  still be efficient.

  Assume q = 64 and e < 0. The idea is to cut the number c * w = f * 2^e into
  two parts, which can be processed independently: An integral part p1, and a
  fractional part p2:

       f * 2^e = ( (f div 2^-e) * 2^-e + (f mod 2^-e) ) * 2^e
               = (f div 2^-e) + (f mod 2^-e) * 2^e
               = p1 + p2 * 2^e

  The conversion of p1 into decimal form requires some divisions and modulos by
  a power-of-ten. These operations are faster for 32-bit than for 64-bit
  integers, so p1 should ideally fit into a 32-bit integer. This be achieved by
  choosing

       -e >= 32   or   e <= -32 := gamma

  In order to convert the fractional part

       p2 * 2^e = d[-1] / 10^1 + d[-2] / 10^2 + ... + d[-k] / 10^k

  into decimal form, the fraction is repeatedly multiplied by 10 and the digits
  d[-i] are extracted in order:

       (10 * p2) div 2^-e = d[-1]
       (10 * p2) mod 2^-e = d[-2] / 10^1 + ... + d[-k] / 10^(k-1)

  The multiplication by 10 must not overflow. For this it is sufficient to have

       10 * p2 < 16 * p2 = 2^4 * p2 <= 2^64.

  Since p2 = f mod 2^-e < 2^-e one may choose

       -e <= 60   or   e >= -60 := alpha

  Different considerations may lead to different digit generation procedures
  and different values of alpha and gamma...
;)
;; constexpr int const kAlpha = -60;
(%define %kAlpha () (i32.const -60))
;; constexpr int const kGamma = -32;
(%define %kGamma () (i32.const -32))

(;
  Grisu needs to find a(normalized) cached power-of-ten c, such that the
  exponent of the product c * w = f * 2^e satisfies (Definition 3.2)

       alpha <= e = e_c + e_w + q <= gamma

  For IEEE double precision floating-point numbers v converted into a DiyFp's
  w = f * 2^e,

       e >= -1022      (min IEEE exponent)
            -52        (IEEE significand size)
            -52        (possibly normalize denormal IEEE numbers)
            -11        (normalize the DiyFp)
          = -1137

  and

       e <= +1023      (max IEEE exponent)
            -52        (IEEE significand size)
            -11        (normalize the DiyFp)
          = 960

  (For IEEE single precision the exponent range is [-196, 80].)

  Now

       alpha <= e_c + e + q <= gamma
           ==> f_c * 2^alpha <= c * 2^e * 2^q

  and since the c's are normalized, 2^(q-1) <= f_c,

           ==> 2^(q - 1 + alpha) <= c * 2^(e + q)
           ==> 2^(alpha - e - 1) <= c

  If c were an exakt power of ten, i.e. c = 10^k, one may determine k as

       k = ceil( log_10( 2^(alpha - e - 1) ) )
         = ceil( (alpha - e - 1) * log_10(2) )

  (From the paper)
  "In theory the result of the procedure could be wrong since c is rounded, and
  the computation itself is approximated [...]. In practice, however, this
  simple function is sufficient."

  The difference of the decimal exponents of adjacent table entries must be
  <= floor( (gamma - alpha) * log_10(2) ) = 8.
;)

(;
struct CachedPower { // c = f * 2^e ~= 10^k
    uint64_t f;
    int e;
    int k;
};
;)

(%define %cp.f (%cp) (i64.load offset=0 (local.get %cp)))
(%define %cp.e (%cp) (i32.load offset=8 (local.get %cp)))
(%define %cp.k (%cp) (i32.load offset=12 (local.get %cp)))

(global $g-grisu-cached-powers (mut i32) (i32.const 0))
(%define %kCachedPowersSize () (i32.const 79))

(func $grisu-init
  (local $ptr i32)

  (global.set $g-grisu-cached-powers (call $calloc (%kCachedPowersSize) (i32.const 16)))
  (local.set $ptr (global.get $g-grisu-cached-powers))

  (%define %store-cache-entry (%f %e %k)
    (i64.store offset=0 (local.get $ptr) (i64.const %f))
    (i32.store offset=8 (local.get $ptr) (i32.const %e))
    (i32.store offset=12 (local.get $ptr) (i32.const %k))
    (%plus-eq $ptr 16))

  (%store-cache-entry 0xAB70FE17C79AC6CA -1060 -300) ;; -1060 + 960 + 64 = -36
  (%store-cache-entry 0xFF77B1FCBEBCDC4F -1034 -292)
  (%store-cache-entry 0xBE5691EF416BD60C -1007 -284)
  (%store-cache-entry 0x8DD01FAD907FFC3C  -980 -276)
  (%store-cache-entry 0xD3515C2831559A83  -954 -268)
  (%store-cache-entry 0x9D71AC8FADA6C9B5  -927 -260)
  (%store-cache-entry 0xEA9C227723EE8BCB  -901 -252)
  (%store-cache-entry 0xAECC49914078536D  -874 -244)
  (%store-cache-entry 0x823C12795DB6CE57  -847 -236)
  (%store-cache-entry 0xC21094364DFB5637  -821 -228)
  (%store-cache-entry 0x9096EA6F3848984F  -794 -220)
  (%store-cache-entry 0xD77485CB25823AC7  -768 -212)
  (%store-cache-entry 0xA086CFCD97BF97F4  -741 -204)
  (%store-cache-entry 0xEF340A98172AACE5  -715 -196)
  (%store-cache-entry 0xB23867FB2A35B28E  -688 -188)
  (%store-cache-entry 0x84C8D4DFD2C63F3B  -661 -180)
  (%store-cache-entry 0xC5DD44271AD3CDBA  -635 -172)
  (%store-cache-entry 0x936B9FCEBB25C996  -608 -164)
  (%store-cache-entry 0xDBAC6C247D62A584  -582 -156)
  (%store-cache-entry 0xA3AB66580D5FDAF6  -555 -148)
  (%store-cache-entry 0xF3E2F893DEC3F126  -529 -140)
  (%store-cache-entry 0xB5B5ADA8AAFF80B8  -502 -132)
  (%store-cache-entry 0x87625F056C7C4A8B  -475 -124)
  (%store-cache-entry 0xC9BCFF6034C13053  -449 -116)
  (%store-cache-entry 0x964E858C91BA2655  -422 -108)
  (%store-cache-entry 0xDFF9772470297EBD  -396 -100)
  (%store-cache-entry 0xA6DFBD9FB8E5B88F  -369  -92)
  (%store-cache-entry 0xF8A95FCF88747D94  -343  -84)
  (%store-cache-entry 0xB94470938FA89BCF  -316  -76)
  (%store-cache-entry 0x8A08F0F8BF0F156B  -289  -68)
  (%store-cache-entry 0xCDB02555653131B6  -263  -60)
  (%store-cache-entry 0x993FE2C6D07B7FAC  -236  -52)
  (%store-cache-entry 0xE45C10C42A2B3B06  -210  -44)
  (%store-cache-entry 0xAA242499697392D3  -183  -36) ;; -183 + 80 + 64 = -39
  (%store-cache-entry 0xFD87B5F28300CA0E  -157  -28) ;;
  (%store-cache-entry 0xBCE5086492111AEB  -130  -20) ;;
  (%store-cache-entry 0x8CBCCC096F5088CC  -103  -12) ;;
  (%store-cache-entry 0xD1B71758E219652C   -77   -4) ;;
  (%store-cache-entry 0x9C40000000000000   -50    4) ;;
  (%store-cache-entry 0xE8D4A51000000000   -24   12) ;;
  (%store-cache-entry 0xAD78EBC5AC620000     3   20) ;;
  (%store-cache-entry 0x813F3978F8940984    30   28) ;;
  (%store-cache-entry 0xC097CE7BC90715B3    56   36) ;;
  (%store-cache-entry 0x8F7E32CE7BEA5C70    83   44) ;; 83 - 196 + 64 = -49
  (%store-cache-entry 0xD5D238A4ABE98068   109   52)
  (%store-cache-entry 0x9F4F2726179A2245   136   60)
  (%store-cache-entry 0xED63A231D4C4FB27   162   68)
  (%store-cache-entry 0xB0DE65388CC8ADA8   189   76)
  (%store-cache-entry 0x83C7088E1AAB65DB   216   84)
  (%store-cache-entry 0xC45D1DF942711D9A   242   92)
  (%store-cache-entry 0x924D692CA61BE758   269  100)
  (%store-cache-entry 0xDA01EE641A708DEA   295  108)
  (%store-cache-entry 0xA26DA3999AEF774A   322  116)
  (%store-cache-entry 0xF209787BB47D6B85   348  124)
  (%store-cache-entry 0xB454E4A179DD1877   375  132)
  (%store-cache-entry 0x865B86925B9BC5C2   402  140)
  (%store-cache-entry 0xC83553C5C8965D3D   428  148)
  (%store-cache-entry 0x952AB45CFA97A0B3   455  156)
  (%store-cache-entry 0xDE469FBD99A05FE3   481  164)
  (%store-cache-entry 0xA59BC234DB398C25   508  172)
  (%store-cache-entry 0xF6C69A72A3989F5C   534  180)
  (%store-cache-entry 0xB7DCBF5354E9BECE   561  188)
  (%store-cache-entry 0x88FCF317F22241E2   588  196)
  (%store-cache-entry 0xCC20CE9BD35C78A5   614  204)
  (%store-cache-entry 0x98165AF37B2153DF   641  212)
  (%store-cache-entry 0xE2A0B5DC971F303A   667  220)
  (%store-cache-entry 0xA8D9D1535CE3B396   694  228)
  (%store-cache-entry 0xFB9B7CD9A4A7443C   720  236)
  (%store-cache-entry 0xBB764C4CA7A44410   747  244)
  (%store-cache-entry 0x8BAB8EEFB6409C1A   774  252)
  (%store-cache-entry 0xD01FEF10A657842C   800  260)
  (%store-cache-entry 0x9B10A4E5E9913129   827  268)
  (%store-cache-entry 0xE7109BFBA19C0C9D   853  276)
  (%store-cache-entry 0xAC2820D9623BF429   880  284)
  (%store-cache-entry 0x80444B5E7AA7CF85   907  292)
  (%store-cache-entry 0xBF21E44003ACDD2D   933  300)
  (%store-cache-entry 0x8E679C2F5E44FF8F   960  308)
  (%store-cache-entry 0xD433179D9C8CB841   986  316)
  (%store-cache-entry 0x9E19DB92B4E31BA9  1013  324) ;; 1013 - 1137 + 64 = -60
)

(func $grisu-cleanup
  (if (global.get $g-grisu-cached-powers) (then
      (call $malloc-free (global.get $g-grisu-cached-powers))
      (global.set $g-grisu-cached-powers (i32.const 0)))))

;; Returns a cached power-of-ten c, such that alpha <= e_c + e + q <= gamma.
(func $get-cached-power-for-binary-exponent (param $e i32) (result i32)
  (local $f i32)
  (local $k i32)
  (local $index i32)
  (local $cached i32)

  ;; NB:
  ;; Actually this function returns c, such that -60 <= e_c + e + 64 <= -34.

  ;; constexpr int const kCachedPowersSize = 79;
  ;; constexpr int const kCachedPowersMinDecExp = -300;
  (%define %kCachedPowersMinDecExp () (i32.const 300))

  (;
    This computation gives exactly the same results for k as

          k = ceil((kAlpha - e - 1) * 0.30102999566398114)

    for |e| <= 1500, but doesn't require floating-point operations.
    NB: log_10(2) ~= 78913 / 2^18
  ;)
  ;; assert(e >= -1500);
  (%assert (i32.ge_s (local.get $e) (i32.const -1500)))
  ;; assert(e <=  1500);
  (%assert (i32.le_s (local.get $e) (i32.const 1500)))
  ;; int const f = kAlpha - e - 1;
  (local.set $f (i32.sub (i32.sub (%kAlpha) (local.get $e)) (i32.const 1)))
  ;; int const k = (f * 78913) / (1 << 18) + (f > 0);
  (local.set $k (i32.add
      (i32.div_s
        (i32.mul (local.get $f) (i32.const 78913))
        (i32.const 0x4_0000))
      (i32.gt_s (local.get $f) (i32.const 0))))

  ;; int const index = (-kCachedPowersMinDecExp + k + (8 - 1)) / 8;
  (local.set $index (i32.shr_s
    (i32.add (i32.add (%kCachedPowersMinDecExp) (local.get $k)) (i32.const 7))
    (i32.const 3)))
  ;; assert(index >= 0);
  (%assert (i32.ge_s (local.get $index) (i32.const 0)))
  ;; assert(index < kCachedPowersSize);
  (%assert (i32.le_s (local.get $index) (%kCachedPowersSize)))
  ;; static_cast<void>(kCachedPowersSize); // Fix warning.

  ;; CachedPower const cached = kCachedPowers[index];
  (local.set $cached (i32.add
      (global.get $g-grisu-cached-powers)
      (i32.shl (local.get $index) (i32.const 4))))
  ;; assert(kAlpha <= cached.e + e + 64);
  (%assert (i32.le_s
      (%kAlpha)
      (i32.add (i32.add (%cp.e $cached) (local.get $e)) (i32.const 64))))
  ;; assert(kGamma >= cached.e + e + 64);
  (%assert (i32.ge_s
      (%kGamma)
      (i32.add (i32.add (%cp.e $cached) (local.get $e)) (i32.const 64))))

  ;; // XXX:
  ;; // cached.k = kCachedPowersMinDecExp + 8*index

  ;; return cached;
  (return (local.get $cached)))

;; For n != 0, returns k, such that pow10 := 10^(k-1) <= n < 10^k.
;; For n == 0, returns 1 and sets pow10 := 1.
(func $init-kappa (param $n i32) (result i64)
  (%define %return-kappa-pair (%pow10 %r)
    (if (i32.ge_u (local.get $n) (i32.const %pow10))
      (then (return (%pack-64 (i32.const %pow10) (i32.const %r))))))

  ;; if (n >= 1000000000) { pow10 = 1000000000; return 10; }
  (%return-kappa-pair 1000000000 10)
  ;; if (n >=  100000000) { pow10 =  100000000; return  9; }
  (%return-kappa-pair  100000000  9)
  ;; if (n >=   10000000) { pow10 =   10000000; return  8; }
  (%return-kappa-pair   10000000  8)
  ;; if (n >=    1000000) { pow10 =    1000000; return  7; }
  (%return-kappa-pair    1000000  7)
  ;; if (n >=     100000) { pow10 =     100000; return  6; }
  (%return-kappa-pair     100000  6)
  ;; if (n >=      10000) { pow10 =      10000; return  5; }
  (%return-kappa-pair      10000  5)
  ;; if (n >=       1000) { pow10 =       1000; return  4; }
  (%return-kappa-pair       1000  4)
  ;; if (n >=        100) { pow10 =        100; return  3; }
  (%return-kappa-pair        100  3)
  ;; if (n >=         10) { pow10 =         10; return  2; }
  (%return-kappa-pair         10  2)

  ;; pow10 = 1; return 1;
  (return (%pack-64 (i32.const 1) (i32.const 1))))


(func $grisu-2-round
  (param $buf i32)
  (param $len i32)
  (param $dist i64)
  (param $delta i64)
  (param $rest i64)
  (param $ten-k i64)

  (local $ptr i32)
  ;; dist, delta, rest and ten_k all are the significands of
  ;; floating-point numbers with an exponent e.

  ;; assert(len >= 1);
  (%assert (i32.ge_s (local.get $len) (i32.const 1)))
  ;; assert(dist <= delta);
  (%assert (i64.le_u (local.get $dist) (local.get $delta)))
  ;; assert(rest <= delta);
  (%assert (i64.le_u (local.get $rest) (local.get $delta)))
  ;; assert(ten_k > 0);
  (%assert (i64.gt_u (local.get $ten-k) (i64.const 0)))

  (;
                  <--------------------------- delta ---->
                                     <---- dist --------->
    --------------[------------------+-------------------]--------------
                  w-                 w                   w+

                                     10^k
                                   <------>
                                          <---- rest ---->
    --------------[------------------+----+--------------]--------------
                                     w    V
                                          = buf * 10^k

    ten_k represents a unit-in-the-last-place in the decimal representation
    stored in buf.
    Decrement buf by ten_k while this takes buf closer to w.
  ;)

  ;; ptr = &buf[len - 1]
  (local.set $ptr (i32.sub
      (i32.add (local.get $buf) (local.get $len))
      (i32.const 1)))

  (block $b_end (loop $b_start
  ;; while (rest < dist
      (br_if $b_end (i64.ge_u (local.get $rest) (local.get $dist)))
  ;;     && delta - rest >= ten_k
      (br_if $b_end (i64.lt_u
          (i64.sub (local.get $delta) (local.get $rest))
          (local.get $ten-k)))
  ;;     && (rest + ten_k < dist || dist - rest > rest + ten_k - dist))
      (block $b_or
        (br_if $b_or (i64.lt_u
              (i64.add (local.get $rest) (local.get $ten-k))
              (local.get $dist)))
        (br_if $b_or (i64.gt_u
              (i64.sub (local.get $dist) (local.get $rest))
              (i64.sub
                (i64.add (local.get $rest) (local.get $ten-k))
                (local.get $dist))))
        (br $b_end))

      ;; assert(buf[len - 1] != '0');
      (%assert (i32.ne (i32.load8_s (local.get $ptr)) (i32.const 0x30)))
      ;; buf[len - 1]--;
      (i32.store8
        (local.get $ptr)
        (i32.sub (i32.load8_s (local.get $ptr)) (i32.const 1)))
      ;; rest += ten_k;
      (local.set $rest (i64.add (local.get $rest) (local.get $ten-k)))

      (br $b_start))))

;; inline void Grisu2DigitGen(char* buffer, int& length, int& decimal_exponent, Fp M_minus, Fp w, Fp M_plus)
(func $grisu-2-digit-gen
  (param $buffer i32)  ;; char*
  (param $length i32)  ;; int&
  (param $decimal-exponent i32)  ;; int&
  (param $m-minus i32) ;; Fp
  (param $w i32) ;; Fp
  (param $m-plus i32) ;; Fp
  (result i64)        ;; (packed decimal-exponent length)
                      ;; This allows them to be handled equivalent to references

  (local $temp i32)
  (local $temp64 i64)
  (local $delta i64)  ;; uint64
  (local $dist i64)   ;; uint64
  (local $one-f i64)  ;; uint64
  (local $one-e i32)  ;; int32
  (local $minus-one-e i64)
  (local $p1 i32)     ;; uint32
  (local $p2 i64)     ;; uint64
  (local $k i32)      ;; int32
  (local $pow10 i32)  ;; uint32
  (local $n i32)      ;; int32
  (local $d i32)      ;; uint32
  (local $r i32)      ;; uint32
  (local $dd i64)     ;; uint64
  (local $rr i64)     ;; uint64
  (local $rest i64)   ;; uint64
  (local $ten_n i64)  ;; uint64
  (local $m i32)      ;; int32

  ;; static_assert(kAlpha >= -60, "invalid parameter");
  ;; static_assert(kGamma <= -32, "invalid parameter");

  ;; assert(M_plus.e >= kAlpha);
  (%assert (i32.ge_s (%fp-e $m-plus) (%kAlpha)))
  ;; assert(M_plus.e <= kGamma);
  (%assert (i32.le_s (%fp-e $m-plus) (%kGamma)))

  ;; uint64_t delta = Fp::Sub(M_plus, M_minus).f; // (significand of (w+ - w-), implicit exponent is e)
  (local.set $temp (call $fp-sub (local.get $m-plus) (local.get $m-minus)))
  (local.set $delta (%fp-f $temp))
  (call $fp-delete (local.get $temp))
  ;; uint64_t dist  = Fp::Sub(M_plus, w      ).f; // (significand of (w+ - w ), implicit exponent is e)
  (local.set $temp (call $fp-sub (local.get $m-plus) (local.get $w)))
  (local.set $dist (%fp-f $temp))
  (call $fp-delete (local.get $temp))

  (;
   ;               <--------------------------- delta ---->
   ;                                  <---- dist --------->
   ; --------------[------------------+-------------------]--------------
   ;               w-                 w                   w+

   ; Split w+ = f * 2^e into two parts p1 and p2 (note: e < 0):

   ;      w+ = f * 2^e
   ;         = ((f div 2^-e) * 2^-e + (f mod 2^-e)) * 2^e
   ;         = ((p1        ) * 2^-e + (p2        )) * 2^e
   ;         = p1 + p2 * 2^e
   ;)

  ;; Fp const one = Fp(uint64_t{1} << -M_plus.e, M_plus.e);
  (local.set $temp (call $fp-new
      (i64.shl
        (i64.const 1)
        (i64.extend_i32_u (i32.sub (i32.const 0) (%fp-e $m-plus))))
      (%fp-e $m-plus)))
  (local.set $one-f (%fp-f $temp))
  (local.set $one-e (%fp-e $temp))
  (call $fp-delete (local.get $temp))

  ;; uint32_t p1 = static_cast<uint32_t>(M_plus.f >> -one.e); // p1 = f div 2^-e (Since -e >= 32, p1 fits into a 32-bit int.)
  (local.set $minus-one-e (i64.extend_i32_u (i32.sub
        (i32.const 0)
        (local.get $one-e))))
  (local.set $p1 (i32.wrap_i64 (i64.shr_u
        (%fp-f $m-plus)
        (local.get $minus-one-e))))
  ;; uint64_t p2 = M_plus.f & (one.f - 1);                    // p2 = f mod 2^-e
  (local.set $p2 (i64.and
      (%fp-f $m-plus)
      (i64.sub (local.get $one-f) (i64.const 1))))

  ;; assert(p1 > 0);
  (%assert (i32.ge_u (local.get $p1) (i32.const 0)))

  (;
   ; 1.
   ; Generate the digits of the integral part p1 = d[n-1]...d[1]d[0]
   ;)

  ;; uint32_t pow10;
  ;; int const k = InitKappa(p1, pow10);
  (local.set $temp64 (call $init-kappa (local.get $p1)))
  (local.set $pow10 (%unpack-64-hi-l $temp64))
  (local.set $k (%unpack-64-lo-l $temp64))


  (;
   ;  We now have
   ;   (B = buffer, L = length = k - n)
   ;
   ;       10^(k-1) <= p1 < 10^k
   ;
   ;       p1 = (p1 div 10^(k-1)) * 10^(k-1) + (p1 mod 10^(k-1))
   ;          = (B[0]           ) * 10^(k-1) + (p1 mod 10^(k-1))
   ;
   ;       w+ = p1 + p2 * 2^e
   ;          = B[0] * 10^(k-1) + (p1 mod 10^(k-1)) + p2 * 2^e
   ;          = B[0] * 10^(k-1) + ((p1 mod 10^(k-1)) * 2^-e + p2) * 2^e
   ;          = B[0] * 10^(k-1) + (                         rest) * 2^e
   ;
   ;  and generate the digits d of p1 from left to right:
   ;
   ;       p1 = (B[0]B[1]...B[L  -1]B[L]B[L+1] ...B[k-2]B[k-1])_10
   ;          = (B[0]B[1]...B[L  -1])_10 * 10^(k-L) + (B[L    ]...B[k-2]B[k-1])_10  (L = 1...k)
   ;          = (B[0]B[1]...B[k-n-1])_10 * 10^(n  ) + (B[k-n  ]...B[k-2]B[k-1])_10  (n = k...1)
   ;)

  ;; int n = k;
  (local.set $n (local.get $k))
  (block $b_end (loop $b_start
      ;; while (n > 0)
      (br_if $b_end (i32.le_s (local.get $n) (i32.const 0)))

      (;
       ; Invariants:
       ;  1.  w+ = (w+ div 10^n) * 10^n + (w+ mod 10^n  )
       ;         = (buffer     ) * 10^n + (p1 + p2 * 2^e)  (buffer = 0, if length = 0)
       ;  2.  p1 >= pow10 = 10^(n-1)
       ;)

      ;; uint32_t const d = p1 / pow10;  // d = p1 div 10^(n-1)
      (local.set $d (i32.div_u (local.get $p1) (local.get $pow10)))
      ;; uint32_t const r = p1 % pow10;  // r = p1 mod 10^(n-1)
      (local.set $r (i32.rem_u (local.get $p1) (local.get $pow10)))

      (;
       ; w+ = buffer * 10^n + ((p1              ) + p2 * 2^e)
       ;    = buffer * 10^n + ((d * 10^(n-1) + r) + p2 * 2^e)
       ;    = (buffer * 10 + d) * 10^(n-1) + (r + p2 * 2^e)
       ;)

      ;; assert(d <= 9);
      (%assert (i32.le_u (local.get $d) (i32.const 9)))
      ;; buffer[length++] = kDigits[d]; // buffer := buffer * 10 + d
      (i32.store8
        (i32.add (local.get $buffer) (local.get $length))
        (i32.add (local.get $d) (i32.const 0x30)))
      (%inc $length)

      ;; w+ = buffer * 10^(n-1) + (r + p2 * 2^e)

      ;; p1 = r;         // p1 := p1 mod 10^(n-1) = r
      (local.set $p1 (local.get $r))
      ;; n -= 1;         // n  := n - 1
      (%dec $n)

      (;
       ; w+ = (buffer     ) * 10^n + (p1 + p2 * 2^e)
       ;    = (w+ div 10^n) * 10^n + (w+ mod 10^n  )
       ;
       ; Invariant (1) has been restored.

       ; Compute
       ; rest * 2^e = w+ mod 10^n = p1 + p2 * 2^e = (p1 * 2^-e + p2) * 2^e
       ;
       ; Note:
       ; only the significand is computed; the exponent e is implicit.
       ;)
      ;; uint64_t const rest = (uint64_t{p1} << -one.e) + p2;
      (local.set $rest (i64.add
          (i64.shl (i64.extend_i32_u (local.get $p1)) (local.get $minus-one-e))
          (local.get $p2)))

      (;
       ; Check if enough digits have been generated:
       ; rest * 2^e <= delta * 2^e
       ;
       ; Note:
       ; rest and delta share the same exponent e, so it suffices to compare
       ; the significands.
       ;)

      ;; if (rest <= delta)
      (if (i64.le_u (local.get $rest) (local.get $delta)) (then
          (;
           ; Found V = buffer * 10^n, with w- <= V <= w+.
           ; And V is correctly rounded.
           ;)
          ;; decimal_exponent += n;
          (local.set $decimal-exponent (i32.add
              (local.get $decimal-exponent)
              (local.get $n)))

          (;
           ; We may now just stop. But instead look if the buffer could be
           ; decremented to bring V closer to w.
           ;
           ; 10^n is now 1 ulp in the decimal representation V.
           ;
           ; The rounding procedure works with DiyFp's with an implicit
           ; exponent e.
           ;
           ;      10^n = ten_n * 2^e = (10^n * 2^-e) * 2^e
           ;
           ; Note:
           ; n has been decremented above, i.e. pow10 = 10^n
           ;)
          ;; uint64_t const ten_n = uint64_t{pow10} << -one.e;
          (local.set $ten_n (i64.shl
              (i64.extend_i32_u (local.get $pow10))
              (local.get $minus-one-e)))
          (call $grisu-2-round
            (local.get $buffer)
            (local.get $length)
            (local.get $dist)
            (local.get $delta)
            (local.get $rest)
            (local.get $ten_n))
          (return (%pack-64-l $decimal-exponent $length))))

      ;; pow10 /= 10;
      (local.set $pow10 (i32.div_u (local.get $pow10) (i32.const 10)))

      (;
       ; p1 >= pow10 = 10^n
       ;
       ; Invariant (2) has been restored.
       ;)

      (br $b_start)))

  ;; assert(p2 != 0);
  (%assert (i64.ne (local.get $p2) (i64.const 0)))
  ;; (otherwise the loop above would have been exited with rest <= delta)

  (;
   ; 2.
   ; The digits of the integral part have been generated:
   ;
   ;      w+ = d[k-1]...d[1]d[0] + p2 * 2^e = buffer + p2 * 2^e
   ;
   ; Now generate the digits of the fractional part p2 * 2^e.
   ;
   ; Note:
   ; No decimal point is generated: the exponent is adjusted instead.
   ;
   ; p2 actually represents the fraction
   ;
   ;      p2 * 2^e
   ;          = p2 / 2^-e
   ;          = d[-1] / 10^1 + d[-2] / 10^2 + d[-3] / 10^3 + ... + d[-m] / 10^m
   ;
   ; or
   ;
   ;      10 * p2 / 2^-e = d[-1] + (d[-2] / 10^1 + ... + d[-m] / 10^(m-1))
   ;
   ; and the digits can be obtained from left to right by
   ;
   ;      (10 * p2) div 2^-e = d[-1]
   ;      (10 * p2) mod 2^-e = d[-2] / 10^1 + ... + d[-m] / 10^(m-1)
   ;)

  ;; int m = 0;
  (local.set $m (i32.const 0))
  ;; for (;;)
  (loop $forever
    (;
     ; Invariant:
     ;  1.  w+ = buffer * 10^m + 10^m * p2 * 2^e  (Note: m <= 0)

     ; p2 * 10 < 2^60 * 10 < 2^60 * 2^4 = 2^64,
     ; so the multiplication by 10 does not overflow.
     ;)

    ;; assert(p2 <= UINT64_MAX / 10);
    (%assert (i64.le_u (local.get $p2) (i64.const 0x1999_9999_9999_9999)))
    ;; p2 *= 10;
    (local.set $p2 (i64.mul (local.get $p2) (i64.const 10)))

    ;; uint64_t const d = p2 >> -one.e;     // = p2 div 2^-e
    (local.set $dd (i64.shr_u (local.get $p2) (local.get $minus-one-e)))
    ;; uint64_t const r = p2 & (one.f - 1); // = p2 mod 2^-e
    (local.set $rr (i64.and
        (local.get $p2)
        (i64.sub (local.get $one-f) (i64.const 1))))

    (;
     ; w+ = buffer * 10^m + 10^m * p2 * 2^e
     ;    = buffer * 10^m + 10^(m-1) * (10 * p2     ) * 2^e
     ;    = buffer * 10^m + 10^(m-1) * (d * 2^-e + r) * 2^e
     ;    = buffer * 10^m + 10^(m-1) * d + 10^(m-1) * r * 2^e
     ;    = (buffer * 10 + d) * 10^(m-1) + 10^(m-1) * r * 2^e
     ;)

    ;; assert(d <= 9);
    (%assert (i64.le_u (local.get $dd) (i64.const 9)))
    ;; buffer[length++] = kDigits[d]; // buffer := buffer * 10 + d
    (i64.store8
      (i32.add (local.get $buffer) (local.get $length))
      (i64.add (local.get $dd) (i64.const 0x30)))
    (%inc $length)

    (;
     ; w+ = buffer * 10^(m-1) + 10^(m-1) * r * 2^e
     ;)

    ;; p2 = r;
    (local.set $p2 (local.get $rr))
    ;; m -= 1;
    (%dec $m)

    (;
     ; w+ = buffer * 10^m + 10^m * p2 * 2^e
     ;
     ; Invariant (1) restored.
     ;

     ; p2 is now scaled by 10^(-m) since it repeatedly is multiplied by 10.
     ; To keep the units in sync, delta and  dist need to be scaled too.
     ;)

    ;; delta *= 10;
    (local.set $delta (i64.mul (local.get $delta) (i64.const 10)))
    ;; dist  *= 10;
    (local.set $dist (i64.mul (local.get $dist) (i64.const 10)))

    ;; uint64_t const rest = p2;
    (local.set $rest (local.get $p2))

    (;
     ; Check if enough digits have been generated.
     ;)
    ;; if (rest <= delta)
    (if (i64.le_u (local.get $rest) (local.get $delta)) (then
        ;; decimal_exponent += m;
        (local.set $decimal-exponent (i32.add
            (local.get $decimal-exponent)
            (local.get $m)))

        (;
         ; ten_m represents 10^m as a Fp with an exponent e.
         ;
         ; Note: m < 0
         ;
         ; Note:
         ; delta and dist are now scaled by 10^(-m) (they are repeatedly
         ; multiplied by 10) and we need to do the same with ten_m.
         ;
         ;      10^(-m) * 10^m = 10^(-m) * ten_m * 2^e
         ;                     = (10^(-m) * 10^m * 2^-e) * 2^e
         ;                     = 2^-e * 2^e
         ;
         ; one.f = 2^-e and the exponent e is implicit.
         ;)
        ;; uint64_t const ten_m = one.f;
        ;; return Grisu2Round(buffer, length, dist, delta, rest, ten_m);
        (call $grisu-2-round
          (local.get $buffer)
          (local.get $length)
          (local.get $dist)
          (local.get $delta)
          (local.get $rest)
          (local.get $one-f))
        (return (%pack-64-l $decimal-exponent $length))))

    (br $forever))

  (;
   ; By construction this algorithm generates the shortest possible decimal
   ; number (Loitsch, Theorem 6.2) which rounds back to w.
   ; For an input number of precision p, at least
   ;
   ;      N = 1 + ceil(p * log_10(2))
   ;
   ; decimal digits are sufficient to identify all binary floating-point
   ; numbers (Matula, "In-and-Out conversions").
   ; This implies that the algorithm does not produce more than N decimal
   ; digits.
   ;
   ;      N = 17 for p = 53 (IEEE double precision)
   ;      N = 9  for p = 24 (IEEE single precision)
   ;)
  (unreachable))

;; v = buf * 10^decimal_exponent
;; len is the length of the buffer (number of decimal digits)
(func $grisu-2
  (param $buf i32)  ;; char*
  (param $len i32)  ;; int&
  (param $decimal-exponent i32)  ;; int&
  (param $m-minus i32) ;; Fp
  (param $v i32) ;; Fp
  (param $m-plus i32) ;; Fp)
  (result i64)        ;; (packed decimal-exponent len)
                      ;; This allows them to be handled equivalent to references

  (local $cached i32)     ;; *CachedPower
  (local $c-minus-k i32)  ;; *Fp (free on return)
  (local $w i32)          ;; *Fp (free on return)
  (local $w-minus i32)    ;; *Fp (free on return)
  (local $w-plus i32)     ;; *Fp (free on return)
  (local $M-minus i32)    ;; *Fp (free on return)
  (local $M-plus i32)     ;; *Fp (free on return)
  (local $result i64)

  ;; assert(v.e == m_minus.e);
  (%assert (i32.eq (%fp-e $v) (%fp-e $m-minus)))
  ;; assert(v.e == m_plus.e);
  (%assert (i32.eq (%fp-e $v) (%fp-e $m-plus)))

  (;
   ;  --------(-----------------------+-----------------------)--------    (A)
   ;          m-                      v                       m+
   ;
   ;  --------------------(-----------+-----------------------)--------    (B)
   ;                      m-          v                       m+
   ;
   ; First scale v (and m- and m+) such that the exponent is in the range
   ; [alpha, beta].
   ;)

  ;; CachedPower const cached = GetCachedPowerForBinaryExponent(m_plus.e);
  (local.set $cached (call $get-cached-power-for-binary-exponent (%fp-e $m-plus)))

  ;; Fp const c_minus_k(cached.f, cached.e); // = c ~= 10^k
  (local.set $c-minus-k (call $fp-new (%cp.f $cached) (%cp.e $cached)))

  ;; Fp const w       = Fp::Mul(v,       c_minus_k); // Exponent of the products is v.e + c_minus_k.e + q
  (local.set $w (call $fp-mul (local.get $v) (local.get $c-minus-k)))
  ;; Fp const w_minus = Fp::Mul(m_minus, c_minus_k);
  (local.set $w-minus (call $fp-mul (local.get $m-minus) (local.get $c-minus-k)))
  ;; Fp const w_plus  = Fp::Mul(m_plus,  c_minus_k);
  (local.set $w-plus (call $fp-mul (local.get $m-plus) (local.get $c-minus-k)))

  (;
   ;  ----(---+---)---------------(---+---)---------------(---+---)----
   ;          w-                      w                       w+
   ;          = c*m-                  = c*v                   = c*m+
   ;
   ; Fp::Mul rounds its result and c_minus_k is approximated too. w (as well
   ; as w- and w+) are now off by a small amount.
   ; In fact:
   ;
   ;      w - v * 10^k < 1 ulp
   ;
   ; To account for this inaccuracy, add resp. subtract 1 ulp.
   ;
   ;  --------+---[---------------(---+---)---------------]---+--------
   ;          w-  M-                  w                   M+  w+
   ;
   ; Now any number in [M-, M+] (bounds included) will round to w when input,
   ; regardless of how the input rounding algorithm breaks ties.
   ;
   ; And DigitGen generates the shortest possible such number in [M-, M+].
   ; This does not mean that Grisu2 always generates the shortest possible
   ; number in the interval (m-, m+).
   ;)
  ;; Fp const M_minus = Fp(w_minus.f + 1, w_minus.e);
  (local.set $M-minus (call $fp-new
      (i64.add (%fp-f $w-minus) (i64.const 1))
      (%fp-e $w-minus)))
  ;; Fp const M_plus  = Fp(w_plus.f  - 1, w_plus.e );
  (local.set $M-plus (call $fp-new
      (i64.sub (%fp-f $w-plus) (i64.const 1))
      (%fp-e $w-plus)))

  ;; decimal_exponent = -cached.k; // = -(-k) = k
  (local.set $decimal-exponent (i32.sub (i32.const 0) (%cp.k $cached)))

  ;; Grisu2DigitGen(buf, len, decimal_exponent, M_minus, w, M_plus);
  (local.set $result (call $grisu-2-digit-gen
      (local.get $buf)
      (local.get $len)
      (local.get $decimal-exponent)
      (local.get $M-minus)
      (local.get $w)
      (local.get $M-plus)))

  ;; free all Fp buffers
  (call $fp-delete (local.get $c-minus-k))
  (call $fp-delete (local.get $w))
  (call $fp-delete (local.get $w-minus))
  (call $fp-delete (local.get $w-plus))
  (call $fp-delete (local.get $M-minus))
  (call $fp-delete (local.get $M-plus))

  (return (local.get $result)))


;; Returns a pointer to the element following the exponent
(func $append-exponent (param $buf i32) (param $e i32) (result i32)
  (local $k i32)

  ;; static constexpr char const* const kDigits = "0123456789";
  ;; assert(e > -1000);
  (%assert (i32.gt_s (local.get $e) (i32.const -1000)))
  ;; assert(e <  1000);
  (%assert (i32.lt_s (local.get $e) (i32.const 1000)))

  ;; if (e < 0)
  ;;     *buf++ = '-', e = -e;
  ;; else
  ;;     *buf++ = '+';
  (if (i32.lt_s (local.get $e) (i32.const 0))
    (then
      (i32.store8 (local.get $buf) (i32.const 0x2D))
      (local.set $e (i32.sub (i32.const 0) (local.get $e))))
    (else (i32.store (local.get $buf) (i32.const 0x2B))))
  (%inc $buf)

  ;; uint32_t k = static_cast<uint32_t>(e);
  (local.set $k (local.get $e))

  (block $b_done
    ;; if (k < 10)
    (if (i32.lt_u (local.get $k) (i32.const 10)) (then
        ;; *buf++ = kDigits[k];
        (i32.store8 (local.get $buf) (i32.add (local.get $k) (i32.const 0x30)))
        (%inc $buf)
        (br $b_done)))
    ;; else if (k < 100)
    (if (i32.lt_u (local.get $k) (i32.const 100)) (then
        ;; *buf++ = kDigits[k / 10]; k %= 10;
        (i32.store8
          (local.get $buf)
          (i32.add (i32.div_u (local.get $k) (i32.const 10)) (i32.const 0x30)))
        (local.set $k (i32.rem_u (local.get $k) (i32.const 10)))
        (%inc $buf)
        ;; *buf++ = kDigits[k];
        (i32.store8 (local.get $buf) (i32.add (local.get $k) (i32.const 0x30)))
        (%inc $buf)
        (br $b_done)))
    ;; else
    ;; *buf++ = kDigits[k / 100]; k %= 100;
    (i32.store8
      (local.get $buf)
      (i32.add (i32.div_u (local.get $k) (i32.const 100)) (i32.const 0x30)))
    (local.set $k (i32.rem_u (local.get $k) (i32.const 100)))
    (%inc $buf)
    ;; *buf++ = kDigits[k / 10]; k %= 10;
    (i32.store8
      (local.get $buf)
      (i32.add (i32.div_u (local.get $k) (i32.const 10)) (i32.const 0x30)))
    (local.set $k (i32.rem_u (local.get $k) (i32.const 10)))
    (%inc $buf)
    ;; *buf++ = kDigits[k];
    (i32.store8 (local.get $buf) (i32.add (local.get $k) (i32.const 0x30)))
    (%inc $buf))

  ;; return buf;
  (return (local.get $buf)))

(func $format-buffer (param $buf i32) (param $k i32) (param $n i32) (result i32)
  (;
   ; v = digits * 10^(n-k)
   ; k is the length of the buffer (number of decimal digits)
   ; n is the position of the decimal point relative to the start of the buffer.
   ;
   ; Format the decimal floating-number v in the same way as JavaScript's ToString
   ; applied to number type.
   ;
   ; See:
   ; https://tc39.github.io/ecma262/#sec-tostring-applied-to-the-number-type
   ;)

  ;; if (k <= n && n <= 21)
  (if (i32.le_s (local.get $k) (local.get $n)) (then
      (if (i32.le_s (local.get $n) (i32.const 21)) (then
          ;; digits[000]

          ;; std::memset(buf + k, '0', static_cast<size_t>(n - k));
          (call $memset
            (i32.add (local.get $buf) (local.get $k))
            (i32.const 0x30)
            (i32.sub (local.get $n) (local.get $k)))
          ;; if (trailing_dot_zero)
          ;; {
          ;;     buf[n++] = '.';
          ;;     buf[n++] = '0';
          ;; }
          ;; return buf + n;                 // (len <= 21 + 2 = 23)
          (return (i32.add (local.get $buf) (local.get $n)))))))

  ;; if (0 < n && n <= 21)
  (if (i32.lt_s (i32.const 0) (local.get $n)) (then
      (if (i32.le_s (local.get $n) (i32.const 21)) (then
          ;; dig.its
          ;; assert(k > n);
          (%assert (i32.gt_s (local.get $k) (local.get $n)))

          ;; std::memmove(buf + (n + 1), buf + n, static_cast<size_t>(k - n));
          (call $memmove
            (i32.add (local.get $buf) (i32.add (local.get $n) (i32.const 1)))
            (i32.add (local.get $buf) (local.get $n))
            (i32.sub (local.get $k) (local.get $n)))
          ;; buf[n] = '.';
          (i32.store8 (i32.add (local.get $buf) (local.get $n)) (i32.const 0x2E))
          ;; return buf + (k + 1);           // (len == k + 1 <= 18)
          (return (i32.add
              (local.get $buf)
              (i32.add (local.get $k) (i32.const 1))))))))

  ;; if (-6 < n && n <= 0)
  (if (i32.lt_s (i32.const -6) (local.get $n)) (then
      (if (i32.le_s (local.get $n) (i32.const 0)) (then
          ;; 0.[000]digits

          ;; std::memmove(buf + (2 + -n), buf, static_cast<size_t>(k));
          (call $memmove
            (i32.add (local.get $buf) (i32.sub (i32.const 2) (local.get $n)))
            (local.get $buf)
            (local.get $k))
          ;; buf[0] = '0';
          (i32.store8 offset=0 (local.get $buf) (i32.const 0x30))
          ;; buf[1] = '.';
          (i32.store8 offset=1 (local.get $buf) (i32.const 0x2E))
          ;; std::memset(buf + 2, '0', static_cast<size_t>(-n));
          (call $memset
            (i32.add (local.get $buf) (i32.const 2))
            (i32.const 0x30)
            (i32.sub (i32.const 0) (local.get $n)))
          ;; return buf + (2 + (-n) + k);    // (len <= k + 7 <= 24)
          (return (i32.add
              (local.get $buf)
              (i32.add
                (i32.sub (i32.const 2) (local.get $n))
                (local.get $k))))))))

  ;; if (k == 1)
  (if (i32.eq (local.get $k) (i32.const 1)) (then
      ;; dE+123

      ;; buf += 1;                       // (len <= 1 + 5 = 6)
      (%inc $buf))
    (else
      ;; d.igitsE+123

      ;; std::memmove(buf + 2, buf + 1, static_cast<size_t>(k - 1));
      (call $memmove
        (i32.add (local.get $buf) (i32.const 2))
        (i32.add (local.get $buf) (i32.const 1))
        (i32.sub (local.get $k) (i32.const 1)))
      ;; buf[1] = '.';
      (i32.store8 offset=1 (local.get $buf) (i32.const 0x2E))
      ;; buf += 1 + k;                   // (len <= k + 6 = 23)
      (local.set $buf (i32.add
          (local.get $buf)
          (i32.add (i32.const 1) (local.get $k))))))

  ;; *buf++ = 'e';
  (i32.store8 (local.get $buf) (i32.const 0x65))
  (%inc $buf)
  ;; return AppendExponent(buf, n - 1);
  (return (call $append-exponent
      (local.get $buf)
      (i32.sub (local.get $n) (i32.const 1)))))

(;
 ; Generates a decimal representation of the input floating-point number V in
 ; BUF.
 ;
 ; The result is formatted like JavaScript's ToString applied to a number type.
 ; Except that:
 ; An argument representing an infinity is converted to "inf" or "-inf".
 ; An argument representing a NaN is converted to "nan".
 ;
 ; This function never writes more than 25 characters to BUF and returns an
 ; iterator pointing past-the-end of the decimal representation.
 ; The result is guaranteed to round-trip (when read back by a correctly
 ; rounding implementation.)
 ;
 ; Note:
 ; The result is not null-terminated.
 ;)
(func $real->string (param $value f64) (result i32)
  (local $str i32)
  (local $str-ptr i32)
  (local $next i32)
  (local $w i32) ;; bounded-fp
  (local $len i32) ;; int32
  (local $decimal-exponent i32) ;; int32
  (local $n i32) ;; int32
  (local $pack64 i64) ;; packed<int32,int32>


  ;; static constexpr char const* const kNaNString = "NaN";      // assert len <= 25
  ;; static constexpr char const* const kInfString = "Infinity"; // assert len <= 24

  ;; using IEEEType = IEEEFloat<Float>;
  ;; static_assert(Fp::kPrecision >= IEEEType::kPrecision + 3, "insufficient precision");

  ;; assert(last - next >= 25);
  ;; static_cast<void>(last); // unused

  ;; IEEEType const v(value);
  ;; assert(!v.IsNaN());
  ;; assert(!v.IsInf());

  ;; if (v.IsNaN())
  (if (call $ieee-nan? (local.get $value)) (then
    (if (call $ieee-negative? (local.get $value))
      (then (return (call $str-dup (%car (global.get $g-neg-nan)))))
      (else (return (call $str-dup (%car (global.get $g-nan))))))))

  (if (call $ieee-inf? (local.get $value)) (then
    (if (call $ieee-negative? (local.get $value))
      (then (return (call $str-dup (%car (global.get $g-neg-inf)))))
      (else (return (call $str-dup (%car (global.get $g-inf))))))))

  ;; allocate string buffer, we'll fill in the length at the end
  (local.set $str (call $malloc (i32.const 29)))
  (local.set $str-ptr (i32.add (local.get $str) (i32.const 4)))
  (local.set $next (local.get $str-ptr))

  ;; if (v.IsNegative())
  (if (call $ieee-negative? (local.get $value)) (then
      ;; *next++ = '-';
      (i32.store8 (local.get $next) (i32.const 0x2D))
      (%inc $next)))

  ;; if (v.IsZero())
  (if (call $ieee-zero? (local.get $value))
    (then
      ;; *next++ = '0';
      (i32.store8 (local.get $next) (i32.const 0x30))
      (%inc $next))
    (else
      ;; BoundedFp w = ComputeBoundedFp(v.Abs());
      (local.set $w (call $compute-bounded-fp (f64.abs (local.get $value))))

      (;
       ; Compute the decimal digits of v = digits * 10^decimal_exponent.
       ; len is the length of the buffer, i.e. the number of decimal digits
       ;)
      ;; int len = 0;
      ;; int decimal_exponent = 0;
      ;; Grisu2(next, len, decimal_exponent, w.minus, w.w, w.plus);
      (local.set $pack64 (call $grisu-2
          (local.get $next)
          (i32.const 0)
          (i32.const 0)
          (%bfp-minus $w)
          (%bfp-w $w)
          (%bfp-plus $w)))
      (local.set $len (%unpack-64-lo-l $pack64))
      (local.set $decimal-exponent (%unpack-64-hi-l $pack64))

      ;; Compute the position of the decimal point relative to the start of the buffer.
      ;; int n = decimal_exponent + len;
      (local.set $n (i32.add (local.get $decimal-exponent) (local.get $len)))

      ;; next = FormatBuffer(next, len, n);
      (local.set $next (call $format-buffer
          (local.get $next)
          (local.get $len)
          (local.get $n)))))
      ;; // (len <= 1 + 24 = 25)

  ;; return next;
  (i32.store
    (local.get $str)
    (i32.sub (local.get $next) (local.get $str-ptr)))
  (return (local.get $str)))
