(global $g-trace-macros (mut i32) (i32.const 0))

;; (trace-macros?)
(func $trace-macros? (param $env i32) (param $args i32) (result i32)
  (block $check (block $fail
      (br_if $fail (call $list-len (local.get $args)))
      (br $check))
    (return (call $argument-error (local.get $args))))

  (return (select
      (global.get $g-true)
      (global.get $g-false)
      (global.get $g-trace-macros))))

;; (trace-macros-set! <bool>)
(func $trace-macros-set! (param $env i32) (param $args i32) (result i32)
  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 1)))
      (br $check))
    (return (call $argument-error (local.get $args))))

  (global.set $g-trace-macros (call $is-truthy (%car-l $args)))

  (return (global.get $g-nil)))


(func $define-syntax (param $env i32) (param $args i32) (result i32)
  (local $keyword i32)
  (local $spec i32)
  (local $syntax-rules i32)

  (block $check (block $fail
      (br_if $fail (i32.ne (call $list-len (local.get $args)) (i32.const 2)))
      (local.set $keyword (%car-l $args))
      (%chk-type $fail $keyword %symbol-type)
      (br_if $fail (call $environment-has (local.get $env) (local.get $keyword)))
      (local.set $spec (%car (%cdr-l $args)))
      (br_if $fail (i32.eqz (call $is-list-impl (local.get $spec))))
      (br $check))

    (return (call $argument-error (local.get $args))))

  (local.set $syntax-rules (call $create-syntax-rules
      (local.get $keyword)
      (local.get $spec)))
  (if (i32.ne (%get-type $syntax-rules) (%syntax-rules-type)) (then
      (return (local.get $syntax-rules))))

  (call $environment-add
    (local.get $env)
    (local.get $keyword)
    (local.get $syntax-rules))
  (return (global.get $g-nil)))

;; (syntax-rules (<literal> ...) <syntax-rule> ...)
;; (syntax-rules <ellipsis> (<literal> ...) <syntax-rule> ...)
(func $create-syntax-rules (param $keyword i32) (param $spec i32) (result i32)
  (local $ellipsis i32)
  (local $literals i32)
  (local $num-args i32)
  (local $temp i32)
  (local $rules i32)
  (local $cmd i32)

  (block $check (block $fail
      (local.set $num-args (call $list-len (local.get $spec)))
      (br_if $fail (i32.lt_u (local.get $num-args) (i32.const 3)))
      (local.set $temp (local.get $spec))
      (%pop-l $cmd $temp)
      (br_if $fail (i32.ne (local.get $cmd) (global.get $g-syntax-rules)))
      (%pop-l $ellipsis $temp)
      (if (i32.eq (%get-type $ellipsis) (%symbol-type))
        (then
          ;; this is the (syntax-rules <ellipsis> form)
          (%pop-l $literals $temp)
          (br_if $fail (i32.lt_u (local.get $num-args) (i32.const 4))))
        (else
          ;; this is the (syntax-rules (<literal> ...) ... ) form
          (local.set $literals (local.get $ellipsis))
          (local.set $ellipsis (global.get $g-ellipsis))))
      (br_if $fail (i32.eqz (call $is-list-impl (local.get $literals))))
      (br_if $fail (i32.eqz (call $all-symbol (local.get $literals))))
      (local.set $rules (local.get $temp))
      (br $check))

    (return (call $argument-error (local.get $spec))))

  (local.set $literals (%alloc-cons (local.get $ellipsis) (local.get $literals)))

  (if (i32.eqz (call $valid-rules?
        (local.get $keyword)
        (local.get $literals)
        (local.get $rules)))  (then
    (return (call $argument-error (local.get $spec)))))

  (return (call $heap-alloc
      (global.get $g-heap)
      (%syntax-rules-type)
      (local.get $literals)
      (local.get $rules))))


(func $all-symbol (param $args i32) (result i32)
  (local $curr i32)

  (block $done (loop $forever
      (br_if $done (i32.eq (%get-type $args) (%nil-type)))

      (%pop-l $curr $args)

      (if (i32.ne (%get-type $curr) (%symbol-type))
        (then (return (i32.const 0))))

      (br $forever)))

  (return (i32.const 1)))

(func $valid-rules? (param $keyword i32) (param $literals i32) (param $rules i32) (result i32)
  (local $rule i32)

  (block $end (loop $start
      (br_if $end (i32.eq (%get-type $rules) (%nil-type)))
      (%pop-l $rule $rules)

      (if (i32.eqz (call $valid-rule?
            (local.get $keyword)
            (local.get $literals)
            (local.get $rule)))
        (then (return (i32.const 0))))

      (br $start)))

  (return (i32.const 1)))

(;
  A <rule> is of the form (<pattern> <template>)
 ;)
(func $valid-rule? (param $keyword i32) (param $literals i32) (param $rule i32) (result i32)
  (local $pattern i32)
  (local $template i32)
  (local $ellipsis i32)

  (if (i32.ne (call $list-len (local.get $rule)) (i32.const 2)) (then
      ;; rule must be a list of 2
      (return (i32.const 0))))

  (local.set $pattern (%car-l $rule))
  (local.set $template (%car (%cdr-l $rule)))

  ;; first literal is the ellipsis
  (local.set $ellipsis (%car-l $literals))

  ;; pattern must be a list starting with keyword
  (if (i32.ne (%get-type $pattern) (%cons-type)) (then
      ;; not a list
      (return (i32.const 0))))
  (if (i32.ne (%car-l $pattern) (local.get $keyword)) (then
      ;; doesn't start with keyword
      (return (i32.const 0))))

  ;; a valid rule must be a valid pattern...
  (if (i32.eqz (call $valid-pattern?
        (local.get $ellipsis)
        (local.get $pattern)))
    (then (return (i32.const 0))))

  ;; ... and a valid template
  (return (call $valid-template? (local.get $ellipsis) (local.get $template))))

(;
 ;  A <pattern> is either an identifier, a constant, or one of the following
 ;    (<pattern> ...)
 ;    (<pattern> <pattern> ... . <pattern>)
 ;    (<pattern> ... <pattern> <ellipsis> <pattern> ...)
 ;    (<pattern> ... <pattern> <ellipsis> <pattern> ...  . <pattern>)
 ;    #(<pattern> ...)
 ;    #(<pattern> ... <pattern> <ellipsis> <pattern> ...)
 ;)
(func $valid-pattern? (param $ellipsis i32) (param $pattern i32) (result i32)
  (local $type i32)
  (local $curr i32)
  (local $ptr i32)
  (local $count i32)
  (local $check-ellipsis i32)
  (local $have-ellipsis i32)

  (local.set $type (%get-type $pattern))
  (if (call $identifier-or-constant? (local.get $type)) (then
      ;; it is invalid for this to be the ellipsis identifier
      (return (i32.ne (local.get $pattern) (local.get $ellipsis)))))

  (if (i32.eq (local.get $type) (%nil-type)) (then
      ;; an empty list is a valid pattern
      (return (i32.const 1))))

  (local.set $check-ellipsis (i32.const 0))
  (local.set $have-ellipsis (i32.const 0))

  (if (i32.eq (local.get $type) (%cons-type)) (then
      (block $end (loop $start
          (%chk-type $end $pattern %cons-type)
          (%pop-l $curr $pattern)

          (if (i32.and (local.get $check-ellipsis) (i32.eq
                (local.get $curr)
                (local.get $ellipsis)))
            (then
              ;; this is an ellipsis in a valid position
              (local.set $check-ellipsis (i32.const 0))
              (local.set $have-ellipsis (i32.const 1)))
            (else
              (if (i32.eqz (call $valid-pattern?
                    (local.get $ellipsis)
                    (local.get $curr))) (then
                  ;; curr is not a valid pattern
                  (return (i32.const 0))))
              ;; Curr is a valid pattern, so an ellipsis is possible, if one
              ;; has not yet been seen in this pattern
              (local.set $check-ellipsis (i32.eqz (local.get $have-ellipsis)))))

          ;; check for a dotted list ending
          (local.set $type (%get-type $pattern))
          (if (i32.and
              (i32.ne (local.get $type) (%nil-type))
              (i32.ne (local.get $type) (%cons-type)))
            (then
              (return (call $valid-pattern?
                  (local.get $ellipsis)
                  (local.get $pattern)))))

          (br $start)))
      (return (i32.const 1))))

  (if (i32.eq (local.get $type) (%vector-type)) (then
      (local.set $ptr (%car-l $pattern))
      (local.set $count (%cdr-l $pattern))

      (block $end (loop $start
          (br_if $end (i32.eqz (local.get $count)))

          (local.set $curr (i32.load (local.get $ptr)))

          (if (i32.and (local.get $check-ellipsis) (i32.eq
                (local.get $curr)
                (local.get $ellipsis)))
            (then
              ;; this is a valid ellipsis
              (local.set $check-ellipsis (i32.const 0))
              (local.set $have-ellipsis (i32.const 1)))
            (else
              (if (i32.eqz (call $valid-pattern?
                    (local.get $ellipsis)
                    (local.get $curr))) (then
                  (return (i32.const 0))))
              ;; Curr is a valid pattern, so an ellipsis is possible, if one
              ;; has not yet been seen in this pattern
              (local.set $check-ellipsis (i32.eqz (local.get $have-ellipsis)))))

          (%dec $count)
          (%plus-eq $ptr 4)
          (br $start)))

      (return (i32.const 1))))

  (return (i32.const 0)))

(;
 ;  A <template> is either an identifier, a constant, or one of the following
 ;    (<element> ...)
 ;    (<element> <element> ... . <template>)
 ;    (<ellipsis> <template>)
 ;    #(<element> ...)
 ;  Where an <element> is a <template> optionally followed by an <ellipsis>
 ;)
(func $valid-template? (param $ellipsis i32) (param $template i32) (result i32)
  (local $type i32)
  (local $curr i32)
  (local $ptr i32)
  (local $count i32)
  (local $check-ellipsis i32)

  (local.set $type (%get-type $template))

  (if (call $identifier-or-constant? (local.get $type)) (then
      ;; it is invalid for this to be the ellipsis identifier
      (return (i32.ne (local.get $template) (local.get $ellipsis)))))

  (if (i32.eq (local.get $type) (%nil-type)) (then
      ;; an empty list is a valid template
      (return (i32.const 1))))

  (if (i32.eq (local.get $type) (%cons-type)) (then
      ;; first check for (<ellipsis> <template>)
      (if (i32.eq (%car-l $template) (local.get $ellipsis)) (then
          (%pop-l $curr $template)
          ;; head of the list is the ellipsis symbol, rest of the list must be a
          ;; single template
          (%pop-l $curr $template)
          (if (i32.eq (%get-type $template) (%nil-type)) (then
              (if (call $valid-template?
                  (local.get $ellipsis)
                  (local.get $curr)) (then
                  (return (i32.const 1))))))
          (return (i32.const 0))))

      (local.set $check-ellipsis (i32.const 0))
      (block $end (loop $start
          (%chk-type $end $template %cons-type)
          (%pop-l $curr $template)

          (if (i32.and (local.get $check-ellipsis) (i32.eq
                (local.get $curr)
                (local.get $ellipsis)))
            (then
              ;; this is a valid ellipsis
              (local.set $check-ellipsis (i32.const 0)))
            (else
              (if (i32.eqz (call $valid-template?
                    (local.get $ellipsis)
                    (local.get $curr))) (then
                  (return (i32.const 0))))
              (local.set $check-ellipsis (i32.const 1))))

          (local.set $type (%get-type $template))
          ;; check for dotted list
          (if (i32.and
              (i32.ne (local.get $type) (%nil-type))
              (i32.ne (local.get $type) (%cons-type))) (then
              (return (call $valid-template? (local.get $ellipsis) (local.get $template)))))
          (br $start)))

      (return (i32.const 1))))

  (if (i32.eq (local.get $type) (%vector-type)) (then
      (local.set $ptr (%car-l $template))
      (local.set $count (%cdr-l $template))
      (local.set $check-ellipsis (i32.const 0))

      (block $end (loop $start
          (br_if $end (i32.eqz (local.get $count)))

          (local.set $curr (i32.load (local.get $ptr)))

          (if (i32.and (local.get $check-ellipsis) (i32.eq
                (local.get $curr)
                (local.get $ellipsis)))
            (then
              ;; this is a valid ellipsis
              (local.set $check-ellipsis (i32.const 0)))
            (else
              (if (i32.eqz (call $valid-template?
                    (local.get $ellipsis)
                    (local.get $curr))) (then
                  (return (i32.const 0))))
              (local.set $check-ellipsis (i32.const 1))))

          (%dec $count)
          (%plus-eq $ptr 4)
          (br $start)))

      (return (i32.const 1))))

  (return (i32.const 0)))

(;
Constants are
  Numbers
  Strings
  Characters
  Booleans
  Vectors -- but we handle vectors explicitly
  Bytevectors
 ;)
(func $identifier-or-constant? (param $obj-type i32) (result i32)
  (block $check
    ;; indentifier
    (br_if $check (i32.eq (local.get $obj-type) (%symbol-type)))
    ;; boolean
    (br_if $check (i32.eq (local.get $obj-type) (%boolean-type)))
    ;; number
    (br_if $check (i32.eq (local.get $obj-type) (%i64-type)))
    (br_if $check (i32.eq (local.get $obj-type) (%f64-type)))
    (br_if $check (i32.eq (local.get $obj-type) (%big-int-type)))
    (br_if $check (i32.eq (local.get $obj-type) (%rational-type)))
    ;; string
    (br_if $check (i32.eq (local.get $obj-type) (%str-type)))
    ;; character
    (br_if $check (i32.eq (local.get $obj-type) (%char-type)))
    ;; vector
    ;; (br_if $check (i32.eq (local.get $obj-type) (%vector-type)))
    ;; bytevector
    (br_if $check (i32.eq (local.get $obj-type) (%bytevector-type)))
    (return (i32.const 0)))

  (return (i32.const 1)))


(func $apply-syntax-rules (param $env i32) (param $syntax-rules i32) (param $args i32) (result i32)
  (local $literals i32)
  (local $ellipsis i32)
  (local $rules i32)
  (local $rule i32)
  (local $pattern i32)
  (local $template i32)
  (local $expanded i32)
  (local $expanded-type i32)
  (local $match i32)

  (local.set $literals (%car-l $syntax-rules))
  (%pop-l $ellipsis $literals)
  (local.set $rules (%cdr-l $syntax-rules))

  (block $end (loop $start
      (br_if $end (i32.eq (%get-type $rules) (%nil-type)))

      (%pop-l $rule $rules)
      (local.set $pattern (%car-l $rule))
      (local.set $template (%car (%cdr-l $rule)))

      ;; the first element of the pattern has already matched...
      (if (local.tee $match (call $match-syntax-pattern
            (%cdr-l $pattern)
            (local.get $args)
            (local.get $ellipsis)
            (local.get $literals)))
        (then
          (local.set $expanded (call $expand-syntax-template
              (local.get $template)
              (local.get $match)
              (local.get $ellipsis)
              (i32.const 0)
              (global.get $g-nil)))

          (if (global.get $g-trace-macros) (then
              (call $print-symbol (%sym-64 0x20646e61707865 7)) ;; 'expand '
              (call $print (local.get $template))
              (call $print-symbol (%sym-32 0x206f7420 4)) ;; ' to '
              (call $print (local.get $expanded))
              (call $print-symbol (global.get $g-newline))))

          (local.set $expanded-type (%get-type $expanded))
          (if (i32.eq (local.get $expanded-type) (%error-type)) (then
              (return (local.get $expanded))))

          (if (i32.eq (local.get $expanded-type) (%cons-type)) (then
              (if (i32.eq (%car-l $expanded) (global.get $g-syntax-error)) (then
                  (return (call $syntax-error
                      (local.get $env)
                      (%cdr-l $expanded)))))))

          (return (call $cont-alloc
              (%eval-fn)
              (local.get $env)
              (local.get $expanded)
              (i32.const 0)))))

      (br $start)))

  (return (call $argument-error (local.get $args))))

;; (syntax-error <msg> <obj> ...)
(func $syntax-error (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $msg i32)

  (block $check (block $fail
      (br_if $fail (i32.eqz (call $list-len (local.get $args))))
      (local.set $temp (local.get $args))
      (%pop-l $msg $temp)
      (%chk-type $fail $msg %str-type)
      (br $check))

    (return (call $argument-error (local.get $args))))

  (return (%alloc-raise (%alloc-error-cons
        (local.get $msg)
        (local.get $temp)))))

(;
 ;  A <template> is either an identifier, a constant, or one of the following
 ;    (<element> ...)
 ;    (<element> <element> ... . <template>)
 ;    (<ellipsis> <template>)
 ;    #(<element> ...)
 ;  Where an <element> is a <template> optionally followed by an <ellipsis>
 ;
 ;  constants expand to themselves
 ;  identifiers that match entries in the match-env expand to that entry
 ;  expanding elements with ellipsis matches single items from lists in the env
 ;)
(func $expand-syntax-template
  (param $template i32)
  (param $match-env i32)
  (param $ellipsis i32)
  (param $in-ellipsis i32)
  (param $context i32)
  (result i32)

  (local $type i32)
  (local $matched i32)
  (local $result i32)
  (local $curr i32)
  (local $head i32)
  (local $tail i32)
  (local $gp i32)
  (local $count i32)
  (local $template-type i32)

  (local.set $type (%get-type $template))

  ;; identifier
  (if (i32.eq (local.get $type) (%symbol-type)) (then
      (local.set $matched (call $environment-get
          (local.get $match-env)
          (local.get $template)))
      (if (i32.ne (%get-type $matched) (%error-type))
        (then
          ;; perform subsitution
          (if (local.get $in-ellipsis)
            (then
              ;; In an ellipsis expand next entry from match
              (if (i32.eq (%get-type $matched) (%cons-type))
                (then
                  (%pop-l $result $matched)
                  (drop (call $environment-set!
                      (local.get $match-env)
                      (local.get $template)
                      (local.get $matched)))
                  (return (local.get $result)))
                (else
                  (return (global.get $g-nil)))))
            (else
              (return (local.get $matched)))))
        (else
          ;; expands to itself
          (if (global.get $g-trace-macros) (then
              (call $print-symbol (local.get $template))
              (call $print-symbol (%sym-128 0x746e6f63206e6920 0x20747865 12)) ;; ' in context '
              (call $print (local.get $context))
              (call $print-symbol (global.get $g-newline))))

          (local.set $count (call $list-len (local.get $context)))
          ;; apply hygiene for binding constructs in lambda
          (if (i32.ge_u (local.get $count) (i32.const 2))
            (then
              (local.set $gp (%car (%cdr-l $context)))
              (block $no-lambda-hygiene (block $need-lambda-hygiene
                  (br_if $no-lambda-hygiene (i32.ne (%get-type $gp) (%cons-type)))
                  (br_if $no-lambda-hygiene (i32.ne (%cdr-l $gp) (global.get $g-nil)))
                  (br_if $need-lambda-hygiene (i32.eq (%car-l $gp) (global.get $g-lambda)))
                  (br $no-lambda-hygiene))

                (call $environment-add
                  (local.get $match-env)
                  (local.get $template)
                  (local.tee $curr (call $make-hygienic)))
                (return (local.get $curr)))

              ;; apply hygiene for binding constructs in let let* letrec letrec*
              (if (i32.ge_u (local.get $count) (i32.const 3))
                (then
                  (local.set $gp (%car (%cdr (%cdr-l $context))))
                  (block $no-let-hygiene (block $need-let-hygiene
                      (br_if $no-let-hygiene (i32.ne (%get-type $gp) (%cons-type)))
                      (br_if $no-let-hygiene (i32.ne (%cdr-l $gp) (global.get $g-nil)))
                      (local.set $gp (%car-l $gp))
                      (br_if $need-let-hygiene (i32.eq (local.get $gp) (global.get $g-let)))
                      (br_if $need-let-hygiene (i32.eq (local.get $gp) (global.get $g-let-star)))
                      (br_if $need-let-hygiene (i32.eq (local.get $gp) (global.get $g-letrec)))
                      (br_if $need-let-hygiene (i32.eq (local.get $gp) (global.get $g-letrec-star)))
                      (br $no-let-hygiene))

                    (call $environment-add
                      (local.get $match-env)
                      (local.get $template)
                      (local.tee $curr (call $make-hygienic)))
                    (return (local.get $curr)))))))

          (return (local.get $template))))))

  ;; list or vector
  (block $list-or-vector (block $not-list-or-vector
      (br_if $not-list-or-vector (i32.eq (local.get $type) (%cons-type)))
      (br_if $not-list-or-vector (i32.eq (local.get $type) (%vector-type)))
      (br $list-or-vector))

      ;; check if this template starts with ellipsis
      (if (i32.eq (%car-l $template) (local.get $ellipsis)) (then
          ;; set the ellipsis to something that won't match
          (local.set $ellipsis (i32.const -1))
          (local.set $template (%cdr-l $template))))

      (local.set $head (global.get $g-nil))
      (local.set $tail (i32.const 0))

      (block $end (loop $start
          (%chk-type $end $template %cons-type)
          (%pop-l $curr $template)

          (block $no-hygiene (block $need-hygiene
              ;; check that this is a list
              (br_if $no-hygiene (i32.ne (local.get $type) (%cons-type)))
              ;; check that we have entries
              (br_if $no-hygiene (i32.ne (%get-type $head) (%cons-type)))
              ;; check that we only have one entry
              (br_if $no-hygiene (i32.ne (%cdr-l $head) (global.get $g-nil)))
              ;; check if this is let, let*, letrec, or letrec*
              ;; TODO let-values, let*-values
              (local.set $gp (%car-l $head))
              (br_if $need-hygiene (i32.eq (local.get $gp) (global.get $g-let)))
              (br_if $need-hygiene (i32.eq (local.get $gp) (global.get $g-let-star)))
              (br_if $need-hygiene (i32.eq (local.get $gp) (global.get $g-letrec)))
              (br_if $need-hygiene (i32.eq (local.get $gp) (global.get $g-letrec-star)))
              (br_if $need-hygiene (i32.eq (local.get $gp) (global.get $g-lambda)))
              (br $no-hygiene))

            ;; hygienic macros need a local environment
            (local.set $match-env (call $environment-init
                (global.get $g-heap)
                (local.get $match-env))))


          (if (i32.eq (%car-l $template) (local.get $ellipsis)) (then
              ;; the next item in the template is an ellipsis,
              ;; skip over the ellipsis
              (local.set $template (%cdr-l $template))
              ;; expand the current element until we get nothing
              (loop $forever
                (local.set $matched (call $expand-syntax-template
                    (local.get $curr)
                    (local.get $match-env)
                    (local.get $ellipsis)
                    (i32.const 1)
                    (%alloc-cons (local.get $head) (local.get $context))))
                (if (i32.eq (local.get $matched) (global.get $g-nil)) (then
                    ;; finished expanding ellipsis
                    (br $start)))
                (local.set $matched (%alloc-list-1 (local.get $matched)))
                (if (i32.eq (local.get $head) (global.get $g-nil))
                  (then (local.set $head (local.get $matched)))
                  (else (%set-cdr!-l $tail $matched)))
                (local.set $tail (local.get $matched))
                (br $forever))))

          (local.set $matched (call $expand-syntax-template
              (local.get $curr)
              (local.get $match-env)
              (local.get $ellipsis)
              (local.get $in-ellipsis)
              (%alloc-cons (local.get $head) (local.get $context))))

          (if (local.get $in-ellipsis) (then
              ;; we are in an ellipsis expansion, if we don't have another
              ;; element (i.e. we get a ()), then return ()
              (if (i32.eq (local.get $matched) (global.get $g-nil)) (then
                  (return (local.get $matched))))))

          (if (i32.eq (%get-type $matched) (%error-type)) (then
              (return (local.get $matched))))

          ;; add this result to the list we are building
          (local.set $matched (%alloc-list-1 (local.get $matched)))
          (if (i32.eq (local.get $head) (global.get $g-nil))
            (then (local.set $head (local.get $matched)))
            (else (%set-cdr!-l $tail $matched)))
          (local.set $tail (local.get $matched))

          ;; check for improper form
          (block $improper
            ;; not relevant for vectors
            (br_if $improper (i32.eq (local.get $type) (%vector-type)))
            (local.set $template-type (%get-type $template))
            ;; only an improper form if the tail is not () or a cons cell
            (br_if $improper (i32.eq (local.get $template-type) (%cons-type)))
            (br_if $improper (i32.eq (local.get $template-type) (%nil-type)))

            ;; this is an improper form.
            (local.set $matched (call $expand-syntax-template
                (local.get $template)
                (local.get $match-env)
                (local.get $ellipsis)
                (local.get $in-ellipsis)
                (%alloc-cons (local.get $head) (local.get $context))))

            (if (local.get $in-ellipsis) (then
                ;; we are in an ellipsis expansion, if we don't have another
                ;; element (i.e. we get a ()), then return ()
                (if (i32.eq (local.get $matched) (global.get $g-nil)) (then
                    (return (local.get $matched))))))

            (if (i32.eq (%get-type $matched) (%error-type)) (then
                (return (local.get $matched))))

            ;; set this result to the cdr of the tail to create an improper list
            (if (i32.eq (local.get $head) (global.get $g-nil))
              (then (local.set $head (local.get $matched)))
              (else (%set-cdr!-l $tail $matched)))

            (return (local.get $head)))

          (br $start)))

      (if (i32.eq (local.get $type) (%cons-type))
        (then
          ;; this is a list so simply return the list that we just built
          (return (local.get $head)))
        (else
          ;; return a vector
          (return (call $make-vector-internal (local.get $head) (i32.const 1))))))

  ;; everything else
  (return (local.get $template)))

(func $match-syntax-pattern
  (param $pattern i32)
  (param $exp i32)
  (param $ellipsis i32)
  (param $literals i32)
  (result i32)

  (local $match-env i32)
  (local $res i32)

  (local.set $match-env (call $environment-init
      (global.get $g-heap)
      (i32.const 0)))

  (if (global.get $g-trace-macros) (then
      (call $print-symbol (%sym-64 0x203F686374616d 7)) ;; 'match? '
      (call $print (local.get $exp))
      (call $print-symbol (%sym-64 0x206874697720 6)) ;; ' with '
      (call $print (local.get $pattern))
      (call $print-symbol (global.get $g-newline))))

  (if (i32.eq (local.get $pattern) (local.get $exp)) (then
      (if (i32.eq (local.get $pattern) (global.get $g-nil)) (then
          (return (local.get $match-env))))))

  (call $initialize-match-env
    (local.get $pattern)
    (local.get $ellipsis)
    (local.get $literals)
    (local.get $match-env)
    (i32.const 0))

  (local.set $res (call $match-syntax-pattern-impl
      (local.get $pattern)
      (local.get $exp)
      (local.get $ellipsis)
      (local.get $literals)
      (local.get $match-env)
      (i32.const 0)))

  (if (i32.eqz (local.get $res)) (then
      (call $environment-destroy (local.get $match-env) (i32.const 0))
      (call $heap-free (global.get $g-heap) (local.get $match-env))
      (return (i32.const 0))))

  (return (local.get $match-env)))

(func $initialize-match-env
  (param $pattern i32)
  (param $ellipsis i32)
  (param $literals i32)
  (param $match-env i32)
  (param $in-ellipsis i32)

  (local $pattern-type i32)
  (local $element i32)
  (local $ptr i32)
  (local $count i32)
  (local $next i32)

  (if (i32.eq (local.get $pattern) (global.get $g-underscore)) (then
    (return)))

  ;; process identifiers
  (if (i32.eq (local.tee $pattern-type (%get-type $pattern)) (%symbol-type))
    (then
      (if (call $symbol-in-list? (local.get $pattern) (local.get $literals))
        ;; ignore literals
        (then (return)))
      (if (local.get $in-ellipsis) (then
          ;; initialize ellipsis element lists
          (call $environment-add
            (local.get $match-env)
            (local.get $pattern)
            (global.get $g-nil))))
      (return)))

  (if (i32.eq (local.get $pattern-type) (%cons-type)) (then
      ;; recursively process each element in the list

      (block $end (loop $start
          (%chk-type $end $pattern %cons-type)
          (%pop-l $element $pattern)

          ;; check if this element is an ellisis element
          (if (i32.eq (%car-l $pattern) (local.get $ellipsis))
            (then
              (call $initialize-match-env
                (local.get $element)
                (local.get $ellipsis)
                (local.get $literals)
                (local.get $match-env)
                (i32.const 1))
              ;; move past the ellipsis
              (local.set $pattern (%cdr-l $pattern)))
            (else
              (call $initialize-match-env
                (local.get $element)
                (local.get $ellipsis)
                (local.get $literals)
                (local.get $match-env)
                (local.get $in-ellipsis))))

          ;; check for improper pattern list
          (local.set $pattern-type (%get-type $pattern))
          (block $improper
            (br_if $improper (i32.eq (local.get $pattern-type) (%cons-type)))
            (br_if $improper (i32.eq (local.get $pattern-type) (%nil-type)))

            (call $initialize-match-env
                (local.get $pattern)
                (local.get $ellipsis)
                (local.get $literals)
                (local.get $match-env)
                (local.get $in-ellipsis))
            (return))

          (br $start)))

      (return)))

  (if (i32.eq (local.get $pattern-type) (%vector-type)) (then
      ;; recursively process each element in the vector
      (local.set $ptr (%car-l $pattern))
      (local.set $count (%cdr-l $pattern))

      (if (local.get $count) (then
          (local.set $next (i32.load (local.get $ptr)))))

      (block $end (loop $start
          (br_if $end (i32.eqz (local.get $count)))
          (local.set $element (local.get $next))
          (%dec $count)
          (%plus-eq $ptr 4)

          (if (local.get $count)
            (then
              (local.set $next (i32.load (local.get $ptr))))
            (else
              (local.set $next (global.get $g-nil))))

          ;; check if this element is an ellipsis element
          (if (i32.eq (local.get $next) (local.get $ellipsis))
            (then
              (call $initialize-match-env
                (local.get $element)
                (local.get $ellipsis)
                (local.get $literals)
                (local.get $match-env)
                (i32.const 1))
              ;; move past the ellipsis
              (%dec $count)
              (%plus-eq $ptr 4))
            (else
              (call $initialize-match-env
                (local.get $element)
                (local.get $ellipsis)
                (local.get $literals)
                (local.get $match-env)
                (local.get $in-ellipsis))))

          (br $start)))

      (return)))

  (return))

(;
 ;  An input expression E matches a pattern P if and only if:
 ;    • P is an underscore (_); OR
 ;    • P is a non-literal identifier; OR
 ;    • P is a literal identifier and E is an identifier with the same binding; OR
 ;    • P is a list (P1 ... Pn) and E is a list of n elements that match P1
 ;        through Pn, respectively; OR
 ;    • P is an improper list (P1 P2 ... Pn . Pn+1) and E is a list or improper
 ;        list of n or more elements that match P1 through Pn, respectively, and
 ;        whose nth tail matches Pn+1; OR
 ;    • P is of the form (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn) where E is a
 ;        proper list of n elements, the first k of which match P1 through Pk,
 ;        respectively, whose next m − k elements each match Pe, whose
 ;        remaining n − m elements match Pm+1 through Pn; OR
 ;    • P is of the form (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px) where E is
 ;        a list or improper list of n elements, the first k of which match P1
 ;        through Pk, whose next m − k elements each match Pe, whose remaining
 ;        n−m elements match Pm+1 through Pn, and whose nth and final cdr
 ;        matches Px; OR
 ;    • P is a vector of the form #(P1 ... Pn) and E is a vector of n elements
 ;        that match P1 through Pn; OR
 ;    • P is of the form #(P1 ... Pk Pe <ellipsis> Pm+1 ... Pn) where E is a
 ;        vector of n elements the first k of which match P1 through Pk, whose
 ;        next m − k elements each match Pe, and whose remaining n − m elements
 ;        match Pm+1 through Pn; OR
 ;    • P is a constant and E is equal to P in the sense of the equal? procedure.
 ;)
(func $match-syntax-pattern-impl
  (param $pattern i32)
  (param $exp i32)
  (param $ellipsis i32)
  (param $literals i32)
  (param $match-env i32)
  (param $in-ellipsis i32)
  (result i32)

  (local $pattern-type i32)
  (local $exp-type i32)
  (local $curr-pattern i32)
  (local $next-pattern i32)
  (local $curr-exp i32)
  (local $next-exp i32)
  (local $pattern-ptr i32)
  (local $exp-ptr i32)
  (local $pattern-count i32)
  (local $exp-count i32)
  (local.set $pattern-type (%get-type $pattern))

  ;; underscore
  (if (i32.eq (local.get $pattern) (global.get $g-underscore)) (then
      (return (i32.const 1))))

  ;; identifiers
  (if (i32.eq (local.get $pattern-type) (%symbol-type)) (then
      ;; pattern is an identifier
      (if (call $symbol-in-list? (local.get $pattern) (local.get $literals)) (then
          ;; pattern is a literal, then this matches if the pattern is the same
          ;; as the exp, otherwise no match;
          (return (i32.eq (local.get $pattern) (local.get $exp)))))
      ;; pattern is a non-literal identifier
      (if (local.get $in-ellipsis)
        (then
          (local.set $curr-exp (call $environment-get
              (local.get $match-env)
              (local.get $pattern)))
          ;; this is in an ellipsis, so store a list

          (if (i32.eq (%get-type $curr-exp) (%error-type)) (then
              ;; There is nothing in the environment, this is an error
              (return (i32.const 0))))

          ;; the list is empty, replace it with a single item list
          (if (i32.eq (local.get $curr-exp) (global.get $g-nil)) (then
              (drop (call $environment-set!
                  (local.get $match-env)
                  (local.get $pattern)
                  (%alloc-list-1 (local.get $exp))))
              (return (i32.const 1))))

          (if (i32.ne (%get-type $curr-exp) (%cons-type)) (then
              ;; current item in the env isn't a list, this is an error
              (return (i32.const 0))))

          ;; the list is non-empty
          (block $end (loop $start
              (br_if $end (i32.eq
                  (local.tee $next-exp (%cdr-l $curr-exp))
                  (global.get $g-nil)))
              (local.set $curr-exp (local.get $next-exp))
              (br $start)))
          (%set-cdr! (local.get $curr-exp) (%alloc-list-1 (local.get $exp)))
          (return (i32.const 1))))

      (if (call $environment-has (local.get $match-env) (local.get $pattern))
        ;; this is already defined in the environment
        (then (return (i32.const 0))))
      (call $environment-add
        (local.get $match-env)
        (local.get $pattern)
        (local.get $exp))
      ;; Expression is now bound to this pattern literal
      (return (i32.const 1))))

  (local.set $exp-type (%get-type $exp))

  ;; expression type must be the same as pattern type from here on
  (block $type-check
    ;; if the pattern is a list, then the expression can be an empty list (for
    ;; example to match ((a b) ...) with ()
    (if (i32.eq (local.get $pattern-type) (%cons-type)) (then
      (br_if $type-check (i32.eq (local.get $exp-type) (%nil-type)))))

    ;; For all other cases the types must be the same
    (if (i32.ne (local.get $exp-type) (local.get $pattern-type)) (then
        (return (i32.const 0)))))

  ;; lists
  (if (i32.eq (local.get $pattern-type) (%cons-type)) (then
      ;; walk along the pattern matching each step
      (block $end (loop $start
          (%chk-type $end $pattern %cons-type)

          (%pop-l $curr-pattern $pattern)

          (local.set $next-pattern (%car-l $pattern))
          (if (i32.eq (local.get $next-pattern) (local.get $ellipsis))
            (then
              ;; this is an ellipsis, so we need to try and match as many as
              ;; possible
              ;; advance pattern past the ellipsis
              (local.set $pattern (%cdr-l $pattern))

              ;; if there is nothing left to match, then this is an empty
              ;; ellipsis
              (%chk-type $end $exp %cons-type)
              (%pop-l $curr-exp $exp)

              (block $inner-end (loop $inner-start
                  (if (call $match-syntax-pattern-impl
                      (local.get $curr-pattern)
                      (local.get $curr-exp)
                      (local.get $ellipsis)
                      (local.get $literals)
                      (local.get $match-env)
                      (i32.const 1))
                    (then
                      ;; current expression matched the ellipsis pattern,
                      ;; try the next
                      ;; TODO: this won't handle some cases where we might
                      ;; need to check the lengths of the lists
                      (%chk-type $inner-end $exp %cons-type)
                      (%pop-l $curr-exp $exp)
                      (br $inner-start))
                    (else
                      ;; current expression didn't match the ellipsis pattern,
                      ;; push it back, and continue
                      (%push-l $curr-exp $exp))))))
            (else
              (%chk-type $end $exp %cons-type)
              (%pop-l $curr-exp $exp)

              ;; regular one-one match
              (if (i32.eqz (call $match-syntax-pattern-impl
                    (local.get $curr-pattern)
                    (local.get $curr-exp)
                    (local.get $ellipsis)
                    (local.get $literals)
                    (local.get $match-env)
                    (local.get $in-ellipsis)))
                  (then (return (i32.const 0))))))

          ;; check for improper pattern list
          (local.set $pattern-type (%get-type $pattern))
          (block $improper
            (br_if $improper (i32.eq (local.get $pattern-type) (%cons-type)))
            (br_if $improper (i32.eq (local.get $pattern-type) (%nil-type)))

            (return (call $match-syntax-pattern-impl
                (local.get $pattern)
                (local.get $exp)
                (local.get $ellipsis)
                (local.get $literals)
                (local.get $match-env)
                (local.get $in-ellipsis))))

          (br $start)))

      (if (i32.eq (local.get $pattern) (global.get $g-nil)) (then
          (if (i32.eq (local.get $exp) (global.get $g-nil)) (then
              (return (i32.const 1))))))

      (return (i32.const 0))))

  (if (i32.eq (local.get $pattern-type) (%vector-type)) (then
      (local.set $pattern-ptr (%car-l $pattern))
      (local.set $pattern-count (%cdr-l $pattern))
      (local.set $exp-ptr (%car-l $exp))
      (local.set $exp-count (%cdr-l $exp))

      (local.set $next-pattern (i32.load (local.get $pattern-ptr)))
      (block $end (loop $start
          (br_if $end (i32.eqz (local.get $pattern-count)))
          (br_if $end (i32.eqz (local.get $exp-count)))

          (local.set $curr-pattern (local.get $next-pattern))
          (local.set $curr-exp (i32.load (local.get $exp-ptr)))
          (%dec $pattern-count)
          (%dec $exp-count)
          (%plus-eq $pattern-ptr 4)
          (%plus-eq $exp-ptr 4)

          (if (local.get $pattern-count)
            (then
              (local.set $next-pattern (i32.load (local.get $pattern-ptr))))
            (else
              (local.set $next-pattern (i32.const 0))))

          (if (i32.eq (local.get $next-pattern) (local.get $ellipsis))
            (then
              ;; skip the counter over the ellipsis
              (%dec $pattern-count)
              (%plus-eq $pattern-ptr 4)
              (if (local.get $pattern-count)
                (then
                  (local.set $next-pattern (i32.load (local.get $pattern-ptr))))
                (else
                  (local.set $next-pattern (i32.const 0))))

              (if (i32.lt_u (local.get $exp-count) (local.get $pattern-count))
                (then
                  ;; there are no matches for this ellipsis, rewind the
                  ;; exp count and ptr to continue matching them against the
                  ;; next pattern
                  (%inc $exp-count)
                  (%minus-eq $exp-ptr 4)
                  (br $start)))

              ;; loop do-while exp-count > pattern-count
              (block $inner-end (loop $inner-start
                  (if (i32.eqz (call $match-syntax-pattern-impl
                        (local.get $curr-pattern)
                        (local.get $curr-exp)
                        (local.get $ellipsis)
                        (local.get $literals)
                        (local.get $match-env)
                        (i32.const 1)))
                      (then (return (i32.const 0))))
                  (br_if $inner-end (i32.eq (local.get $exp-count) (local.get $pattern-count)))

                  (local.set $curr-exp (i32.load (local.get $exp-ptr)))
                  (%dec $exp-count)
                  (%plus-eq $exp-ptr 4)
                  (br $inner-start))))
            (else
              (if (i32.eqz (call $match-syntax-pattern-impl
                    (local.get $curr-pattern)
                    (local.get $curr-exp)
                    (local.get $ellipsis)
                    (local.get $literals)
                    (local.get $match-env)
                    (local.get $in-ellipsis)))
                  (then (return (i32.const 0))))))

          (br $start)))))

  ;; at this point pattern must be a constant, only match if (equal? pattern exp)
  (return (call $equal-inner
      (local.get $pattern)
      (local.get $exp)
      (i32.const 1))))

(func $symbol-in-list? (param $symbol i32) (param $list i32) (result i32)
  (local $curr i32)
  (block $end (loop $start
      (%chk-type $end $list %cons-type)
      (%pop-l $curr $list)

      (if (i32.eq (local.get $symbol) (local.get $curr)) (then
          (return (i32.const 1))))

      (br $start)))
  (return (i32.const 0)))

;; gets the length of a list, if it is an improper list, returns the length, not
;; including the improper cdr value
;; i.e  (a b c) => 3
;;      (a b . c) => 2
(func $improper-list-len (param $list i32) (result i32)
  (local $count i32)

  (local.set $count (i32.const 0))
  (block $end (loop $start
      (%chk-type $end $list %cons-type)
      (%inc $count)
      (local.set $list (%cdr-l $list))
      (br $start)))

  (return (local.get $count)))

(global $g-hygienic-counter (mut i32) (i32.const 0x10000000))

(func $make-hygienic (result i32)
  (local $str i32)
  (%ginc $g-hygienic-counter)
  (local.set $str (call $integer->string-impl
      (i64.extend_i32_u (global.get $g-hygienic-counter))
      (i32.const 16)))
  (i32.store8 offset=4 (local.get $str) (i32.const 0x58)) ;; set first char to X
  (return (%alloc-symbol (local.get $str))))
