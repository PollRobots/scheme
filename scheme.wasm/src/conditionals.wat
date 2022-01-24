;; (if <test> <consequent> [<alternate>])
(func $if (param $env i32) (param $args i32) (result i32)
  (local $num-args i32)
  (local $temp i32)
  (local $test i32)
  (local $consequent i32)
  (local $alternate i32)

  (block $b_check
    (block $b_2_args
      (block $b_fail
        (local.set $temp (local.get $args))
        (local.set $num-args (call $list-len (local.get $temp)))
        (br_if $b_fail (i32.lt_u (local.get $num-args) (i32.const 2)))

        (%pop-l $test $temp)
        (%pop-l $consequent $temp)
        (br_if $b_2_args (i32.eq (local.get $num-args) (i32.const 2)))

        (local.set $alternate (%car-l $temp))
        (br_if $b_check (i32.eq (local.get $num-args) (i32.const 3))))

      (return (call $argument-error (local.get $args))))

    (local.set $alternate (global.get $g-nil)))

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $test)
        (call $cont-alloc
          (%cont-if)
          (local.get $env)
          (%cdr-l $args)
          (i32.const 0)))))

;; (cont-if test consequent [alternate])
(func $cont-if (param $env i32) (param $args i32) (result i32)
  (local $test i32)
  (local $expr i32)

  (%pop-l $test $args)
  (%pop-l $expr $args)

  (if (i32.eqz (call $is-truthy (local.get $test)))
    (then
      (if (i32.eq (%get-type $args) (%cons-type))
        (then (%pop-l $expr $args))
        (else (return (global.get $g-nil))))))

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $expr)
        (i32.const 0))))

;; (cond <clause_1> <clause_2> ...)
;;    clause::=
;;      (<test> <expression_1> ...)
;;      (<test> => <expression>)
;;    final clause::=
;;      (else <expression_1> ...)
(func $cond (param $env i32) (param $args i32) (result i32)
  (local $clauses i32)
  (local $clause i32)
  (local $test i32)

  (block $b_error
    (if (i32.eq (%get-type $args) (%nil-type)) (then
      (return (global.get $g-nil))))

    (local.set $clauses (local.get $args))
    (%pop-l $clause $clauses)

    ;; verify that the clause is a list at least 2 long
    (br_if $b_error (i32.eqz (call $is-list-impl (local.get $clause))))
    (br_if $b_error (i32.lt_u (call $list-len (local.get $clause)) (i32.const 2)))

    (%pop-l $test $clause)
    ;; check if this is an else clause
    (if (i32.eq (local.get $test) (global.get $g-else))
      (then
        ;; check that this is a final clause
        (%chk-type $b_error $clauses %nil-type)
        (return (call $eval-body (i32.const 0) (local.get $env) (local.get $clause)))))

    (%push-l $clause $clauses)
    (return (call $cont-alloc
          (%eval-fn)
          (local.get $env)
          (local.get $test)
          (call $cont-alloc
            (%cont-cond)
            (local.get $env)
            (local.get $clauses)
            (i32.const 0)))))

  (return (call $argument-error (local.get $args))))

;; (cont-cond test-res clause clauses ...)
(func $cont-cond (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $clauses i32)
  (local $test-res i32)
  (local $clause i32)
  (local $expr i32)

  (local.set $temp (local.get $args))
  (%pop-l $test-res $temp)
  (%pop-l $clause $temp)
  (local.set $clauses (local.get $temp))

  ;; if test isn't truthy then call cond with the remaining clauses
  (if (i32.eqz (call $is-truthy(local.get $test-res)))
    (then (return (call $cond (local.get $env) (local.get $clauses)))))

  ;; check if this is an arrow clause
  (if (i32.eq (%car-l $clause) (global.get $g-arrow))
    (then
      ;; clause, which should be (=> expr)
      ;; rewrite as (expr (quote test-res))
      (%pop-l $temp $clause)
      (%pop-l $expr $clause)
      (if (i32.ne (%get-type $clause) (%nil-type))
        (then (return (call $argument-error (local.get $args)))))

      (local.set $clause (%alloc-cons
          (local.get $expr)
          (%alloc-cons
            (%alloc-quote (local.get $test-res))
            (global.get $g-nil))))
      ;; evaluate re-written clause
      (return (call $cont-alloc
            (%eval-fn)
            (local.get $env)
            (local.get $clause)
            (i32.const 0)))))

  (return (call $eval-body (i32.const 0) (local.get $env) (local.get $clause))))

;; (case <key> <clause_1> <clause_2> ...)
;;    clause ::=
;;      ((<datum_1> ...) <expression_1> ...)
;;      ((<datim_1> ...) => <expression>)
;;      (else <expression_1> ...)
;;      (else => <expression>)
(func $case (param $env i32) (param $args i32) (result i32)
  (if (i32.eqz (call $list-len (local.get $args)))
    (then (return (call $argument-error (local.get $args)))))

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (%car-l $args)
        (call $cont-alloc
          (%cont-case)
          (local.get $env)
          (%cdr-l $args)
          (i32.const 0)))))

;; (cont-case <key> <clause_1> <clause_2> ...)
(func $cont-case (param $env i32) (param $args i32) (result i32)
  (local $temp i32)
  (local $key i32)
  (local $clause i32)
  (local $clauses i32)
  (local $head i32)
  (local $tail i32)
  (local $fn i32)
  (local $datum i32)

  (local.set $temp (local.get $args))
  (%pop-l $key $temp)

  (local.set $clauses (local.get $temp))

  (block $b_error
    (loop $b_start
      (if (i32.eq (%get-type $clauses) (%nil-type))
        (then (return(local.get $clauses))))

      (%pop-l $clause $clauses)

      ;; check that clause is a list
      (br_if $b_error (i32.eqz (call $is-list-impl (local.get $clause))))
      ;; at least 2 long
      (br_if $b_error (i32.lt_u (call $list-len (local.get $clause)) (i32.const 2)))

      (local.set $head (%car-l $clause))
      (local.set $tail (%cdr-l $clause))

      (if (i32.eq (local.get $head) (global.get $g-else))
        (then
          ;; else clause must be last
          (%chk-type $b_error $clauses %nil-type)

          (if (i32.eq (%car-l $tail) (global.get $g-arrow))
            (then
              ;; this is an arrow else,
              ;; check that the tail is (=> expr)
              (br_if $b_error (i32.ne (call $list-len (local.get $tail)) (i32.const 2)))

              ;; construct (expr (quote key))
              (local.set $fn (global.get $g-nil))
              (%push (%alloc-quote (local.get $key)) $fn)
              (%push (%car (%cdr-l $tail)) $fn)

              (return (call $cont-alloc
                    (%eval-fn) ;; eval
                    (local.get $env)
                    (local.get $fn)
                    (i32.const 0))))

            (else
              ;; this is an ordinary else
              (return (call $eval-body (i32.const 0) (local.get $env) (local.get $tail)))))))

      ;; this is a regular clause, check that head is a list...
      (br_if $b_error (i32.eqz (call $is-list-impl (local.get $head))))

      ;; compare each item in the head with the key (using eqv?)
      (block $check_key_end
        (loop $check_key_start
          (br_if $check_key_end (i32.eq (%get-type $head) (%nil-type)))

          (%pop-l $datum $head)
          (if (call $equal-inner (local.get $key) (local.get $datum) (i32.const 0))
            (then
              ;; we have a match, evaluate the tail
              (if (i32.eq (%car-l $tail) (global.get $g-arrow))
                (then
                  ;; this is an arrow clause
                  ;; check that the tail is (=> expression)
                  (br_if $b_error (i32.ne (call $list-len (local.get $tail)) (i32.const 2)))

                  ;; construct (expr (quote key))
                  (local.set $fn (global.get $g-nil))
                  (%push (%alloc-quote (local.get $key)) $fn)
                  (%push (%car (%cdr-l $tail)) $fn)

                  (return (call $cont-alloc
                        (%eval-fn) ;; eval
                        (local.get $env)
                        (local.get $fn)
                        (i32.const 0))))
                (else
                  ;; this is a regular clause
                  (return (call $eval-body (i32.const 0) (local.get $env) (local.get $tail)))))))

          (br $check_key_start)))

    ;; no match, so call again
    (br $b_start)))

  (return (call $argument-error (local.get $args))))

;; (and <test> ...)
(func $and (param $env i32) (param $args i32) (result i32)
  (local $head i32)

  (if (i32.eq (%get-type $args) (%nil-type))
    (then (return (global.get $g-true))))

  (%pop-l $head $args)

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $head)
        (call $cont-alloc
          (%cont-and)
          (local.get $env)
          (local.get $args)
          (i32.const 0)))))

;; (cont-and <test-res> <test> ...)
(func $cont-and (param $env i32) (param $args i32) (result i32)
  (local $test-res i32)

  (%pop-l $test-res $args)
  (if (i32.eqz (call $is-truthy (local.get $test-res)))
    (then (return (local.get $test-res))))

  (if (i32.eq (%get-type $args) (%nil-type))
    (then (return (local.get $test-res))))

  (return (call $and (local.get $env) (local.get $args))))

(func $or (param $env i32) (param $args i32) (result i32)
  (local $head i32)

  (if (i32.eq (%get-type $args) (%nil-type))
    (then (return (global.get $g-false))))

  (%pop-l $head $args)

  (return (call $cont-alloc
        (%eval-fn) ;; eval
        (local.get $env)
        (local.get $head)
        (call $cont-alloc
          (%cont-or)
          (local.get $env)
          (local.get $args)
          (i32.const 0)))))

;; (cont-or <test-res> <test> ...)
(func $cont-or (param $env i32) (param $args i32) (result i32)
  (local $test-res i32)

  (%pop-l $test-res $args)
  (if (call $is-truthy (local.get $test-res))
    (then (return (local.get $test-res))))

  (if (i32.eq (%get-type $args) (%nil-type))
    (then (return (local.get $test-res))))

  (return (call $or (local.get $env) (local.get $args))))

;; (when <test> <expression_1> ...)
(func $when (param $env i32) (param $args i32) (result i32)
  (local $test i32)

  (if (i32.lt_u (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args)))))

  (%pop-l $test $args)

  (return (call $cont-alloc
        (%eval-fn)
        (local.get $env)
        (local.get $test)
        (call $cont-alloc
          (%cont-when)
          (local.get $env)
          (local.get $args)
          (i32.const 0)))))

;; (cont-when test-res <expr> ...)
(func $cont-when (param $env i32) (param $args i32) (result i32)
  (local $test-res i32)

  (%pop-l $test-res $args)
  (if (call $is-truthy (local.get $test-res))
    (then (return (call $eval-body (i32.const 0) (local.get $env) (local.get $args)))))

  (return (global.get $g-nil)))

;; (unless <test> <expression_1> ...)
(func $unless (param $env i32) (param $args i32) (result i32)
  (local $test i32)

  (if (i32.lt_u (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args)))))

  (%pop-l $test $args)

  (return (call $cont-alloc
        (%eval-fn)
        (local.get $env)
        (local.get $test)
        (call $cont-alloc
          (%cont-unless)
          (local.get $env)
          (local.get $args)
          (i32.const 0)))))

;; (cont-unless test-res <expr> ...)
(func $cont-unless (param $env i32) (param $args i32) (result i32)
  (local $test-res i32)

  (%pop-l $test-res $args)
  (if (call $is-truthy (local.get $test-res))
    (then (return (global.get $g-nil))))

  (return (call $eval-body (i32.const 0) (local.get $env) (local.get $args))))

;; (begin <expression_1> ...)
(func $begin (param $env i32) (param $args i32) (result i32)
  (if (i32.eqz (call $list-len (local.get $args)))
    (then (return (call $argument-error (local.get $args)))))

  (return (call $eval-body (i32.const 1) (local.get $env) (local.get $args))))
