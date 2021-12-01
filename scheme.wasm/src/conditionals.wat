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

  (return
    (call $eval
      (local.get $env)
      (select
        (local.get $consequent)
        (local.get $alternate)
        (call $is-truthy
          (call $eval (local.get $env) (local.get $test)))))))


;; (cond <clause_1> <clause_2> ...)
;;    clause::=
;;      (<test> <expression_1> ...)
;;      (<test> => <expression>)
;;    final clause::=
;;      (else <expression_1> ...)
(func $cond (param $env i32) (param $args i32) (result i32)
  (local $clauses i32)
  (local $curr i32)
  (local $test i32)
  (local $test-result i32)
  (local $exprs i32)
  (local $fn i32)
  (local $tail i32)

  (local.set $clauses (local.get $args))
  (block $b_error
    (loop $forever
      (if (i32.eq (%get-type $clauses) (%nil-type))
        (then (return (global.get $g-nil))))

      (%pop-l $curr $clauses)
      (br_if $b_error (i32.eqz (call $is-list-impl (local.get $curr))))
      (local.set $test (%car-l $curr))
      (local.set $exprs (%cdr-l $curr))

      (if (i32.eq (local.get $test) (global.get $g-else))
        ;; this is an else clause
        (then
          ;; check that this is a final clause
          (%chk-type $b_error $clauses %nil-type)
          (return (call $cond-eval-exprs (local.get $env) (local.get $exprs)))))

      (local.set $test-result (call $eval (local.get $env) (local.get $test))) 
      (if (call $is-truthy (local.get $test-result))
        (then
          (if (i32.eq (%car-l $exprs) (global.get $g-arrow))
            (then
              ;; exprs at this point should be (=> <expression>)
              (br_if $b_error (i32.ne (call $list-len (local.get $exprs)) (i32.const 2)))
              (local.set $exprs (%cdr-l $exprs))
              ;; evaluate the argument
              (local.set $fn (call $eval (local.get $env) (%car-l $exprs)))
              ;; apply that to the result of the test
              (return 
                (call $apply-internal
                  (local.get $env) 
                  (local.get $fn)
                  (%alloc-cons (local.get $test-result) (global.get $g-nil)))))
            (else
              (return 
                (call $cond-eval-exprs 
                  (local.get $env) 
                  (local.get $exprs)))))))

      (br $forever)))
  
  (return (call $argument-error (local.get $args))))

(func $cond-eval-exprs (param $env i32) (param $exprs i32) (result i32)
  (local $curr i32)
  (local $last i32)

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $exprs) (%nil-type)))
      (%pop-l $curr $exprs)
      (local.set $last (call $eval (local.get $env) (local.get $curr)))

      (br $b_start)))

  (return (local.get $last)))

;; (case <key> <clause_1> <clause_2> ...)
;;    clause ::=
;;      ((<datum_1> ...) <expression_1> ...)
;;      ((<datim_1> ...) => <expression>)
;;      (else <expression_1> ...)
;;      (else => <expression>)
(func $case (param $env i32) (param $args i32) (result i32)
  (local $clauses i32)
  (local $curr i32)
  (local $key i32)
  (local $head i32)
  (local $tail i32)
  (local $fn i32)
  (local $datum i32)

  (if (i32.eqz (call $list-len (local.get $args)))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $key (call $eval (local.get $env) (%car-l $args)))

  (local.set $clauses (%cdr-l $args))

  (block $b_error
    (loop $forever
      (if (i32.eq (%get-type $clauses) (%nil-type))
        (then (return (global.get $g-nil))))

      (%pop-l $curr $clauses) 
      ;; check that clause is a list
      (br_if $b_error (i32.eqz (call $is-list-impl (local.get $curr))))
      ;; at least 2 long
      (br_if $b_error (i32.lt_u (call $list-len (local.get $curr)) (i32.const 2)))

      (local.set $head (%car-l $curr))
      (local.set $tail (%cdr-l $curr))

      (if (i32.eq (local.get $head) (global.get $g-else))
        (then
          ;; else clause must be last
          (%chk-type $b_error $clauses %nil-type)

          (if (i32.eq (%car-l $tail) (global.get $g-arrow))
            (then
              ;; this is an arrow else, 
              ;; check that the tail is (=> expression)
              (br_if $b_error (i32.ne (call $list-len (local.get $tail)) (i32.const 2)))

              (local.set $fn (call $eval (local.get $env) (%car (%cdr-l $tail))))
              (return 
                (call $apply-internal 
                  (local.get $env) 
                  (local.get $fn) 
                  (%alloc-cons (local.get $key) (global.get $g-nil)))))
            (else
              ;; this is an ordinary else
              (return (call $cond-eval-exprs (local.get $env) (local.get $tail)))))))
      
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
                  ;; check that the tial is (=> expression)
                  (br_if $b_error (i32.ne (call $list-len (local.get $tail)) (i32.const 2)))

                  (local.set $fn (call $eval (local.get $env) (%car (%cdr-l $tail))))
                  (return 
                    (call $apply-internal 
                      (local.get $env) 
                      (local.get $fn) 
                      (%alloc-cons (local.get $key) (global.get $g-nil)))))
                (else
                  ;; this is a regular clause
                  (return (call $cond-eval-exprs (local.get $env) (local.get $tail)))))))

          (br $check_key_start)))

      (br $forever)))

  
  (return (call $argument-error (local.get $args))))

;; (and <test> ...)
(func $and (param $env i32) (param $args i32) (result i32)
  (local $result i32)

  (local.set $result (global.get $g-true))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (local.set $result (call $eval (local.get $env) (%car-l $args)))
      (br_if $b_end (i32.eqz (call $is-truthy (local.get $result))))

      (local.set $args (%cdr-l $args))
      (br $b_start)))

  (return (local.get $result)))

;; (or <test> ...)
(func $or (param $env i32) (param $args i32) (result i32)
  (local $result i32)

  (local.set $result (global.get $g-false))

  (block $b_end
    (loop $b_start
      (br_if $b_end (i32.eq (%get-type $args) (%nil-type)))

      (local.set $result (call $eval (local.get $env) (%car-l $args)))
      (br_if $b_end (call $is-truthy (local.get $result)))

      (local.set $args (%cdr-l $args))
      (br $b_start)))

  (return (local.get $result)))

;; (when <test> <expression_1> ...)
(func $when (param $env i32) (param $args i32) (result i32)
  (local $test i32)

  (if (i32.lt_u (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $test (%car-l $args))  
  (if 
    (call $is-truthy (call $eval (local.get $env) (%car-l $args)))
    (then
      (return (call $cond-eval-exprs (local.get $env) (%cdr-l $args)))))
  
  (return (global.get $g-nil)))

;; (unless <test> <expression_1> ...)
(func $unless (param $env i32) (param $args i32) (result i32)
  (local $test i32)

  (if (i32.lt_u (call $list-len (local.get $args)) (i32.const 2))
    (then (return (call $argument-error (local.get $args)))))

  (local.set $test (%car-l $args))  
  (if 
    (i32.eqz (call $is-truthy (call $eval (local.get $env) (%car-l $args))))
    (then
      (return (call $cond-eval-exprs (local.get $env) (%cdr-l $args)))))
  
  (return (global.get $g-nil)))

;; (begin <expression_1> ...)
(func $begin (param $env i32) (param $args i32) (result i32)
  (if (i32.eqz (call $list-len (local.get $args)))
    (then (return (call $argument-error (local.get $args)))))

  (return (call $cond-eval-exprs (local.get $env) (local.get $args))))