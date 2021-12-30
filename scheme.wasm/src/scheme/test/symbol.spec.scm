(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "symbols"
  (test-case "(symbol? <obj>)" (lambda ()
    (assert (symbol? 'foo) "(symbol? 'foo)")
    (assert (symbol? (car '(a b))) "(symbol? (car '(a b)))")
    (assert-not (symbol? "bar") "(symbol? \"bar\")")
    (assert (symbol? 'nil) "(symbol? 'nil)")
    (assert-not (symbol? '()) "(symbol? '())")
    (assert-not (symbol? #f) "(symbol? #f)")))

  (test-case "(symbol=? <symbol_1> <symbol_2> ...)" (lambda ()
    (assert (symbol=? 'a 'a) "(symbol=? 'a 'a)")
    (assert (symbol=? 'a 'a 'a) "(symbol=? 'a 'a 'a)")
    (assert-not (symbol=? 'a 'a #\a) "(symbol=? 'a 'a #\\a)")
    (assert-not (symbol=? 'a 'b) "(symbol=? 'a 'b)")))

  (test-case "(symbol->string <symbol>)" (lambda ()
    (assert-equal (symbol->string 'flying-fish) "flying-fish")
    (assert-equal (symbol->string 'Martin) "Martin")
    (assert-equal (symbol->string
                    (string->symbol "Malvina")) "Malvina")))

  (test-case "(string->symbol <string>)" (lambda ()
    (assert-equal (string->symbol "mISSISSIppi") 'mISSISSIppi)
    (assert 
      (eqv? 'bitBlt (string->symbol "bitBlt")) 
      "(eqv? 'bitBlt (string->symbol \"bitBlt\"))")
    (assert
      (eqv? 'LollyPop (string->symbol (symbol->string 'LollyPop)))
      "(eqv? 'LollyPop (string->symbol (symbol->string 'LollyPop)))")
    (assert
      (string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D.")))
      "(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))")))
)