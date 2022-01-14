(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "symbols"
  (test-case "(symbol? <obj>)" (lambda ()
    (assert (symbol? 'foo))
    (assert (symbol? (car '(a b))))
    (assert-not (symbol? "bar"))
    (assert (symbol? 'nil))
    (assert-not (symbol? '()))
    (assert-not (symbol? #f))))

  (test-case "(symbol=? <symbol_1> <symbol_2> ...)" (lambda ()
    (assert (symbol=? 'a 'a))
    (assert (symbol=? 'a 'a 'a))
    (assert-not (symbol=? 'a 'a #\a))
    (assert-not (symbol=? 'a 'b))))

  (test-case "(symbol->string <symbol>)" (lambda ()
    (assert-equal (symbol->string 'flying-fish) "flying-fish")
    (assert-equal (symbol->string 'Martin) "Martin")
    (assert-equal (symbol->string
                    (string->symbol "Malvina")) "Malvina")))

  (test-case "(string->symbol <string>)" (lambda ()
    (assert-equal (string->symbol "mISSISSIppi") 'mISSISSIppi)
    (assert (eqv? 'bitBlt (string->symbol "bitBlt")))
    (assert (eqv? 'LollyPop (string->symbol (symbol->string 'LollyPop))))
    (assert (string=? "K. Harper, M.D."
                (symbol->string (string->symbol "K. Harper, M.D."))))
    (assert (string=? "K. Harper, M.D."
                (symbol->string '|K. Harper, M.D.|)))))
)

#!fold-case

(RUN-TESTS "symbols (with fold-case)"
  (TEST-CASE "(SYMBOL? <obj>)" (lambda ()
    (ASSERT (SYMBOL? 'foo))
    (ASSERT (SYMBOL? (CAR '(A B))))
    (ASSERT-NOT (SYMBOL? "bar"))
    (ASSERT (SYMBOL? 'NIL))
    (ASSERT-NOT (SYMBOL? '()))
    (ASSERT-NOT (SYMBOL? #F))))

  (TEST-CASE "(symbol=? <symbol_1> <symbol_2> ...)" (lambda ()
    (ASSERT (SYMBOL=? 'A 'a))
    (ASSERT (SYMBOL=? 'A 'A 'a))
    (ASSERT-NOT (SYMBOL=? 'A 'A #\a))
    (ASSERT-NOT (SYMBOL=? 'A 'B))))

  (TEST-CASE "(symbol->string <symbol>)" (lambda ()
    (ASSERT-EQUAL (SYMBOL->STRING 'FLYING-FISH) "flying-fish")
    (ASSERT-EQUAL (SYMBOL->STRING 'MARTIN) "martin")
    (ASSERT-EQUAL (SYMBOL->STRING
                    (STRING->SYMBOL "Malvina")) "Malvina")))

  (TEST-CASE "(string->symbol <string>)" (lambda ()
    (ASSERT-EQUAL (STRING->SYMBOL "mississippi") 'mISSISSIppi)
    (ASSERT (EQV? 'BITBLT (STRING->SYMBOL "bitblt")))
    (ASSERT (EQV? 'LOLLYPOP (STRING->SYMBOL (SYMBOL->STRING 'lollypop))))
    (ASSERT (STRING=? "K. Harper, M.D."
              (SYMBOL->STRING (STRING->SYMBOL "K. Harper, M.D."))))))
)
