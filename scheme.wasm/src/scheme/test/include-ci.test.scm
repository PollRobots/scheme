#| This file is designed to be included with include-ci |#

(DEFINE (FACT X)
        (LETREC ((INNER (LAMBDA (A N)
                            (IF (= N 1)
                                A
                                (INNER (* A N) (- N 1))))))
            (INNER 1 X)))
