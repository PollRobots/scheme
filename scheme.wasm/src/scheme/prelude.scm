(define have-prelude? #f)
(cond 
  ((not have-prelude?)
    (include 
      "pair.scm"
      "numerics.scm"
      "procedures.scm")
    (display "Loaded prelude")
    (set! have-prelude? #t))
  (else
    (display "\x1B;[0;31mprelude has already been loaded!\x1B;[0m")))
(display #\newline)