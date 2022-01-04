(define have-prolog? #f)
(cond 
  ((not have-prolog?)
    (include 
      "pair.scm"
      "numerics.scm"
      "procedures.scm")
    (display "Loaded prolog")
    (set! have-prolog? #t))
  (else
    (display "\x1B;[0;31mProlog has already been loaded!\x1B;[0m")))
(display #\newline)