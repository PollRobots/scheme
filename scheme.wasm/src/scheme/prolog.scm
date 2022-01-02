(cond 
  ((error-object? have-prolog?)
    (define have-prolog? #f)
    (include 
      "pair.scm"
      "numerics.scm"
      "procedures.scm")
    (display "Loaded prolog")
    (set! have-prolog? #t))
  ((not have-prolog?)
    (display "\x1B;[0;31mProlog is being loaded!\x1B;[0m"))
  (else
    (display "\x1B;[0;31mProlog has already been loaded!\x1B;[0m")))
(display #\newline)