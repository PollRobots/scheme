(define have-prelude? #f)
(cond
  ((not have-prelude?)
    (include
      "lazy.scm"
      "pair.scm"
      "numerics.scm"
      "procedures.scm"
      "guard.scm")
    (for-each display (list "Version: " (version) #\newline))
    (display "Loaded prelude")
    (set! have-prelude? #t))
  (else
    (display "\x1B;[0;31mprelude has already been loaded!\x1B;[0m")))
