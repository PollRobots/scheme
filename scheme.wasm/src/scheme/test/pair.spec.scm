(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests
  (list "test pair?" (lambda ()
    (assert (pair? '(a . b)) "'(a . b) is a pair")
    (assert (pair? '(a b c)) "'(a b c) is a pair")
    (assert (not (pair? '())) "'() is not a pair")
    (assert (not (pair? '#(a b))) "'#(a b) is not a pair")))

  (list "test cons" (lambda ()
    (assert (equal? (cons 'a '()) '(a)) "(cons 'a '()) is (a)")
    (assert (equal? (cons 'a '(b c d)) '(a b c d)) "(cons 'a '(b c d)) is (a b c d)")
    (assert (equal? (cons "a" '(b c)) '("a" b c)) "(cons \"a\" '(b c)) is (\"a\" b c)")
    (assert (equal? (cons 'a 3) '(a . 3)) "(cons 'a 3) is (a . 3)")
    (assert (equal? (cons '(a b) 'c) '((a b) . c)) "(cons '(a b) 'c) is ((a b) . c)")))
  )
