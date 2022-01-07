(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "list and pair"

  (test-case "(pair? <obj>)" (lambda ()
    (assert (pair? '(a . b)) "'(a . b) is a pair")
    (assert (pair? '(a b c)) "'(a b c) is a pair")
    (assert-not (pair? '()) "'() is not a pair")
    (assert-not (pair? '#(a b)) "'#(a b) is not a pair")
  ))

 (test-case "(cons <obj_1> <obj_2>)" (lambda ()
    (assert-equal (cons 'a '()) '(a) "(cons 'a '()) is (a)")
    (assert-equal (cons 'a '(b c d)) '(a b c d) "(cons 'a '(b c d)) is (a b c d)")
    (assert-equal (cons "a" '(b c)) '("a" b c) "(cons \"a\" '(b c)) is (\"a\" b c)")
    (assert-equal (cons 'a 3) '(a . 3) "(cons 'a 3) is (a . 3)")
    (assert-equal (cons '(a b) 'c) '((a b) . c) "(cons '(a b) 'c) is ((a b) . c)")))

  (test-case "(car <pair>)" (lambda ()
    (assert-equal (car '(a b c)) 'a "(car '(a b c)) is a")
    (assert-equal (car '((a) b c d)) '(a) "(car '((a) b c d)) is (a)")
    (assert-equal (car '(1 . 2)) 1 "(car '(1 . 2)) is 1")
    (assert-error (car '()) "(car '()) is an error")))

  (test-case "(cdr <pair>)" (lambda ()
    (assert-equal (cdr '(a b c)) '(b c) "(cdr '(a b c)) is (b c)")
    (assert-equal (cdr '((a) b c d)) '(b c d) "(cdr '((a) b c d)) is (b c d)")
    (assert-equal (cdr '(1 . 2)) 2 "(cdr '(1 . 2)) is 2")
    (assert-error (cdr '()) "(cdr '()) is an error")))

  (test-case "(set-car! <pair> <obj>)" (lambda ()
    (define f (list 'not-a-constant-list))
    (define g '(constant-list))
    (set-car! f 3)
    (assert-equal f '(3) "set-car! sets the first item on the cons")
    (assert-error (set-car! g 3) "cannot set-car! on a constant list")))

  (test-case "(null? <obj>)" (lambda ()
    (assert (null? ()) "(null? ()) is true")
    (assert-not (null? '(a b)) "(null? (a b)) is false")))

  (test-case "(list? <obj>)" (lambda ()
    (assert (list? '(a b c)) "(list? '(a b c)) is true")
    (assert (list? '()) "(list? '()) is true")
    (assert-not (list? '(a . b)) "(list? '(a . b)) is false")
    (let ((x (list 'a)))
      (set-cdr! x x)
      (assert-not (list? x) "list? on a circular list is false"))))

  (test-case "(make-list <k> <fill>)" (lambda ()
    (assert-equal (make-list 2 3) '(3 3) "(make-list 2 3) is (3 3)")))

  (test-case "(list <obj> ...)" (lambda ()
    (assert-equal (list 'a (+ 3 4) 'c) '(a 7 c) "(list 'a (+ 3 4) 'c) is (a 7 c)")
    (assert-equal (list) '() "(list) is ()")))

  (test-case "(length <list>)" (lambda ()
    (assert-equal (length '(a b c)) 3 "(length '(a b c)) is 3")
    (assert-equal (length '(a (b) (c d e))) 3 "(length '(a (b) (c d e))) is 3")
    (assert-equal (length '()) 0 "(length '()) is 0")))

  (test-case "(append <list> ...)" (lambda ()
    (assert-equal (append '(x) '(y)) '(x y) "(append '(x) '(y)) is (x y)")
    (assert-equal (append '(a) '(b c d)) '(a b c d) "(append '(a) '(b c d)) is (a b c d)")
    (assert-equal (append '(a (b)) '((c))) '(a (b) (c)) "(append '(a (b)) '((c))) is (a (b) (c))")
    (assert-equal (append '(a b) '(c . d)) '(a b c . d) "(append '(a b) '(c . d)) is (a b c . d)")
    (assert-equal (append '() 'a) 'a "(append '() 'a) is a")))

  (test-case "(reverse <list>)" (lambda ()
    (assert-equal (reverse '(a b c)) '(c b a) "(reverse '(a b c))")
    (assert-equal (reverse '(a (b c) d (e (f)))) '((e (f)) d (b c) a) "(reverse '(a (b c) d (e (f))))")))

  (test-case "(list-tail <list> <k>)" (lambda ()
    (assert-equal (list-tail '(a b c d) 2) '(c d) "(list-tail '(a b c d))")))

  (test-case "(list-ref <list> <k>)" (lambda ()
    (assert-equal (list-ref '(a b c d) 2) 'c "(list-ref '(a b c d))")
    (assert-equal (list-ref '(a b c d) (exact (round 1.8))) 'c "(list-ref '(a b c d) (exact (round 1.8)))")))

  (test-case "(list-set! <list> <k> <obj>)" (lambda ()
    (let ((ls (list 'one 'two 'five)))
      (list-set! ls 2 'three)
      (assert-equal ls '(one two three)))
    (assert-error (list-set! '(0 1 2) 1 "oops") "(list-set! '(0 1 2) 1 \"oops\")")))

  (test-case "(memq <obj> <list>)" (lambda ()
    (assert-equal (memq 'a '(a b c)) '(a b c) "(memq 'a '(a b c))")
    (assert-equal (memq 'b '(a b c)) '(b c) "(memq 'b '(b c))")
    (assert-equal (memq 'a '(b c d)) #f "(memq 'a '(b c d))")
    (assert-equal (memq (list a) '(b (a) c)) #f "(memq (list a) '(b (a) c))")
    (assert-equal (memq 101 '(100 101 102)) #f "(memq 101 '(100 101 102))")))

  (test-case "(member <obj> <list>)" (lambda ()
    (assert-equal (member (list 'a) '(b (a) c)) '((a) c) "(member (list a) '(b (a) c))")
    (assert-equal (member "B" '("a" "b" "c") string-ci=?) '("b" "c") "(member \"B\" '(\"a\" \"b\" \"c\") string-ci=?)")))

  (test-case "(memv <obj> <list>)" (lambda ()
    (assert-equal (memv 101 '(100 101 102)) '(101 102) "(memv 101 '(100 101 102))")))

  (test-case "(assq <obj> <alist>)" (lambda ()
    (define e '((a 1) (b 2) (c 3)))
    (assert-equal (assq 'a e) '(a 1) "(assq 'a e)")
    (assert-equal (assq 'b e) '(b 2) "(assq 'b e)")
    (assert-equal (assq 'd e) #f "(assq 'd e)")
    (assert-equal (assq (list 'a) '(((a)) ((b)) ((c)))) #f "(assq (list 'a) '(((a)) ((b)) ((c))))")
    (assert-equal (assq 5 '((2 3) (5 7) (11 13))) #f "(assq 5 '((2 3) (5 7) (11 13)))")))

  (test-case "(assoc <obj> <alist>)" (lambda ()
    (assert-equal (assoc (list 'a) '(((a)) ((b)) ((c)))) '((a)) "(assoc (list 'a) '(((a)) ((b)) ((c))))")
    (assert-equal (assoc 2.0 '((1 1) (2 4) (3 9))) '(2 4) "(assoc 2.0 '((1 1) (2 4) (3 9)))")))

  (test-case "(assv <obj> <alist>)" (lambda ()
    (define e '((a 1) (b 2) (c 3)))
    (assert-equal (assv 5 '((2 3) (5 7) (11 13))) '(5 7) "(assv 5 '((2 3) (5 7) (11 13)))")))

  (test-case "(list-copy <obj>)" (lambda ()
    (define a '(1 8 2 8))
    (define b (list-copy a))
    (set-car! b 3)
    (assert-equal b '(3 8 2 8) "Should have changed")
    (assert-equal a '(1 8 2 8) "Should be the same")))
)
