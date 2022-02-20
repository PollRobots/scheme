#|
 |  Test trig operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "vector"
  (test-case "(vector? <obj>)" (lambda ()
    (assert (vector? #()))
    (assert (vector? #(1 2 3)))
    (assert-not (vector? #u8(1 2 3)))
    (assert-not (vector? #t))
    (assert-not (vector? '(1 2 3)))))

  (test-case "(make-vector <k> [<fill>])"  (lambda ()
    (assert-equal (make-vector 6 'a) #(a a a a a a))
    (assert-equal (vector-length (make-vector 4)) 4)))

  (test-case "(vector <obj> ...)" (lambda ()
    (assert-equal (vector 'a 'b 'c) #(a b c))
    (assert-equal (vector) #())))

  (test-case "(vector-length <vector>)" (lambda ()
    (assert-equal (vector-length #(1 2 3 4)) 4)
    (assert-equal (vector-length #()) 0)))

  (test-case "(vector-ref <vector> <k>)" (lambda ()
    (assert-equal (vector-ref #(1 1 2 3 5 8 13 21) 5) 8)
    (assert-equal (vector-ref 
                    #(1 1 2 3 5 8 13 21)
                    (exact (round (* 2 (acos -1)))))
                  13)))

  (test-case "(vector-set! <vector> <k> <obj>)" (lambda ()
    (let ((vec (vector 0 '(2 2 2 2) "Anna")))
      (vector-set! vec 1 '("Sue" "Sue"))
      (assert-equal vec #(0 ("Sue" "Sue") "Anna"))
      (assert-error (vector-set! vec 7 "Oops!")))
    (assert-error (vector-set! #(0 1 2) 1 "doe"))))

  (test-case "(vector->list <vector> [<start> [<end>]])" (lambda ()
    (assert-equal (vector->list #(dah dah didah)) '(dah dah didah))
    (assert-equal (vector->list #(dah dah didah) 1) '(dah didah))
    (assert-equal (vector->list #(dah dah didah) 1 2) '(dah))))

  (test-case "(list->vector <list>)" (lambda ()
    (assert-equal (list->vector '(dah dah didah)) #(dah dah didah))
    (assert-equal (list->vector '()) #())))

  (test-case "(vector->string <vector> [<start> [<end>]])" (lambda ()
    (assert-equal (vector->string #(#\a #\b #\c)) "abc")
    (assert-equal (vector->string #(#\a #\b #\c) 1) "bc")
    (assert-equal (vector->string #(a #\b c) 1 2) "b")
    (assert-error (vector->string #(a b c)))))

  (test-case "(string->vector <string> [<start> [<end>]])" (lambda ()
    (assert-equal (string->vector "foo bar") #(#\f #\o #\o #\space #\b #\a #\r))
    (assert-equal (string->vector "foo bar" 4) #(#\b #\a #\r))
    (assert-equal (string->vector "foo bar" 4 6) #(#\b #\a))
    (assert-error (string->vector "foo bar" 4 8))))

  (test-case "(vector-copy <vector> [<start> [<end>]])" (lambda ()
    (define a #(1 8 2 8)) ; a is immutable
    (define b (vector-copy a))
    (vector-set! b 0 3) ; b is mutable
    (assert-equal b #(3 8 2 8))
    (assert-equal (vector-copy b 1 3) #(8 2))
    (assert-equal (vector-copy b 1) #(8 2 8))))

  (test-case "(vector-copy! <to> <at> <from> [<start> [<end>]])" (lambda ()
    (define a (vector 1 2 3 4 5))
    (define b (vector 10 20 30 40 50))
    (define c #(a b c d e)) ;; c is immutable
    (vector-copy! b 1 a 0 2)
    (assert-equal b #(10 1 2 40 50))
    (assert-error (vector-copy! c 1 a 0 2))))

  (test-case "(vector-append <vector> ...)" (lambda ()
    (assert-equal (vector-append #(a b c) #(d e f)) #(a b c d e f))))

  (test-case "(vector-fill! <vector> <fill> [<start> [<end>]])" (lambda ()
    (define a (vector 1 2 3 4 5))
    (vector-fill! a 'smash 2 4)
    (assert-equal a #(1 2 smash smash 5))
    (assert-error (vector-fill! #(a b c d e) 'x 1 2) "filling an immutable vector")))

)
