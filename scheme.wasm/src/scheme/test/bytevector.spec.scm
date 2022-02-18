#|
 |  Test bytevector operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "bytevector"
  (test-case "(bytevector? <obj>)" (lambda ()
    (assert (bytevector? #u8()))
    (assert (bytevector? #u8(1 2 3)))
    (assert-not (bytevector? 0))
    (assert-not (bytevector? '()))))

  (test-case "(equal? <bytevector1> <bytevector2>)" (lambda ()
    (assert-equal #u8(#x66 #x69 #x69) #u8(102 105 105))))

  (test-case "(make-bytevector <k> [<byte>])" (lambda ()
    (assert-equal (make-bytevector 2 12) #u8(12 12))
    (assert-equal (bytevector-length (make-bytevector 14)) 14)))

  (test-case "(bytevector <byte> ...)" (lambda ()
    (assert-equal (bytevector 1 3 5 1 3 5) #u8(1 3 5 1 3 5))
    (assert-equal (bytevector) #u8())))

  (test-case "(bytevector-length <bytevector>)" (lambda ()
    (assert-equal (bytevector-length (bytevector)) 0)
    (assert-equal (bytevector-length #u8(1 1 2 3 5 8 13 21)) 8)))

  (test-case "(bytevector-u8-ref <bytevector> <k>)" (lambda ()
    (assert-equal (bytevector-u8-ref #u8(1 1 2 3 5 8 13 21) 5) 8)))

  (test-case "(bytevector-u8-set! <bytevector> <k> <byte>)" (lambda ()
    (let ((bv (bytevector 1 2 3 4)))
      (bytevector-u8-set! bv 1 3)
      (assert-equal bv #u8(1 3 3 4)))))

  (test-case "(bytevector-copy <bytevector> [<start> [<end>]])" (lambda ()
    (let ((bv #u8(1 2 3 4 5)))
      (assert-equal (bytevector-copy bv) #u8(1 2 3 4 5))
      (assert-equal (bytevector-copy bv 2) #u8(3 4 5))
      (assert-equal (bytevector-copy bv 2 4) #u8(3 4)))))

  (test-case "(bytevector-copy! <to> <at> <from> [<start> [<end>]])" (lambda ()
    (let ((a #u8(1 2 3 4 5))
          (b #u8(10 20 30 40 50)))
      (bytevector-copy! b 1 a 0 2)
      (assert-equal b #u8(10 1 2 40 50)))))

  (test-case "(bytevector-append <bytevector> ...)" (lambda ()
    (assert-equal (bytevector-append #u8(0 1 2) #u8(3 4 5))
                  #u8(0 1 2 3 4 5))))

  (test-case "(utf8->string <bytevector> [<start> [<end>]])" (lambda ()
    (assert-equal (utf8->string #u8(#x46 #x6f #x6f)) "Foo")
    (assert-equal (utf8->string #u8(#x42 #x61 #x72) 1) "ar")
    (assert-equal (utf8->string #u8(#x42 #x61 #x72) 1 2) "a")))

  (test-case "(string->utf8 <string>)" (lambda ()
    (assert-equal (string->utf8 "Foo") #u8(70 111 111))
    (assert-equal (string->utf8 "λ") #u8(#xCE #xBB))))
)
