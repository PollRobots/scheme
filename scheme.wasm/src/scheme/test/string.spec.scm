#|
 |  Test string operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "string"
  (test-case "(string? <obj>)" (lambda ()
    (assert (string? "Foo"))
    (assert (string? ""))
    (assert-not (string? '()))
    (assert-not (string? #\x))))

  (test-case "(make-string <k> <char>)" (lambda ()
    (assert-equal (make-string 4 #\a) "aaaa")
    (assert-equal (string-length (make-string 4)) 4)))

  (test-case "(string <char> ...)" (lambda ()
    (assert-equal (string #\f #\o #\o) "foo")
    (assert-equal (string) "")))

  (test-case "(string-length <string>)" (lambda ()
    (assert-equal (string-length "foobar") 6)
    (assert-equal (string-length "φδπ") 3)
    (assert-equal (string-length "") 0)))

  (test-case "(string-ref <string> <k>)" (lambda ()
    (assert-equal (string-ref "bar" 1) #\a)  
    (assert-error (string-ref "bar" 3))))

  (test-case "(string-set! <string> <k> <char>" (lambda ()
    (define f (make-string 3 #\*))
    (define g "***") ;; immutable
    (string-set! f 0 #\?)
    (assert-equal f "?**")
    (assert-error (string-set! g 0 #\?) "writing to an immutable string")
    (assert-error (string-set! (symbol->string 'immutable)
                               0
                               #\?)
                  "writing to an immutable string")))

  (test-case "(string=? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string=? "foo" "foo" (symbol->string 'foo)))
    (assert-not (string=? "foo" "FOO"))
    (assert-not (string=? "foo" "bar"))))

  (test-case "(string-ci=? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string-ci=? "foo" "FOO" (symbol->string 'foo)))
    (assert-not (string-ci=? "foo" "bar"))))

  (test-case "(string<? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string<? "abc" "def" "ghi"))
    (assert (string<? "ABC" "abc" "ghi"))
    (assert-not (string<? "abc" "DEF" "ghi"))))

  (test-case "(string-ci<? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string-ci<? "Abc" "def" "Ghi"))
    (assert (string-ci<? "abc" "DEF" "ghi"))
    (assert-not (string-ci<? "abc" "ABC" "ghi"))))

  (test-case "(string>? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string>? "ghi" "def" "abc"))
    (assert (string>? "ghi" "abc" "ABC"))
    (assert-not (string>? "abc" "DEF" "ghi"))))

  (test-case "(string-ci>? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string-ci>? "Ghi" "def" "Abc"))
    (assert (string-ci>? "ghi" "DEF" "abc"))
    (assert-not (string-ci>? "ghi" "abc" "ABC"))))

  (test-case "(string<=? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string<=? "abc" "def" "def" "ghi"))
    (assert (string<=? "ABC" "abc" "abc" "ghi"))
    (assert-not (string<=? "abc" "def" "DEF" "ghi"))))

  (test-case "(string-ci<=? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string-ci<=? "Abc" "def" "DEF" "Ghi"))
    (assert (string-ci<=? "abc" "DEF" "def" "ghi"))
    (assert-not (string-ci<=? "abc" "ABb" "ghi"))))

  (test-case "(string>=? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string>=? "ghi" "def" "def" "abc"))
    (assert (string>=? "ghi" "abc" "ABC"))
    (assert-not (string>=? "abc" "DEF" "def" "ghi"))))

  (test-case "(string-ci>=? <string1> <string2> <string3> ...)" (lambda ()
    (assert (string-ci>=? "Ghi" "def" "DEF" "Abc"))
    (assert (string-ci>=? "ghi" "DEF" "def" "abc"))
    (assert-not (string-ci>=? "ghi" "abc" "ABD"))))

  (test-case "(string-upcase <string>)" (lambda ()
    (assert-equal (string-upcase "Foo Bar") "FOO BAR")  
    (assert-equal (string-upcase "δπφ") "ΔΠΦ")))

  (test-case "(string-downcase <string>)" (lambda ()
    (assert-equal (string-downcase "Foo Bar") "foo bar")  
    (assert-equal (string-downcase  "ΔΠΦ") "δπφ")))

  (test-case "(string-foldcase <string>)" (lambda ()
    (assert-equal (string-foldcase "Foo Bar") "foo bar")  
    (assert-equal (string-foldcase  "ΔΠΦ") "δπφ")))

  (test-case "(string-append <string> ...)" (lambda ()
    (assert-equal (string-append) "")
    (assert-equal (string-append "abc" "def") "abcdef")
    (assert-equal (string-append "abc" "def" "ghi") "abcdefghi")))

  (test-case "(string->list <string> [<start> [<end>]])" (lambda ()
    (assert-equal (string->list "Foo Bar") '(#\F #\o #\o #\space #\B #\a #\r)) 
    (assert-equal (string->list "Foo Bar" 4) '(#\B #\a #\r)) 
    (assert-equal (string->list "Foo Bar" 4 6) '(#\B #\a)) 
    (assert-error (string->list "Foo Bar" 4 8))))

  (test-case "(list->string <list>)" (lambda ()
    (assert-equal (list->string '(#\F #\o #\o)) "Foo")
    (assert-error (list->string '(#\F o #\o)))))

  (test-case "(string-copy <string> [<start> [<end>]])" (lambda ()
    (assert-equal (string-copy "Foo Bar") "Foo Bar")
    (assert-equal (string-copy "Foo Bar" 4) "Bar")
    (assert-equal (string-copy "Foo Bar" 4 6) "Ba")
    (assert-error (string-copy "Foo Bar" 4 8))))

  (test-case "(substring <string> <start> <end>)" (lambda ()
    (assert-equal (substring "Foo Bar" 4 6) "Ba")
    (assert-error (substring "Foo Bar" 4 8))))

  (test-case "(string-copy! <to> <at> <from> [<start> [<end>]])" (lambda ()
    (define a "12345")
    (define b (string-copy "abcde"))
    (string-copy! b 1 a 0 2)
    (assert-equal b "a12de")
    (assert-error (string-copy! a 0 b))))

  (test-case "(string-fill! <string> <fill> [<start> [<end>]])" (lambda ()
    (define a (string-copy "Foo Bar"))

    (string-fill! a #\x)
    (assert-equal a "xxxxxxx")
    (string-fill! a #\y 4)
    (assert-equal a "xxxxyyy")
    (string-fill! a #\z 4 6)
    (assert-equal a "xxxxzzy")
    (assert-error (string-fill! a 4 8))
    (assert-error (string-fill! "foo" #\*))))
)