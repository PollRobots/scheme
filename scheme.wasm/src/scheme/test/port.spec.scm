#|
 |  Test port operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "port"
  (test-case "port type assertions on input string port" (lambda ()
    (let ((p (open-input-string "test")))
      (assert (port? p))
      (assert (input-port? p))
      (assert-not (output-port? p))
      (assert (textual-port? p))
      (assert-not (binary-port? p))
      (assert (input-port-open? p))
      (assert-not (output-port-open? p))
      (close-input-port p)
      (assert-not (input-port-open? p)))))

  (test-case "port type assertions on output string port" (lambda ()
    (let ((p (open-output-string)))
      (assert (port? p))
      (assert (output-port? p))
      (assert-not (input-port? p))
      (assert (textual-port? p))
      (assert-not (binary-port? p))
      (assert (output-port-open? p))
      (assert-not (input-port-open? p))
      (close-output-port p)
      (assert-not (output-port-open? p)))))

  (test-case "port type assertions on input bytevector port" (lambda ()
    (let ((p (open-input-bytevector (string->utf8 "test"))))
      (assert (port? p))
      (assert (input-port? p))
      (assert-not (output-port? p))
      (assert (binary-port? p))
      (assert-not (textual-port? p))
      (assert (input-port-open? p))
      (assert-not (output-port-open? p))
      (close-input-port p)
      (assert-not (input-port-open? p)))))

  (test-case "port type assertions on output bytevector port" (lambda ()
    (let ((p (open-output-bytevector)))
      (assert (port? p))
      (assert (output-port? p))
      (assert-not (input-port? p))
      (assert (binary-port? p))
      (assert-not (textual-port? p))
      (assert (output-port-open? p))
      (assert-not (input-port-open? p))
      (close-output-port p)
      (assert-not (output-port-open? p)))))

  (test-case "read-char from input-bytevector" (lambda ()
    (let ((p (open-input-string "bar")))
      (assert-equal (read-char p) #\b)
      (assert-equal (read-char p) #\a)
      (assert-equal (read-char p) #\r)
      (assert (eof-object? (read-char p)))
      (close-input-port p)
      (assert-error (read-char p)))))

  (test-case "peek-char from input-string" (lambda ()
    (let ((p (open-input-string "bar")))
      (assert-equal (peek-char p) #\b)
      (assert-equal (read-char p) #\b)
      (assert-equal (peek-char p) #\a)
      (assert-equal (read-char p) #\a)
      (assert-equal (peek-char p) #\r)
      (assert-equal (read-char p) #\r)
      (assert (eof-object? (peek-char p)))
      (close-input-port p)
      (assert-error (peek-char p)))))

  (test-case "read-line from input-string" (lambda () 
    (let ((p (open-input-string "one\ntwo\r\nthree\rfour")))
      (assert-equal (read-line p) "one")
      (assert-equal (read-line p) "two")
      (assert-equal (read-line p) "three")
      (assert-equal (read-line p) "four")
      (assert (eof-object? (read-line p)))
      (close-input-port p)
      (assert-error (read-line p)))))

  (test-case "read-string from input-string" (lambda ()
    (let ((p (open-input-string "foobarbaz")))
      (assert-equal (read-string 3 p) "foo")
      (assert-equal (read-string 0 p) "")
      (assert-error (read-string -1 p))
      (assert-error (read-string "oops" p))
      (assert-equal (read-string 10 p) "barbaz")
      (assert (eof-object? (read-string 4 p)))
      (close-input-port p)
      (assert-error (read-string 1 p)))))

  (test-case "write-char to output-string" (lambda ()
    (let ((p (open-output-string)))
      (write-char #\f p)
      (assert-equal (get-output-string p) "f")
      (write-char #\o p)
      (assert-equal (get-output-string p) "fo")
      (write-char #\o p)
      (assert-equal (get-output-string p) "foo")
      (newline p)
      (assert-equal (get-output-string p) "foo\n")
      (close-output-port p)
      (assert-error (write-char #\newline p)))))

  (test-case "write-string to output-string" (lambda ()
    (let ((p (open-output-string)))
      (write-string "0123456789" p)
      (assert-equal (get-output-string p) "0123456789")
      (write-string "0123456789" p 5)
      (assert-equal (get-output-string p) "012345678956789")
      (write-string "0123456789" p 5 7)
      (assert-equal (get-output-string p) "01234567895678956")
      (close-output-port p)
      (assert-error (write-string "help" p)))))

  (test-case "read-u8 from input-bytevector" (lambda ()
    (let ((p (open-input-bytevector #u8(65 66 67))))
      (assert-equal (read-u8 p) 65)
      (assert-equal (read-u8 p) 66)
      (assert-equal (read-u8 p) 67)
      (assert (eof-object? (read-u8 p)))
      (close-input-port p)
      (assert-error (read-u8 p)))))

  (test-case "peek-u8 from input-bytevector" (lambda ()
    (let ((p (open-input-bytevector #u8(65 66 67))))
      (assert-equal (peek-u8 p) 65)
      (assert-equal (read-u8 p) 65)
      (assert-equal (peek-u8 p) 66)
      (assert-equal (read-u8 p) 66)
      (assert-equal (peek-u8 p) 67)
      (assert-equal (read-u8 p) 67)
      (assert (eof-object? (peek-u8 p)))
      (close-input-port p)
      (assert-error (peek-u8 p)))))
    
  (test-case "read-bytevector from input-bytevector" (lambda ()
    (let ((p (open-input-bytevector (string->utf8 "foobarbaz"))))
      (assert-equal (read-bytevector 3 p) #u8(102 111 111))
      (assert-equal (read-bytevector 0 p) #u8())
      (assert-error (read-bytevector -1 p))
      (assert-error (read-bytevector "oops" p))
      (assert-equal (read-bytevector 10 p) #u8(98 97 114 98 97 122))
      (assert (eof-object? (read-bytevector 4 p)))
      (close-input-port p)
      (assert-error (read-bytevector 1 p)))))

  (test-case "write-u8 to output-bytevector" (lambda ()
    (let ((p (open-output-bytevector)))
      (write-u8 98 p)
      (write-u8 97 p)
      (write-u8 114 p) 
      (assert-equal (get-output-bytevector p) #u8(98 97 114))
      (assert-equal (utf8->string (get-output-bytevector p)) "bar")
      (close-output-port p)
      (assert-error (write-u8 1 p)))))

  (test-case "write-bytevector to output-bytevector" (lambda ()
    (let ((p (open-output-bytevector)))
      (write-bytevector #u8(1 2 3 4 5 6) p)
      (assert-equal (get-output-bytevector p) #u8(1 2 3 4 5 6))
      (write-bytevector #u8(1 2 3 4 5 6) p 3)
      (assert-equal (get-output-bytevector p) #u8(1 2 3 4 5 6 4 5 6))
      (write-bytevector #u8(1 2 3 4 5 6) p 2 4)
      (assert-equal (get-output-bytevector p) #u8(1 2 3 4 5 6 4 5 6 3 4))
      (close-output-port p)
      (assert-error (write-bytevector #u8(1 2 3 4 5 6) p)))))
)