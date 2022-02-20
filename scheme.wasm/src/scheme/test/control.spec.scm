#|
 |  Test control operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "control"
  (test-case "(procedure? <obj>)" (lambda ()
    (assert (procedure? car))
    (assert-not (procedure? 'car))
    (assert (procedure? (lambda (x) (* x x))))
    (assert-not (procedure? '(lambda (x) (* x x))))
    (assert (call-with-current-continuation procedure?))))

  (test-case "(apply <proc> <arg1> ... <args>)" (lambda ()
    (assert-equal (apply + (list 3 4)) 7)
    (assert-equal (apply + 1 2 (list 3 4)) 10)
    (define compose
      (lambda (f g)
        (lambda args
          (f (apply g args)))))
    (assert-equal ((compose sqrt *) 12 75) 30)))

  (test-case "(map <proc> <list1> <list2> ...)" (lambda ()
    (assert-equal (map cadr '((a b) (d e) (g h))) '(b e h))
    (assert-equal 
      (map (lambda (n) (expt n n)) '(1 2 3 4 5))
      '(1 4 27 256 3125))
    (assert-equal
      (map + '(1 2 3) '(4 5 6 7))
      '(5 7 9))

    (define counts
      (let ((count 0))
        (map (lambda (ignored)
                (set! count (+ count 1))
                count)
            '(a b c))))
    (assert (or (equal? counts '(1 2 3))
                (equal? counts '(3 2 1))) counts)))

  (test-case "(string-map <proc> <string1> <string2> ...)" (lambda ()
    (assert-equal (string-map char-foldcase "AbdEgH") "abdegh")
    (assert-equal
      (string-map 
        (lambda (c) (integer->char (+ 1 (char->integer c))))
        "HAL")
      "IBM")
    (assert-equal
      (string-map
        (lambda (c k) ((if (eqv? k #\u) char-upcase char-downcase) c))
        "studlycaps xxx"
        "ululululul")
      "StUdLyCaPs")))

  (test-case "(vector-map <proc> <vector1> <vector2> ...)" (lambda ()
    (assert-equal (vector-map cadr #((a b) (d e) (g h))) #(b e h))
    (assert-equal 
      (vector-map (lambda (n) (expt n n)) #(1 2 3 4 5))
      #(1 4 27 256 3125))
    (assert-equal
      (vector-map + #(1 2 3) #(4 5 6 7))
      #(5 7 9))

    (define counts
      (let ((count 0))
        (vector-map (lambda (ignored)
                (set! count (+ count 1))
                count)
            #(a b c))))
    (assert (or (equal? counts #(1 2 3))
                (equal? counts #(3 2 1))) counts)))

  (test-case "(for-each <proc> <list1> <list2> ...)" (lambda ()
    (let ((v (make-vector 5)))
      (for-each 
        (lambda (i) (vector-set! v i (* i i)))
        '(0 1 2 3 4))
      (assert-equal v #(0 1 4 9 16)))))

  (test-case "(string-for-each <proc> <string1> <string2> ...)" (lambda ()
    (let ((v '()))
      (string-for-each 
        (lambda (c) (set! v (cons (char->integer c) v)))
        "abcde")
      (assert-equal v '(101 100 99 98 97)))))

  (test-case "(vector-for-each <proc> <vector1> <vector2> ...)" (lambda ()
    (let ((v (make-vector 5)))
      (vector-for-each 
        (lambda (i) (vector-set! v i (* i i)))
        #(0 1 2 3 4))
      (assert-equal v #(0 1 4 9 16)))))

  (test-case "call-with-current-continuation" (lambda ()
    (assert-equal
      (call-with-current-continuation
        (lambda (exit)
          (for-each 
            (lambda (x) (if (negative? x) (exit x)))
            '(54 0 37 -3 245 19))
        #t))
      -3)
      
    (define list-length
      (lambda (obj)
        (call/cc
          (lambda (return)
            (letrec ((r (lambda (obj)
                (cond ((null? obj) 0)
                      ((pair? obj) (+ (r (cdr obj)) 1)) 
                      (else (return #f))))))
              (r obj))))))

    (assert-equal (list-length '(1 2 3 4)) 4)
    (assert-equal (list-length '(a b . c)) #f)))
)
