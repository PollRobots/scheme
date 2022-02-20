#|
 |  Test cxr operations
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "cxr"
  (test-case "(car <pair>)" (lambda ()
    (assert-equal (car '(a b c)) 'a)
    (assert-equal (car '(a . b)) 'a)
    (assert-error (car '()))))

  (test-case "(cdr <pair>)" (lambda ()
    (assert-equal (cdr '(a b c)) '(b c))
    (assert-equal (cdr '(a . b)) 'b)
    (assert-error (cdr '()))))

  (test-case "(caar <pair>)" (lambda ()
    (assert-equal (caar '((a b) (d e) (g h))) 'a)
    (assert-equal (caar '((a . b) (d . e))) 'a)
    (assert-error (caar '(a b c)))))

  (test-case "(cadr <pair>)" (lambda ()
    (assert-equal (cadr '((a b) (d e) (g h))) '(d e))
    (assert-equal (cadr '((a b) d e)) 'd)
    (assert-error (cadr '(a)))))

  (test-case "(cdar <pair>)" (lambda ()
    (assert-equal (cdar '((a b) (d e) (g h))) '(b))
    (assert-equal (cdar '((a . b) (d . e))) 'b)
    (assert-error (cdar '(a b c)))))

  (test-case "(cddr <pair>)" (lambda ()
    (assert-equal (cddr '((a b) (d e) (g h))) '((g h)))
    (assert-equal (cddr '((a . b) d e)) '(e))
    (assert-error (cddr '(a)))))

  (test-case "(caaar <pair>)" (lambda ()
    (assert-equal (caaar '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'a)
    (assert-error (caaar '((a . (c . d)) . ((e . f) . (g . h)))))))

  (test-case "(caadr <pair>)" (lambda ()
    (assert-equal (caadr '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'e)
    (assert-error (caadr '(((a . b) . (c . d)) . (e . (g . h)))))))

  (test-case "(cadar <pair>)" (lambda ()
    (assert-equal (cadar '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'c)
    (assert-error (cadar '(((a . b) . c) . ((e . f) . (g . h)))))))

  (test-case "(caddr <pair>)" (lambda ()
    (assert-equal (caddr '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'g)
    (assert-error (caddr '(((a . b) . (c . d)) . ((e . f) . g))))))

  (test-case "(cdaar <pair>)" (lambda ()
    (assert-equal (cdaar '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'b)
    (assert-error (cdaar '((b . (c . d)) . ((e . f) . (g . h)))))))

  (test-case "(cdadr <pair>)" (lambda ()
    (assert-equal (cdadr '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'f)
    (assert-error (cdadr '(((a . b) . (c . d)) . (f . (g . h)))))))

  (test-case "(cddar <pair>)" (lambda ()
    (assert-equal (cddar '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'd)
    (assert-error (cddar '(((a . b) . d) . ((e . f) . (g . h)))))))

  (test-case "(cdddr <pair>)" (lambda ()
    (assert-equal (cdddr '(((a . b) . (c . d)) . ((e . f) . (g . h)))) 'h)
    (assert-error (cdddr '(((a . b) . (c . d)) . ((e . f) . h))))))

  (test-case "(caaaar <pair>)" (lambda ()
    (assert-equal (caaaar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'a)
    (assert-error (caaaar '(((a . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(caaadr <pair>)" (lambda ()
    (assert-equal (caaadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'i)
    (assert-error (caaadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . ((i . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(caadar <pair>)" (lambda ()
    (assert-equal (caadar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'e)
    (assert-error (caadar '((((a . b) . (c . d)) . (e . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(caaddr <pair>)" (lambda ()
    (assert-equal (caaddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'm)
    (assert-error (caaddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . (m . (o . p))))))))

  (test-case "(cadaar <pair>)" (lambda ()
    (assert-equal (cadaar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'c)
    (assert-error (cadaar '((((a . b) . c) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cadadr <pair>)" (lambda ()
    (assert-equal (cadadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'k)
    (assert-error (cadadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . k) . ((m . n) . (o . p))))))))

  (test-case "(caddar <pair>)" (lambda ()
    (assert-equal (caddar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'g)
    (assert-error (caddar '((((a . b) . (c . d)) . ((e . f) . g)) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cadddr <pair>)" (lambda ()
    (assert-equal (cadddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'o)
    (assert-error (cadddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . o)))))))

  (test-case "(cdaaar <pair>)" (lambda ()
    (assert-equal (cdaaar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'b)
    (assert-error (cdaaar '(((b . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cdaadr <pair>)" (lambda ()
    (assert-equal (cdaadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'j)
    (assert-error (cdaadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . ((j . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cdadar <pair>)" (lambda ()
    (assert-equal (cdadar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'f)
    (assert-error (cdadar '((((a . b) . (c . d)) . (f . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cdaddr <pair>)" (lambda ()
    (assert-equal (cdaddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'n)
    (assert-error (cdaddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . (n . (o . p))))))))

  (test-case "(cddaar <pair>)" (lambda ()
    (assert-equal (cddaar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'd)
    (assert-error (cddaar '((((a . b) . d) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cddadr <pair>)" (lambda ()
    (assert-equal (cddadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'l)
    (assert-error (cddadr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . l) . ((m . n) . (o . p))))))))

  (test-case "(cdddar <pair>)" (lambda ()
    (assert-equal (cdddar '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'h)
    (assert-error (cdddar '((((a . b) . (c . d)) . ((e . f) . h)) . (((i . j) . (k . l)) . ((m . n) . (o . p))))))))

  (test-case "(cddddr <pair>)" (lambda ()
    (assert-equal (cddddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . (o . p))))) 'p)
    (assert-error (cddddr '((((a . b) . (c . d)) . ((e . f) . (g . h))) . (((i . j) . (k . l)) . ((m . n) . p)))))))
)