(define string-for-each
  (lambda (proc . args)
    (apply for-each proc (map string->list args))))

(define vector-for-each
  (lambda (proc . args)
    (apply for-each proc (map vector->list args))))

(define call-with-current-continuation call/cc)
