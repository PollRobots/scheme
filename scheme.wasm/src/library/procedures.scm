(set! string-for-each
    (lambda (proc . args)
        (apply for-each proc (map string->list args))))

(set! vector-for-each
    (lambda (proc . args)
        (apply for-each proc (map vector->list args))))