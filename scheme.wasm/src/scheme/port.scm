(define (close-input-port port)
  (if (input-port? port) 
    (close-port port)
    (error "Invalid argument" port)))

(define (close-output-port port)
  (if (output-port? port) 
    (close-port port)
    (error "Invalid argument" port)))

(define read-char (case-lambda 
  (() (read-char (current-input-port)))
  ((port)
    (cond 
      ((not (port? port)) (error "Cannot read from non-port object" port))
      ((not (input-port-open? port)) (error "Port is not open for input" port))
      ((not (textual-port? port)) (error "Cannot read char from a binary port" port))
      ((string-port? port)
        (let* ((port-obj (string-port-obj port))
               (off (car port-obj))
               (str (cdr port-obj)))
          (if (>= off (string-length str))
              (eof-object)
              (let ((c (string-ref str off)))
                (set-car! port-obj (+ off 1))
                c))))
      (else (error "Not Implemented"))))))

(define char-ready? (case-lambda 
  (() (char-ready? (current-input-port)))
  ((port)
    (cond 
      ((not (port? port)) (error "Cannot check char ready from non-port object" port))
      ((not (input-port-open? port)) (error "Port is not open for input" port))
      ((not (textual-port? port)) (error "Cannot check char ready from a binary port" port))
      ((string-port? port) #t)
      (else (error "Not Implemented"))))))

(define read-line (case-lambda
  (() (read-line (current-input-port)))
  ((port)
    (if (eof-object? (peek-char port)) 
        (eof-object)
        (letrec ((fn (lambda (accum)
                        (let ((char (read-char port)))
                          (cond 
                            ;; eof or newline (0x0A) terminates a line
                            ((or (eof-object? char) (eqv? #\newline char))
                              (list->string (reverse accum)))
                            ;; handle #x0D (optionally followed by #x0A)
                            ;; these also terminate a line
                            ((eqv? #\return char)
                              (if (eqv? (peek-char port) #\newline)
                                (read-char port))
                              (list->string (reverse accum)))
                            (else (fn (cons char accum))))))))
          (fn ()))))))

(define read-string (case-lambda
  ((k) (read-string k (current-input-port)))
  ((k port)
    (cond
      ((or (not (integer? k)) (negative? k))
        (error "string length must be a positive integer" k port))
      ((eof-object? (peek-char port)) (eof-object))
      ((zero? k) "")
      (else
        (letrec ((fn (lambda (accum i)
                        (let ((char (read-char port)))
                          (cond
                            ;; eof terminates the string
                            ((eof-object? char) accum)
                            ;; i is zero, we have the full string
                            ((= i 1) (cons char accum))
                            ;; read anotheer character
                            (else (fn (cons char accum) (- i 1))))))))
          (list->string (reverse (fn () k)))))))))

(define peek-char (case-lambda 
  (() (peek-char (current-input-port)))
  ((port)
    (cond 
      ((not (port? port)) (error "Cannot peek from non-port object" port))
      ((not (input-port-open? port)) (error "Port is not open for input" port))
      ((not (textual-port? port)) (error "Cannot peek char from a binary port" port))
      ((string-port? port)
        (let* ((port-obj (string-port-obj port))
               (off (car port-obj))
               (str (cdr port-obj)))
          (if (>= off (string-length str))
              (eof-object)
              (string-ref str off))))
      (else (error "Not Implemented"))))))

(define write-char (case-lambda
  ((char) (write-char char (current-output-port)))
  ((char port)
    (cond
      ((not (port? port)) (error "Cannot write to non-port object" port))
      ((not (output-port-open? port)) (error "Port is not open for output" port))
      ((not (textual-port? port)) (error "Cannot write char to a binary port" port))
      ((string-port? port)
        (let* ((port-obj (string-port-obj port))
               (accum (car port-obj))
               (chars (cons char (cdr port-obj))))
          (cond
            ((null? accum)
              (set-car! port-obj "")
              (set-cdr! port-obj chars))
            ((> (length chars) 16)
              (set-car! port-obj (string-append accum (list->string (reverse chars))))
              (set-cdr! port-obj ()))
            (else
              (set-cdr! port-obj chars)))))
      (else (error "Not Implemented"))))))

(define newline (case-lambda
  (() (write-char #\newline (current-output-port)))
  ((port) (write-char #\newline port))))

(define write-string (case-lambda
  ((str) (write-string str (current-output-port)))
  ((str port)
    (for-each (lambda (c) (write-char c port)) (string->list str)))
  ((str port start)
    (for-each (lambda (c) (write-char c port)) (string->list str start)))
  ((str port start end)
    (for-each (lambda (c) (write-char c port)) (string->list str start end)))))

(define (get-output-string port)
  (cond
    ((not (port? port)) (error "Cannot write to non-port object" port))
    ((not (output-port? port)) (error "Port is not open for output" port))
    ((string-port? port)
      (let* ((port-obj (string-port-obj port))
              (accum (car port-obj))
              (chars (cdr port-obj)))
        (cond
          ((null? accum) "")
          ((> (length chars) 0)
            (set-car! port-obj (string-append accum (list->string (reverse chars))))
            (set-cdr! port-obj ())
            (car port-obj))
          (else accum))))
    (else (error "Port was not opened with (open-output-string)" port))))

(define write (case-lambda
  ((obj) (write obj (current-output-port)))
  ((obj port)
    (cond
      ((null? obj) (write-string "()" port))
      ((boolean? obj) (write-string (if obj "#t" "#f") port))
      ((number? obj) (write-string (number->string obj) port))
      ((char? obj)
        (write-string "#\\" port)
        (let ((cp (char->integer obj)))
          (cond
            ((or (and (> cp #x20) (< cp #x7F)) (char-alphabetic? obj))
              (write-char obj port))
            ((eqv? cp #x07) (write-string "alarm" port))
            ((eqv? cp #x08) (write-string "backspace" port))
            ((eqv? cp #x7F) (write-string "delete" port))
            ((eqv? cp #x1b) (write-string "escape" port))
            ((eqv? cp #x0a) (write-string "newline" port))
            ((eqv? cp #x00) (write-string "null" port))
            ((eqv? cp #x0d) (write-string "return" port))
            ((eqv? cp #x20) (write-string "space" port))
            ((eqv? cp #x09) (write-string "tab" port))
            (else 
              (write-char #\x port)
              (write-string (number->string cp 16) port)))))
      ((string? obj)
        (write-char #\" port)
        (for-each 
          (lambda (c)
            (let ((cp (char->integer c)))
              (cond
                ((eqv? cp #x22) (write-string "\\\"" port))
                ((eqv? cp #x57) (write-string "\\\\" port))
                ((eqv? cp #x7C) (write-string "\\|" port))
                ((or (and (>= cp #x20) (< cp #x7F)) (char-alphabetic? c))
                  (write-char c port))
                ((eqv? cp #x07) (write-string "\\a" port))
                ((eqv? cp #x08) (write-string "\\b" port))
                ((eqv? cp #x09) (write-string "\\t" port))
                ((eqv? cp #x0a) (write-string "\\n" port))
                ((eqv? cp #x0d) (write-string "\\r" port))
                (else 
                  (write-string "\\x" port)
                  (write-string (number->string cp 16) port)
                  (write-string ";" port)))))
           (string->list obj))
        (write-char #\" port))
      ((symbol? obj) (write-string (symbol->string obj) port))
      ((pair? obj)
        (write-char #\( port)
        (letrec ((fn (lambda (head tail)
                        (write head port)
                        (cond 
                          ((pair? tail) 
                            (write-char #\space port)
                            (fn (car tail) (cdr tail)))
                          ((not (null? tail))
                            (write-string " . " port)
                            (write tail port))))))
          (fn (car obj) (cdr obj)))
        (write-char #\) port))
      ((vector? obj)
        (write-string "#(" port)
        (let ((first #t))
          (vector-for-each 
            (lambda (el) 
              (if first (set! first #f) (write-char #\space port))
              (write el port))
            obj))
        (write-string ")" port))
      ((bytevector? obj)
        (write-string "#u8(" port)
        (letrec ((len (bytevector-length obj))
                 (fn (lambda (k)
                        (if (< k len)
                          (begin
                            (if (> k 0) (write-char #\space port))
                            (write (bytevector-u8-ref obj k) port)
                            (fn (+ 1 k)))))))
          (fn 0))
        (write-string ")" port))
        ))))

(define read-u8 (case-lambda 
  (() (read-u8 (current-input-port)))
  ((port)
    (cond 
      ((not (port? port)) (error "Cannot read from non-port object" port))
      ((not (input-port-open? port)) (error "Port is not open for input" port))
      ((not (binary-port? port)) (error "Cannot read byte from a textual port" port))
      ((bytevector-port? port)
        (let* ((port-obj (bytevector-port-obj port))
               (off (car port-obj))
               (vec (cdr port-obj)))
          (if (>= off (bytevector-length vec))
              (eof-object)
              (let ((c (bytevector-u8-ref vec off)))
                (set-car! port-obj (+ off 1))
                c))))
      (else (error "Not Implemented"))))))

(define u8-ready? (case-lambda 
  (() (u8-ready? (current-input-port)))
  ((port)
    (cond 
      ((not (port? port)) (error "Cannot check char ready from non-port object" port))
      ((not (input-port-open? port)) (error "Port is not open for input" port))
      ((not (binary-port? port)) (error "Cannot check u8 ready from a textual port" port))
      ((bytevector-port? port) #t)
      (else (error "Not Implemented"))))))

(define peek-u8 (case-lambda 
  (() (peek-u8 (current-input-port)))
  ((port)
    (cond 
      ((not (port? port)) (error "Cannot peek from non-port object" port))
      ((not (input-port-open? port)) (error "Port is not open for input" port))
      ((not (binary-port? port)) (error "Cannot peek byte from a textual port" port))
      ((bytevector-port? port)
        (let* ((port-obj (bytevector-port-obj port))
               (off (car port-obj))
               (vec (cdr port-obj)))
          (if (>= off (bytevector-length vec))
              (eof-object)
              (bytevector-u8-ref vec off))))
      (else (error "Not Implemented"))))))

(define read-bytevector (case-lambda
  ((k) (read-bytevector k (current-input-port)))
  ((k port)
    (cond
      ((or (not (integer? k)) (negative? k))
        (error "bytevector length must be a positive integer" k port))
      ((eof-object? (peek-u8 port)) (eof-object))
      ((zero? k) #u8())
      (else
        (letrec ((fn (lambda (accum i)
                        (let ((byte (read-u8 port)))
                          (cond
                            ; eof terminates the bytevector
                            ((eof-object? byte) accum)
                            ; i is zero, we have the full bytevector
                            ((zero? i) (cons byte accum))
                            ; read another byte
                            (else (fn (cons byte accum) (- i 1))))))))
          (apply bytevector (reverse (fn () (- k 1))))))))))

(define write-u8 (case-lambda
  ((byte) (write-u8 byte (current-output-port)))
  ((byte port)
    (cond
      ((not (port? port)) (error "Cannot write to non-port object" port))
      ((not (output-port-open? port)) (error "Port is not open for output" port))
      ((not (binary-port? port)) (error "Cannot write byte to a textual port" port))
      ((bytevector-port? port)
        (let* ((port-obj (bytevector-port-obj port))
               (accum (car port-obj))
               (bytes (cons byte (cdr port-obj))))
          (cond
            ((null? accum)
              (set-car! port-obj #u8())
              (set-cdr! port-obj bytes))
            ((> (length bytes) 64)
              (set-car! port-obj 
                (bytevector-append 
                  accum 
                  (apply bytevector (reverse (bytes)))))
              (set-cdr! port-obj ()))
            (else
              (set-cdr! port-obj bytes)))))
      (else (error "Not Implemented"))))))

(define (get-output-bytevector port)
  (cond
    ((not (port? port)) (error "Cannot get output from to non-port object" port))
    ((not (output-port? port)) (error "Port is not open for output" port))
    ((bytevector-port? port)
      (let* ((port-obj (bytevector-port-obj port))
              (accum (car port-obj))
              (bytes (cdr port-obj)))
        (cond
          ((null? accum) #u8())
          ((> (length bytes) 0)
            (set-car! port-obj 
              (bytevector-append 
                accum 
                (apply bytevector (reverse bytes))))
            (set-cdr! port-obj ())
            (car port-obj))
          (else accum))))
    (else (error "Port was not opened with (open-output-bytevector)" port))))

(define write-bytevector (case-lambda
  ((vec) (write-bytevector vec (current-output-port) 0 (bytevector-length vec)))
  ((vec port) (write-bytevector vec port 0 (bytevector-length vec)))
  ((vec port start) (write-bytevector vec port start (bytevector-length vec)))
  ((vec port start end)
    (letrec ((fn (lambda (i)
                    (if (< i end)
                      (begin
                        (write-u8 (bytevector-u8-ref vec i) port)
                        (fn (+ i 1)))))))
      (fn start)))))