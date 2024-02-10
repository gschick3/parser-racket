#lang racket
(require data/either) ; requires functional-lib package

; TODO: Improve code tracing ability with line numbers

; take one line of input as string and break into tokens
(define (tokenize s)
  (flatten (map (λ (str) (regexp-split (pregexp "(?<!^)(\\b|(?=[\\(\\)])|(?<=[\\(\\)]))(?!$)") str))
                (string-split s)))) ; split on spaces first, then split each on symbols, then flatten

; Take filename, read lines and tokenize, then join all lines together
(define (tokenize-file filename)
  (append* (map tokenize (file->lines filename))))

; Applies funcs in-order to lst until one succeeds or all fail
(define (or-then lst on-fail . funcs) ; take list, return value on fail, and any number of functions
  (define (iter f)
    (if (empty? f)
        on-fail
        (let ([result ((first f) lst)])
          (if (success? result)
              result
              (apply or-then lst on-fail (rest f))))))
  (iter funcs))

; Applies funcs in-order to lst until all succeed or one fails
(define (and-then lst . funcs)
  (if (empty? funcs)
      (success lst)
      (let ([result ((first funcs) lst)])
        (if (failure? result)
            result
            (apply and-then (from-success #f result) (rest funcs))))))

; Return and-then function for list of functions (useful to combine with or-then)
(define (and-then-func . funcs)
  (lambda (lst)
    (apply and-then lst funcs)))

; Mark as success and return remainder of list
(define (pass-next lst)
  (success (rest lst)))

; Used for when specific token is unrecognized
(define (fail-token type lst)
  (failure (format "Unexpected ~a: '~a'" type (if (empty? lst) '() (first lst)))))

; Used for when longer statement is unrecognized
(define (fail-statement lst)
  (failure (format "Unexpected phrase: '~a'"
                   (cond [(empty? lst) '()]
                         [(= (length lst) 1) (first lst)]
                         [else (string-join (take lst 2))]))))

; Terminals

; Return function to test if the next token matches a certain string
(define (token=? s) ; call like ((token=? ";") lst)
  (lambda (lst)
    (cond
      [(empty? lst) (fail-token "token" lst)]
      [(equal? (first lst) s) (pass-next lst)]
      [else (fail-token "token" lst)])))

(define (digits? lst)
  (cond
    [(empty? lst) (fail-token "digit" lst)]
    [(regexp-match-exact? #rx"[0-9]+" (first lst)) (pass-next lst)]
    [else (fail-token "digit" lst)]))

(define (numsign? lst)
  (cond
    [(empty? lst) (success lst)]
    [(regexp-match-exact? #rx"[+|-]" (first lst)) (pass-next lst)]
    [else (success lst)])) ; if it can be epsilon and sees a character that doesn't match, still returns success, but doesn't remove element

(define (id? lst)
  (define reserved '("if" "while" "read" "write" "goto" "gosub" "return" "break" "end"))
  (cond
    [(empty? lst) (fail-token "id" lst)]
    [(and (not (member (first lst) reserved)) ; id cannot be reserved keyword
          (regexp-match-exact? #rx"[a-zA-Z][a-zA-Z0-9]*" (first lst))) (pass-next lst)]
    [else (fail-token "id" lst)]))
  
(define (bool-op? lst)
  (cond
    [(empty? lst) (fail-token "boolean operator" lst)]
    [(member (first lst) '("<" ">" "<=" ">=" "=" "<>")) (pass-next lst)]
    [else (fail-token "boolean operator" lst)]))

; Non-terminals

(define (num? lst)
  (and-then lst numsign? digits?)) ; numsign? will always return a success

(define (etail? lst)
  (if (and (cons? lst) (member (first lst) '("+" "-" "*" "/")))
      (expr? (rest lst)) ; etail must start with arithmetic operator and end with expr
      (success lst))) ; it can be empty, so we assume a success if it isn't there. Just don't remove the next token

(define (expr? lst)
  (or-then lst (fail-statement lst)
           (and-then-func id? etail?)
           (and-then-func num? etail?)
           (and-then-func (token=? "(") expr? (token=? ")"))))

(define (boolean? lst)
  (if (and (cons? lst) (member (first lst) '("true" "false")))
      (pass-next lst) ; if simple boolean true/false, succeed for next token
      (and-then lst expr? bool-op? expr?))) ; otherwise, evaluate on (expr? (bool-op? (expr? lst)))

(define (stmt? lst)
  (or-then lst (fail-statement lst)
           (and-then-func id? (token=? "=") expr?)
           (and-then-func (token=? "if") (token=? "(") boolean? (token=? ")") stmt?)
           (and-then-func (token=? "while") (token=? "(") boolean?
                          (token=? ")") linelist? (token=? "endwhile"))
           (and-then-func (token=? "read") id?)
           (and-then-func (token=? "write") expr?)
           (and-then-func (token=? "goto") id?)
           (and-then-func (token=? "gosub") id?)
           (token=? "return")
           (token=? "break")
           (token=? "end")))

(define (linetail? lst)
  (if (empty? lst)
      (success lst)
      (or-then lst (success lst) ; linetail can be empty
               (and-then-func (token=? ";") stmt? linetail?))))

(define (label? lst)
  (or-then lst (success lst) ; label can be empty, so default to success
           (and-then-func id? (token=? ":"))))

(define (line? lst)
  (and-then lst label? stmt? linetail?)) ; return whatever failure or success returns from these funcs

(define (linelist? lst) ; takes list of lists
  (if (empty? lst) (success lst)
      (or-then lst (success lst)
               (and-then-func line? linelist?))))

(define (program? lst)
  (let ([result ((and-then-func linelist? (token=? "$$")) lst)])
    (cond
      [(success? result) "Successful"]
      [else (from-failure #f result)])))

(define (parse filename)
  (program? (tokenize-file filename)))

; Unit tests
(module+ test
  (require rackunit)

  ; tokenizer
  (define test-str "asdf: dog=(-12+ (12 * 11)); goto asgf$$")
  (check-equal? (tokenize test-str) '("asdf" ":" "dog" "=" "(" "-" "12" "+" "(" "12" "*" "11" ")" ")" ";" "goto" "asgf" "$$"))

  ; line
  (check-equal? (line? '("asdf" ":" "dog" "=" "(" "-" "12" "+" "(" "12" "*" "11" ")" ")" "$$")) (success '("$$")))
  (check-true (failure? (line? '("$$"))))
  
  ; num
  (check-equal? (num? '("-" "12" "+")) (success '("+")))
  (check-equal? (num? '("12" "+")) (success '("+")))
  (check-true (failure? (num? '("*" "12" "+"))))

  ; expr
  (check-equal? (expr? '("abc" "+" "1")) (success '()))
  (check-equal? (expr? '("abc" "1" "+" "1")) (success '("1" "+" "1")))
  (check-equal? (expr? '("(" "abc" "+" "1" ")")) (success '()))
  (check-true (failure? (expr? '("+" "abc"))))

  ; boolean
  (check-equal? (boolean? '("false" "+" "1")) (success '("+" "1")))
  (check-equal? (boolean? '("abc" "+" "1" ">" "4")) (success '()))
  (check-true (failure? (boolean? '("abc" "+" "1"))))

  ; token=?
  (check-equal? ((token=? ":") '(":" "+" "1")) (success '("+" "1")))
  (check-true (failure? ((token=? ";") '(":" "+" "1"))))

  ; label
  (check-equal? (label? '("lbl" ":" "goto" "dog")) (success '("goto" "dog")))
  (check-equal? (label? '("blah" ":" ":" "4")) (success '(":" "4")))
  (check-equal? (label? '(":" "goto" "dog")) (success '(":" "goto" "dog")))

  ; stmt
  (check-equal? (stmt? '("abc" "=" "32" "+" "1" ";")) (success '(";")))
  (check-equal? (stmt? '("if" "(" "32" ">" "34" ")" "return" ";")) (success '(";")))
  (check-equal? (stmt? '("read" "avc" "32" "+" "1" ";")) (success '("32" "+" "1" ";")))
  (check-true (failure? (stmt? '("if" "(" "32" ")" "return" ";"))))

  ; linetail
  (check-equal? (linetail? '(";" "abc" "=" "32")) (success '()))
  (check-equal? (linetail? '(";")) (success '(";"))) ; this is above the paygrade of the linetail? function

  ; linelist
  (check-equal? (linelist? '("asdf" ":" "dog" "=" "(" "-" "12" "+" "(" "12" "*" "11" ")" ")" "$$")) (success '("$$")))
  )