#lang racket
(require data/either) ; requires functional-lib package

; Helper Functions

; Applies funcs in-order to lst until one succeeds or all fail
(define (or-then lst on-fail . funcs) ; take list, return value on fail, and any number of functions
  (if (empty? funcs)
      on-fail
      (let ([result ((first funcs) lst)])
        (if (success? result)
            result
            (apply or-then lst on-fail (rest funcs))))))

; Applies funcs in-order to lst until all succeed or one fails
(define (and-then lst . funcs)
  (if (empty? funcs)
      (success lst)
      (let ([result ((first funcs) lst)])
        (if (failure? result)
            result
            (apply and-then (from-success #f result) (rest funcs))))))

; Return and-then function for list of functions (used to combine with or-then)
(define (and-then-func . funcs)
  (lambda (lst)
    (apply and-then lst funcs)))

; Mark as success and return remainder of list
(define (pass-next lst)
  (success (rest lst)))

; Terminals

; Return function to test if the next token matches a certain string
(define (token=? s) ; call like ((token=? ";") lst)
  (lambda (lst)
    (cond
      [(empty? lst) (failure lst)]
      [(equal? (first lst) s) (pass-next lst)]
      [else (failure lst)])))

(define (digits? lst)
  (cond
    [(empty? lst) (failure lst)]
    [(regexp-match-exact? #rx"[0-9]+" (first lst)) (pass-next lst)]
    [else (failure lst)]))

(define (numsign? lst)
  (cond
    [(empty? lst) (success lst)]
    [(regexp-match-exact? #rx"[+|-]" (first lst)) (pass-next lst)]
    [else (success lst)])) ; numsign can be epsilon

(define (id? lst)
  (define reserved '("if" "while" "endwhile" "read" "write" "goto" "gosub" "return" "break" "end"))
  (cond
    [(empty? lst) (failure lst)]
    [(and (not (member (first lst) reserved)) ; id cannot be reserved keyword
          (regexp-match-exact? #rx"[a-zA-Z][a-zA-Z0-9]*" (first lst))) (pass-next lst)]
    [else (failure lst)]))
  
(define (bool-op? lst)
  (cond
    [(empty? lst) (failure lst)]
    [(member (first lst) '("<" ">" "<=" ">=" "=" "<>")) (pass-next lst)]
    [else (failure lst)]))

; Non-terminals

(define (num? lst)
  (and-then lst numsign? digits?))

(define (etail? lst)
  (or-then lst (success lst) ; etail can be epsilon
           (and-then-func (token=? "+") expr?)
           (and-then-func (token=? "-") expr?)
           (and-then-func (token=? "*") expr?)
           (and-then-func (token=? "/") expr?)))

(define (expr? lst)
  (or-then lst (failure lst)
           (and-then-func id? etail?)
           (and-then-func num? etail?)
           (and-then-func (token=? "(") expr? (token=? ")"))))

(define (boolean? lst)
  (or-then lst (failure lst)
           (token=? "true")
           (token=? "false")
           (and-then-func expr? bool-op? expr?)))

(define (stmt? lst)
  (or-then lst (failure lst)
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
  (or-then lst (success lst) ; linetail can be epsilon
           (and-then-func (token=? ";") stmt? linetail?)))

(define (label? lst)
  (or-then lst (success lst) ; label can be epsilon, so default to success
           (and-then-func id? (token=? ":"))))

(define (line? lst)
  (and-then lst label? stmt? linetail?))

(define (linelist? lst)
  (or-then lst (success lst) ; linelist can be epsilon
           (and-then-func line? linelist?)))

(define (program? lst)
  (and-then lst linelist? (token=? "$$")))

; Tokenization

; take one line of input as string and break into tokens
(define (tokenize s)
  (flatten (map (λ (str) (regexp-split (pregexp "(?<!^)(\\b|(?=[\\(\\)])|(?<=[\\(\\)]))(?!$)") str))
                (string-split s)))) ; split on spaces first, then split each on symbols, then flatten

; Take filename, read lines and tokenize, return list of lines
(define (tokenize-file-lines filename)
  (map tokenize (file->lines filename)))

; Errors

; To simplify line matching, find line based on list of lines and [flat] list of remaining tokens
(define (match-line orig-lst flat-lst)
  (define (loop flat-len lst)
    (let ([next-line (length (first lst))])
      (if (<= flat-len next-line)
          (length lst)
          (loop (- flat-len next-line) (rest lst)))))
  (loop (length flat-lst) (reverse orig-lst)))

; Create syntax errors from original tokenized lines and the remaining tokens at error time
(define (syntax-error all-lines remaining-tokens)
  (format "Line ~a: Syntax error: Unexpected token: '~a'"
          (match-line all-lines remaining-tokens) ; line number
          (first remaining-tokens))) ; failed token

; Driver Functions

; Given file name, parses according to grammar
(define (parse filename)
  (let* ([lines (tokenize-file-lines filename)]
         [result (program? (append* lines))])
    (cond [(success? result) "Accepted"]
          [else (syntax-error lines (from-failure #f result))])))

; Unit Tests

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
