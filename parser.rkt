#lang racket
(require data/either) ; requires functional-lib package

; TODO: Improve code tracing ability; currently only reports on first token of line and uses generic "Unexpected token" error message

; take one line of input as string and break into tokens
(define (tokenize s)
  (flatten (map (λ (str) (regexp-split (pregexp "(?<!^)(\\b|(?=[\\(\\)])|(?<=[\\(\\)]))(?!$)") str))
                (string-split s)))) ; split on spaces first, then split each on symbols, then flatten

(define (tokenize-file filename)
  (append* (map tokenize (file->lines filename))))

(define (check-or on-fail . eithers)
  (define result (findf success? eithers)) ; return the first success, or the failure message of the first tried one
  (if result result on-fail))

; Applies funcs in-order to lst until all succeed or one fails
(define (and-then lst . funcs)
  (if (empty? funcs)
      (success lst)
      (let ([result ((first funcs) lst)])
        (if (failure? result)
            result
            (apply and-then (get-success result) (rest funcs))))))

; Make the success getter easier to read. Assumes failure not possible
(define (get-success x)
  (from-success #f x))

; Mark as success and pass rest of list
(define (pass-next lst)
  (success (rest lst)))

(define (fail lst)
  (failure (format "Unexpected token: ~a" (if (empty? lst) '() (first lst)))))

; Terminals

; Return function to test if the next token matches a certain string
(define (token=? s) ; call like ((token=? ";") lst)
  (lambda (lst)
    (cond
      [(empty? lst) (fail lst)]
      [(equal? (first lst) s) (pass-next lst)]
      [else (fail lst)])))

(define (digits? lst)
  (cond
    [(empty? lst) (fail lst)]
    [(regexp-match-exact? #rx"[0-9]+" (first lst)) (pass-next lst)]
    [else (fail lst)]))

(define (numsign? lst)
  (cond
    [(empty? lst) (success lst)]
    [(regexp-match-exact? #rx"[+|-]" (first lst)) (pass-next lst)]
    [else (success lst)])) ; if it can be epsilon and sees a character that doesn't match, still returns success, but doesn't remove element

(define (id? lst)
  (define reserved '("if" "while" "read" "write" "goto" "gosub" "return" "break" "end"))
  (cond
    [(empty? lst) (fail lst)]
    [(and (not (member (first lst) reserved)) ; id cannot be reserved keyword
      (regexp-match-exact? #rx"[a-zA-Z][a-zA-Z0-9]*" (first lst))) (pass-next lst)]
    [else (fail lst)]))
  
(define (bool-op? lst)
  (cond
    [(empty? lst) (fail lst)]
    [(member (first lst) '("<" ">" "<=" ">=" "=" "<>")) (pass-next lst)]
    [else (fail lst)]))

; Non-terminals

(define (num? lst)
  (and-then lst numsign? digits?)) ; numsign? will always return a success

(define (etail? lst)
  (if (and (cons? lst) (member (first lst) '("+" "-" "*" "/")))
      (expr? (rest lst)) ; etail must start with arithmetic operator and end with expr
      (success lst))) ; it can be empty, so we assume a success if it isn't there. Just don't remove the next token

(define (expr? lst)
  (check-or (fail lst) ; default to failed
            (and-then lst id? etail?)
            (and-then lst num? etail?)
            (and-then lst (token=? "(") expr? (token=? ")"))))

(define (boolean? lst)
  (if (and (cons? lst) (member (first lst) '("true" "false")))
      (pass-next lst) ; if simple boolean true/false, succeed for next token
      (and-then lst expr? bool-op? expr?))) ; otherwise, evaluate on (expr? (bool-op? (expr? lst)))

(define (stmt? lst)
  (check-or (fail lst)
            (and-then lst id? (token=? "=") expr?)
            (and-then lst (token=? "if") (token=? "(") boolean? (token=? ")") stmt?)
            (and-then lst (token=? "while") (token=? "(") boolean?
                      (token=? ")") linelist? (token=? "endwhile"))
            (and-then lst (token=? "read") id?)
            (and-then lst (token=? "write") expr?)
            (and-then lst (token=? "goto") id?)
            (and-then lst (token=? "gosub") id?)
            ((token=? "return") lst)
            ((token=? "break") lst)
            ((token=? "end") lst)))

(define (linetail? lst)
  (if (empty? lst)
      (success lst)
      (check-or (success lst) ; linelist can be empty, but it should be the last thing. If there's anything past the linetail, this is a problem
                (and-then lst (token=? ";") stmt? linetail?))))

(define (label? lst)
  (check-or (success lst) ; label can be empty, so default to success
            (and-then lst id? (token=? ":"))))

(define (line? lst)
  (check-or (fail lst)
            (and-then lst label? stmt? linetail?)))

(define (linelist? lst) ; takes list of lists
  (if (empty? lst) (success lst)
      (check-or (success lst)
                (and-then lst line? linelist?))))

(define (program? filename)
  (let ([result (and-then (tokenize-file filename) linelist? (token=? "$$"))])
    (cond
      [(success? result) "Successful"]
      [else (from-failure #f result)])))

(program? "test1.txt")

(tokenize-file "test1.txt")

(module+ test
  (require rackunit)

  ; tokenizer
  (define test-str "asdf: dog=(-12+ (12 * 11)); goto asgf$$")
  (check-equal? (tokenize test-str) '("asdf" ":" "dog" "=" "(" "-" "12" "+" "(" "12" "*" "11" ")" ")" ";" "goto" "asgf" "$$"))

  ; line
  ; it's above line?'s paygrade to know if the ";" could be part of a different line or program end
  (check-equal? (line? '("asdf" ":" "dog" "=" "(" "-" "12" "+" "(" "12" "*" "11" ")" ")" "$$")) (success '("$$")))
  (check-true (failure? (line? '("$$"))))
  ;(check-true (failure? (line? '("asdf" ":" "dog" "=" "(" "-" "12" "+" "(" "12" "*" "11" ")" ")" ";"))))
  
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

  ; token=
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