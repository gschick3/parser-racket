#lang racket

(define (digit? s)
  (regexp-match-exact? #rx"[0-9]" s))

(define (numsign? s)
  (regexp-match-exact? #rx"[+|-]?" s))

(define (split-char-str s)
  (string-split s #rx"(?<=.)(?=.)"))

(define (num? s)
  (let ([s-split (split-char-str s)])
    (and (or (numsign? (first s-split)) (digit? (first s-split)))
         (andmap digit? (rest s-split)))))

(define (id? s)
  (regexp-match-exact? #rx"[a-zA-Z][a-zA-Z0-9]*" s))

(define (bool-op? s)
  (if (member s '("<" ">" "<=" ">=" "=" "<>")) #t #f))

; take one line of input as string and break into tokens
(define (split-tokens s)
  (flatten (map split-sym (string-split s)))) ; split on spaces first, then split each on symbols, then flatten

(define (split-sym s) ; split on every change from alphanumeric (abc, 123, _) to nonalpanumeric. Also split all parentheses by themselves
  (regexp-split (pregexp "(?<!^)(\\b|(?=[\\(\\)])|(?<=[\\(\\)]))(?!$)") s))

(define (string->token s)
  (cond
    [(equal? s ";") 'semicolon]
    [(equal? s ":") 'colon]
    [(equal? s "+") 'add-op]
    [(equal? s "-") 'sub-op]
    [(equal? s "*") 'mul-op]
    [(equal? s "/") 'div-op]
    [(equal? s "=") 'equal]
    [(equal? s "(") 'open-paren]
    [(equal? s ")") 'close-paren]
    [(equal? s "$$") 'eof]
    [(equal? s "if") 'if]
    [(equal? s "while") 'while]
    [(equal? s "read") 'read]
    [(equal? s "write") 'write]
    [(equal? s "goto") 'goto]
    [(equal? s "gosub") 'gosub]
    [(equal? s "return") 'return]
    [(equal? s "break") 'break]
    [(equal? s "end") 'end]
    [(bool-op? s) 'bool-op]
    [(num? s) 'number] ; last resort for numbers
    [(id? s) 'id] ; last resort for strings
    [else 'unknown]))

; tokenize line of code
(define (tokenize s)
  (map string->token (split-tokens s)))

(define test-str "asdf: dog=(12+ (12 * 11)); goto asdf")
;(string-split test-str)
;(split-tokens test-str)
(tokenize test-str)

(map tokenize (file->lines "test1.txt"))