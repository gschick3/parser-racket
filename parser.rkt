#lang racket

(define (read-lines filename)
  (map(lambda (line) (string-split line))
      (file->lines filename)))

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

(define (etail? lst)
  #t)

(define (expr? lst)
  #t)

(define (bool-op? lst)
  #t)

(define (boolean? lst)
  #t)

(define (stmt? lst)
  #t)

(define (linetail? lst)
  #t)

(define (label? lst)
  #t)

(define (line? lst)
  #t)

(define (linelist? lst)
  (cond
    [(empty? lst) #t] ; linelist can be epsilon
    [(and (line? (car lst)) (linelist? (cdr lst))) #t] ; linelist is line + linelist
    [else #f]))

(define (program? lst)
  (and (linelist? (drop-right lst 1))
       (equal? "$$" (car (last lst)))))

(program? (read-lines "test1.txt"))