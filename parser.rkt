#lang racket

(define (etail? lst)
  #t)

(define (expr? lst)
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