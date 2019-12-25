#lang racket
; ニュートン法による平方根の計算

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define error 0.001)
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) error))
  (define (improve guess) (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))