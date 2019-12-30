#lang racket

(define (square x) (* x x))
(define (even? n)
	(= (remainder n 2) 0))

; return x
; base^exp  == x mod m
(define (expmod base exp m)
	(cond
		((= exp 0) 1)
		((even? exp)
			(remainder
				(square (expmod base (/ exp 2) m))
				m
			)
		)
		(else
			(remainder
				(* base (expmod base (- exp 1) m))
				m
			)
		); else end
	); cond end
); define end

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond
		((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)
	)
)