#lang racket

; リストの長さを取得
(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))
	)
)

; n番目のリストの要素を取得
(define (list-ref items n)
	(if (< (- (length items) 1) n)
		null
		(if (= n 0)
			(car items)
			(list-ref (cdr items) (- n 1))
		)
	)
)

; 2つのlistを連結する
(define (list-add list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (list-add (cdr list1) list2))
	)
)

; map関数
(define (map proc items)
	(if (null? items)
		null
		(cons (proc (car items)) (map proc (cdr (items))))
	)
)

; accumulate
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))
	)
)