#lang racket

; 集合に含まれるか
(define (element-of-set? x set)
	(cond
		((null? set) false)
		((equal? x (car set)) true)
		(else (element-of-set? x (cdr set)))
	)
)

; 集合への追加
(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)
	)
)

; 積集合
(define (intersection-set set1 set2)
	(cond
		;; どちらかが空集合なら空集合
		((or (null? set1) (null? set2)) `())
		((element-of-set? (car set1) set2)
			(cons (car set1)
				(intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))
	)
)

; 和集合
(define (union-set set1 set2)
	(cond
		((null? set1) set2)
		((and (null? set1) (null? set2)) `())
		((not (element-of-set? (car set1) set2))
			(cons (car set1) (union-set (cdr set1) set2)))
		(else (union-set (cdr set1) set2))
	)
)