#lang racket

; 階乗を計算する関数の各実装はどちらも再帰的に記述されているが
; 計算過程（プロセス）でみると振る舞いはことなる
; recursive processでは,式の展開で遅延評価を用いると長い展開となり
; スペース資源が奪われやすい
; iterative processでは,直前の値だけを覚えておけばいいので固定スペースで計算できる

(define (recursive-factorial n)
	(if (= n 1)
		1
		(* n (recursive-factorial (- n 1)))
	)
)

(define (iterative-factorial n)
	(fact-iter 1 1 n)
)

(define (fact-iter product counter max-count)
	(if (> counter max-count)
		product
		(fact-iter (* counter product) (+ counter 1) max-count)
	)
)