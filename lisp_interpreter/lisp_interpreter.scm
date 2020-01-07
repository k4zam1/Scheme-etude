#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-
(add-load-path "." :relative)
(use eval_base)
(use env_base)
(use procedure_base)

(define (setup-environment)
	(let ((initial-env
		(extend-environment (primitive-procedure-names)
							(primitive-procedure-objects)
							the-empty-environment)))
		(define-variable! '#t #t initial-env)
		(define-variable! '#f #f initial-env)
		initial-env
	)
)
(define the-global-environment (setup-environment))

;; プロンプト
(define input-prompt " M-Eval input:")
(define output-prompt " M-Eval value:")
(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (eval input the-global-environment)))
			(announce-output output-prompt)
			(user-print output)
		)
	)
	(driver-loop)
)
(define (prompt-for-input string)
	(newline) (newline) (display string) (newline)
)
(define (announce-output string)
	(newline) (display string) (newline)
)
(define (user-print object)
	(if (compound-procedure? object)
		(display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>
		))
		(display object)
	)
)

;; *-[CORE]-----------------------------------------------------------*
(define (eval exp env)
	(cond
		;; *** 基本式
		;; 数値などの自己評価式はそのまま返す
		((self-evaluating? exp) exp)
		;; 変数なら環境から対応する自己評価式を探す
		((variable? exp) (lookup-variable-value exp env))

		;; *** 特殊式
		;; クォート式はそのままクォート式とする
		((quoted? exp) (text-of-quotation exp))
		;; 代入
		((assignment? exp) (eval-assignment exp env))
		;; 定義
		((definition? exp) (eval-definition exp env))
		;; if文
		((if? exp) (eval-if exp env))
		;; ラムダ式
		;; プロシージャをつくるmake-procedureにパラメータと本体と環境を投げる
		((lambda? exp)
			(make-procedure (lambda-parameters exp) (lambda-body exp) env))
		;; begin文
		;; expをactionsに分割してeval-sequenceで処理
		((begin? exp)
			(eval-sequence (begin-actions exp) env)
		)
		;; cond文
		;; cond->ifでcond文をif文の入れ子として評価器に再投入
		((cond? exp) (eval (cond->if exp) env))
		;; 関数適用
		;; 演算子(operator)と被演算子(operand)を再帰的に評価する
		((application? exp)
			(my-apply
				(eval (operator exp) env)
				;; exp := (2+3) * (3+5)
				;; このとき (2+3),(3+5)がoperandsとなり
				;; これらを評価して5,8といったoperandのリストを返す
				(list-of-values (operands exp) env)
			)
		)
		;; エラー
		(else (error "Unknown expression type -- EVAL" exp))
	)
)

;; 手続きの引数
(define (eval-if exp env)
	;; if文の条件部(predicate)を評価して
	;; trueならexpのif-consequent部分を評価して返す
	;; falseならexpのif-alternative部分を評価して返す
	(if (true? (eval (if-predicate exp) env))
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env)
	)
)

;; if文
(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons
			;; evalに投げることで再帰的に評価が行われ
			;; 基本的な数値のみがlist-of-valuesの要素となる
			;; (2+(3+1))*2 -> 2,(2+(3+1))
			;; (2+(3+1)) -> 2,(3+1)
			;; (3+1) -> 4
			;; operands : [4,2] operator : *
			(eval (first-operand exps) env)
			(list-of-values (rest-operands exps) env)
		)
	)
)

;; シーケンス
(define (eval-sequence exps env)
	(cond
		((last-exp? exps) (eval (first-exp? exps) env))
		(else
			(eval (first-exp? exps) env)
			(eval-sequence (rest-exps exps) env)
		)
	)
)

;; 代入
(define (eval-assignment exp env)
	(set-variable-value!
		(assignment-variable exp)
		(eval (assignment-value exp) env)
		env
	)
	'ok
)

(define (eval-definition exp env)
	(define-variable!
		(definition-variable exp)
		(eval (definition-value exp) env)
		env
	)
	'ok
)

;; applyは手続き(プロシージャ)と引数のリスト(arguments)を受け取る
;; 基本手続きはapply-primitive-procedure
;; 合成手続きは基本手続きまで分解し再帰的に処理
(define (my-apply procedure arguments)
	(cond
		;; 基本手続き(関数が基本式のみ)
		((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments)
		)
		;; 合成手続き(関数の中に別の呼び出し(手続き)がある場合)
		((compound-procedure? procedure)
			;; (eval-sequence actions env)
			(eval-sequence
				;; actions ... primitiveな手続きの集合
				(procedure-body procedure)
				;; env ... ローカル変数 引数 グローバル変数(環境)をあわせた環境
				(extend-environment (procedure-parameters procedure) arguments (procedure-environment procedure))
			)
		)
		;; エラー
		(else (error "Unknown procedure type -- APPLY" procedure))
	)
)
(define (apply-primitive-procedure proc args)
	(apply
		(primitive-implementation proc)
		args
	)
)

;; 実行
(driver-loop)