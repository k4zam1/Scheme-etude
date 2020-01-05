#!/usr/local/bin/gosh
;; -*- coding: utf-8 -*-

;; 自己評価式
;; 基本データはnumber,stringのみ
(define (self-evaluating? exp)
	(cond
		((number? exp) #t)
		((string? exp) #t)
		(else #f)
	)
)


;; タグ付きリスト
;; 指定したtagではじまるタグ付きリストであるか判定
;; 多くの構文をtagged-listとして処理するために定義
(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		#f
	)
)


;; 変数
;; 変数はsymbol型で表現する
(define (variable? exp) (symbol? exp))


;; クォート式 ::= (quote <text-of-quotation>)
;; クォート式はquoteとタグのついたtagged-list
(define (quoted? exp)
	(tagged-list? exp 'quote)
)
;; 先頭のquoteタグを取り除けばテキスト
(define (text-of-quotation exp) (cadr exp))


;; 代入文 ::= (set! <var> <value>)
;; 代入文はset!でタグ付けされたtagged-list
(define (assignment? exp)
	(tagged-list? exp 'set!)
)
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;; 定義
;;   ::= (define <var> <value>)
;;   ::= (define (<var> <param 1> ... <param n>) <body>)
;; 定義文はdefineでタグ付けされたtagged-list
(define (definition? exp)
	(tagged-list? exp 'define)
)
(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		;; 第2定義でcadrをすると(var param...)というリストが取得される
		;; varを取り出すにはcaadrを適用する
		(caadr exp)
	)
)
(define (definition-value exp)
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda
			(cdadr exp)	;; params
			(cddr exp)	;; body
		)
	)
)


;; lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body))
)


;; if文
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(caddr exp)
		'#f
	)
)
(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative)
)
;; cond文
(define (cond? exp) (tagged-list? exp 'cond))
;; cond文の各節を取得
(define (cond-clauses exp) (cdr exp))
;; ある節がelse節か?
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else)
)
;; ある節の条件部を取得
(define (cond-predicate clause) (car clause))
;; ある節のactionsを取得
(define (cond-actions clause) (cdr clause))
;; cond文であるexpをif文にスイッチ
(define (cond->if exp)
	(expand-clauses (cond-clauses exp))
)
;; cond文の各節の集合clausesをif文にする
(define (expand-clauses clauses)
	(if(null? clauses)
		'#f
		(
			let ((first (car clauses))
				(rest (cdr clauses)))
			(if (cond-else-clause? first)
				;; firstがelse節のとき
				;; (cond (else 1)) -> 1のような特殊な場合
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clauses isn't last -- COND->IF" clauses)
				)
				;; firstがelse節でないとき
				(make-if
					(cond-predicate first)					;; 条件部
					(sequence->exp (cond-actions first))	;; then
					(expand-clauses rest)					;; else if ...
				)
			)
		)
	)
)

;; begin文
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp? seq) (car seq))
(define (rest-exps seq) (cdr seq))
;; シーケンスを評価するなら
;; make-beginでbegin文として単一の値とする
(define (sequence->exp seq)
	(cond
		((null? seq) seq)
		((last-exp? seq) (first-exp? seq))
		(else (make-begin seq))
	)
)
(define (make-begin seq) (cons 'begin seq))


;; 手続き作用
;; carが演算子,cdrが被演算子のリスト
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; 条件式
;; false以外のものはtrueであるとする
(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))



;; *-[apply]-------------------------------------------------------------*

;; 合成手続き
;; 合成手続きを作成する
(define (make-procedure parameters body env)
	(list 'procedure parameters body env)
)
(define (compound-procedure? p)
	(tagged-list? p 'procedure)
)
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (caddr p))



;; 環境
;; 環境をフレームのリストとして実装する
;; ある環境の外側の環境はリストのcdr,空の環境は単に空リストとする
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
;; 各フレームはリストの対(変数名リストと対応する値リスト)
(define (make-frame variables values)
	(cons variables values)
)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
	(set-car! frame (cons var (car frame)))
	(set-cdr! frame (cons val (cdr frame)))
)
;; 新しいフレームを追加する
(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		;; フレームを作成し現在の環境に追加する
		(cons (make-frame vars vals) base-env)
		;; 変数の個数と値の個数が一致しない場合
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals)
		)
	)
)
(define (lookup-variable-value var env)
	;; あるフレームを走査する
	(define (env-loop env)
		;; フレームの各変数を走査する
		(define (scan vars vals)
			(cond
				;; frameで変数が見つからなかったらvarsがnullとなる
				;; そうしたら次のframeにうつってまたscanする
				((null? vars) (env-loop (enclosing-environment env)))
				;; 変数が見つかったら値を返す
				((eq? var (car vars)) (car vals))
				;; 変数が目的の変数と異なるなら次のscanへ
				(else (scan (cdr vars) (cdr vals)))
			)
		)
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame) (frame-values frame))
			)
		)
	)
	(env-loop env)
)
;; 指定された環境の中で変数を新しい値に設定する
(define (set-variable-value! var val env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars) (env-loop (enclosing-environment env)))
				((eq? var (car vars)) (set-car! vals val))
				(else (scan (cdr vars) (cdr vals)))
			)
		)
		(if (eq? env the-empty-environment)
			(error "Unbound variable -- SET!" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame) (frame-values frame))
			)
		)
	)
	(env-loop env)
)
(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond
				((null? vars) (add-binding-to-frame! var val frame))
				((eq? var (car vars)) (set-car! vals val))
				(else (scan (cdr vars) (cdr vals)))
			)
		)
		(scan (frame-variables frame) (frame-values frame))
	)
)

;; procedure
(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive)
)
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
	(list
		(list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)
		(list 'pair? pair?)
		(list 'list list)
		(list 'cons cons)
		(list 'eq? eq?)
	)
)
(define (primitive-procedure-names)
	(map car primitive-procedures)
)
(define (primitive-procedure-objects)
	(map
		(lambda (proc) (list 'primitive (cadr proc)))
		primitive-procedures
	)
)

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
			(apply
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
		null
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