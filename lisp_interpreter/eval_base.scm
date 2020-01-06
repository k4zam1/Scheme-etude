(define-module eval_base
	(export-all)
)
(select-module eval_base)
(add-load-path "." :relative)
(use base_utils)

;; 自己評価式
;; 基本データはnumber,stringのみ
(define (self-evaluating? exp)
	(cond
		((number? exp) #t)
		((string? exp) #t)
		(else #f)
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