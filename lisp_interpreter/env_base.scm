(define-module env_base
	(export-all)
)
(select-module env_base)
(add-load-path "." :relative)
(use base_utils)

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