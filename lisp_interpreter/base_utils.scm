;; モジュールのインタフェースの定義
(define-module base_utils
	(export tagged-list?)
)
;; モジュール本体
(select-module base_utils)

;; タグ付きリスト
;; 指定したtagではじまるタグ付きリストであるか判定
;; 多くの構文をtagged-listとして処理するために定義
(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		#f
	)
)