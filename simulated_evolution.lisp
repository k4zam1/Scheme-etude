(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
	; randomで植物を生やす位置をposに格納
	(let ((pos (cons (+ left (random width)) (+ top (random height)) )))
		; ハッシュテーブルに植物を登録する
		; 集合を扱える言語なら集合で実装しても良いかも
		(setf (gethash pos *plants*) t)
	)
)

(defun add-plants ()
	(apply #'random-plant *jungle*)
	(random-plant 0 0 *width* *height*)
)

(defstruct animal x y energy dir genes)

(defparameter *animals*
	(list (make-animal
		:x		(ash *width*  -1)
		:y		(ash *height* -1)
		:energy 1000
		:dir	0
		:genes	(loop repeat 8 collecting (1+ (random 10)))
	))
)

(defun move (animal)
	(let ((dir (animal-dir animal))
		(x (animal-x animal))
		(y (animal-y animal)))
		(setf (animal-x animal)
			; dir方向に加算した値を*width*でmodとり,animal-xに代入
			(mod (+ x 
				(cond
					((and (>= dir 2) (< dir 5)) 1)
					((or (= dir 1) (= dir 5)) 0)
					(t -1)
				))
			*width*
		))

		(setf (animal-y animal)
			; dir方向に加算した値を*height*でmodとり,animal-yに代入
			(mod (+ y 
				(cond
					((and (>= dir 0) (< dir 3)) -1)
					((or (= dir 4) (= dir 7)) 1)
					(t 0)
				))
			*height*
		))

		; 生命力をひとつ減らす
		(decf (animal-energy animal))
	)
)

(defun turn (animal)
	(let ((x (random (apply #'+ (animal-genes animal)))))
		(labels ((angle (genes x)
			(let ((xnu (- x (car genes))))
				(if (< xnu 0)
					0
					(1+ (angle (cdr genes) xnu))))))
		(setf (animal-dir animal)
			(mod (+ (animal-dir animal) (angle (animal-genes animal) x))
				8))
		)
	)
)

(defun eat (animal)
	(let ((pos (cons (animal-x animal) (animal-y animal))))
		(when (gethash pos *plants*)
			(incf (animal-energy animal) *plant-energy*)
			(remhash pos *plants*)
		)
	)
)

; 200日以上の生命力があるときのみ子をつくれる
(defparameter *reproduction-energy* 200)
(defun reproduce (animal)
	(let ((e (animal-energy animal)))
		(when (>= e *reproduction-energy*)
			(setf (animal-energy animal) (ash e -1))
			(let ((animal-nu (copy-structure animal))
				; 遺伝子は親と共有しないために深いコピーのcopy-listを使う
				(genes (copy-list (animal-genes animal)))

				; ランダムに選ばれたスロット(mutation)を遺伝子変化させる(-1,0,1のいずれかを加算)
				(mutation (random 8)))
				(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
				(setf (animal-genes animal-nu) genes)

				; 新たに誕生したanimal-nuを*animals*へ加える
				(push animal-nu *animals*)))
	)
)

(defun update-world()
	; 生命力が0以下になった動物は世界から取り除く
	(setf *animals* (remove-if (lambda (animal)
								(<= (animal-energy animal) 0))
								*animals*))
	
	; すべての動物の一日
	(mapc (lambda (animal)
		(turn animal)
		(move animal)
		(eat animal)
		(reproduce animal))
		*animals*)

	; 植物の一日
	(add-plants)
)

(defun draw-world ()
	(loop for y below *height* do
		(progn
			(fresh-line)
			(princ "|")
			; x,yについて走査する
			; *animals*に登録されている動物の位置でx,yが一致したらMを描画する
			; 植物なら*を表示する
			(loop for x below *width* do
				(princ (cond ((some (lambda (animal)
									(and (= (animal-x animal) x)
										(= (animal-y animal) y)))
									*animals*)
								#\M)
								((gethash (cons x y) *plants*) #\*)
								(t #\space)))
			)
			(princ "|")
		)
	)
)

(defun evolution ()
	(draw-world)
	(fresh-line)
	(let ((str (read-line)))
		(cond 
			((equal str "quit") ())
			(t (let ((x (parse-integer str :junk-allowed t)))
				(if x
					(loop for i below x do
						(update-world)
						if (zerop (mod i 1000))
						do (princ #\.))
					(update-world))
				(evolution)))
		)
	)
)