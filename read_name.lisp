
(defun say-hello()
	(princ "please type your name :")
	(let ((name (read-line)))
		(princ "nice to meet you, ")
		(princ name)
		(princ #\newline)
		(princ "good bye")
		(print name)
	)
)

(say-hello)