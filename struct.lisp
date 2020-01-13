(defstruct person
	name
	age
	waist-size
	favorite-color
)
(defun person-age(person)
	(cadr person)
)

(defparameter *bob* (
	make-person
		:name "bob"
		:age 35
		:waist-size 32
		:favorite-color "blue"
))
