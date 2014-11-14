;xが列数
;yが行数
(define (pascal x y)
  (cond ((> x y ) 0)
	((= x y ) 1)
	((= x 1 ) 1)
	(else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))

(print (pascal 1 1))
(print (pascal 1 2) (pascal 2 2))
(print (pascal 1 3) (pascal 2 3) (pascal 3 3))
(print (pascal 1 4) (pascal 2 4) (pascal 3 4) (pascal 4 4))
(print (pascal 1 5) (pascal 2 5) (pascal 3 5) (pascal 4 5) (pascal 5 5))

