(use srfi-27)

(define (square x)
  (* x x))

(define (scale-list items factor)
  (if (null? items)
	()
	(cons (* (car items) factor)
		  (scale-list (cdr items)
					  factor))))

(define (map proc items)
  (if (null? items)
	()
	(cons (proc (car items))
		  (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
	   items))

(define (square-list items)
  (if (null? items)
	()
	(cons (square (car items)) (square-list (cdr items)))))
(print (square-list (list 1 2 3 4)))

(define (square-list items)
  (map square items))
(print (square-list (list 1 2 3 4)))

(define (square-list items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons (square (car things))
				  answer))))
  (iter items ()))
(print (square-list (list 1 2 3 4)))
;thingは一番始めの要素が一つずつ減っていく
;answerはその減った要素を自乗した値を一番始めの要素としてanswerというlistに追加する
;そのため最終的にはanswerは逆順になってしまう
(define (square-list items)
  (define (iter things answer)
	(if (null? things)
	  answer
	  (iter (cdr things)
			(cons answer
				  (square (car things))))))
  (iter items ()))
(print (square-list (list 1 2 3 4)))
;リストを作るときに最初の要素がnilのためリストの形にならない
;リストは最後のペアがnilになって終わる
