; Church数
; ある数字nは、ある関数fを引数にとってそれをxにn回適用するものとして考える
; lambda reductionしていく感じ
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; exsample
; x = 0, f = x+1
(define (f x) (+ 1 x))
(print ((zero f) 0))
(print ((one f) 0))
(print ((two f) 0))
(print (((add-1 two) f) 0))

