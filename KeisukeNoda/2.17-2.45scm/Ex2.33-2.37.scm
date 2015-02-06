;;;; Exercise 2.33 ;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define nil '())

;; map
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map2 square (list 1 2 3 4 5))
;; gosh> (1 4 9 16 25)


;; append
(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(append2 (list 1 2 3 4 5) (list 5 4 3 2 1))
;; gosh> (1 2 3 4 5 5 4 3 2 1)
;; (cons (car seq1)
;;       (accumulate cons seq2 (cdr seq1)))...


;; length
(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length2 (list 1 2 3 4 5))
;; gosh> 5
;; (accumulate (lambda (x y) (+ 1 y)) initial (cdr sequence))...

;;;; Exercise 2.34 ;;;;


;; Hornerの方法

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
;; gosh> 79

;; (+ (car (list 1 3 0 5 0 1))
;;    (* 2 (accumulate op 0 (cdr (list 1 3 0 5 0 1)))))...

;;;; Exercise 2.35 ;;;;


(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(count-leaves (list 1 2 3 4 5))
;; gosh> 5

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
			 (cond ((null? x) 0)
			       ((not (pair? x)) 1)
			       (else count-leaves x)))
		       t)))

(count-leaves (list 1 2 3 4 5))
;; gosh> 5

;;;; Exercise 2.36 ;;;;


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;; gosh> (22 26 30)

;;;; Exercise 2.37 ;;;;


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v (list 1 2 3))
(define w (list 4 5 6))

(dot-product v w)
;; gosh> 32
;; 1 * 4 + 2 * 5 + 3 * 6 = 4 + 10 + 18 = 32


(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector m v)
;; gosh> (14 32 50)
;; ((1*1+2*2+3*3) (4*1+5*2+6*3) (7*1+8*2+9*3))
;; =>(14 32 50)


(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose m)
;; gosh> ((1 4 7) (2 5 8) (3 6 9))


(define n (list (list 3 2 1) (list 9 8 7) (list 6 4 5)))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix m n)
;; gosh> ((39 30 30) (93 72 69) (147 114 108))  

