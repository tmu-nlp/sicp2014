;ex2_1.scm

;最大公約数を導出
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;分数を符号を正規化し、約分して定義する
(define (make-rat n d)
  (define (maker n_r d_r)
    (let ((g (gcd n_r d_r)))
      (if (< g 0)
         (cons (/ n_r (- g)) (/ d_r (- g)))
         (cons (/ n_r g) (/ d_r g)))))
  (if (< d 0)
      (maker (- n) (- d))
      (maker n d)))

;分子、分母を取得する関数
(define (numer x) (car x))
(define (denom x) (cdr x))

;分数を表示
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



(print-rat (make-rat 3 6))
(print-rat (make-rat -3 6))
(print-rat (make-rat 3 -6))
(print-rat (make-rat -3 -6))


