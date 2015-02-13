#lang racket


; 型が異なるデータ同士の演算
; ex2.83 の raise 手続きを利用する
; 

 
;;;==================================================
;;;汎用手続き
;;;==================================================

; #| put, get
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
; |#

;;;==============================
;;;ex2.84 tower
;;;==============================
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let* ((a1 (car args))
                     (a2 (cadr args))
                     (level1 (tower-level a1))
                     (level2 (tower-level a2)))
                (cond ((< level1 level2)
                       (apply-generic op (raise-to level2 a1) a2))
                      ((< level2 level1)
                       (apply-generic op a1 (raise-to level1 a2)))
                      (else
                       (error "No method for these types"
                              (list op type-tags)))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum0 -- CONTENTS" datum))))

;;;==============================
;;;ex2.84 tower
;;;==============================
(define (tower-level x) 
  (let ((typex (type-tag x)))
    (cond ((eq? typex 'rational) 1)
          ((eq? typex 'complex) 3)
          (else ; scheme-number
           (let ((y (contents x)))
             (if (exact-integer? y)
                 0
                 2))))))
(define (raise-to level x)
  (if (= level (tower-level x))
      x
      (raise-to level (raise x))))


(define (square x) (* x x))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;;;==============================
;;;ex2.83 raise
;;;==============================
(define (raise x) (apply-generic 'raise x))


;;;==================================================
;;;scheme型の数字に関連するパッケージ
;;;==================================================
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  
  
  ;;;==============================
  ;;;ex2.83 raise
  ;;;==============================
  (put 'raise '(scheme-number)
       (lambda (x)
         (if (exact-integer? x)
             (make-rational x 1)
             (make-complex-from-real-imag x 0))))
  
  'done)
(install-scheme-number-package) ;インストールする

;;;構成子
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;;;==================================================
;;;有理数に関連する演算パッケージ
;;;==================================================
(define (install-rational-package)
  (define (gcd a b)
    (if (= b 0)
	a
	(gcd b (remainder a b))))

  ;private
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
	              (* (numer y) (denom x)))
	           (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	           (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	           (* (denom x) (numer y))))

  ;public
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (= (/ (numer x) (denom x))
                        (/ (numer y) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  
  ;;;==============================
  ;;;ex2.83 raise
  ;;;==============================
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (exact->inexact (/ (numer r) (denom r))))))
  
  'done)
(install-rational-package) ;インストールする

;;;構成子
(define (make-rational n d)
  ((get 'make 'rational) n d))


;;;==================================================
;;;直交座標形式のパッケージ
;;;==================================================
(define (install-rectangular-package)
  ;内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;システムの他の部分とのインタフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? '(rational)
       (lambda (x) (and (= 0 (real-part x))
                        (= 0 (imag-part x)))))
  'done)
(install-rectangular-package) ;インストールする


;;;==================================================
;;;極座標形式のパッケージ
;;;==================================================
(define (install-polar-package)
  ;内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;システムの他の部分とのインタフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? '(rational)
       (lambda (x) (and (= 0 (real-part x))
                        (= 0 (imag-part x)))))
  
  'done)
(install-polar-package) ;インストールする


;;;==================================================
;;;統合型の複素数の演算パッケージ
;;;==================================================
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;private
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		            (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		            (- (angle z1) (angle z2))))
  
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))

  ;public
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle) 
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x)
         (and (= 0 (real-part x))
              (= 0 (imag-part x)))))
  
  'done)
(install-complex-package) ;インストールする

;;;構成子
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;;選択子
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


; 以下, 実行テスト
(newline)

(display "ex2.83") (newline)

(raise (make-scheme-number 3))
(raise (make-rational 3 4))
(raise (make-rational 4 2))
(raise (make-scheme-number 3.14))
(raise (make-scheme-number 1))
(raise (make-scheme-number 1.0))

(newline)

(display "ex2.84") (newline)

(sub (make-scheme-number 10) (make-rational 3 4))
(add (make-scheme-number 3) (make-scheme-number 3.14))
(add (make-scheme-number 10) (make-complex-from-real-imag 2. 1.))
(add (make-rational 4 3) (make-scheme-number 2.0))
(mul (make-rational 4 3) (make-complex-from-mag-ang 3.0 0.55))
(sub (make-scheme-number 2.0) (make-complex-from-real-imag 1.0 5.0))


