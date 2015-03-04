(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;----------------------- 追加部分１------------------------------

(define (sin-gen x) (apply-generic 'sin-gen x))
(define (cos-gen x) (apply-generic 'cos-gen x))
(define (atan-gen x y) (apply-generic 'atan-gen x y))
(define (square-gen x) (apply-generic 'square-gen x))
(define (sqrt-gen x) (apply-generic 'sqrt-gen x))

;----------------------------------------------------------------


; get, put
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


(define global-array '())

(define (put-coercion op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get-coercion op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

;---------------------------------------------------

; attach-tag, type-tag, contents
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else "Bad tagged datum -- CONTENTS" datum)))


; apply-generic 修正版
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (equal? type1 type2))
                    (let ((raised-args (raise-to-same-type args)))
                      (let ((raised-type-tags (map type-tag raised-args)))
                        (let ((proc (get op raised-type-tags)))
                          (if proc
                            (apply proc (map contents raised-args))
                            (error "No method for these types"
                                   (list op type-tags))))))
                    (error "No method for these types" (list op type-tags))))
                (error "No method for these types" (list op type-tags)))))))

(define (higher-type x y)
  (let ((type-order '(complex real-number rational scheme-number)))
    (define (higher-type-iter z)
      (cond ((equal? x (car z)) x)
            ((equal? y (car z)) y)
            (else (higher-type-iter (cdr z)))))
    (higher-type-iter type-order)))

(define (raise-to-same-type two-arg)
  (let ((type1 (type-tag (car two-arg)))
        (type2 (type-tag (cadr two-arg))))
    (if (equal? type1 type2) 
        two-arg
        (if (equal? (higher-type type1 type2) type1)
            (raise-to-same-type (list (car two-arg) (raise (cadr two-arg))))
            (raise-to-same-type (list (raise (car two-arg)) (cadr two-arg)))))))



;------------------------------------------------------


; install-scheme-number-package
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
  (put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise '(scheme-number)
    (lambda (x) (make-rational x 1))) 

;-------------------- 追加部分２--------------------------
  (put 'sin-gen '(scheme-number)
       (lambda (x) (tag (sin x))))
  (put 'cos-gen '(scheme-number)
       (lambda (x) (tag (cos x))))
  (put 'atan-gen '(scheme-number)
       (lambda (x) (tag (atan x))))
  (put 'square-gen '(scheme-number)
       (lambda (x) (tag (mul x x))))
  (put 'sqrt-gen '(scheme-number)
       (lambda (x) (tag (sqrt x))))
;---------------------------------------------------------

  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))




; install-rational-package
(define (install-rational-package)
   ;; 内部手続き
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

   ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (x) (make-real-number (/ (* (numer x) 1.0) (denom x)))))

;--------------------- 追加部分３------------------------------
; 分子と分母を取り出して割ることで小数にし、その sin, cos 等をとり、
; 適当な大きな数を掛けてから truncate で小数点以下を切り捨て、
; 掛けた数を分母にして、再度有理数を構成する

  (put 'sin-gen '(rational)
       (lambda (x) (tag (make-rat (truncate (* 1000000 (sin (/ (numer x) (denom x)))))
                                  1000000))))
  (put 'cos-gen '(rational)
       (lambda (x) (tag (make-rat (truncate (* 1000000 (cos (/ (numer x) (denom x)))))
                                  1000000))))
  (put 'atan-gen '(rational)
       (lambda (x) (tag (make-rat (truncate (* 1000000 (atan (/ (numer x) (denom x)))))
                                  1000000))))
  (put 'square-gen '(rational)
       (lambda (x) (tag (make-rat (truncate (* 1000000 (square (/ (numer x) (denom x)))))
                                  1000000))))
  (put 'sqrt-gen '(rational)
       (lambda (x) (tag (make-rat (truncate (* 1000000 (sqrt (/ (numer x) (denom x)))))
                                  1000000))))
;---------------------------------------------------------------

  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))




; install-real-number-package
(define (install-real-number-package)
  (define (tag x)
    (attach-tag 'real-number x))
  (put 'add '(real-number real-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real-number real-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real-number real-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real-number real-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(real-number real-number)
       (lambda (x y) (tag (expt x y))))
  (put 'make 'real-number
       (lambda (x) (tag x)))
  (put 'raise '(real-number)
       (lambda (x) (make-complex-from-real-imag x 0)))

;------------------------ 追加部分４------------------------------
  (put 'sin-gen '(real-number)
       (lambda (x) (tag (sin x))))
  (put 'cos-gen '(real-number)
       (lambda (x) (tag (cos x))))
  (put 'atan-gen '(real-number)
       (lambda (x) (tag (atan x))))
  (put 'square-gen '(real-number)
       (lambda (x) (tag (mul x x))))
  (put 'sqrt-gen '(real-number)
       (lambda (x) (tag (sqrt x))))
;------------------------------------------------------------------

  'done)

(install-real-number-package)

(define (make-real-number n)
  ((get 'make 'real-number) n))




(define (square x) (* x x))


; 以下 con, sin などを上で定義したもの (cos-gen, sin-gen) に変更
; また、四則演算を汎用のもの (add, mul 等) に変更

; install-rectangular-package
(define (install-rectangular-package)
   ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-gen (add (square-gen (real-part z))
                 (square-gen (imag-part z)))))
  (define (angle z)
    (atan-gen (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cos-gen a)) (mul r (sin-gen a))))

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)



; install-polar-package
(define (install-polar-package)
   ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos-gen (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin-gen (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt-gen (add (square-gen x) (square-gen y)))
          (atan-gen y x)))

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)




; install-complex-package
(define (install-complex-package)
   ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

   ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))  

   ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



; selector
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; equ?, =zero?
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

; raise 演算
(define (raise z) (apply-generic 'raise z))







; テスト
(define a (make-rational 5 6))
(define b (make-scheme-number 3))
(define c (make-complex-from-mag-ang b a))

(print (add c c))
   ;(complex rectangular (rational -95723.0 . 125000.0) rational 995407.0 . 125000.0) 
(print (real-part (add c c))) ; (rational -95723.0 . 125000.0)
(print (imag-part (add c c))) ; (rational 995407.0 . 125000.0)
