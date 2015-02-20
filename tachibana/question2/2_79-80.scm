; 2_79
; 各算術演算パッケージに equ? 手続きとインターフェースを追加する

;; 通常(ordinary)の算術演算パッケージに equ? を追加
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))


;; 有理数算術演算パッケージ equ? を追加
  (define (equ-rat? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))


;; 複素数算術演算パッケージに equ? を追加
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ-complex? z1 z2)))

(define (equ? x y) (apply-generic 'equ? x y))


; 2_80
; 各算術演算パッケージに =zero? 手続きとインターフェースを追加する

;; 通常(ordinary)の算術演算パッケージに =zero? を追加
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))


;; 有理数算術演算パッケージに =zero? を追加
  (define (=zero-rat? x)
    (= (numer x) 0))
  (put '=zero? '(rational)
       (lambda (x) (=zero-rat? x)))


;; 複素数算術演算パッケージに =zero? を追加
  (define (=zero-complex? z1)
    (and (= (real-part z1) 0)
         (= (imag-part z1) 0)))
  (put '=zero? '(complex)
       (lambda (z1) (=zero-complex? z1)))

(define (=zero? x) (apply-generic '=zero? x))

