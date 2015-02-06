;;;; Exercise 2.07 ;;;;


;; 区間の抽象化の実装

;; 2.1.4で定義した演算
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 区間構成子
(define (make-interval a b) (cons a b))


;; 選択子upper-bound, lower-boundの実装
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))


;; 試してみる
(define R1 (make-interval 7.48 6.12))
(define R2 (make-interval 4.735 4.205))

(upper-bound R1)
;; gosh> 7.48
(lower-bound R1)
;; gosh> 6.12

(add-interval R1 R2)
;; gosh> (10.325 . 12.215)
(mul-interval R1 R2)
;; gosh> (25.7346 . 35.41780000000001)
(div-interval R1 R2)
;; gosh> (1.2925026399155226 . 1.7788347205707493)

;;;; Exercise 2.08 ;;;;

;; 二つの区間の差の計算

;; x > yとした時
;; x-y の upper-bound : xのupper-bound - yのlower-bound
;; x-y の lower-bound : xのlower-bound - yのupper-bound

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))

(define R1 (make-interval 7.48 6.12))
(define R2 (make-interval 4.735 4.205))

(sub-interval R1 R2) 
;; gosh> (3.2750000000000004 . 1.3849999999999998)


;;;; Exercise 2.09 ;;;;
;; 証明 ;;

upper-bound = U
lower-bound = L
center      = C
width       = W
とする.

A = [L(A), U(A)]
B = [L(B), U(B)]
の範囲があると仮定.

(1) 和と差
A + B = [L(A) + L(B), U(A) + U(B)]
A - B = [L(A) - U(B), U(A) - L(B)]

それぞれの区間の幅は、
W(A + B) = 1/2 * [L(A) + L(B) + U(A) + U(B)]
         = 1/2 * [L(A) + U(A)+ L(B) + U(B)]
         = W(A) + W(B)

W(A - B) = 1/2 * [L(A) - U(B) + U(A) - L(B)]
         = 1/2 * [L(A) + U(A) - L(B) + U(B)] 
         = W(A) - W(B)

よって、和と差は、元の区間の幅のみで表現できる.

(2) 積と商
全ての数が正の数である範囲と仮定.
A × B = [L(A)・ L(B), U(A)・U(B)]

W(A × B) = 1/2 * [L(A)・L(B) + U(A)・U(B)]
         = 1/2 * [ {C(A) - W(A)}・{C(B) - W(B)} + {C(A) + W(A)}・{C(A) + W(A)} ]
         = 1/2 * [C(A)・C(B) - C(A)・W(B) - C(B)・W(A) + W(A)・W(B) + C(A)・C(B) + C(A)・W(B) + C(B)・W(A) + W(A)・W(B)]
         = C(A)・C(B) + W(A)・W(B)

商も同様なので、積と商は、元の区間の幅のみで表現できない.

