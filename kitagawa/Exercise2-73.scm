(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

;;; ハッシュテーブルを使い put, get を実装
;;; キーとして演算と型のリストを利用する
(define table (make-hash-table 'equal?))

(define (put op type item)
  (hash-table-put! table (list op type) item))

(define (get op type)
  (hash-table-get table (list op type)))

(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2) (list '+ a1 a2))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2) (list '* m1 m2))

  (define (deriv-product exp var)
    ((get 'make '+)
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'make '* make-product)
  (put 'deriv '* deriv-product)
  'done)

(define (install-exponentiation-package)
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exponentiation a1 a2)
    (cond ((=number? a2 0) 1)
          ((=number? a2 1) a1)
          (else (list '** a1 a2))))

  (define (deriv-exp exp var)
    ((get 'make '*) (exponent exp)
                  ((get 'make '*)
                   (make-exponentiation
                    (base exp)
                    (- (exponent exp) 1))
                   (deriv (base exp) var))))

  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv-exp)
  'done)

;;; a.
;;; 演算(deriv)と型タグから手続きから対応する手続きを取り出し
;;; 実行するように変更した。
;;; number?, variable? では扱うデータに型タグが存在しないため
;; データ手動の振り分けに組み込むことができない。


;;; b.
(install-sum-package)
(install-product-package)

;; p87 の make-sum, make-product を利用しており、
;; 結果が冗長に見えるが正しい結果を得ている。
(deriv 1 'x)                            ; 0
(deriv 'x 'x)                           ; 1
(deriv '(+ x 3) 'x)                     ; (+ 1 0)
(deriv '(* x y) 'x)                     ; (+ (* x 0) (* 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
;; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))


;;; c
(install-exponentiation-package)
(deriv '(** x 2) 'x)                    ; (* 2 (* x 1))
(deriv '(** x 3) 'x)                    ; (* 3 (* (** x 2) 1))


;;; d
;;; put に渡す引数の順番を変更するだけ。
;;; put 側(パッケージ)を変更したくないのであれば、
;;; get 内部でキーを作成する際に順番を逆にしてキーを作成すればよい。
