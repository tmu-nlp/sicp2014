; (define (sum? x)
;   (and (pair? x) (eq? (car x) '+)))

; (define (addend s) (cadr s))

; (define (augend s)
;   (if (null? (cdddr s))
;       (caddr s)
;       (cons '+ (cddr s))))

; (define (product? x)
;   (and (pair? x) (eq? (car x) '*)))

; (define (multiplier p) (cadr p))

; (define (multiplicand p)
;   (if (null? (cdddr p))
;       (caddr p)
;       (cons '* (cddr p))))

; (define (make-sum a1 a2)
;   (cond ((=number? a1 0) a2)
;         ((=number? a2 0) a1)
;         ((and (number? a1) (number? a2)) (+ a1 a2))
;         (else (list '+ a1 a2))))


; (define (make-product m1 m2)
;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;         ((=number? m1 1) m2)
;         ((=number? m2 1) m1)
;         ((and (number? m1) (number? m2)) (* m1 m2))
;         (else (list '* m1 m2))))


; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp)
;          (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum
;            (make-product (multiplier exp)
;                          (deriv (multiplicand exp) var))
;            (make-product (deriv (multiplier exp) var)
;                          (multiplicand exp))))
;         ((exponentiation? exp)
;          (if (variable? (exponent exp))
;          (make-product
;            (make-product
;              (exponent exp)
;              (make-exponentiation (base exp) (list '- (exponent exp) 1)))
;            (deriv (base exp) var))
;          (make-product
;            (make-product
;              (exponent exp)
;              (make-exponentiation (base exp) (- (exponent exp) 1)))
;            (deriv (base exp) var))))
;         (else
;          (error "unknown expression type -- DERIV" exp))))


; (define (exponentiation? x)
;   (and (pair? x) (eq? (car x) '**)))

; (define (base s) (cadr s))

; (define (exponent s) (caddr s))

; (define (make-exponentiation m1 m2)
;   (cond ((=number? m2 0) 1)
;         ((=number? m2 1) m1)
;         ((and (number? m1) (number? m2)) (expt m1 m2))
;         (else (list '** m1 m2))))

; (print (deriv (make-exponentiation (list '+ 'x 1) 5) 'x))
; (print (deriv (make-exponentiation (list '+ (list '* 2 'x) 1) 'a) 'x))



(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))



(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; a
; expを対として入力させており、carの方に変数が和なのか積なのかというタグ、cdrの方に処理したい数式を入力させて、
; data-directedに書き換えている。
; number?、variable? は引数が対でないため型を持たないため、組み込めない。


; b
; 2_57で定義したものを組み合わせてパッケージ化する。
; また実行のために3.3.3にあるmake-table,operation-table,get,putを定義する。

(define (install-sum-package)
  ;; 内部手続き
  (define (addend s) (car s))
  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (cons '+ (cdr s))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; システムの他の部分とのインターフェース
  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  ;; 内部手続き
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (if (null? (cddr p))
        (cadr p)
        (cons '* (cdr p))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-product exp var)
    ((get 'make '+)
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
  ;; 他の部分とのインターフェース
  (put 'make '* make-product)
  (put 'deriv '* deriv-product)
  'done)


(install-sum-package)
(install-product-package)

(print (deriv '(+ 1 2 3) 'x))
(print (deriv '(+ x x 10) 'x))

(print (deriv '(* 1 2 3) 'x))
(print (deriv '(* x x 10) 'x))


; c　やることはbと同じ
(define (install-exponentiation-package)
  ;; 内部手続き
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (make-exponentiation m1 m2)
    (cond ((=number? m2 0) 1)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (expt m1 m2))
          (else (list '** m1 m2))))
  (define (deriv-exponentiation exp var)
    (if (variable? (exponent exp))
         ((get 'make '*)
           ((get 'make '*)
             (exponent exp)
             (make-exponentiation (base exp) (list '- (exponent exp) 1)))
           (deriv (base exp) var))
         ((get 'make '*)
           ((get 'make '*)
             (exponent exp)
             (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var))))
  ;; 他の部分とのインターフェース
  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv-exponentiation)
  'done)


(install-exponentiation-package)

(print (deriv '(** x 2) 'x))
(print (deriv '(** 5 2) 'x))
(print (deriv '(** x 1) 'x))


; d
; 表においての行と列が入れ替わったと解釈できるので、
; 各手続きの put の引数の"演算"と"型"を逆転すればよい。





