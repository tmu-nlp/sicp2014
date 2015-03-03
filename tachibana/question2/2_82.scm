
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



(define (install-rectangular-package)
   ;; 内部手続き
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


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


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
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))


(install-rectangular-package)

(print (real-part (cons 'rectangular (cons 1 2))))
(print (imag-part (cons 'rectangular (cons 1 2))))




(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

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
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(print (add (make-scheme-number 1) (make-scheme-number 2)))
(print (mul (make-scheme-number 1) (make-scheme-number 2)))


(define (install-complex-package)
   ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

   ;; 内部手続き
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
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


;2_81
(define (scheme-number->scheme-number n) n)

(define (complex->complex z) z)

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
                         (if (eq? type1 type2) ;ここのif文で型が同じであるかどうか見ている
                             (error "E1. No method for these types" (list op type-tags))
                             (let ((t1->t2 (get-coercion type1 type2))
                                   (t2->t1 (get-coercion type2 type1)))
                                  (cond (t1->t2
                                          (apply-generic op (t1->t2 a1) a2))
                                        (t2->t1
                                          (apply-generic op a1 (t2->t1 a2)))
                                        (else (error "E2. No method for these types" (list op type-tags))))))
                         (error "E3. No method for these types" (list o type-tags))))))))



; ans
; apply-genericを可変長引数が使えるようにする.そのために可変長引数の全ての引数を同じ型にする必要がある。コードを自力では書けず、ネットから拾ってきた物を
; 以下に書くが、流れはわかったが不十分な気がする。coercion-all-first-type-tagでのprocが全てのargsに対応しているかわからない。


(define (apply-generic op . args)
  (define (coerce-all args target-type-tag) ;; 引数(args) の各要素を全て target-type-tag 型に変換する(target-type-tag 型のみで構成されるリストを返す)。
    (if (null? args)
        '()
        (let ((proc (get-coercion (type-tag (car args)) target-type-tag)))
             (if proc
                 (cons (proc (car args)) (coerce-all (cdr args) target-type-tag))
                 (cons (car args) (coerce-all (cdr args) target-type-tag))))))
  (let ((type-tags (map type-tag args))) ;; type-tagsは引数の型のリスト ex (complex scheme-number complex)
       (define (coercion-all-first-type-tag types) ;; 引数の最初の型に合わせて強制型変換をして演算を試みる
         (let ((first-type (car types))) ;first-typeはtype-tagsをcarしたもの
              (if (null? first-type)
                  #f
                  (let ((first-type-args (coerce-all args first-type))) ;first-type-argsは強制型変換されたargs。リストのリストになっている。
                       (let ((proc (get op (map type-tag first-type-args)))) ;この部分がどういう動作をしているかわからない
                            (if proc
                                (apply proc (map contents first-type-args)) ;演算と型の表に処理がのっていれば、その処理をfirst-type-argsの各リストにするようにする
                                (coercion-all-first-type-tag (cdr types)))))))) ;全てのargsが同じ型にならなければ次の型をfirst-typeとしてもう一巡する
       (coercion-all-first-type-tag type-tags)))


; 実数と複素数のような直接的に型変換可能になるように定義されている場合は今回のような方針がとれるが、例えば型の階層構造で言う有理数と複素数のような実数を介した
; 型の関係の場合は直接的に型変換可能になるように定義されていないため、今回のような方針はとれない。


