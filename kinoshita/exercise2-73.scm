(define (lookup key-1 key-2 table)
    (let ((subtable
            (assoc key-1 (cdr table))))
        (if subtable
            (let ((record
                    (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    #f))
            #f)))

(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                              (cond (cons key-2 value)
                                    (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
    'ok)

(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable
                    (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                            (assoc key-2 (cdr subtable))))
                        (if record (cdr record) #f))
                    #f)))
        (define (insert! key-1 key-2 value)
            (let ((subtable
                    (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                            (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1 (cons key-2 value))
                                    (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation: TABLE" m))))
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
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp))
                 (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
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

    (put 'make '+ make-sum)
    (put 'deriv '+ deriv-sum)
    'done)

(define (install-product-package)
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

    (put 'make '* make-product)
    (put 'deriv '* deriv-product)
    'done)

(define (install-exponent-package)
    (define (base s) (car s))
    (define (exponent s) (cadr s))
    (define (make-exponentiation b e)
        (cond ((=number? e 0) 1)
              ((=number? e 1) b)
              (else (list '** b e))))
    (define (deriv-exponentation exp var)
        (let ((make-p (get 'make '*)))
            (make-p
                (make-p
                    (exponent exp)
                    (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var))))

    (put 'make '** make-exponentiation)
    (put 'deriv '** deriv-exponentation)
    'done)

(install-sum-package)
(install-product-package)
(install-exponent-package)

(print (deriv '(+ x 3) 'x))
(print (deriv '(+ x x x) 'x))
(print (deriv '(* 3 x) 'x))
(print (deriv '(** x 3) 'x))

;a
;違いは((get 'deriv (operator exp)) (operands exp) var)
;のみ
;演算子によって対応する演算を選び実行する
;number? variable?は引数がリストではないため型による判別ができない

;d
;各putの引数の演算と型を逆転させる
