; deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


; x $B$OJQ?t$+!)(B
(define (variable? x) (symbol? x))
; $BF1$8JQ?t$+!)(B
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; $BOB$H@Q$N9=@.(B
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; x $B$OOB$+!)(B
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
; $B2C?t(B
(define (addend s) (cadr s))
; $BHo2C?t(B
(define (augend s) (caddr s))

; x $B$O@Q$+!)(B
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
; $B>h?t(B
(define (multiplier p) (cadr p))
; $BHo>h?t(B
(define (multiplicand p) (caddr p))


(print (deriv '(+ x (* x 3) (* x 2)) 'x))
(print (deriv '(* x y (+ x 3)) 'x))


; $BOB$H@Q$NI=8=$rJQ99$7!"(B2 $B8D0J>e$N9`$,07$($k$h$&$K$9$k(B

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cons '* (cddr p))))

(print (deriv '(+ x (* x 3) (* x 2)) 'x))
(print (deriv '(* x y (+ x 3)) 'x))

