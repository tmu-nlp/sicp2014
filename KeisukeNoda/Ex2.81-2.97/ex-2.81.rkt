#lang racket

(define h1 (make-hash))
(define (put op type element)
  (dict-set! h1 (list op type) element))
(define (get op type)
  (dict-ref h1 (list op type) false))

(define h2 (make-hash))
(define (put-coercion op type element)
  (dict-set! h2 (list op type) element))
(define (get-coercion op type)
  (dict-ref h2 (list op type) false))
;-------------------------------------------------
(define (attach-tag type-tag contents)
  (if (number? contents) contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Incorrectly marked data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Incorrectly marked data -- CONTENTS" datum))))

(define (tag x)
  (attach-tag 'scheme-number x))

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
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))
                    (error "No method for these types" (list op type-tags))))                
              (error "No method for these types"
                     (list op type-tags)))))))


;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number
;              scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)
;
(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
;---------------------------------------------
(define c1 (attach-tag 'complex (list 3)))
(define c2 (attach-tag 'complex (list 5)))

"
running (exp c1 c2) results in loop:
1. (exp c1 c2)
2. (apply-generic 'exp c1 c2)
3. (map type-tag (list c1 c2)) -> '(complex complex)
4. (get 'exp '(complex complex)) -> false
5. type1 = complex
6. type2 = complex
7. t1->t2 = (get-coercion 'complex 'complex) -> #<procedure:complex->complex>
8. (apply-generic 'exp (#<procedure:complex->complex> c1) c2)
9. (apply-generic 'exp c1 c2) .. <-- go to step 2

So answer to a) is:
No, Hugo is not right that the current version is not working and adding his fix break the game :)
"
(exp 2 2) ; ok
(exp c1 c2) ; error! (which is ok)
