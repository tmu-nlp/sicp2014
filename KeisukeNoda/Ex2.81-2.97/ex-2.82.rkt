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
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Incorrectly marked data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Incorrectly marked data -- CONTENTS" datum))))

; -------------solution---------------------
(define (coerce-types from to) 
  (map (lambda (type) 
         (if (equal? type from) 
             (lambda (x) (attach-tag from (contents x)))
             (get-coercion from type))) to))

(define (map-coercion-procs procs type-tags)
  (if (null? type-tags)
      '()
      (cons ((car procs) (car type-tags)) (map-coercion-procs (cdr procs) (cdr type-tags)))))

(define (contains-all-procs? lst)
  (foldl (lambda (x y) (and x y)) true lst))

(define (apply-generic op . args)
  (define (apply-generic-impl op args search-tags type-tags)
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (null? search-tags))
              (let ((coercion-procs (coerce-types (car search-tags) type-tags)))
                (if (contains-all-procs? coercion-procs)
                    (let ((coerced-args (map-coercion-procs coercion-procs args)))
                      (apply-generic-impl op coerced-args (cdr search-tags) (map type-tag coerced-args)))
                    (apply-generic-impl op args (cdr search-tags) type-tags)))
              (error "No method for these types" (list op type-tags))))))
  (let ((type-tags (map type-tag args)))
    (apply-generic-impl op args type-tags type-tags)))

;-------------------------------------------------
(define (exp x y z) (apply-generic 'exp x y z))

(put 'exp '(type1 type1 type1)
     (lambda (x y z) (attach-tag 'type1 (expt x y))))

(put 'exp '(type2 type2 type2)
     (lambda (x y z) (attach-tag 'type2 (expt x y))))
;---------------------------------------------
(define c1 (attach-tag 'type1 2))
(define c2 (attach-tag 'type2 2))
(define c3 (attach-tag 'type3 3))

(put-coercion 'type2 'type1 (lambda (x) 
                              (attach-tag 'type2 (+ (contents x) 2))))
(put-coercion 'type2 'type3 (lambda (x) (attach-tag 'type2 (+ (contents x) 3))))


(apply-generic 'exp c1 c2 c3)

; the apply-generic will fail if there is something like (put 'exp '(type1 type2 type1) ...) as the types will always be converted to one of the types, not a mixed combination of them.
