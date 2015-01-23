(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))



(define A (make-mobile (make-branch 3 8) (make-branch 6 7)))
(define B (make-mobile (make-branch 4 3) (make-branch 5 2)))
(define C (make-mobile (make-branch 3 A) (make-branch 5 B)))


; a

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))


(print (left-branch C))
(print (right-branch C))
(print (branch-length (left-branch C)))
(print (branch-structure (right-branch C)))



; b

(define (total-weight x)
   (let ((y (branch-structure (left-branch x)))
         (z (branch-structure (right-branch x))))
     (+ (if (pair? y)
            (total-weight y)
            y)
        (if (pair? z)
            (total-weight z)
            z))))


(print (total-weight C))



; c

(define (balanced? x)
    (if (= (* (total-weight (branch-structure (left-branch x)))
              (branch-length (left-branch x)))
           (* (total-weight (branch-structure (right-branch x)))
              (branch-length (right-branch x))))
        "Yes"
        "No"))

(print (balanced? C))


; d

(define (make-mobile left right)
  (cons left right))

 (define (make-branch length structure)
  (cons length structure))

(define A (make-mobile (make-branch 3 8) (make-branch 6 7)))
(define B (make-mobile (make-branch 4 3) (make-branch 5 2)))
(define C (make-mobile (make-branch 3 A) (make-branch 5 B)))



(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))


(print (left-branch C))
(print (right-branch C))
(print (branch-length (left-branch C)))
(print (branch-structure (right-branch C)))

(print (total-weight C))
(print (balanced? C))
