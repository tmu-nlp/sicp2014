(define (make-mobile left right)
  (list left right))
(define (make-branch length structure) 
  (list length structure))

;2-29-a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length brabch)
  (car branch))
(define (branch-stracture branch)
  (car (cdr branch)))

;b
(define (branch-wight branch)
  (let ((st (branch-stracture branch)))
    (cond ((null? branch) 0)
          ((not (pair? st)) st)
          (else
           (total-weight st)))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;c
(define (torque branch)
  (let ((st (branch-structure branch)))
       (if (pair? st)
           (+ (torque (left-branch st)) 
              (torque (right-branch st)))
           (* (branch-length branch) 
              (branch-weight branch)))))

(define (balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

;d
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))