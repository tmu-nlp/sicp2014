;exercise 1.32 a

(define (accumulate-a combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
          (accumulate-a combiner null-value term (next a) next b))))

;exercise 1.32 b
;線形プロセス

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
   	    result
	    (iter (next a) (combiner (term a) result))))
    (iter a null-value))
