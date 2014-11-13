
; 再帰
(define (fr n)
        (if (< n 3)
            n
            (+ (f (- n 1)) (* 2 (f (- n 2))) + (* 3 (f (- n 3))))))

; 反復
(define (fi n)
        (define (iter a b c counter)
                if (= counter 0)
                   a
                   (iter (+ a (* 2 b) (* 3 c)) a b (- counter 1)))
        (if (< n 3)
            n
            (iter 2 1 0 (- n 2))))
      
