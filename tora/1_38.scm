;if (i+1)%3==0 then Di=(i+1)/3*2
;else Di=1

(define (cont-frac N D k)
    (define (cf i)
        (if (= k i)
            (/ (N k) (D k))
            (/ (N i)
               (+ (D i) (cf (+ i 1))))))
  (cf 1))

(define (e k)
  (define (N i) 1)
  (define (D i)
    (if (= 0 (remainder (+ i 1) 3))
      (* 2 (/ (+ i 1) 3))
      1))
  (+ 2.0 (cont-frac N D k)))

(e 1)
