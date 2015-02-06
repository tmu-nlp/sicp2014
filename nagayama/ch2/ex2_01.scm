; #lang scheme

; 負数も扱える make-rat を定義する

(define (make-rat n d)
  (if (= d 0) #f
    (let ((g (gcd n d)))
      (if (> d 0) 
          (cons (/ n g) (/ d g))
          (cons (* -1 (/ n g))
                (* -1 (/ d g)))))))

; ran
(make-rat 2 -8)