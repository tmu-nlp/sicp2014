(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; Carmichael 数が Fermat テストをだますかどうかを調べるには random 手続きではなく a < n なるすべての a について確かめる必要がある.
(define (fermat-test-v2 n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (check a)
    (if (= a 1)
        #t
        (and (try-it a)
             (check (- a 1)))))
  (check (- n 1)))

(display "fermat-test-version2")
(newline)
(display "Carmichael numbers:")
(newline)
(display "561,1105,1729,2465,2821,6601")
(newline)
(display (fermat-test-v2 561))  ;; => #t
(display (fermat-test-v2 1105)) ;; => #t
(display (fermat-test-v2 1729)) ;; => #t
(display (fermat-test-v2 2465)) ;; => #t
(display (fermat-test-v2 2821)) ;; => #t
(display (fermat-test-v2 6601)) ;; => #t
(newline)

;;Carmichael 数は a < n なるすべての a で Fermat テストをパスすることが確かめられた.