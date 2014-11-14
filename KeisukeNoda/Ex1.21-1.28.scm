;;;; Exercise 1.21 ;;;;
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; result ;;
;; gosh> 199
;; gosh> 1999
;; gosh> 7

;;;; Exercise 1.22 ;;;;

(use srfi-19)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (prime? n)
       (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes f n)
  (cond ((= n 0) (newline) 'done)
        ((even? f) (search-for-primes (+ f 1) n))
        ((timed-prime-test f) (search-for-primes (+ f 2) (- n 1)))
        (else (search-for-primes (+ f 2) n))))

(define (search n)
  (display (search-for-primes n 3))
  (newline))

(search 1000)
(search 10000)
(search 100000)
(search 1000000)

;; result ;;
#|---
gosh> 
1001
1003
1005
1007
1009 *** #<time-duration 0.000007000>
1011
1013 *** #<time-duration 0.000007000>
1015
1017
1019 *** #<time-duration 0.000007000>
done
#<undef>
gosh> 
10001
10003
10005
10007 *** #<time-duration 0.000022000>
10009 *** #<time-duration 0.000022000>
10011
10013
10015
10017
10019
10021
10023
10025
10027
10029
10031
10033
10035
10037 *** #<time-duration 0.000022000>
done
#<undef>
gosh> 
100001
100003 *** #<time-duration 0.000069000>
100005
100007
100009
100011
100013
100015
100017
100019 *** #<time-duration 0.000068000>
100021
100023
100025
100027
100029
100031
100033
100035
100037
100039
100041
100043 *** #<time-duration 0.000068000>
done
#<undef>
gosh> 
1000001
1000003 *** #<time-duration 0.000217000>
1000005
1000007
1000009
1000011
1000013
1000015
1000017
1000019
1000021
1000023
1000025
1000027
1000029
1000031
1000033 *** #<time-duration 0.000264000>
1000035
1000037 *** #<time-duration 0.000214000>
done
#<undef>

ステップ数に比例

--- |#

;;;; Exercise 1.23 ;;;;

(use srfi-19)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (prime? n)
       (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes f n)
  (cond ((= n 0) (newline))
        ((even? f) (search-for-primes (+ f 1) n))
        ((timed-prime-test f) (search-for-primes (+ f 2) (- n 1)))
        (else (search-for-primes (+ f 2) n))))

(define (search n)
  (search-for-primes n 3))

(search 1000)
(search 10000)
(search 100000)
(search 1000000)


;; result ;;
#|---

gosh> 
1001
1003
1005
1007
1009 *** #<time-duration 0.000009000>
1011
1013 *** #<time-duration 0.000007000>
1015
1017
1019 *** #<time-duration 0.000006000>
#<undef>
gosh> 
10001
10003
10005
10007 *** #<time-duration 0.000019000>
10009 *** #<time-duration 0.000020000>
10011
10013
10015
10017
10019
10021
10023
10025
10027
10029
10031
10033
10035
10037 *** #<time-duration 0.000019000>
#<undef>
gosh> 
100001
100003 *** #<time-duration 0.000057000>
100005
100007
100009
100011
100013
100015
100017
100019 *** #<time-duration 0.000064000>
100021
100023
100025
100027
100029
100031
100033
100035
100037
100039
100041
100043 *** #<time-duration 0.000057000>
#<undef>
gosh> 
1000001
1000003 *** #<time-duration 0.000185000>
1000005
1000007
1000009
1000011
1000013
1000015
1000017
1000019
1000021
1000023
1000025
1000027
1000029
1000031
1000033 *** #<time-duration 0.000196000>
1000035
1000037 *** #<time-duration 0.000237000>
#<undef>

 (next x) のせい？いちいちifするから

---|#

;;;; Exercise 1.24 ;;;;

(use srfi-19)
(use srfi-27)

(define trials 10)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define true #t)
(define false #f)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (fast-prime? n trials)
       (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes f n)
  (cond ((= n 0) (newline) 'done)
        ((even? f) (search-for-primes (+ f 1) n))
        ((timed-prime-test f) (search-for-primes (+ f 2) (- n 1)))
        (else (search-for-primes (+ f 2) n))))

(define (search n)
  (display (search-for-primes n 3))
  (newline))

;;;; Exercise 1.25 ;;;;

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; remainder に使う引数が莫大になるため演算に時間がかかる

;;;; Exercise 1.26 ;;;;

;; Louisのは、squareを利用するべき所で、乗算を利用しているが、
;; その引数として、expmodが二度評価されている。
;; このため、expmodの評価回数が、n回になってしまうので、Θ(n)のプロセスになった。

;;;; Exercise 1.27 ;;;;

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; Carmichael数がFermatテストをだますかどうか
;;  a < n なるすべての a について確かめる

(define (fermat-test-2 n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (check a)
    (if (= a 1)
        #t
        (and (try-it a)
             (check (- a 1)))))
  (check (- n 1)))

(display (fermat-test-2 561))
(display (fermat-test-2 1105))
(display (fermat-test-2 1729))
(display (fermat-test-2 2465))
(display (fermat-test-2 2821))
(display (fermat-test-2 6601))

;; gosh> fermat-test-2
;; gosh> #t#<undef>
;; gosh> #t#<undef>
;; gosh> #t#<undef>
;; gosh> #t#<undef>
;; gosh> #t#<undef>
;; gosh> #t#<undef> 

;; Carmichael数は a < n のすべての a でFermatテストに対してtrueを返した

;;;; Exercise 1.28 ;;;;

;; nを法とした1の自明でない平方根を見つけたらシグナルを出すようにexpmod手続きを修正

;; random関数
(use srfi-27)
(define s (make-random-source))
(random-source-pseudo-randomize! s 314159 265358)
(define random (random-source-make-integers s))

;; ここから
(define (mr-test n times)
  (cond ((= n 0) (display 'done))
        ((fermat-test n) (mr-test n (- times 1)))
        (else false)))
        
(define (fermat-test n)
    (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (= (remainder (square base) m) 1)
             0
             (remainder (square (expmod base (/ exp 2) m))
                    m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))  

(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)

;; 1回目
;; gosh> #t
;; gosh> #t
;; gosh> #f
;; gosh> #t
;; gosh> #t
;; gosh> #t

;; 2回目
;; gosh> #t
;; gosh> #f
;; gosh> #t
;; gosh> #t
;; gosh> #t
;; gosh> #t

;; 3回目
;; gosh> #f
;; gosh> #f
;; gosh> #t
;; gosh> #t
;; gosh> #t
;; gosh> #t


;; 常に真とは限りません、なぜ？

