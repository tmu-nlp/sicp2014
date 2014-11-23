;ex1_24.scm

(define (square x) (* x x))

;--------------------------------------------------
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;-----------------------fast-----------------------------
;フェルマーの小定理
;p を素数とし、a を p の倍数でない整数（a と p は互いに素）とするときに以下を満たす
;a^(p-1) = 1 (mod p)
;a の p − 1 乗を p で割った余りは 1 
; p = 3, a = 5 のとき　5^(3-1) = 10 つまりp = 3 で割ると余りが１

;フェルマーテスト : フェルマーの小定理の対偶(十分条件のみを与える)を用いて確率的素数判定を行うアルゴリズム
;n と互いに素な(2つの整数が 1 と −1 以外に公約数を持たない場合の2数の関係)整数 a が　a^(n-1) != 1 (mod n) を満たせば n は合成数である
;1.パラメータとして、2 以上 n 未満の自然数 a を1つ定める
;2.a と n が互いに素でなければ「n は合成数」と出力して終了
;3.a^(n-1) != 1 (mod n) ならば「n は合成数」と出力して終了する。そうでないとき「n は確率的素数」と出力して終了
;「確率的素数」と出力された場合にはまた異なる a を用いて再びテストを行う。十分な回数だけ a を取り替えて繰り返せば、フェルマーテストが「確率的素数」と判定した数は実際に素数である可能性が高い。

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
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fast-timed-prime-test n times)
  (newline)
  (display n)
  (fast-start-prime-test n times (runtime)))

(define (fast-start-prime-test n times start-time)
  (if (fast-prime? n times)
      (report-prime (- (runtime) start-time))))

;----------------------------------------------------------
;O(log 1000099)
(fast-timed-prime-test 1000099 300)
;O(log 1000)
(fast-timed-prime-test 1009 300)

;expect log1000099 / log1000 = 1.66
;result(timese = 100) 2860 / 1113 = 2.59
;result(timese = 300) 4869 / 3431 = 1.41