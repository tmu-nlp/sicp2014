;;;; Exercise 1.34 ;;;;

(define (f g)
  (g 2))

(print (f square))
;; gosh> 4
;; (f square) => (square 2)

(print (f (lambda (z) (* z (+ z 1)))))
;; gosh> 6
;; ((lambda (z) (* z (+ z 1))) 2)
;; => 6

(f f)
;; gosh> *** ERROR: invalid application: (2 2)
;; Stack Trace:
;; _______________________________________
;;   0  (eval expr env)
;;         At line 179 of "/usr/local/share/gauche-0.9/0.9.4/lib/gauche/interactive.scm"

;; エラーを見ると、
;; (f f)
;; => (f 2)
;; => (2 2)
;; 置き換えモデルに従うと上記のようになる.
;; 最終的に手続きでない2に引数2を与えて評価しようとしてエラーになる.

;;;; Exercise 1.35 ;;;;

;; SICPで、黄金比は φ^2 = φ + 1 を満たすとある.
;; 変形すると φ = 1 + 1/φ となるので、
;; つまりφは変換 x |→ 1 + 1/xの不動点である.
;; 黄金比 φ が不動点であることを示したので、fixed-point手続きで黄金比を求める

;; 関数の不動点の探索
;; Ex 1.3.3
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; fixed-pointを用いて黄金比を求める
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

;; gosh> 1.618033813400125
;; SICPで、φ = 1.6180 となっているので正しいと考えられる.


;;;; Exercise 1.36 ;;;;

;; 関数の不動点の探索
;; newlineとdisplayで探索過程が出力されるようにする
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; fixed-pointを用いて黄金比を求める
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

;; x |→ log(1000)/log(x)の不動点の探索を行い
;; x^x = 1000を求める
;; xlog(x) = log(1000) => x = log(1000)/log(x)
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)

;; gosh> 2.0
;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.759850702401539
;; 5.215843784925895
;; 4.182207192401397
;; 4.8277650983445906
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.555532270803653
;; 4.555537970114198
;; 4.555534211524127
;; 4.555536690243655
;; 4.555535055574168
;; 4.5555361336081
;; 4.555535422664798
;; 41ステップ

;; 緩和法を使ったら
(define (average a b)
  (/ (+ a b) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)
;; gosh> 2.0
;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136242
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624
;; 4.5555465521473675
;; 4.555537551999825
;; 4.555536019631145
;; 4.555535758730802
;; 12ステップ

;; 緩和法が収束に役だっている.


;;;; Exercise 1.37 ;;;;

;; (a) 連分数の近似、k項有限連分数近似の実装　;;
;; 黄金比φ = 1.6180であるから
;; 1/φ = 0.61804...

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           9)
;; gosh> 0.6181818181818182

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
;; gosh> 0.6179775280898876

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
;; gosh> 0.6180555555555556

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)
;; gosh> 0.6180257510729613

;; 以上からkが11以上で4桁の精度の近似ができる


;; (b) aを再帰的プロセスで実装したので、反復的プロセスで実装 ;;

(define (cont-frac-iter n d k)
  (define (iter i val)
    (if (= i 0)
        val
        (iter (- i 1)
              (/ (n i) (+ (d i) val)))))   (iter k 0.0))



(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           9)
;; gosh> 0.6181818181818182

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
;; gosh> 0.6179775280898876

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
;; gosh> 0.6180555555555556

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)
;; gosh> 0.6180257510729613

;; 再帰版と同様の結果が得られる

;;;; Exercise 1.38 ;;;;

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (e-2 k)
  (cont-frac (lambda (x) 1.0)
             (lambda (x)
               (if (= (remainder x 3) 2)
                   (* 2 (+ 1 (quotient x 3)))
                   1.0))             10.0))

(define (e k)
  (+ 2 (e-2 k)))

(e-2 1)
(e-2 100)

;; gosh> 0.7182817182817183
;; gosh> 0.7182817182817183


;;;; Exercise 1.39 ;;;;

(define (tan-cf x k)
  (define (iter i)
    (cond ((> i k) 0)
          ((= i 1)
           (/ x (- 1 (iter 2))))
          (else
           (/ (* x x)
              (- (- (* i 2) 1) (iter (+ i 1)))))))   (iter 1))


(use math.const)
(tan-cf (/ pi 4) 2)
(tan-cf (/ pi 4) 8)


