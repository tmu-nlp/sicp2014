;;;; Exercise 2.17 ;;;

;; 与えられたリストの最後の要素だけからなるリストを返す手続きlast-pair

(define (last-pair items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))
;; gosh> (34)

;;;; Exercise 2.18 ;;;;

;; 引数としてリストをとり、同じ要素の逆順のリストを返す
;; 手続きreverseを定義せよ

;; gaucheにreverseが実装されているので
;; reverse-newとして実装する

(define (reverse-new items)
  (define (reverse-iter rev-items items)
    (if (null? items)
        rev-items
        (reverse-iter (cons (car items) rev-items) (cdr items))))
  (reverse-iter () items))

(reverse-new (list 1 4 9 16 25))
;; gosh> (25 16 9 4 1)

;;;; Exercise 2.19 ;;;;


(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))]))

;; コインのリストの先頭を返す
(define (first-denomination coin-values)
  (car coin-values))

;; コインのリストの先頭意外を返す
(define (except-first-denomination coin-values)
  (cdr coin-values))

;; コインのリストに残りがあるか
(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins)
;; gosh> 292


;; アルゴリズム上重要なのはコインの種類であり
;; コインの順序ではない
(define us-coins2 (list 5 25 10 1 50))
(cc 100 us-coins2) 
;; gosh> 292

;;;; Exercise 2.20 ;;;;


;; ドット末尾記法を使って
;; １つかそれを超える引数を取り
;; 先頭と同じ偶奇性を持つ引数のリストを返す手続き
(define (same-parity head . remaind)
  (define (filter pred items)
    (cond [(null? items) ()]
          [(pred (car items))
           (cons (car items) (filter pred (cdr items)))]
          [else
           (filter pred (cdr items))]))
  (if (odd? head)
      (cons head (filter odd? remaind))
      (cons head (filter even? remaind))))

(same-parity 1 2 3 4 5 6 7)

;; gosh> (1 3 5 7)

(same-parity 2 3 4 5 6 7)

;; gosh> (2 4 6)

;;;; Exercise 2.21 ;;;;


;; 引数として数のリストをとり、2乗のリストを返す
;; square-listについて

(define (square-list items)
  (if (null? items)
      ()
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(square-list (list 1 2 3 4))
;; gosh> (1 4 9 16)

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4))
;; gosh> (1 4 9 16)

;;;; Exercise 2.22 ;;;;


(define (square-list items)
  (define (square x)
    (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items ()))

(square-list (list 1 2 3 4))
;; gosh> (16 9 4 1)

;; 理由
;; (1^2 . ())
;; => (2^2 . (1^2 . ()))
;; => (3^2 . (2^2 . (1^2 .())))
;; のように評価され、リストが作られていってしまうから

(define (square-list items)
  (define (square x)
    (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items ()))

(square-list (list 1 2 3 4))
;; gosh> ((((() . 1) . 4) . 9) . 16)

;; consがリストを返すのは第1引数が値で、
;; 第2引数がリストの場合


;; ではどうやったら期待通り動くのか
;; 値をリスト化してappendする
(define (square-list items)
  (define (square x)
    (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items ()))

(square-list (list 1 2 3 4))
;; gosh> (1 4 9 16)

;;;; Exercise 2.23 ;;;;


;; for-eachの実装
(define (for-eaach proc items)
  (cond [(null? items) #f]
        [else
         (proc (car items))
         (for-each proc (cdr items))]))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; gosh> 
;; 57
;; 321 
;; 88#<undef>
