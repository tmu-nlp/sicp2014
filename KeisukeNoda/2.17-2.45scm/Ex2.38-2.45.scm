;;;; Exercise 2.38 ;;;;

;; erse を fold-{right, left} を使って書け．という問題
;; fold-right が再帰．fold-left が反復ということがわかればいい感じかな？

;; 準備
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

; 問題文より
(define (fold-left op initial  sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; 答え

;; 確認
(define x (list 1 2 3))

(display x) (newline) ; (1 2 3)
(display (fold-right / 1 x)) (newline) ; 3/2 {= 1 / (2 / (3 / 1))}
(display (fold-left / 1 x)) (newline) ; 1/6 {= (1 / 2) / 3}
(display (fold-right list (list) x)) (newline) ; (1 (2 (3 ())))
(display (fold-left list (list) x)) (newline) ; (((() 1) 2) 3)

;;;; Exercise 2.39 ;;;;


;; 準備
(define nil (list))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

; 問題文より
(define (fold-left op initial  sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; 答え
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; 確認
(define x (list 1 2 3))

(display x) (newline) ; (1 2 3)
(display (reverse-r x)) (newline) ; (3 2 1)
(display (reverse-l x)) (newline) ; (3 2 1)

;;;; Exercise 2.40 ;;;;

;; 1 <= j < i <= n なる unique-pairs を定義せよ．という問題．
;; ついでに，prime-sum-pairs もそれを使って書き直せともある．
;; 元のprime-sum-pairs の部分を取ってくればよい．
;; 下記回答だと，unique-pairs の出力がx, y でソートされるようにしてある．

;; 準備
(define nil (list))

;; 2.2.3 より (日本語版 p.28)
(define (square x) (* x x))

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

;; 2.2.3 より (日本語版 p.66)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 2.2.3 より (日本語版 p.67)
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; 2.2.3 より (日本語版 p.71)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; 問題文より

;; 答え
(define (unique-pairs n)
  (flatmap (lambda (j)
             (map (lambda (i)
                    (list i j))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
               

;; 確認
(define n 5)

(display n) (newline) ; 5
(display (unique-pairs n)) (newline) ; ((1 2) (1 3) (2 3) (1 4) (2 4) (3 4) (1 5) (2 5) (3 5) (4 5))
(display (prime-sum-pairs n)) (newline) ; ((1 2 3) (2 3 5) (1 4 5) (3 4 7) (2 5 7))

;;;; Exercise 2.41 ;;;;

;; n に対し 0 < i < j < k <= n という三つの数字の組のうち，合計が s になるものをみつけたい．
;; 問題文では，i, j, k の大小について記述されていないが，
;; 順序が無い場合は回答重複するだけなので，省いてもよい．まいどおなじみの accumurate の中で再帰しています．
;; 再帰は，size を減らしていって，accumurate のループは，決まった数値より後ろの部分で回ります．


;; 準備
(define nil (list))

;; 2.2.3 より (日本語版 p.66)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 2.2.3 より (日本語版 p.67)
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; 問題文より

;; 答え
(define (make-unique-tupple size max)
  (define (iter size n)
    (if (= size 0)
        (list nil)
        (accumulate append nil
                    (map (lambda (i)
                           (map (lambda (l)
                                  (cons i l))
                                (iter (- size 1) (+ i 1))))
                         (enumerate-interval n max)))))
  (iter size 1))

(define (find-parted-3-numbers n s)
  (filter (lambda (triple)
            (= s (+ (car triple)
                    (cadr triple)
                    (caddr triple))))
          (make-unique-tupple 3 n)))

;; 確認
(define n 10)
(define s 15)

(format #t "n = ~d" n) (newline) ; 10
(format #t "s = ~d" s) (newline) ; 15
(display (find-parted-3-numbers n s)) (newline) ; ((1 4 10) (1 5 9) (1 6 8) (2 3 10) (2 4 9) (2 5 8) (2 6 7) (3 4 8) (3 5 7) (4 5 6))


;;;; Exercise 2.42 ;;;;

;; http://csnagoya-sicp.g.hatena.ne.jp/clairvy/20090705/sicp_ex_2_42

;; n x nのチェス盤にn個のクイーンを置く問題への全ての解の列を返す．
;; queens は，queens-cols という盤上の先頭k column 分のクイーンの位置の全部の場合のシーケンスを返す内部手続きを持つ．

(define nil (list))

;;; en p120
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;;; en p125
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;; jp p66
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;; jp p66
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; answer
(define empty-board nil)

;;; answer
(define (adjoin-position new-row col rest-of-queens)
  (cons (cons new-row col) rest-of-queens))

(define (queen-x queen)
  (car queen))
(define (queen-y queen)
  (cdr queen))

;;; answer
(define (safe? col positions)
  (define (diagonal a b)
    (let ((ax (queen-x a))
          (ay (queen-y a))
          (bx (queen-x b))
          (by (queen-y b)))
      (= (abs (- ax bx))
         (abs (- ay by)))))
  (let ((last-queen (car positions))
        (rest-of-queens (cdr positions)))
    (cond ((null? rest-of-queens)
           #t)
          (else
           (let ((x (queen-x last-queen))
                 (y (queen-y last-queen)))
             (null? (filter
                     (lambda (pos)
                       (cond ((= x (queen-x pos)) ; x
                              #t)
                             ((= y (queen-y pos)) ; y
                              #t)
                             ((diagonal last-queen pos) ; diagnal
                              #t)
                             (else
                              #f))) ; ok
                     rest-of-queens)))))))

;;; from question
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; for print
(define (print-queens positions)
  (let ((size (length positions)))
    (for-each
     (lambda (i)
       (display "|")
       (for-each
        (lambda (j)
          (display
           (if (null?
                (filter
                 (lambda (pos)
                   (cond ((and (= i (queen-x pos))
                               (= j (queen-y pos)))
                             #t)
                         (else
                          #f)))
                 positions))
               " " "*"))
          (display "|"))
        (enumerate-interval 1 size))
       (newline))
     (enumerate-interval 1 size))))

(for-each (lambda (x)
            (print-queens x)
            (newline))
          (queens (string->number (car *argv*))))


;;;; Exercise 2.43 ;;;;

;; http://www.serendip.ws/archives/798


;;;; Exercise 2.44 ;;;;

;; http://www.serendip.ws/archives/803

;;;; Exercise 2.45 ;;;;
