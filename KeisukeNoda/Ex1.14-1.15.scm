;;;; Exercise 1.14 ;;;;

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc
                  (- amount
                   (first-denomination kinds-of-coins))
                  kinds-of-coins)
                 (cc amount (- kinds-of-coins 1))))))
;; (cc 11 5) ==> 4
;; 段階数は、57
;; 時間計算量(ステップ数) = n*n個程度
;; amount(記憶域)       = n*n個程度

;; cc 11 5
;;    |    \
;; cc 11 4  cc -39 5
;;    |    \
;; cc 11 3  cc -14 4
;;    |    \----------------------------------------\
;; cc 11 2                                           cc 1 3
;;    |    \------------------------\                   |   \
;; cc 11 1                           cc 6 2          cc 0 3  cc -9 3
;;    |    \                            |   \---------------\
;; cc 11 0  cc 10 1                  cc 6 1                  cc 1 2
;;             |    \                   |   \                   |   \
;;          cc 10 0  cc 9 1          cc 6 0  cc 5 1          cc 0 2  cc -4 2
;;                      |   \                   |   \
;;                   cc 9 0  cc 8 1          cc 5 0  cc 4 1
;;                              |   \                   |   \
;;                           cc 8 0  cc 7 1          cc 4 0  cc 3 1
;;                                      |   \                   |   \
;;                                   cc 7 0  cc 6 1          cc 3 0  cc 2 1
;;                                              |   \                   |   \
;;                                           cc 6 0  cc 5 1          cc 2 0  cc 1 1
;;                                                      |   \                   |   \
;;                                                   cc 5 0  cc 4 1          cc 1 0  cc 0 1
;;                                                              |   \
;;                                                           cc 4 0  cc 3 1
;;                                                                      |   \
;;                                                                   cc 3 0  cc 2 1
;;                                                                              |   \
;;                                                                           cc 2 0  cc 1 1
;;                                                                                      |   \
;;                                                                                   cc 1 0  cc 0 1


;;;; Exercise 1.15 ;;;;

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; 5回 ;;
;; 4.05
;; 1.35
;; 0.45
;; 0.15
;; 0.05

;; 時間計算量 ;;
;; log ( a )

;; 空間計算量 ;;
;; log ( a )
