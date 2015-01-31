(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


; accumulate, flatmap, enumerate-interval
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))


; adjoin-position, empty-board
(define (adjoin-position new-low k rest-of-queens)
  (append rest-of-queens (list (list new-low k))))

(define empty-board ())

; safe?
(define (safe? k positions)
  (define (judge-iter x y)
    (cond ((null? y) #t)
          ((= (car x) (caar y)) #f)
          ((= (- (car x) (caar y))
              (- (cadr x) (cadar y))) #f)
          ((= (- (car x) (caar y))
              (- (cadar y) (cadr x))) #f)
          (else (judge-iter (car (reverse positions)) (cdr y)))))
  (judge-iter (car (reverse positions)) (cdr (reverse positions))))





(print (queens 7))

; $BCY$/$J$kM}M3(B:
;  2.42 $B$N%W%m%0%i%`$O(B queen-cols $B$,(B bordsize $BJ,$N2s?t8F$P$l$k$@$1$@$,!"(B
;  2.43 $B$N%W%m%0%i%`$O(B enumerate-interval $B$,@8@.$9$k(B 1 ~ bordsize $B$KBP1~$9$k(B
;  $B3F9TKh$K(B queen-cols $B$,(B bordsize $BJ,$N2s?t8F$P$l$k$+$i!#(B

; 2.43 $B$N%W%m%0%i%`$,%(%$%H%/%$!<%s%Q%:%k$r2r$/$N$K$+$+$k;~4V(B:
; 8^8 / T(=8) = 8^7 * T $B$[$I(B(?) 
