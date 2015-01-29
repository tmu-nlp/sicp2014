(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;2乗するやつ
(display (map (lambda (x) (* x x)) (list 1 2 3 4 5 6 7)))
(newline)

;append
(define (append seq1 seq2) 
  (accumulate cons seq2 seq1))
(display (append (list 1 2 3 4) (list 5 6 7)))
(newline)

;長さ
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(display (length (list 1 2 3 4 5 6 7 9 "aaaaa")))