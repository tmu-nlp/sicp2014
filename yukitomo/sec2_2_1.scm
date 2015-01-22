;sec2_2_1.scm

(cons 1 (cons 2 (cons 3 (cons 4 nil))))

;list 上記と同等
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))
one-through-four

;対の鎖を終端するのに使うnilの値は要素の一つもない並び, 空リスト(empty list)と考えてよい.
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(cons 5 one-through-four)

;list-refはリストのcdrの(n - 1) 番目の項を返す.
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

;リストの中にある項の個数を返す手続き length
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))


(define odds (list 1 3 5 7))
(length odds)

(append squares odds)
(append odds squares)
