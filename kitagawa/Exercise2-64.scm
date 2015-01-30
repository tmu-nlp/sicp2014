(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;; 見づらいので、 let* を使って書きなおす
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts)
                                         right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))


;;; 使用例
(define lst (list 1 3 5 7 9 11))
(list->tree lst)

;;; a. partial-tree がどう働くか？
;;;
;;; 入力されたリスト(elts)と数(n)を元にし、
;;; リストの先頭から n 番目の要素までを使って以下の木を構成する
;;; (リストの中間の要素を m とする)
;;;
;;; 節の見出し: m
;;; 左部分木: リスト先頭 <= x < m を満たす要素を使って作られた部分木
;;; 右部分木: m < x <= リスト末尾 を満たす要素を使って作られた部分木
;;;
;;; 構成した部分木と、部分木の構成に利用しなかった要素リストを cons
;;; して返す。


;;; b. list-> tree のオーダーは？
;;;
;;; 要素の増加に比例して、partial-tree が呼ばれる回数が増加する。
;;; オーダーは: Θ(n)

;;; n = 1 のとき、 partial-tree は 3回
(define lst1 (list 1))
(list->tree lst1)

;;; n = 2 のとき、 partial-tree は 5回
(define lst2 (list 2 5))
(list->tree lst2)

;;; n = 3 のとき、partial-tree は 7回
(define lst3 (list 2 5 8))
(list->tree lst3)

;;; n = 4 のとき、partial-tree は 9回
(define lst4 (list 2 5 8 10))
(list->tree lst4)
