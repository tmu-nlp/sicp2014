(car ''abracadabra)                     ; quote

;;; '(a b c) は (quote (a b c)) と等価。
;;; (car ''abracadabra) を quote を利用して書きなおすと
;;;
;;; (car (quote (quote abracadabra)))
;;;
;;; となる。
;;; したがって、car でリストの先頭にある quote が取り出される

