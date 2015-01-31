(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (enc-iter tree)
    (if (leaf? tree)
        '()
        (if (memq symbol (symbols (left-branch tree)))
            (cons 0 (enc-iter (left-branch tree)))
            (cons 1 (enc-iter (right-branch tree))))))
  (if (memq symbol (symbols tree))
      (enc-iter tree)
      (error "Not Found symbol of " symbol)))


; 最低頻度の記号の符号化の場合:O(1)
; 最高頻度の記号の符号化の場合:O(n * treeの深さ数)で、n個の記号の相対頻度が問題2.71にあるような
; 特別な場合では最大のtreeの深さ数はn - 1なので、
; 最高頻度の記号の符号化の場合:O(n * n - 1)

