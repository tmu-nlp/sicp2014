(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coefficient higher-term)
                (+ this-coefficient (* x higher-term)))
              0
              coefficient-sequence))

(print (horner-eval 2 (list 1 3 0 5 0 1)))

;accumulateにおいて、lambda式はopに当たり、またthis-coefficientは(car sequence),higher-termは(accumulate op initial (cdr sequence))に対応し、この関数は再帰される毎にxかけられるように表現されている
;つまりseauenceがnull?になるまで再帰が行われる。よって(horner-eval 2 (list 1 3 0 5 0 1))の式の展開は以下のようになる
;(+ 1 (* 2 (+ 3 (* 2 (+ 0 (* 2 (+ 5 (* 2 (+ 0 (* 2 (+ 1 (* 2 0))))))))))))
