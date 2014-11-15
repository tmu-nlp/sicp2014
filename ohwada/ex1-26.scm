; square の代わりに乗算を用いた expmod 手続き

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; この手続きが遅いのは、square を用いている場合には
; (even? exp) が #t である時のsquare の引数は一つだが、
; 上の手続きでは二つあるため、(even? exp) が #t になる
; 分岐に入る度にその先のプロセス数が2倍になってしまうから。
