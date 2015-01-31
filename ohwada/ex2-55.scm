(print (car ''abracadabra))

; この式が "quote" と印字する理由
; (car '(a b c)) → (car (quote (a b c))) → a
; (car ''abracadabra) → (car (quote (quote abracadabra))) → quoto
