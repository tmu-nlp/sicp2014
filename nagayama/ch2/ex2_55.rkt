#lang racket

; ' と quote 

; 'A = (quote A)
; であることに注意すれば自明
;
;   ''abracadabra
; = (quote (quote (abracadabra)))
; = '(quote (abracadabra))

; run
(car ''abracadabra)
(cdr ''abracadabra)
