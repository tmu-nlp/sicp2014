#lang racket

(car ''abracadabra)
"="
(car '(quote abracadabra))
"="
'quote

;; ''abracadabra は (quote (quote abracadabra)) に等しい.
;; (car (quote (quote abracadabra))) は (car '(quote abracadabra)) に等しい.
;; よって、(car ''abracadabra) は、quote 応答する.