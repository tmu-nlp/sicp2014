(use srfi-27)

(print (car ''abracadabra))
(car (quote (quote abracadabra)))
;二番目のquoteを表示する

