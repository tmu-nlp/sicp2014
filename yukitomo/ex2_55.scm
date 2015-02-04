;ex2_55.scm

(display (car ''abracadabra))
(display "\n")
(display (cdr ''abracadabra))
(display "\n")
(display (car '(abracadabra)))
(display "\n")
(display (cdr '(abracadabra)))
(display "\n")
(display (quote (quote abracadabra)))

;gosh> ''abracadabra
;'abracadabra
;gosh> (car ''abracadabra)
;quote
;gosh> (cdr ''abracadabra)
;(abracadabra)

;上記のように''abracadabraは記号'abracadabraを示し、それに対して対のような扱いをすると'abracadabraは要素'と要素abracadabraの対になるっぽい

;''abracadabraは、
;gosh> (quote (quote abracadabra))
;'abracadabra
;と等しいので、
;gosh> (car (quote (quote abracadabra)))
;quote
;というようになる。