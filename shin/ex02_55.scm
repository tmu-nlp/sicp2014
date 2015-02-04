#!/usr/bin/gosh

#?=''abracadabra
#?=(car ''abracadabra)
;quote
#?=(cdr ''abracadabra)
;(abracadabra)
;「''abracadabra」は記号「'abracadabra」を示し、
;carとcdrにより，「'」と「abracadabra」の２つの要素になった

#?=(quote (quote abracadabra))
#?=(car (quote (quote abracadabra)))
