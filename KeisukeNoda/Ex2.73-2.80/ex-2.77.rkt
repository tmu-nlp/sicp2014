#lang racket

(require "public.rkt")

before fix the code substitution looks like this:

(magnitude z)
(apply-generic 'magnitude z)
(type-tag z) -> complex
(get 'magnitude 'complex) -> null
as result == null -> error !

after fix:
(magnitude z)
(apply-generic 'magnitude z)
(type-tag z) -> complex
(get 'magnitude 'complex) -> magnitude
(apply magnitude (contents z))
(apply-generic 'magnitude (contents z))
(type-tag (contents z)) -> rectangular
(get 'magnitude 'rectangular) -> magnitude from install-rectangular-package (mag2)
(apply mag2 (contents (contents z)))
-> get result!
