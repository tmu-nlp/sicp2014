
追加する前のコードでは以下のようになる。

(magnitude z)
(apply-generic 'magnitude z)
(type-tag z) -> complex
(get 'magnitude 'complex) -> null
as result == null -> error !

追加した後では、

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
