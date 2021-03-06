; 2 つの区間の中央値 a, b 、相対許容誤差を p1, p2 とすると、2 つの区間は、
; ( a - a * p1, a + a * p1), ( b - b * p2, b + b * p2)
; 2 つの区間の下限がどちらも正だとするとその積は、
; ( ab - ( p1 + p2 ) ab + p1p2ab, ab + (p1 + p2) ab + p1p2ab )
; となり、p1, p2 が小さいとき、上限下限とも p1p2ab の項は無視できるので、2 つの区間の積は
; ( ab - ( p1 + p2 ) ab, ab + ( p1 + p2 ) ab )
; で近似できる。従って、2 つの区間の相対許容誤差は、p1 + p2 で近似できる。
