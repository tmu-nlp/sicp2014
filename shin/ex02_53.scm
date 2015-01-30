#!/usr/bin/gosh

#?=(list 'a 'b 'c)
#?=(list (list 'george))
#?=(cdr '((x1 x2) (y1 y2)))
;p139
#?=(cadr '((x1 x2) (y1 y2)))
#?=(pair? (car '(a short list)))
#?=(memq 'red '((red shoes) (blue socks)))
;再帰的に見てはいないので#F
#?=(memq 'red '(red shoes blue socks))
