(use srfi-27)

(print (list 'a 'b 'c))
(print (list (list 'george)))
(print (car '((x1 x2) (y1 y2))))
(print (cadr '((x1 x2) (y1 y2))))
(print (pair? (car '(a short list))))
(print (memq 'red '((red shoes) (blue socks))))
(print (memq 'red '(red shoes blue socks)))
