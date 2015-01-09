(define (for-each func list1)
    (if (null? (cdr list1))
        (func (car list1))
        (begin
            (func (car list1))
            (for-each func (cdr list1)))))

(for-each
    (lambda (x) (newline) (display x))
    (list 57 321 88))
