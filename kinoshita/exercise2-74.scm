(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable
                    (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                            (assoc key-2 (cdr subtable))))
                        (if record (cdr record) #f))
                    #f)))
        (define (insert! key-1 key-2 value)
            (let ((subtable
                    (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record
                            (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1 (cons key-2 value))
                                    (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation: TABLE" m))))
        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;東京事業所のレコード構造
;(type-tag (record1) (record2) ...)
;(name address salary)

(define (install-tokyo-company-package)
    (define (make-employee-record name address salary)
        (list name address salary))
    (define (make-company-record record-list)
        (cons 'tokyo record-list))
    (define (name record) (car record))
    (define (address record) (cadr record))
    (define (salary record) (caddr record))
    (define (get-record company-record employee-name)
        (let ((records (cdr company-record)))
            (define (get-rcd records)
                (if (null? records)
                    #f
                    (let ((record (car records)))
                         (cond ((equal? employee-name
                                        (name record))
                                record)
                               (else (get-rcd (cdr records)))))))
            (get-rcd records)))

    (put 'get-name 'tokyo name)
    (put 'get-address 'tokyo address)
    (put 'get-salary 'tokyo salary)
    (put 'make-employee-record 'tokyo make-employee-record)
    (put 'make-company-record 'tokyo make-company-record)
    (put 'get-record 'tokyo get-record)
    'done)

(install-tokyo-company-package)

(define (make-tokyo-employee-record name address salary)
    ((get 'make-employee-record 'tokyo) name address salary))

(define tokyo-db
    ((get 'make-company-record 'tokyo)
     (list
        (make-tokyo-employee-record 'amami 'yokohama1-3 600000)
        (make-tokyo-employee-record 'shimamura 'nakano3-2 200000))))

(define (install-osaka-company-package)
    (define (make-employee-record name salary address)
        (list name salary address))
    (define (make-company-record company-address phone record-list)
        (cons 'osaka (cons '(company-address phone) record-list)))
    (define (name record) (car record))
    (define (salary record) (cadr record))
    (define (address record) (caddr record))
    (define (get-record company-record employee-name)
        (let ((records (cdr company-record)))
             (define (get-rcd records)
                (if (null? records)
                    #f
                    (let ((record (car records)))
                         (cond ((equal? employee-name
                                        (name record))
                                record)
                               (else (get-rcd (cdr records)))))))
             (get-rcd records)))

    (put 'get-name 'osaka name)
    (put 'get-salary 'osaka salary)
    (put 'get-address 'osaka address)
    (put 'make-employee-record 'osaka make-employee-record)
    (put 'make-company-record 'osaka make-company-record)
    (put 'get-record 'osaka get-record)
    'done)

(install-osaka-company-package)

(define (make-osaka-employee-record name salary address)
    ((get 'make-employee-record 'osaka) name salary address))
(define osaka-db
    ((get 'make-company-record 'osaka) 'umeda2-1 '06-0000-0000
     (list (make-osaka-employee-record 'junyo 300000 'namba2-1)
           (make-osaka-employee-record 'hiyo 250000 'tennou66-1))))

;a
;各事業所を識別するためのタイプタグを用意し，get-recordに対応した手続きを
;用意する．
(define (type-tag company-record) (car company-record))
(define (get-record company-record name)
    ((get 'get-record (type-tag company-record)) company-record name))
(print (get-record tokyo-db 'amami))

;b
(define (get-salary company-record name)
    (let ((employee-record (get-record company-record name)))
         (if employee-record
             ((get 'get-salary (type-tag company-record)) employee-record)
             (error "Not found data -- GET-SALARY:" name))))
(print (get-salary tokyo-db 'shimamura))

;c
(define (find-employee-record whole-record-list name)
    (if (null? whole-record-list)
        (error "Record not Found. -- FIND-EMPLOYEE-RECORD:" name)
        (let ((record-list (car whole-record-list)))
            (let ((record
                    ((get 'get-record (type-tag record-list))
                     record-list
                     name)))
                (if record
                    record
                    (find-employee-record (cdr whole-record-list) name))))))
(define wrl (list tokyo-db osaka-db))

(print (find-employee-record wrl 'shimamura))
(print (find-employee-record wrl 'hiyo))

;d
;新たに事業所のパッケージを追加する
