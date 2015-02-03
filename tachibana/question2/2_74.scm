(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; それぞれの事業所をパッケージ化して抽象化できるようにする


; 事業所レコードの構造を次のようにする。
; 事業所レコードの構造(東京事業所)
; (type-tag (record1) (record2) ...)
; (tokyo (isono nakano1-2 150000) (nakajima setagaya2-1 250000) ...)

; 従業員レコードの構造(東京事業所)
; (name address salary)
; (isono nakano1-2 150000)
; (nakajima setagaya2-1 250000)

(define (install-tokyo-company-package)
  ;; 内部手続き
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
                    (cond ((equal? employee-name (name record)) record)
                          (else (get-rcd (cdr records)))))))
         (get-rcd records)))
  ;; 外部とのインターフェース
  (put 'get-name 'tokyo name)
  (put 'get-address 'tokyo address)
  (put 'get-salary 'tokyo salary)
  (put 'make-employee-record 'tokyo make-employee-record)
  (put 'make-company-record 'tokyo make-company-record)
  (put 'get-record 'tokyo get-record)
  'done)

(install-tokyo-company-package)

; 東京事業所のデータベースを作る
(define (make-tokyo-employee-record name address salary)
  ((get 'make-employee-record 'tokyo) name address salary))

(define tokyo-db
  ((get 'make-company-record 'tokyo)
   (list
     (make-tokyo-employee-record 'isono 'nakano1-2 150000)
     (make-tokyo-employee-record 'nakajima 'setagaya2-1 250000))))


(print "-----------------------------a------------------------------")

; a
(define (type-tag company-record) (car company-record))

(define (get-record company-record name)
  ((get 'get-record (type-tag company-record)) company-record name))

(print (get-record tokyo-db 'isono))

(print (get-record tokyo-db 'nakajima))

(print (get-record tokyo-db 'sakura))

(print "-----------------------------b------------------------------")


; b
(define (get-salary company-record name)
  (let ((employee-record (get-record company-record name)))
       (if employee-record
           ((get 'get-salary (type-tag company-record)) employee-record)
           #f)))

(print (get-salary tokyo-db 'isono))

(print (get-salary tokyo-db 'nakajima))

(print (get-salary tokyo-db 'sakura))

(define (get-address company-record name)
  (let ((employee-record (get-record company-record name)))
       (if employee-record
           ((get 'get-address (type-tag company-record)) employee-record)
           #f)))

(print (get-address tokyo-db 'isono))

(print (get-address tokyo-db 'nakajima))

(print (get-address tokyo-db 'sakura))

(print "---------------------------大阪のデータ--------------------------------")

; 大阪事業所レコードの構造を次のようにする。
; 事業所レコードの構造(大阪事業所)
; (type-tag (address phone) (record1) (record2) ...)
; (osaka ('nakanoshima2-1 06-5524-88xx) (fuguta 200000 kitaku2-1) (hanazawa 180000 sakai3-2) ...)

; 従業員レコードの構造(大阪事業所)
; (name salary address)
; (fuguta 200000 kitaku2-1)
; (hanazawa 180000 sakai3-2)

(define (install-osaka-company-package)
  ;; 内部手続き
  (define (make-employee-record name salary address)
    (list name salary address))
  (define (make-company-record record-list)
    (cons 'osaka record-list))
  (define (name record) (car record))
  (define (salary record) (cadr record))
  (define (address record) (caddr record))
  (define (get-record company-record employee-name)
    (let ((records (cdr company-record)))
         (define (get-rcd records)
           (if (null? records)
               #f
               (let ((record (car records)))
                    (cond ((equal? employee-name (name record)) record)
                          (else (get-rcd (cdr records)))))))
         (get-rcd records)))
  ;; 外部とのインターフェース
  (put 'get-name 'osaka name)
  (put 'get-salary 'osaka salary)
  (put 'get-address 'osaka address)
  (put 'make-employee-record 'osaka make-employee-record)
  (put 'make-company-record 'osaka make-company-record)
  (put 'get-record 'osaka get-record)
  'done)

(install-osaka-company-package)


(define (make-osaka-employee-recode name salary address)
  ((get 'make-employee-record 'osaka) name salary address))

(define osaka-db
  ((get 'make-company-record 'osaka)
   (list (make-osaka-employee-recode 'fuguta 200000 'kitaku2-1)
         (make-osaka-employee-recode 'hanazawa 180000 'sakai3-2))))

(print osaka-db)

(print (get-record osaka-db 'fuguta))

(print (get-address osaka-db 'fuguta))

(print (get-salary osaka-db 'hanazawa))

(print (get-record osaka-db 'ryoutu))

(print "-----------------------------c-----------------------------")

; c
; ただこれだと同じ名前の人が違う事業所にいたら対応できないので名前以外もキーにできないといけない。
(define (find-employee-record whole-record-list name)
  (if (null? whole-record-list)
      #f
      (let ((record-list (car whole-record-list)))
           (let ((record ((get 'get-record (type-tag record-list)) record-list name)))
                (if record
                    record
                    (find-employee-record (cdr whole-record-list) name))))))

(define wrl (list tokyo-db osaka-db))


(print (find-employee-record wrl 'isono))

(print (find-employee-record wrl 'nakajima))

(print (find-employee-record wrl 'fuguta))

(print (find-employee-record wrl 'hanazawa))


; d
; 別の会社の従業員情報をまたパッケージ化すればよい。
