#lang racket

; 会社の社員データベースの実装する.
; 事業所によってデータベースの形式が異なっている.
;


 ; key     : 名前
 ; address : 住所
 ; salary  : 給料
 
 ; boxos データベース
 (define boxnos-db
   '((Tanaka (0  Kashiwa))
     (Suzuki (20 Matudo ))
     (Yamada (23 Kitasenju))))
 
 ; sizp データベース
 (define sicp-db
     '((Wada    Tokyo          2200)
       (Sassman Masasachusetts 2400)))



; #| put, get
(require r5rs)
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

; |#


#|

; [a]
; get-record : 指定された職員のデータを返す手続き
; グループを識別するための事業所名のようなものが必要.
; (このプログラムでは, 'boxnos, 'sicp を用いている)

; boxnos-package
(define (install-boxnos-package)
  (define db boxnos-db)
  (define (get-record name)
    (cons 'boxnos
          (assq name db)))
  (put 'get-record 'boxnos get-record))

(install-boxnos-package)

; sicp-package
(define (install-sicp-package)
  (define db sicp-db)
  (define (get-record name)
    (cons 'sicp
          (assq name db)))
  (put 'get-record 'sicp get-record))

(install-sicp-package)


(define (get-record file name)
  ((get 'get-record file) name))

; run
(get-record 'boxnos 'Tanaka) ; (boxnos Tanaka (0 Kashiwa))
(get-record 'sicp 'Sassman)  ; (sicp Sassman Masasachusetts 2400)

; |#


; #|

; [b]
; 指定された職員の特定のデータを取り出す手続きの設計
; get-salary : 名前を引数に取り, 給与を返す


; boxnos-package
(define (install-boxnos-package)
  (define db boxnos-db)

  (define name car)
  (define address cadadr)
  (define (salary r)  (* (caadr r) 12))

  (define (get-record name)
    (cons 'boxnos
          (assq name db)))

  (put 'get-name    'boxnos name)
  (put 'get-address 'boxnos address)
  (put 'get-salary  'boxnos salary)
  (put 'get-record  'boxnos get-record))

(install-boxnos-package)

; sicp-package
(define (install-sicp-package)
  (define db sicp-db)

  (define name car)
  (define address cadr)
  (define salary caddr)

  (define (get-record name)
    (cons 'sicp
          (assq name db)))

  (put 'get-name    'sicp name)
  (put 'get-address 'sicp address)
  (put 'get-salary  'sicp salary)
  (put 'get-record  'sicp get-record))

(install-sicp-package)


(define (get-package record) (car record))
(define (get-data record) (cdr record))

(define (get-record file name)
  ((get 'get-record file) name))

(define (get-info info record)
  ((get info (get-package record)) (get-data record)))
(define (get-name record) (get-info 'get-name record))
(define (get-address record) (get-info 'get-address record))
(define (get-salary record) (get-info 'get-salary record))

;; test
(define tanaka (get-record 'boxnos 'Tanaka)) ; (boxnos Tanaka (0 Kashiwa))
(define sassman (get-record 'sicp 'Sassman)) ; (sicp Sassman Masasachusetts 2400)

(display "name    : ") (get-name tanaka)    ; Tanaka
(display "address : ") (get-address tanaka) ; Kashiwa
(display "salary  : ") (get-salary tanaka)  ; 0
(newline)
(display "name    : ") (get-name sassman)    ; Sassman
(display "address : ") (get-address sassman) ; Masasachusetts
(display "salary  : ") (get-salary sassman)  ; 2400
(newline)

; |#


; #|

; [c]
; 
; find-employee-record : 社員名から

(define (find-employee-record employee lis)
  (define (iter name lst)
    (if (null? lst)
        (error "undefind name" name)
        (let ((record (get-record (car lst) name)))
          (if (cdr record)
              record
              (iter name (cdr lst))))))
  (for-each (lambda (x)
              (display " ")
              (display x)
              (newline))
             (iter employee lis)))

(find-employee-record 'Wada '(boxnos sicp)) ; (sicp Wada Tokyo 2200)

; |#


; [d]
; さらに会社を吸収した場合はどうすれば良いか.

; 同様にパッケージを追加すれば良い.

