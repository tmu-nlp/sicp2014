;;; ハッシュテーブルを使い put, get を実装
;;; キーとして演算と型のリストを利用する
(define table (make-hash-table 'equal?))

(define (put op type item)
  (hash-table-put! table (list op type) item))

(define (get op type)
  (hash-table-get table (list op type)))


;;; 東京事業所は社員情報として、address, salary を持つ
(define (install-tokyo-division-package)
    (define (employee-records file)
    (cdr file))

  (define (employee-name record)
    (car record))

  (define (employee-salary record)
    (caddr record))

  (define (get-record file name)
    (define (find records)
      (cond ((null? records) #f)
            ((equal? (employee-name (car records)) name)
             (car records))
            (else (find (cdr records)))))
    (find (employee-records file)))

  (define (get-salary file name)
    (employee-salary (get-record file name)))

  (put 'get-record 'tokyo get-record)
  (put 'get-salary 'tokyo get-salary)
  'done)


;;; 大阪事業所は社員情報として、address, salary, age を持つ
(define (install-osaka-division-package)
  (define (employee-records file)
    (cdr file))

  (define (employee-name record)
    (car record))

  (define (employee-salary record)
    (cadddr record))

  (define (get-record file name)
    (define (find records)
      (cond ((null? records) #f)
            ((equal? (employee-name (car records)) name)
             (car records))
            (else (find (cdr records)))))
    (find (employee-records file)))

  (define (get-salary file name)
    (employee-salary (get-record file name)))

  (put 'get-record 'osaka get-record)
  (put 'get-salary 'osaka get-salary)

  'done)

;;; パッケージインストール
(install-tokyo-division-package)
(install-osaka-division-package)

;;; 東京事業所のファイル(データベース)
(define tokyo-file
  (list 'tokyo
        (list "suzuki" "東京都新宿区" 200000)
        (list "sato" "東京都渋谷区" 300000)
        (list "yoshida" "東京都三鷹市" 400000)))

;;; 大阪事業所のファイル(データベース)
(define osaka-file
  (list 'osaka
        (list "tanaka" "大阪府池田市" 28 250000)
        (list "sato" "大阪府堺市" 33 320000)))

;;; a --------------------------------------------------------------------------
;;; それぞれの事業所ファイルに事業所を表す型タグを持たせる必要がある。
;;; また、自身の表現するレコードにアクセスする内部関数を持つ必要がある。
(define (division file)
  (car file))

(define (get-record file name)
  ((get 'get-record (division file)) file name))

;;; 使用例
(get-record tokyo-file "suzuki")        ; ("suzuki" "東京都新宿区" 200000)
(get-record tokyo-file "sato")          ; ("sato" "東京都渋谷区" 300000)
(get-record osaka-file "sato")          ; ("sato" "大阪府堺市" 33 320000)


;;; b --------------------------------------------------------------------------
;;; どの事業所の従業員レコードにも salary を持たせておき、
;;; それにアクセスできる関数を用意しておく。
(define (get-salary file name)
  ((get 'get-salary (division file)) file name))

;;; 使用例
(get-salary tokyo-file "suzuki")        ; 200000
(get-salary tokyo-file "yoshida")       ; 400000
(get-salary osaka-file "tanaka")        ; 250000


;;; c --------------------------------------------------------------------------
(define (find-employee-record files name)
  (if (null? files)
      '()
      (let ((record (get-record (car files) name)))
        (if record
            (cons record (find-employee-record (cdr files) name))
            (find-employee-record (cdr files) name)))))

;;; 使用例
(find-employee-record (list tokyo-file osaka-file) "suzuki")
;; (("suzuki" "東京都新宿区" 200000))

(find-employee-record (list tokyo-file osaka-file) "sato")
;; (("sato" "東京都渋谷区" 300000) ("sato" "大阪府堺市" 33 320000))


;;; d --------------------------------------------------------------------------
;;; 別会社の事業部毎に install-tokyo-package, install-osaka-package と同様
;;; の手続きを作成して実行すれば良い。
