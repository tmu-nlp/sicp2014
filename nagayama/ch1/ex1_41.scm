#lang scheme

; 手続きを 2回適用する手続き double を定義する.
; 例題の処理順を書き出してみたがややこしい


; 前提関数
(define (inc x) (+ x 1))

; double
(define (double f)
  (lambda (x) (f (f x))))

; run
(((double (double double)) inc) 5)  ; -> 21

; 
; (((double (double double)) inc) 5) の処理フロー
; 
; < 処理順序 >
; - 適用順評価なので, 最上位の()から展開していく
; - 複数候補があるならば左から優先
; - 展開できなければ 1つ下位の()を調べる
; - 1度 展開したら最上位の()から調べる
; 
; < 基本の形 >
;  ((double f) x)  -> (f (f x))
;  ((double double) x) -> (double (double x))
; 
; 
; < 処理フロー >
; 
;  1. ( ((double (double double)) inc) 5)
;           |    \-------------/   |
;         double        f          x
;  
;  2. ( ( (double double) ((double double) inc) ) 5)
;                             |      |      |
;                           double   f      x
; 
;   3. ( ( (double double) (double (double inc)) ) 5)
;            |      |     \-------------------/
;          double   f                x
; 
;  4. ( ( double (double (double (double inc))) ) 5)
;           |    \----------------------------/   |
;         double               f                  x
;
;  5. ( (double (double (double inc))) ( (double (double (double inc)))  5 ))
;                                           |    \-------------------/   |
;                                         double           f             x
; 
;  6. ( (double (double (double inc))) ( (double (double inc)) ( (double (double inc)) 5 )))
;                                                                   |    \----------/  |
;                                                                 double       f       x
; 
;  7. ( (double (double (double inc))) ( (double (double inc)) ( (double inc) ((double inc) 5 ))))
;                                                                                 |     |   |
;                                                                               double  f   x
; 
;  8. ( (double (double (double inc))) ( (double (double inc)) ( (double inc) (inc (inc 5 )))))
;                                                                   |     |   \------------/
;                                                                 double  f          x
; 
;  9. ( (double (double (double inc))) ( (double (double inc)) (inc (inc (inc (inc 5 ))))))
;                                           |    \----------/  \-------------------------/
;                                         double      f                     x
; 
; 10. ( (double (double (double inc))) ( (double inc) ( (double inc) (inc (inc (inc (inc 5 )))))))
;                                                          |     |   \-------------------------/
;                                                        double  f                x
;   
; 11. ( (double (double (double inc))) ( (double inc) (inc (inc (inc (inc (inc (inc 5 ))))))))
;                                           |     |   \-------------------------------------/
;                                         double  f                x
; 
; 12. ( (double (double (double inc))) (inc (inc (inc (inc (inc (inc (inc (inc 5 )))))))))
;          |    \-------------------/  \------------------------------------------------/
;        double           f                                   x
; 
; 13. ( (double (double inc)) ( (double (double inc)) (inc (inc (inc (inc (inc (inc (inc (inc 5 ))))))))))
;                                  |    \----------/  \------------------------------------------------/
;                                double       f                               x
;
; 14. ( (double (double inc)) ( (double inc) ((double inc) (inc (inc (inc (inc (inc (inc (inc (inc 5 )))))))))))
;                                                |     |   \------------------------------------------------/   
;                                              double  f                           x
; 
; 15. ( (double (double inc)) ( (double inc) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5 ))))))))))))
;                                  |     |   \------------------------------------------------------------/
;                                double inc                                x
; 
; 16. ( (double (double inc)) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5 )))))))))))))
;          |    \----------/  \------------------------------------------------------------------------/
;        double       f                                        x
; 
; 17. ( (double inc) ((double inc) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5 ))))))))))))))
;                        |     |   \------------------------------------------------------------------------/
;                      double  f                                        x
; 
; 18. ( (double inc) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5 )))))))))))))))
;          |     |   \------------------------------------------------------------------------------------/
;        double  f                                        x
; 
; 19. (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5 ))))))))))))))))
;     \-----------------------------------------------------------------------------/ |
;                                  inc * 16                                           5
; 
; 20. 21
; 








