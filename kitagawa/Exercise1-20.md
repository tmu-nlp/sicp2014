Exercise 1.20
=====================

解法
-------

以下の与えられた式を評価した場合の展開式を考えていく.

.. sourcecode:: scheme

   (define (gcd a b)
       (if (= b 0)
           a
           (gcd b (remainder a b))))

   (print (gcd 28 16))
   ;; => 4

---------------------
正規順序評価の場合
---------------------

順に評価を見ていく.

.. sourcecode:: scheme

   (gcd 206 40)

.. sourcecode:: scheme

   (if (= 40 0) ;; => #f
       206
       (gcd 40 (remainder 206 40))) ;; => (1)

.. sourcecode:: scheme

   ; (1)
   ; if 文の評価で remainder 使用累計 1 回
   (if (= (remainder 206 40) 0) ;; => #f
       40
       (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))) ;; => (2)

.. sourcecode:: scheme

   ; (2)
   ; if 文の評価で remainder 使用累計 2 回
   (if (= (remainder 40 (remainder 206 40)) 0) ;; => #f, (= 4 0)
       (remainder 206 40)
       (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ;; => (3)

.. sourcecode:: scheme
   
   ; (3)
   ; if 文の評価で remainder 使用累計 4 回
   (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ;; => #f, (= 2 0)
       (remainder 40 (remainder 206 40))
       (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) ;; => (4)

.. sourcecode:: scheme
   
   ; (4)
   ; if 文の評価で remainder 使用累計 7 回
   (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ;; => #t, (= 0 0)
       (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ;; => (5)
       (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

if 文の評価で remainder は 1 + 2 + 4 + 7 = 14 回使用され, 最終的に評価される述部(5) に 4 回使用されるので, 計 18 回となる.

---------------------
作用順序評価の場合
---------------------

順に評価を見ていく.

.. sourcecode:: scheme

   ; (1)
   ; if 文の評価で remainder 使用累計 1 回
 
   (gcd 206 40)
   
   ; remainder 使用累計 0 回
   (if (= 40 0) ;; => #f
       206
       (gcd 40 (remainder 206 40)))
   
   ; remainder 使用累計 1 回
   (if (= 6 0) ;; => #f
       40
       (gcd 6 (remainder 40 6)))

   ; remainder 使用累計 2 回
   (if (= 4 0) ;; => #f
       6
       (gcd 4 (remainder 6 4)))
   
   ; remainder 使用累計 3 回
   (if (= 2 0) ;; => #f
       4
       (gcd 2 (remainder 4 2)))
   
   ; remainder 使用累計 4 回
   (if (= 0 0) ;; => #t
       2
       (gcd 0 (remainder 2 0)))

計 4 回となる.