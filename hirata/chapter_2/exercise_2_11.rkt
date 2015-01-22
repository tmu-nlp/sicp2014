(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
       (cond ((> lbx 0) ; x が正
              (cond ((> lby 0) ; y が正
                     (make-interval (* lbx lby)
                                    (* ubx uby)))
                    ((< uby 0) ; y が負
                     (make-interval (* ubx lby)
                                    (* lbx uby)))
                    (else ; y が零を跨る
                      (make-interval (* ubx lby)
                                     (* ubx uby)))))
             ((< ubx 0) ; x が負
              (cond ((> lby 0) ; y が正
                     (make-interval (* lbx uby)
                                    (* ubx lby)))
                    ((< uby 0) ; y が負
                     (make-interval (* ubx uby)
                                    (* lbx lby)))
                    (else ; y が零を跨る
                      (make-interval (* lbx uby)
                                     (* lbx lby)))))
             (else ; x が零を跨る
               (cond ((> lby 0) ; y が正
                      (make-interval (* lbx uby)
                                     (* ubx uby)))
                     ((< uby 0) ; y が負
                      (make-interval (* ubx lby)
                                     (* lbx lby)))
                     (else ; y が零を跨る
                       (make-interval (min (* lbx uby) (* ubx lby))
                                      (max (* lbx lby) (* ubx uby)))))))))
