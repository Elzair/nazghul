;; ----------------------------------------------------------------------------
;; money.scm - items that convert to gold
;; ----------------------------------------------------------------------------

(define money-ifc
  (ifc '()
       (method 'amount (lambda () 1))
       (method 'get (lambda (kobj getter)
                      (kern-obj-remove kobj)
                      (kern-obj-add-gold getter
                                         (* ((kobj-ifc kobj) 'amount)
                                            (kern-obj-get-count kobj)))
                      (kern-obj-destroy kobj)))))

(mk-obj-type 't_gold_coins ;; tag
             "gold coin"  ;; name
             s_coins_gold  ;; sprite
             layer-item    ;; layer
             money-ifc      ;; ifc
             )
