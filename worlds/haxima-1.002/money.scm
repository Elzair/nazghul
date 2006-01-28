;; ----------------------------------------------------------------------------
;; money.scm - items that convert to gold
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_money 32 32 1 1 0 0 "money.png")

(kern-mk-sprite 's_gold_coins ss_money 1 0 #f 0)

(define money-ifc
  (ifc '()
       (method 'amount (lambda () 1))
       (method 'get (lambda (kobj getter)
                      (kern-obj-inc-ref kobj)
                      (kern-obj-remove kobj)
                      (kern-obj-add-gold getter
                                         (* ((kobj-ifc kobj) 'amount)
                                            (kern-obj-get-count kobj)))
                      (kern-obj-dec-ref kobj)))))

(mk-obj-type 't_gold_coins "gold coin" s_gold_coins layer-item money-ifc)
