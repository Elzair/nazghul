;; food.scm - edible types


(define food-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-obj-remove kobj)
                      (kern-obj-add-food getter
                                         (* ((kobj-ifc kobj) 'food-amount)
                                            (kern-obj-get-count kobj)))
                      (kern-obj-destroy kobj)))))

 (define mushroom-ifc
   (ifc food-ifc
        (method 'food-amount (lambda () 10))))

(mk-obj-type 't_mushroom "edible mushroom" s_dg_mushroom_white 
             layer-item mushroom-ifc)
