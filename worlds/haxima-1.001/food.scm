;; food.scm - edible types


(define food-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-obj-remove kobj)
                      (kern-obj-add-food getter
                                         ((kobj-ifc kobj) 'food-amount))
                      (kern-obj-destroy kobj)))))

 (define mushroom-ifc
   (ifc food-ifc
        (method 'food-amount (lambda () 10))))

(mk-obj-type 't_mushroom 
             "edible mushroom" 
             s_silver_mushroom 
             layer-item 
             mushroom-ifc)
