(kern-mk-sprite-set 'ss_food 32 32 1 1 0 0 "food.png")

(kern-mk-sprite 's_food ss_food 1 0 #f 0)

(define food-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-obj-inc-ref kobj)
                      (kern-obj-remove kobj)
                      (kern-obj-add-food getter (* 10 (kern-obj-get-count kobj)))
                      (kern-obj-dec-ref kobj)))))

(mk-obj-type 't_food "food" s_food layer-item food-ifc)
