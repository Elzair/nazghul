(kern-mk-sprite-set 'ss_food 32 32 2 2 0 0 "food.png")

(kern-mk-sprite 's_food ss_food 1 0 #f 0)
(kern-mk-sprite 's_beer ss_food 1 1 #f 0)
(kern-mk-sprite 's_wine ss_food 1 2 #f 0)

;; caloric values
(define food-value 1)
(define alcohol-value 1)

(define food-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-obj-inc-ref kobj)
                      (kern-obj-remove kobj)
                      (kern-obj-add-food getter (* food-value
                                                   (kern-obj-get-count kobj)))
                      (kern-obj-dec-ref kobj)))
       (method 'buy (lambda (kbuyer q)
                      (kern-obj-add-food kbuyer (* food-value q))
                      ))
       ))

(define alcohol-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-obj-inc-ref kobj)
                      (kern-obj-remove kobj)
                      (kern-obj-add-food getter (* alcohol-value
                                                   (kern-obj-get-count kobj)))
                      (kern-obj-add-effect getter ef_drunk nil)
                      (kern-obj-dec-ref kobj)))
       (method 'buy (lambda (kbuyer q)
                      (display "buy")(newline)
                      (kern-obj-add-effect kbuyer ef_drunk nil)
                      (kern-obj-add-food kbuyer (* alcohol-value q))))
       ))


(mk-obj-type 't_food (list "food" "food") s_food layer-item food-ifc)
(mk-obj-type 't_beer "beer" s_beer layer-item alcohol-ifc)
(mk-obj-type 't_wine (list "wine" "wine") s_wine layer-item alcohol-ifc)
