(kern-mk-sprite-set 'ss_reagents 32 32 1 9 0 0 "reagents.png")

(kern-mk-sprite 's_spider_silk ss_reagents    1 0 #f 0)
(kern-mk-sprite 's_sulphorous_ash ss_reagents 1 1 #f 0)
(kern-mk-sprite 's_ginseng ss_reagents        1 2 #f 0)
(kern-mk-sprite 's_blood_moss ss_reagents     1 3 #f 0)
(kern-mk-sprite 's_garlic ss_reagents         1 4 #f 0)
(kern-mk-sprite 's_black_pearl ss_reagents    1 5 #f 0)
(kern-mk-sprite 's_nightshade ss_reagents     1 6 #f 0)
(kern-mk-sprite 's_mandrake ss_reagents       1 7 #f 0)
(kern-mk-sprite 's_royal_cape ss_reagents 1 8 #f 0)


;; Extend the basic object interface to support mixing. Currently mix does
;; nothing in the script, but it's important to let the kernel know that this
;; type of object can be used with the M)ix command.
(define reagent-ifc
  (ifc obj-ifc
       (method 'mix (lambda () '()))))

(define royal-cape-ifc
  (ifc reagent-ifc
       (method 'get (lambda (kobj getter)
		      (if (not (quest-data-complete? 'questentry-shroom))
			  (begin
			    (kobj-get kobj getter)
			    (quest-data-update-with 'questentry-shroom 'found 1 
						    (quest-notify (grant-party-xp-fn 10)))))))))

(define (mk-reagent-type tag name sprite)
  (mk-obj-type tag name sprite layer-item reagent-ifc))

(mk-reagent-type 'sulphorous_ash "sulphurous ash" s_sulphorous_ash)
(mk-reagent-type 'ginseng        "ginseng"        s_ginseng)
(mk-reagent-type 'garlic         "garlic"         s_garlic)
(mk-reagent-type 'spider_silk    "spider silk"    s_spider_silk)
(mk-reagent-type 'blood_moss     "blood moss"     s_blood_moss)
(mk-reagent-type 'black_pearl    "black pearl"    s_black_pearl)
(mk-reagent-type 'nightshade     "nightshade"     s_nightshade)
(mk-reagent-type 'mandrake       "mandrake"       s_mandrake)

(mk-obj-type 't_royal_cape  "royal cape mushroom" s_royal_cape layer-item royal-cape-ifc)
