;;----------------------------------------------------------------------------
;; Containers - objects that contain stuff
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------
(define (add-content quantity type)
  (list quantity type))

(define (mk-contents . contents)
  (filter notnull? contents))

(define (roll-100 prob)
  (>= prob (modulo (random-next) 100)))

(define (roll-q dice type)
  (add-content (kern-dice-roll dice) type))

(define (roll-to-add prob dice type)
  (if (roll-100 prob)
      (roll-q dice type)
      nil))

;;----------------------------------------------------------------------------
;; Container Types
;;----------------------------------------------------------------------------
(mk-obj-type 't_small_wooden_chest 
             "small wooden chest"  s_chest_small_wooden_closed
             layer-container nil)

(mk-obj-type 't_large_wooden_chest 
             "large wooden chest"  s_chest_large_wooden_closed
             layer-container nil)

(mk-obj-type 't_small_iron_chest 
             "small iron chest"   s_chest_small_metal_1_closed
             layer-container nil)

(mk-obj-type 't_large_iron_chest 
             "large iron chest"   s_chest_large_metal_1_closed
             layer-container nil)

;; Trick: make a "troll corpse" container type and use it as the troll's
;; container. When the troll dies the kernel will drop the troll's container,
;; making it look like the troll corpse is a container.
(mk-obj-type 'troll-corpse-type "troll corpse" s_troll_corpse
             layer-container nil)

;;----------------------------------------------------------------------------
;; Container Constructors
;;----------------------------------------------------------------------------
(define (mk-small-wooden-chest trap contents)
  (kern-mk-container t_small_wooden_chest trap contents))

(define (mk-large-iron-chest trap contents)
  (kern-mk-container t_large_iron_chest trap contents))
