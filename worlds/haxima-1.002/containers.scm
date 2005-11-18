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
(mk-obj-type 't_chest 
             "chest"  s_chest
             layer-container nil)

;;----------------------------------------------------------------------------
;; Container Constructors
;;----------------------------------------------------------------------------
(define (mk-chest trap contents)
  ;;(println "mk-chest: " trap contents)
  (kern-mk-container t_chest trap contents))

;;----------------------------------------------------------------------------
;; Corpse -- not really a container, if you search it then it sort of acts like
;; opening a container
;;----------------------------------------------------------------------------
(define (corpse-mk loot)
  (list loot))
(define (corpse-loot corpse) (car corpse))
(define (corpse-set-loot! corpse val) (set-car! corpse val))

(define (corpse-search kobj)
  (let* ((corpse (kobj-gob-data kobj))
         (loot (corpse-loot corpse)))
    (display "corpse-search:")(display loot)(newline)
    (if (not (null? loot))
        (let ((loc (kern-obj-get-location kobj)))
          (map (lambda (type-q) 
                 (kern-obj-put-at (kern-mk-obj (eval (car type-q))
                                               (cadr type-q))
                                  loc))
               loot)
          (corpse-set-loot! corpse nil)))))

(define corpse-ifc
  (ifc nil
       (method 'search corpse-search)))

(mk-obj-type 't_corpse "corpse" s_corpse layer-item corpse-ifc)

(define (mk-corpse) 
  (bind (kern-mk-obj t_corpse 1)
        (corpse-mk nil)))

(define (mk-corpse2 loot)
  (bind (kern-mk-obj t_corpse 1)
        (corpse-mk loot)))
