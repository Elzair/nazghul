;; ----------------------------------------------------------------------------
;; potions.scm -- potion object types. Potions work on the drinker.
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_potions 32 32 2 5 0 0 "potions.png")

(kern-mk-sprite 's_healing_potion       ss_potions 1 0 #f 0)
(kern-mk-sprite 's_mana_potion          ss_potions 1 1 #f 0)
(kern-mk-sprite 's_immunity_potion      ss_potions 1 2 #f 0)
(kern-mk-sprite 's_cure_potion          ss_potions 1 3 #f 0)
(kern-mk-sprite 's_invisibility_potion  ss_potions 1 4 #f 0)
(kern-mk-sprite 's_red_bubbly_potion    ss_potions 1 5 #f 0)
(kern-mk-sprite 's_green_bubbly_potion  ss_potions 1 6 #f 0)
(kern-mk-sprite 's_yellow_bubbly_potion ss_potions 1 7 #f 0)

;; mk-potion -- utility for making potion types
(define (mk-potion tag name sprite drink-proc)
  (mk-usable-item tag name sprite 1 drink-proc
                  (lambda (kpotion kuser)(drink-proc kpotion kuser))))

;; mk-clingy-potion -- utility for making potion types that automatically cause
;; npc's that want them to get them
(define (mk-clingy-potion tag name sprite drink-proc wants-it?)
  (mk-usable-clingy-item tag name sprite 1 drink-proc wants-it?))

;; healing (red) potion     
(mk-clingy-potion 't_heal_potion "healing potion" s_healing_potion 
                  (lambda (kpotion kuser)
                    (kern-obj-heal kuser (kern-dice-roll "2d10")))
                  wants-healing?)

;; mana (blue) potion
(mk-clingy-potion 't_mana_potion "mana potion" s_mana_potion 
                  (lambda (kpotion kuser)
                    (kern-char-dec-mana kuser (- 0 (kern-dice-roll "2d10"))))
                  wants-mana?)

;; cure (green) potion
(mk-potion 't_cure_potion "cure potion" s_cure_potion
           (lambda (kpotion kuser) 
             (kern-obj-remove-effect kuser ef_poison)))

;; posion immunity (bubbly yellow) potion
(mk-potion 't_poison_immunity_potion "immunity potion" s_immunity_potion
           (lambda (kpotion kuser) 
             (kern-obj-add-effect kuser ef_temporary_poison_immunity nil)))

;; invisibility (black) potion
(mk-potion 't_inv_potion "invisibility potion" s_invisibility_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect kuser ef_invisibility nil)))

;; FIXME: the following "blood" potions need to do stuff
(mk-potion 't_dragons_blood "dragon's blood"  s_red_bubbly_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect kuser ef_temporary_fire_immunity nil)))

;; hydra's blood -- turn arrows into poisoned arrows?
(mk-potion 't_hydras_blood "hydra's blood" s_green_bubbly_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect kuser ef_temporary_grow_head nil)))

;; lich's blood -- turn arrows into diseased arrows?
(mk-potion 't_lichs_blood "lich's blood" s_yellow_bubbly_potion
           (lambda (kpotion kuser)
               (kern-obj-add-effect kuser ef_temporary_disease_immunity nil)
               (kern-obj-add-effect kuser ef_temporary_poison_immunity nil)
               ))


