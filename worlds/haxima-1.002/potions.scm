;; ----------------------------------------------------------------------------
;; potions.scm -- potion object types. Potions work on the drinker.
;; ----------------------------------------------------------------------------

(kern-mk-sprite-set 'ss_potions 32 32 1 8 0 0 "potions.png")

(kern-mk-sprite 's_healing_potion      ss_potions 1 0 #f 0)
(kern-mk-sprite 's_mana_potion         ss_potions 1 1 #f 0)
(kern-mk-sprite 's_immunity_potion     ss_potions 1 2 #f 0)
(kern-mk-sprite 's_cure_potion         ss_potions 1 3 #f 0)
(kern-mk-sprite 's_invisibility_potion ss_potions 1 4 #f 0)

;; mk-potion -- utility for making potion types
(define (mk-potion tag name sprite drink-proc)
  (mk-usable-item tag name sprite 1 drink-proc
                  (lambda (kpotion kuser)(drink-proc kpotion kuser))))
             
;; healing (red) potion     
(mk-potion 't_heal_potion "potion" s_healing_potion 
           (lambda (kpotion kuser)
             (kern-obj-heal kuser (kern-dice-roll "2d10"))))

;; mana (blue) potion
(mk-potion 't_mana_potion "potion" s_mana_potion 
           (lambda (kpotion kuser)
             (kern-char-dec-mana kuser (0 - (kern-dice-roll "2d10")))))

;; cure (green) potion
(mk-potion 't_cure_potion "potion" s_cure_potion
           (lambda (kpotion kuser) 
             (kern-obj-remove-effect kuser ef_poison)))

;; posion immunity (bubbly yellow) potion
(mk-potion 't_poison_immunity_potion "potion" s_immunity_potion
           (lambda (kpotion kuser) 
             (kern-obj-add-effect kuser ef_temporary_poison_immunity nil)))

;; invisibility (black) potion
(mk-potion 't_inv_potion "potion" s_invisibility_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect target ef_invisibility nil)))
