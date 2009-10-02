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
(kern-mk-sprite 's_round_bubbly_purple  ss_potions 1 10 #f 0)
(kern-mk-sprite 's_round_bubbly_lblue   ss_potions 1 11 #f 0)
(kern-mk-sprite 's_round_bubbly_yellow  ss_potions 1 12 #f 0)

;; mk-potion -- utility for making potion types. 'drink-proc' should return one
;; of the result-* codes.
(define (mk-potion tag name sprite drink-proc)
  (mk-usable-item tag name sprite norm drink-proc
                  (lambda (kpotion kuser) 
                    (drink-proc kpotion kuser))))

;; mk-clingy-potion -- utility for making potion types that automatically cause
;; npc's that want them to get them. 'drink-proc' should return one of the
;; result-* codes.
(define (mk-clingy-potion tag name sprite drink-proc wants-it?)
  (mk-usable-clingy-item tag name sprite norm drink-proc wants-it?))

;; healing (red) potion     
(mk-clingy-potion 't_heal_potion "healing potion" s_healing_potion 
                  (lambda (kpotion kuser)
                    (kern-obj-heal kuser (kern-dice-roll "2d10"))
                    result-ok)
                  wants-healing?)

;; mana (blue) potion
(mk-clingy-potion 't_mana_potion "mana potion" s_mana_potion 
                  (lambda (kpotion kuser)
                    (kern-char-dec-mana kuser (- 0 (kern-dice-roll "1d8+2")))
                    result-ok)
                  wants-mana?)

;; cure (green) potion
(mk-potion 't_cure_potion "cure potion" s_cure_potion
           (lambda (kpotion kuser) 
             (kern-obj-remove-effect kuser ef_poison)
             result-ok))
			 
(mk-potion 't_xp_potion "potion of gain level" s_cure_potion
           (lambda (kpotion kuser) 
             (kern-char-add-experience kuser 500)
             result-ok))

(define (potion-gain-stats kuser current-stat stat-name stat-setter)
  (println "cur:" current-stat)
  (cond ((< current-stat 20)
         (kern-log-msg (kern-obj-get-name kuser) " gains " stat-name "!")
         (stat-setter kuser (+ current-stat (kern-dice-roll "1d3+1")))
         result-ok)
        ((< current-stat 25)
         (kern-log-msg (kern-obj-get-name kuser) " gains a little " stat-name "!")
         (stat-setter kuser (+ current-stat (kern-dice-roll "1d3")))
         result-ok)
        ((< current-stat 35)
         (let ((droll (kern-dice-roll "1d2-1")))
           (println "droll:" droll)
           (cond ((> droll 0)
                  (kern-log-msg (kern-obj-get-name kuser) " already has a lot of " stat-name ", but gets a wee bit more.")
                  (stat-setter kuser (+ current-stat 1))
                  result-ok)
                 (else
                  (kern-log-msg (kern-obj-get-name kuser) " already has a lot of " stat-name " and now just feels a little sick.")
                  result-no-effect))))
        (else
         (kern-log-msg (kern-obj-get-name kuser) " has too much " stat-name " and has become such an arrogant bore that potions have no more effect.")
         result-no-effect)))

(mk-potion 't_str_potion "potion of strength" s_round_bubbly_yellow
		(lambda (kpotion kuser)
			(potion-gain-stats kuser (kern-char-get-base-strength kuser)
                                           "strength" kern-char-set-strength)))
			 
(mk-potion 't_dex_potion "potion of dexterity" s_round_bubbly_purple
		(lambda (kpotion kuser)
			(potion-gain-stats kuser (kern-char-get-base-dexterity kuser)
                                           "dexterity" kern-char-set-dexterity)))
			 
(mk-potion 't_int_potion "potion of intelligence" s_round_bubbly_lblue
		(lambda (kpotion kuser)
			(potion-gain-stats kuser (kern-char-get-base-intelligence kuser)
				"intelligence" kern-char-set-intelligence)))

(mk-potion 't_info_potion "potion of enlightenment" s_mana_potion
           (lambda (kpotion kuser) 
            (kern-log-msg "Information about " (kern-obj-get-name kuser))
			(kern-log-msg "Thief skill: " (number->string (occ-ability-thief kuser)))
			(kern-log-msg "Offensive magic: " (number->string (occ-ability-blackmagic kuser)))
			(kern-log-msg "Utility magic: " (number->string (occ-ability-whitemagic kuser)))
			(kern-log-msg "Magic resistance: " (number->string (occ-ability-magicdef kuser)))
			(kern-log-msg "Combat strength: " (number->string (occ-ability-strattack kuser)))
			(kern-log-msg "Combat dexterity: " (number->string (occ-ability-dexattack kuser)))
			(kern-log-msg "Avoidance: " (number->string (occ-ability-dexdefend kuser)))
			result-ok))


;; posion immunity (bubbly yellow) potion
(mk-potion 't_poison_immunity_potion "immunity potion" s_immunity_potion
           (lambda (kpotion kuser) 
             (kern-obj-add-effect kuser ef_temporary_poison_immunity nil)
             result-ok))

;; invisibility (black) potion
(mk-potion 't_invisibility_potion "invisibility potion" s_invisibility_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect kuser ef_invisibility nil)
             result-ok))

;; FIXME: the following "blood" potions need to do stuff
(mk-potion 't_dragons_blood "dragon's blood"  s_red_bubbly_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect kuser ef_temporary_fire_immunity nil)
             result-ok))

;; hydra's blood -- turn arrows into poisoned arrows?
(mk-potion 't_hydras_blood "hydra's blood" s_green_bubbly_potion
           (lambda (kpotion kuser)
             (kern-obj-add-effect kuser ef_temporary_grow_head nil)
             result-ok))

;; lich's blood -- turn arrows into diseased arrows?
(mk-potion 't_lichs_blood "lich's blood" s_yellow_bubbly_potion
           (lambda (kpotion kuser)
               (kern-obj-add-effect kuser ef_temporary_disease_immunity nil)
               (kern-obj-add-effect kuser ef_temporary_poison_immunity nil)
               result-ok))


