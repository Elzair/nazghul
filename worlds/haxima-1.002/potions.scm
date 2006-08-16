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
                    (kern-obj-heal kuser (kern-dice-roll "2d10"))
                    #t)
                  wants-healing?)

;; mana (blue) potion
(mk-clingy-potion 't_mana_potion "mana potion" s_mana_potion 
                  (lambda (kpotion kuser)
                    (kern-char-dec-mana kuser (- 0 (kern-dice-roll "1d8+2")))
                    #t)
                  wants-mana?)

;; cure (green) potion
(mk-potion 't_cure_potion "cure potion" s_cure_potion
           (lambda (kpotion kuser) 
             (kern-obj-remove-effect kuser ef_poison)))
			 
(mk-potion 't_xp_potion "potion of gain level" s_cure_potion
           (lambda (kpotion kuser) 
             (kern-char-add-experience kuser 500)
			 #t))

(define (potion-gain-stats kuser current-stat stat-name stat-setter)
	(let ((total-stats (+ (kern-char-get-base-strength kuser)
				(kern-char-get-base-dexterity kuser)
				(kern-char-get-base-intelligence kuser))))
		(kern-log-msg "Total stats: " total-stats)
		(if (> (kern-dice-roll "1d30") total-stats)
			(begin (kern-log-msg (kern-obj-get-name kuser) " gains " stat-name "!")
				(println stat-setter)
				(stat-setter kuser (+ current-stat 1))
				(println stat-setter)
				)
			(kern-log-msg "No effect")
			)
		#t
	))

(mk-potion 't_str_potion "potion of strength" s_healing_potion
		(lambda (kpotion kuser)
			(potion-gain-stats kuser (kern-char-get-base-strength kuser)
				"strength" kern-char-set-strength)
		))
			 
(mk-potion 't_dex_potion "potion of dexterity" s_immunity_potion
		(lambda (kpotion kuser)
			(potion-gain-stats kuser (kern-char-get-base-dexterity kuser)
				"dexterity" kern-char-set-dexterity)
		))
			 
(mk-potion 't_int_potion "potion of intelligence" s_mana_potion
		(lambda (kpotion kuser)
			(potion-gain-stats kuser (kern-char-get-base-intelligence kuser)
				"intelligence" kern-char-set-intelligence)
		))

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
			#t))


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


