;;----------------------------------------------------------------------------
;; NPC type constructors
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Local Constants
;;----------------------------------------------------------------------------
(define default-level 1)

;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------

;; mk-stock-char -- convenience wrapper for kern-mk-char. Handles the
;; boilerplate associated with first-time "stock" character creations. A stock
;; character is a monster, guard or similar cannon-fodder NPC, with no
;; interesting conversation, no schedule of appointments, etc.
(define (mk-stock-char name species occupation sprite faction ai container 
                       arms conv)
  (kern-mk-char
   nil ;;..........tag
   name ;;.........name
   species ;;.......species
   occupation ;;...occupation
   sprite ;;.......sprite
   faction ;;....;.faction
   0 ;;............custom strength modifier
   0 ;;............custom intelligence modifier
   0 ;;............custom dexterity modifier
   0 ;;............custom base hp modifier
   0 ;;............custom hp multiplier (per-level)
   0 ;;............custom base mp modifier
   0 ;;............custom mp multiplier (per-level)
   (max-hp species occupation default-level 0 0) ;;.current hit points
   -1  ;;...........current experience points
   (max-mp species occupation default-level 0 0) ;;.current magic points
   0  ;; AP_per_turn
   default-level  ;;............current level
   #f ;;...........dead?
   conv ;;.........conversation (optional)
   nil ;;..........schedule (optional)
   ai ;;...........custom ai (optional)
   container ;;....container (and contents)
   arms ;;.........readied arms (in addition to the container contents)
   nil ;;..........hooks in effect
   ))

;; Curried version of mk-stock-char for characters without an occupation, ai,
;; container or armamenets
(define (mk-animal name species sprite)
  (mk-stock-char name species nil sprite faction-none nil nil nil nil))

(define (mk-readied-items . items)
  items)

(define (mk-at-level ctor-tag lvl-dice . args)
  (set-level (apply (eval ctor-tag) args) 
             (kern-dice-roll lvl-dice)))

;; npct -- NPC type
(define (mk-npct2 name spec occ spr traps equip eff ai faction conv drop-fx drop-fx-parms)
  (list name spec occ spr traps equip eff ai faction conv drop-fx drop-fx-parms))
(define (mk-npct name spec occ spr traps equip eff ai faction conv)
  (mk-npct2 name spec occ spr traps equip eff ai faction conv nil nil))
(define (npct-name npct) (car npct))
(define (npct-spec npct) (cadr npct))
(define (npct-occ npct) (caddr npct))
(define (npct-spr npct) (cadddr npct))
(define (npct-traps npct) (list-ref npct 4))
(define (npct-eqp npct) (list-ref npct 5))
(define (npct-effects npct) (list-ref npct 6))
(define (npct-ai npct) (list-ref npct 7))
(define (npct-faction npct) (list-ref npct 8))
(define (npct-conv npct) (list-ref npct 9))
(define (npct-drop-fx npct) (list-ref npct 10))
(define (npct-drop-fx-parms npct) (list-ref npct 11))



;; npcg -- generic NPC gob
(define (npcg-mk type) 
  (list 'npcg 
        type 
        #f  ;; taunted
        #f  ;; spawned
        nil ;; post
        nil ;; subgob
        ))
(define (npcg-type npcg) (cadr npcg))
(define (npcg-taunted? npcg) (caddr npcg))
(define (npcg-spawned? npcg) (cadddr npcg))
(define (npcg-is-type? npcg type) (equal? type (npcg-type npcg)))
(define (npcg-set-taunted! npcg val) (set-car! (cddr npcg) val))
(define (npcg-set-spawned! npcg val) (set-car! (cdddr npcg) val))
(define (npcg-set-post! npcg val) (set-car! (list-tail npcg 4) val))
(define (npcg-has-post? npcg) (not (null? (npcg-get-post npcg))))
(define (npcg-get-post npcg) (list-ref npcg 4))
(define (npcg-get-subgob npcg) (list-ref npcg 5))
(define (npcg-set-subgob! npcg val) (set-car! (list-tail npcg 5) val))
(define (is-npcg? gob) (eq? (car gob) 'npcg))

(define (kbeing-is-npc-type? kbeing type)
  (let ((npcg (gob kbeing)))
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-is-type? npcg type))))

(define (kbeing-was-spawned? kbeing)
  (let ((npcg (gob kbeing)))
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-spawned? npcg))))

;; An NPC inventory is kind of an abstract container. It should never appear on
;; the world map.
(define (mk-inventory contents)
  (kern-mk-inventory contents))

;; mk-npc -- create a kernel character of the given type, faction and level
(define (mk-npc npct-tag lvl)
  (let* ((npct (eval npct-tag))
         (npc (bind
               (set-level
                (kern-char-arm-self
                 (mk-stock-char
                  (npct-name npct)
                  (npct-spec npct)
                  (npct-occ npct)
                  (npct-spr npct)
                  (npct-faction npct)
                  (npct-ai npct)
                  (mk-inventory
                   (filter notnull?
                           (map (lambda (x)
                                  (apply roll-to-add x))
                                (npct-eqp npct))))
                  nil
                  (npct-conv npct)))
                lvl)
               (npcg-mk npct-tag))))
    ;; revisit -- will this work or will effects need to be symbol-tags?
    (map (lambda (eff) (apply-eff-pkg npc eff))
         (npct-effects npct))
    (if (not (null? (npct-drop-fx npct)))
        (kern-obj-add-effect npc 
                             ef_loot_drop 
                             (loot-drop-mk (npct-drop-fx npct)
                                           (npct-drop-fx-parms npct))))
    npc))

;; spawn-npc -- like mk-npc but mark the npc as spawned (this allows monster
;; managers to periodically clean up old spawned NPC's)
(define (spawn-npc npct-tag lvl)
  
  (let ((kchar (mk-npc npct-tag lvl)))
    (npcg-set-spawned! (gob kchar) #t)
    kchar))

;;----------------------------------------------------------------------------
;; trap packages
(define no-traps (list nil))
(define basic-traps  (list nil 'burn 'spike-trap))
(define wizard-traps (list nil 'poison-trap 'sleep-trap 'lightning-trap))
(define wrogue-traps (list nil 'self-destruct-trap 'bomb-trap 'sleep-trap 'poison-trap 'spike-trap 'sleep-trap 'burn))

;;----------------------------------------------------------------------------
;; effect packages
(define slime-effects  (list ef_poison_immunity 
                             (list ef_split split-gob-mk 'green-slime)))
(define yellow-slime-effects  (list ef_poison_immunity))
(define undead-effects (list ef_poison_immunity 
                             ef_fire_immunity 
                             ef_disease_immunity 
                             ef_sleep_immunity 
                             ef_magical_kill_immunity))
(define demon-effects (list ef_poison_immunity 
                            ef_fire_immunity 
                            ef_disease_immunity 
                            ef_sleep_immunity
                            ef_magical_kill_immunity))
(define hydra-effects (list ef_poison_immunity ef_grow_head))
(define drag-effects (list ef_fire_immunity))
(define wisp-effects (list ef_poison_immunity 
                           ef_disease_immunity))
(define fire-slime-effects (list ef_fire_immunity
                                 (list ef_split split-gob-mk 'fire-slime)))
(define sludge-kraken-effects (list ef_cleanup_tentacles))

(define (apply-eff-pkg knpc pkg)
  (if (pair? pkg)
      (let* ((eff (car pkg))
             (gob-ctor (cadr pkg))
             (gob-args (cddr pkg))
             (gob (apply gob-ctor gob-args)))
        (kern-obj-add-effect knpc eff gob))
      (kern-obj-add-effect knpc pkg nil)))

;;----------------------------------------------------------------------------
;; equipment packages for different types of npcs
(define wizard-equip 
  (list (list 100 "1"     t_dagger)
        ))
(define archer-equip 
  (list (list 100 "1"     t_bow)
        (list 100 "1d6"   t_arrow)
        (list 100 "1"     t_dagger)
        ))
(define stalker-equip 
  (list (list 100 "2"     t_dagger)
        (list 100 "2"     t_dagger)
        ))
(define slinger-equip 
  (list (list 100 "1"     t_sling)
        ))
(define berserker-equip 
  (list (list 100 "2"     t_axe)         
        (list 100 "1d2"   t_heal_potion)
        ))
(define ranger-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_bow)
        (list 100 "20"    t_arrow)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define skeletal-warrior-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_shield)
        (list 100 "1"     t_iron_helm)
        ))
(define spear-thrower-equip
  (list (list 100 "1d20"  t_spear)
        (list 100 "1"     t_iron_helm)
        (list 100 "1"     t_axe)
        ))
(define death-knight-equip
  (list (list 100 "1"     t_2h_axe)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d3-1" t_mana_potion)
        ))
(define knight-equip
  (list (list 100 "1"     t_2h_sword)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define squire-equip
  (list (list 100 "1"     t_crossbow)
        (list 100 "1d10"  t_bolt)
        (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_chain)
        (list 100 "1"     t_chain_coif)
        (list 100 "1d2-1" t_heal_potion)
        ))
(define halberdier-equip
  (list (list 100 "1"     t_halberd)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_armor_chain)
        (list 100 "1d3-1" t_heal_potion)
        (list 10  "1"     t_vas_mani_scroll)
        (list 10  "1"     t_in_an_scroll)
        ))
(define crossbowman-equip
  (list (list 100 "1"     t_crossbow)
        (list 100 "10"    t_bolt)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_chain)
        (list 100 "1d3-1" t_heal_potion)
        (list 10  "1"     t_vas_mani_scroll)
        (list 10  "1"     t_in_an_scroll)
        ))
(define wrogue-1-equip
  (list (list 100 "1"     t_dagger)
        (list 100 "2d6-2" t_gold_coins)
        (list 50  "1d5"   t_food)
        (list 10  "1d3"   t_torch)
        ))
(define wrogue-2-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_sling)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        ))
(define wrogue-3-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        (list 100 "1d10"  t_arrow)
        (list 75  "1"     t_bow)
        ))
(define wrogue-4-equip
  (list (list 100 "1"     t_armor_chain)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_sword)
        (list 100 "1d10"  t_bolt)
        (list 75  "1"     t_crossbow)
        ))
(define medik-equip
  (list (list 100 "1d3"   t_heal_potion)
        (list 100 "1d2"   t_mana_potion)
        (list 25  "1d2"   t_cure_potion)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_staff)
        (list 100 "1"     t_armor_chain)
        ))
(define troll-equip
  (list (list 100 "1d3" t_thrown_boulder)
        ))
(define geomancer-equip
  (list (list 100 "1d3-1" t_mana_potion)
        ))
(define gint-warrior-equip
  (list (list 100 "1"     t_2h_axe)
        (list 100 "1"     t_2h_sword)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define headless-equip
  (list (list 100 "1"     t_axe)
        (list 100 "1"     t_shield)
        ))
(define craven-archer-equip
  (list (list 100 "1"     t_bow)
        (list 100 "20"    t_arrow)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1"     t_dagger)
        (list 100 "1d3-1" t_mana_potion)
        ))
(define nixie-1-equip
  (list (list 100 "1d20" t_spear)
        ))
(define nixie-2-equip
  (list (list 100 "1d20" t_sword)
        ))
(define bomber-equip
  (list (list 100 "1d5" t_oil)
        (list 100 "1"   t_dagger)
        (list 25  "1d3" t_smoke_bomb)
        ))

(define accursed-1-equip
  (list (list 100 "1" t_dagger)
        ))
(define accursed-2-equip
  (list (list 100 "1" t_dagger)
        (list 75  "1" t_sling)
        ))
(define accursed-3-equip
  (list (list 100 "1" t_staff)
        (list 75  "1" t_sling)
        ))
(define accursed-4-equip
  (list (list 100 "1" t_sword)
        (list 100 "1" t_shield)
        (list 100 "1" t_leather_helm)
        (list 100 "1" t_armor_leather)
        ))
(define accursed-5-equip
  (list (list 100 "1" t_sword)
        (list 100 "1" t_shield)
        (list 100 "1" t_chain_coif)
        (list 100 "1" t_armor_chain)
        (list 100 "1" t_crossbow)
        (list 100 "1d10" t_bolt)
        ))
(define accursed-6-equip
  (list (list 100 "1" t_sword)
        (list 100 "1" t_morning_star)
        (list 100 "1" t_iron_helm)
        (list 100 "1" t_armor_plate)
        ))
        
(define demon-equip
  (list (list 100 "1" t_flaming_sword)
        ))

;;----------------------------------------------------------------------------
;; Loot drops
(define animal-loot
  (list (list 25 "1" 't_food)
        ))

(define deer-loot
  (list (list 100 "1" 't_animal_corpse)
        ))

(define bull-loot
  (list (list 100 "5" 't_food)
        ))

(define wizard-loot
  (list (list 100 "1d2-1" 't_heal_potion)
        (list 100 "1d2+1" 't_mana_potion)
        (list 100 "1d20"  't_gold_coins)
        (list 10  "1d3"   't_food)
        (list 10  "1"     't_cure_potion)
        (list 10  "1"     't_poison_immunity_potion)
        (list 20  "1d5"   'sulphorous_ash)
        (list 20  "1d5"   'ginseng)
        (list 20  "1d5"   'garlic)
        (list 10  "1d3"   'spider_silk)
        (list 10  "1d3"   'blood_moss)
        (list 10  "1d3"   'black_pearl)
        (list 5   "1d2"   'nightshade)
        (list 5   "1d2"   'mandrake)
        (list 5   "1"     't_in_mani_corp_scroll)
        (list 5   "1"     't_xen_corp_scroll)
        (list 10  "1"     't_in_quas_xen_scroll)
        (list 10  "1"     't_an_xen_ex_scroll)
        (list 20  "1"     't_in_an_scroll)
        (list 20  "1"     't_vas_mani_scroll)
        (list 5   "1"     't_dagger)
        ))

(define std-loot
  (list (list 25 "1d2" 't_food)
        (list 100 "1d10" 't_gold)
        (list 25 "1" 't_heal_potion)
        (list 10 "1" 't_torch)
        (list 1 "1" 't_gem)
        ))

(define archer-loot 
  (list
        (list 100 "1d6"   't_arrow)
        (list 100 "1d10"  't_gold_coins)
        (list 20  "1d3"   't_food)
        (list 10  "1"   't_bow)
        ))
(define stalker-loot 
  (list
        (list 100 "1d15"  't_gold_coins)
        (list 30  "1d3"   't_food)
        (list 10  "1d2"    't_dagger)
        ))
(define slinger-loot 
  (list (list 100 "1d10"  't_gold_coins)
        (list 20  "1d3"   't_food)
        (list 10  "1"     't_sling)
        ))
(define berserker-loot 
  (list (list 100 "1d2"   't_heal_potion)
        (list 100 "1d15"  't_gold_coins)
        (list 30  "1d3"   't_food)
        (list 10  "1"   't_axe)
        ))
(define ranger-loot
  (list (list 100 "1d10" 't_gold_coins)
        (list 100 "1d10" 't_arrow)
        (list 30  "1d3"   't_food)
        (list 100 "1d3-1" 't_heal_potion)
        (list 3 "1"     't_sword)
        (list 6 "1"     't_bow)
        (list 7 "1"     't_leather_helm)
        (list 4 "1"     't_armor_leather)
        ))
(define skel-war-loot
  (list (list 100 "1d20"  't_gold_coins)
  		  (list 3 "1"     't_sword)
        (list 3 "1"     't_shield)
        (list 3 "1"     't_iron_helm)
        ))
(define spear-thrower-loot
  (list (list 50 "1d3" 't_spear)
        (list 100 "1d20"  't_gold_coins)
        (list 3  "1"     't_iron_helm)
        (list 3  "1"     't_axe)
        ))
(define dea-kni-loot
  (list
        (list 100 "1d20"  't_gold_coins)
        (list 100 "1d3-1" 't_mana_potion)
        (list 2 "1"     't_2h_axe)
        ;;(list 1 "1"     't_armor_plate)  if you wear anything that comes off one of these, something *really* nasty should happen
        (list 4 "1"     't_iron_helm)
        ))
(define cra-arch-loot
  (list (list 100 "1d5" 't_bolt)
        (list 100 "1d20"  't_gold_coins)
        (list 100 "1d3-1" 't_mana_potion)
		  (list 10  "1"   't_bow)
        ;;(list 1 "1"     't_armor_plate)  if you wear anything that comes off one of these, something *really* nasty should happen
        (list 4   "1"     't_iron_helm)
        ))
(define knight-loot
  (list (list 100 "1d20"  't_gold_coins)
        (list 100 "1d3-1" 't_heal_potion)
        (list 2 "1"     't_2h_sword)
        (list 1 "1"     't_armor_plate)
        (list 4 "1"     't_iron_helm)
        ))
(define squire-loot
  (list (list 100 "1d10"  't_bolt)
        (list 100 "1d10"  't_gold_coins)
        (list 100 "1d2-1" 't_heal_potion)
        (list 2  "1"     't_crossbow)
        (list 10 "1"     't_dagger)
        (list 3 "1"     't_armor_chain)
        (list 5 "1"     't_chain_coif)
        ))
(define halberdier-loot
  (list (list 100 "1d3-1" 't_heal_potion)
        (list 10  "1"     't_vas_mani_scroll)
        (list 10  "1"     't_in_an_scroll)
        (list 50  "1d5"   't_food)
        (list 1   "1"     't_halberd)
        (list 5   "1"     't_chain_coif)
        (list 3   "1"     't_armor_chain)
        ))
(define crossbowman-loot
  (list (list 100 "1d10"    't_bolt)
        (list 100 "1d3-1" 't_heal_potion)
        (list 10  "1"     't_vas_mani_scroll)
        (list 10  "1"     't_in_an_scroll)
        (list 50  "1d5"   't_food)
        (list 2   "1"     't_crossbow)
        (list 5   "1"     't_chain_coif)
        (list 10  "1"     't_dagger)
        (list 3   "1"     't_armor_chain)
        ))
(define wrogue-1-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 50  "1d5"   't_food)
        (list 10  "1d3"   't_torch)
        (list 10  "1"     't_dagger)
        (list 15  "1d3"   't_smoke_bomb)
        ))
(define wrogue-2-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 100 "1d3-1" 't_picklock)
        (list 50  "1d5"   't_food)
        (list 10  "1d3"   't_torch)
        (list 5   "1"     't_sword)
        (list 7   "1"     't_sling)
        (list 5   "1"     't_leather_helm)
        (list 4   "1"     't_armor_leather)
        (list 10  "1d3"  't_smoke_bomb)
        ))
(define wrogue-3-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 100 "1d3-1" 't_picklock)
        (list 50  "1d5"   't_food)
        (list 10  "1d3"   't_torch)
        (list 100 "1d10"  't_arrow)
        (list 5   "1"     't_sword)
        (list 6   "1"     't_leather_helm)
        (list 7   "1"     't_armor_leather)
        (list 3   "1"   't_bow)
        (list 5   "1d3"   't_smoke_bomb)
        ))
(define wrogue-4-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 100 "1d3-1" 't_picklock)
        (list 50  "1d5"   't_food)
        (list 50  "1d10"  't_bolt)
        (list 10  "1"     't_in_ex_por_scroll)
        (list 10  "1"     't_wis_quas_scroll)
        (list 5   "1"     't_sanct_lor_scroll)
        (list 5   "1"     't_an_tym_scroll)
        (list 5   "1"     't_vas_rel_por_scroll)
        (list 20  "1"     't_mana_potion)
        (list 10  "1"     't_cure_potion)
        (list 10  "1"     't_poison_immunity_potion)
        (list 10  "1d3"   't_torch)
        (list 3   "1"     't_armor_chain)
        (list 5   "1"     't_chain_coif)
        (list 5   "1"     't_sword)
        (list 3   "1"     't_crossbow)
        (list 2   "1d3"   't_smoke_bomb)
        ))
(define medik-loot
  (list (list 100 "1d3"   't_heal_potion)
        (list 100 "1d2"   't_mana_potion)
        (list 25  "1d2"   't_cure_potion)
        (list 25  "1d2"   't_vas_mani_scroll)
        (list 20  "1d5"   'sulphorous_ash)
        (list 20  "1d5"   'ginseng)
        (list 20  "1d5"   'garlic)
        (list 10  "1d3"   'spider_silk)
        (list 10  "1d3"   'blood_moss)
        (list 10  "1d3"   'black_pearl)
        (list 5   "1d2"   'nightshade)
        (list 5   "1d2"   'mandrake)
        (list 5   "1"     't_chain_coif)
        (list 7   "1"     't_staff)
        (list 3   "1"     't_armor_chain)
        ))
(define troll-loot
  (list (list 100 "1d3-1" 't_thrown_boulder)
        (list 25  "1d3"   't_food)
        (list 100 "2d10"  't_gold_coins)
        ))
(define geomancer-loot
  (list (list 50  "1d3"   't_gem)
        (list 50  "1d20"  't_gold_coins)
        (list 100 "1d3-1" 't_mana_potion)
        ))
(define gint-loot
  (list (list 100 "4d25"  't_gold_coins)
        (list 100 "1d5"   't_food)
        (list 100 "1d3-1" 't_heal_potion)
        (list 3   "1"      't_2h_axe)
        (list 3   "1"      't_2h_sword)
        ))
(define reaper-loot
  (list (list 100 "1d5"   't_torch)
        ))
(define headless-loot
  (list (list 100 "1d5-1" 't_gold_coins)
  		  (list 2 "1"     't_axe)
        (list 2 "1"     't_shield)
        ))
(define dragon-loot
  (list (list 100 "1d100+19" 't_gold_coins)
        (list 100 "1d20"     't_food)
        (list 100 "1d5-1"    't_gem)
        (list 100 "1"        't_dragons_blood)
        ;; none of this is logical, beyond 'dragons oughta have the good stuff'
        (list 10  "1"     't_in_ex_por_scroll)
        (list 10  "1"     't_wis_quas_scroll)
        (list 5   "1"     't_sanct_lor_scroll)
        (list 5   "1"     't_an_tym_scroll)
        (list 5   "1"     't_vas_rel_por_scroll)
        (list 20  "1"     't_mana_potion)
        (list 10  "1"     't_cure_potion)
        (list 20  "1d5"   'sulphorous_ash)
        (list 20  "1d5"   'ginseng)
        (list 20  "1d5"   'garlic)
        (list 10  "1d3"   'spider_silk)
        (list 10  "1d3"   'blood_moss)
        (list 10  "1d3"   'black_pearl)
        (list 5   "1d2"   'nightshade)
        (list 5   "1d2"   'mandrake)
        ))

(define hydra-loot
  (list (list 100 "1" 't_hydras_blood)
        ))

(define lich-loot
  (cons (list 100 "1" 't_lichs_blood)
        wizard-loot))

(define zorn-loot
  (list (list 100 "1d20+9" 't_gold_coins)
        ))

(define bomber-loot
  (list (list 50 "1d3" 't_oil)
        ))
        
(define dryad-loot
  (list (list 100 "1d5" 't_torch)
        ))
        
(define demon-loot
  (list (list 100 "2d20" 't_gold_coins)
        (list 5   "1"    't_flaming_sword)
        ))
        
(define ghast-loot
  (list (list 50 "1" 't_mana_potion)
        ))
        
(define yellow-slime-loot
  (list (list 50 "1" 't_royal_cape)
        ))
        
(define fire-slime-loot
  (list (list 100 "1" 't_oil)
        ))
        
(define spider-loot
  (list (list 50 "1" 'spider_silk)
        ))
        
(define queen-spider-loot
  (list (list 50 "1d3" 'spider_silk)
        (list 25 "1" 't_poison_immunity_potion)
        ))

(define accursed-1-loot
  (list (list 50 "1d2-1" 't_heal_potion)
        (list 50 "1d2" 't_mana_potion)
        (list 100 "1d10"  't_gold_coins)
        (list 10  "1d2"   't_food)
        (list 5  "1"     't_cure_potion)
        (list 5  "1"     't_poison_immunity_potion)
        (list 10  "1d3"   'sulphorous_ash)
        (list 10  "1d3"   'ginseng)
        (list 10  "1d3"   'garlic)
        (list 5  "1d2"   'spider_silk)
        (list 5  "1d2"   'blood_moss)
        (list 5  "1d2"   'black_pearl)
        (list 5  "1"     't_in_quas_xen_scroll)
        (list 5  "1"     't_an_xen_ex_scroll)
        (list 5  "1"     't_in_an_scroll)
        (list 5  "1"     't_vas_mani_scroll)
        )) 

(define accursed-5-loot
  (list (list 100 "1d10"    't_bolt)
        (list 10 "1d3-1" 't_heal_potion)
        (list 50  "1d5"   't_food)
        (list 100 "2d10" 't_gold_coins)
        (list 4 "1" 't_sword)
        (list 6 "1" 't_shield)
        (list 5 "1" 't_chain_coif)
        (list 7 "1" 't_armor_chain)
        (list 3 "1" 't_crossbow)
        ))  
        
        
(define (drop-generic knpc loot)
  (if (not (kern-place-is-wilderness? (loc-place (kern-obj-get-location knpc))))
           (let ((loc (kern-obj-get-location knpc)))
             (map (lambda (triple)
                    (println triple)
                    (let ((thresh (car triple))
                          (dice (cadr triple))
                          (type-tag (caddr triple)))
                      (if (< (modulo (random-next) 100) thresh)
                          (let ((quantity (kern-dice-roll dice)))
                            (if (> quantity 0)
                            		(let ((obj (kern-mk-obj (eval type-tag) quantity)))
                            			(if (can-be-dropped? obj loc cant)
                                			(kern-obj-put-at obj loc)
                       			)))))))
                  loot)
             )))
  
;; npc types
;;      scheme variable                 name                       species          occup.     sprite             chest traps  equipment              effects       ai               faction
;;      ======================          ========================== ================ ========== ================== ============ ====================== ============= ==============   ========
(define forest-goblin-shaman  (mk-npct2 "forest goblin shaman"  sp_forest_goblin oc_wizard  s_fgob_shaman wizard-traps wizard-equip  nil 'shaman-ai  faction-forest-goblin nil 'drop-generic wizard-loot ))
(define forest-goblin-hunter  (mk-npct2 "forest goblin hunter"  sp_forest_goblin oc_warrior s_fgob_archer  basic-traps  archer-equip  nil 'generic-ai faction-forest-goblin nil 'drop-generic archer-loot ))
(define forest-goblin-stalker (mk-npct2 "forest goblin stalker" sp_forest_goblin oc_warrior s_fgob_stalker  basic-traps  stalker-equip nil 'generic-ai faction-forest-goblin nil 'drop-generic stalker-loot))

(define cave-goblin-slinger   (mk-npct2 "cave goblin slinger"   sp_cave_goblin  oc_warrior s_cgob_slinger    basic-traps  slinger-equip    nil 'generic-ai faction-cave-goblin  nil 'drop-generic slinger-loot))
(define cave-goblin-berserker (mk-npct2 "cave goblin berserker" sp_cave_goblin  oc_warrior s_cgob_berserk    basic-traps  berserker-equip  nil 'generic-ai faction-cave-goblin  nil 'drop-generic berserker-loot))
(define cave-goblin-priest    (mk-npct2 "cave goblin priest"    sp_cave_goblin  oc_wizard  s_cgob_shaman    wizard-traps wizard-equip     nil 'priest-ai  faction-cave-goblin  nil 'drop-generic wizard-loot))

(define ranger                (mk-npct2 "ranger"                sp_human        oc_ranger s_ranger basic-traps  ranger-equip     nil 'ranger-ai  faction-men  'ranger-conv 'drop-generic ranger-loot))

(define skeletal-spear-thrower (mk-npct2 "skeletal spear-thrower" sp_skeleton oc_warrior s_spearskeleton basic-traps spear-thrower-equip    undead-effects 'nolight-ai faction-monster nil 'drop-generic spear-thrower-loot))
(define skeletal-warrior (mk-npct2 "skeletal warrior" sp_skeleton oc_warrior s_skeleton basic-traps skeletal-warrior-equip undead-effects 'nolight-ai faction-monster nil 'drop-generic skel-war-loot))
(define skeletal-archer (mk-npct2 "skeletal archer" sp_skeleton oc_warrior s_skeletonarcher basic-traps archer-equip undead-effects 'nolight-ai faction-monster nil 'drop-generic archer-loot))

(define death-knight  (mk-npct2 "death knight"  sp_skeleton oc_warrior s_deathknight   basic-traps death-knight-equip  undead-effects 'death-knight-ai faction-monster  nil 'drop-generic dea-kni-loot))
(define craven-archer (mk-npct2 "craven archer" sp_skeleton oc_warrior s_deatharcher   basic-traps craven-archer-equip nil            'craven-archer-ai faction-monster nil 'drop-generic cra-arch-loot))

(define halberdier    (mk-npct2 "halberdier"    sp_human    oc_warrior s_guard        no-traps halberdier-equip   nil 'guard-ai faction-men   nil 'drop-generic halberdier-loot))
(define crossbowman   (mk-npct2 "crossbowman"   sp_human    oc_warrior s_xbowguard        no-traps crossbowman-equip  nil 'guard-ai faction-men   nil 'drop-generic crossbowman-loot))
(define medik         (mk-npct2 "medik"         sp_human    oc_wizard  s_blue_wizard  no-traps medik-equip        nil 'medik-ai faction-men   nil 'drop-generic medik-loot))
(define troll         (mk-npct2 "troll"         sp_troll    oc_warrior s_troll        no-traps troll-equip        nil 'std-ai   faction-troll nil 'drop-generic troll-loot))
(define glasdrin-halberdier    (mk-npct2 "halberdier"    sp_human    oc_warrior s_guard        no-traps halberdier-equip   nil 'guard-ai faction-glasdrin   nil 'drop-generic halberdier-loot))
(define glasdrin-crossbowman   (mk-npct2 "crossbowman"   sp_human    oc_warrior s_xbowguard        no-traps crossbowman-equip  nil 'guard-ai faction-glasdrin   nil 'drop-generic crossbowman-loot))

;; Bandit types
(define footpad    (mk-npct2 "footpad"    sp_human oc_wrogue s_brigand wrogue-traps wrogue-1-equip nil 'std-ai faction-outlaw nil 'drop-generic wrogue-1-loot))
(define bandit     (mk-npct2 "bandit"     sp_human oc_wrogue s_brigand wrogue-traps wrogue-2-equip nil 'std-ai faction-outlaw nil 'drop-generic wrogue-2-loot))
(define highwayman (mk-npct2 "highwayman" sp_human oc_wrogue s_brigand wrogue-traps wrogue-3-equip nil 'std-ai faction-outlaw nil 'drop-generic wrogue-3-loot))
(define blackguard (mk-npct2 "blackguard" sp_human oc_wrogue s_brigand wrogue-traps wrogue-4-equip nil 'std-ai faction-outlaw nil 'drop-generic wrogue-4-loot))
(define bomber     (mk-npct2 "mad jester" sp_human oc_wrogue s_jester  wrogue-traps bomber-equip   nil 'std-ai faction-outlaw nil 'drop-generic bomber-loot))

(define bat (mk-npct2 "bat" sp_bat nil s_bat nil nil nil 'animal-ai faction-monster nil 'drop-generic animal-loot))
(define rat (mk-npct2 "dire rat" sp_rat nil s_rat nil nil nil 'rat-ai faction-monster nil 'drop-generic animal-loot))
(define zorn (mk-npct2 "zorn" sp_zorn oc_wrogue s_zorn wrogue-traps nil nil 'animal-ai faction-monster nil 'drop-generic zorn-loot))
(define bull (mk-npct "bull" sp_bull nil s_bull nil nil nil 'animal-ai faction-none nil 'drop-generic bull-loot))
(define lich (mk-npct2 "lich" sp_lich oc_wizard s_lich wizard-traps wizard-equip undead-effects 'spell-sword-ai faction-monster nil 'drop-generic lich-loot))
(define dryad (mk-npct2 "dryad" sp_dryad nil s_reaper nil nil nil 'dryad-ai faction-monster nil 'drop-generic dryad-loot))
(define gazer (mk-npct2 "gazer" sp_gazer oc_wizard s_gazer wizard-traps nil nil 'gazer-ai faction-monster nil 'drop-generic wizard-loot))
(define demon (mk-npct2 "demon" sp_demon nil s_demon basic-traps demon-equip demon-effects 'demon-ai faction-monster nil 'drop-generic demon-loot))
(define ghast (mk-npct2 "ghast" sp_ghast nil s_ghost nil nil undead-effects 'std-ai faction-monster nil 'drop-generic ghast-loot))
(define snake (mk-npct "snake" sp_snake nil s_snake nil nil nil 'snake-ai faction-monster nil 'drop-generic animal-loot))
(define insect (mk-npct "insect swarm" sp_insect nil s_insects nil nil nil 'animal-ai faction-monster nil 'drop-generic animal-loot))
(define dragon (mk-npct2 "dragon" sp_dragon nil s_dragon wizard-traps nil drag-effects 'dragon-ai faction-monster nil 'drop-generic dragon-loot))
(define knight (mk-npct2 "knight" sp_human oc_warrior s_knight no-traps knight-equip nil 'guard-ai faction-trigrave 'knight-conv 'drop-generic knight-loot))
(define paladin (mk-npct2 "paladin" sp_human oc_warrior s_companion_paladin no-traps knight-equip nil 'std-ai faction-men 'knight-conv 'drop-generic knight-loot))
(define tinker (mk-npct2 "tinker" sp_human oc_warrior s_companion_tinker no-traps wrogue-4-equip nil 'std-ai faction-men nil 'drop-generic wrogue-4-loot))
(define squire (mk-npct2 "squire" sp_human oc_warrior s_xbowguard no-traps squire-equip nil 'guard-ai faction-trigrave 'knight-conv 'drop-generic squire-loot))
(define warlock (mk-npct2 "warlock" sp_human oc_wizard s_wizard wizard-traps wizard-equip nil 'warlock-ai faction-monster nil 'drop-generic wizard-loot))
(define wizard (mk-npct2 "wizard" sp_human oc_wizard s_companion_wizard wizard-traps wizard-equip nil 'spell-sword-ai faction-men nil 'drop-generic wizard-loot))
(define headless (mk-npct2 "headless" sp_headless oc_warrior s_headless basic-traps headless-equip nil 'animal-ai faction-monster nil 'drop-generic headless-loot))
(define gint-mage (mk-npct2 "gint mage" sp_gint oc_wizard s_gint_mage wizard-traps wizard-equip nil 'shaman-ai faction-gint nil 'drop-generic wizard-loot))
(define gint-warrior (mk-npct2 "gint warrior" sp_gint oc_warrior s_gint basic-traps gint-warrior-equip nil 'std-ai faction-gint nil 'drop-generic gint-loot))
(define yellow-slime (mk-npct2 "yellow slime" sp_yellow_slime nil s_yellow_slime nil nil yellow-slime-effects 'yellow-slime-ai faction-monster nil 'drop-generic yellow-slime-loot))
(define troll-geomancer (mk-npct2 "troll geomancer" sp_troll oc_wizard s_troll_geomancer no-traps geomancer-equip nil 'geomancer-ai faction-troll nil 'drop-generic geomancer-loot))
(define corrupt-halberdier (mk-npct2 "halberdier" sp_human oc_warrior s_guard no-traps halberdier-equip nil 'guard-ai faction-monster nil 'drop-generic halberdier-loot))
(define corrupt-crossbowman (mk-npct2 "crossbowman" sp_human oc_warrior s_guard no-traps crossbowman-equip nil 'guard-ai faction-monster nil 'drop-generic crossbowman-loot))
(define giant-spider (mk-npct2 "giant spider" sp_spider nil s_spider nil nil nil 'spider-ai faction-monster nil 'drop-generic spider-loot))
(define queen-spider (mk-npct2 "queen spider" sp_queen_spider nil s_queen_spider nil nil nil 'spider-ai faction-monster nil 'drop-generic queen-spider-loot))
(define fire-slime (mk-npct2 "fire slime" sp_fire_slime nil s_red_slime nil nil fire-slime-effects 'animal-ai faction-monster nil 'drop-generic fire-slime-loot))
(define hydra (mk-npct2 "hydra" sp_hydra nil s_hydra no-traps nil hydra-effects 'hydra-ai faction-monster nil 'drop-generic hydra-loot))
(define mimic (mk-npct2 "mimic" sp_mimic nil s_mimic no-traps nil nil 'animal-ai faction-monster nil 'drop-generic zorn-loot))
(define ratling-swarmer (mk-npct2 "ratling swarmer" sp_ratling nil s_mouse no-traps nil nil 'ratling-ai faction-monster nil 'drop-generic animal-loot))
(define ratling-sorcerer (mk-npct2 "ratling sorcerer"  sp_ratling oc_wizard s_ratling_sorcerer wizard-traps wizard-equip  nil 'ratling-sorcerer-ai faction-monster nil 'drop-generic animal-loot))
(define carabid (mk-npct2 "carabid" sp_carabid nil s_carabid nil nil nil 'carabid-ai faction-monster nil 'drop-generic animal-loot))
(define deer (mk-npct2 "deer" sp_deer nil s_deer nil nil nil nil faction-none nil 'drop-generic deer-loot))
(define chicken (mk-npct2 "chicken" sp_chicken nil s_chicken nil nil nil 'animal-ai faction-none nil 'drop-generic animal-loot))

;; NPC's with no drops

(define green-slime     (mk-npct "green slime"            sp_green_slime   nil        s_slime        nil          nil           slime-effects 'animal-ai       faction-monster       nil))
(define kraken          (mk-npct "kraken"                 sp_kraken        nil        s_kraken       nil          nil           nil           'kraken-ai       faction-monster       nil))
(define sea-serpent     (mk-npct "sea serpent"            sp_sea_serpent   nil        s_sea_serpent  nil          nil           nil           'sea-serpent-ai  faction-monster       nil))
(define wolf            (mk-npct "wolf"                   sp_wolf          nil        s_wolf         nil          nil           nil           'wolf-ai       faction-monster       nil))
(define wisp            (mk-npct "wisp"                   sp_wisp          nil        s_wisp         nil          nil           wisp-effects  'wisp-ai         faction-monster       nil))
(define nixie-spearman  (mk-npct "nixie spearman"         sp_nixie         oc_warrior s_nixie_spear  no-traps     nixie-1-equip nil           'nixie-ai        faction-monster       nil))
(define nixie-swordsman (mk-npct "nixie swordsman"        sp_nixie         oc_warrior s_nixie_sword  no-traps     nixie-2-equip nil           'nixie-ai        faction-monster       nil))
(define sludge-kraken   (mk-npct "sludge kraken"          sp_great_kraken  nil        s_great_kraken nil          nil           sludge-kraken-effects 'sludge-kraken-ai faction-monster nil))
(define sludge-tentacle (mk-npct "sludge kraken tentacle" sp_kraken_tentacle nil      s_tentacle     nil          nil           nil           'sludge-tentacle-ai faction-monster    nil))
(define griffin         (mk-npct "griffin"                sp_griffin       nil        s_griffin      nil          nil           nil           'griffin-ai      faction-monster       nil))
(define griffin-chick   (mk-npct "griffin chick"          sp_griffin_chick nil        s_griffin_chick nil         nil           nil           'griffin-ai      faction-monster       nil))

;; accursed
(define accursed-acolyte    (mk-npct2 "an accursed acolyte"    sp_human oc_wizard s_shepherd nil accursed-1-equip nil 'spell-sword-ai faction-accursed nil 'drop-generic accursed-1-loot))
(define accursed-apprentice (mk-npct2 "an accursed apprentice" sp_human oc_wizard s_shepherd nil accursed-2-equip nil 'spell-sword-ai faction-accursed nil 'drop-generic accursed-1-loot))
(define accursed-journeyman (mk-npct2 "an accursed journeyman" sp_human oc_wizard s_wizard   nil accursed-3-equip nil 'spell-sword-ai faction-accursed nil 'drop-generic wizard-loot))
(define accursed-master     (mk-npct2 "an accursed master"     sp_human oc_wizard s_wizard   nil accursed-3-equip nil 'spell-sword-ai faction-accursed nil 'drop-generic wizard-loot))
(define accursed-adept      (mk-npct2 "an accursed adept"      sp_human oc_wizard s_wizard   nil accursed-3-equip nil 'spell-sword-ai faction-accursed nil 'drop-generic wizard-loot))
(define accursed-guardian   (mk-npct2 "an accursed guardian"   sp_human oc_warrior s_fighter nil accursed-4-equip nil 'std-ai         faction-accursed nil 'drop-generic wrogue-1-loot))
(define accursed-defender   (mk-npct2 "an accursed defender"   sp_human oc_warrior s_knight  nil accursed-5-equip nil 'std-ai         faction-accursed nil 'drop-generic accursed-5-loot))
(define accursed-templar    (mk-npct2 "an accursed templar"    sp_human oc_warrior s_avatar  nil accursed-6-equip nil 'std-ai         faction-accursed nil 'drop-generic wrogue-4-loot))

;; npcs with odd alignments
(define gint-warrior-m (mk-npct2 "gint warrior" sp_gint oc_warrior s_gint basic-traps gint-warrior-equip nil 'std-ai faction-monster nil 'drop-generic gint-loot))
(define troll-m         (mk-npct2 "troll"         sp_troll    oc_warrior s_troll        no-traps troll-equip        nil 'std-ai   faction-monster nil 'drop-generic troll-loot))
(define cave-goblin-slinger-m   (mk-npct2 "cave goblin slinger"   sp_cave_goblin  oc_warrior s_cgob_slinger    basic-traps  slinger-equip    nil 'generic-ai faction-monster  nil 'drop-generic slinger-loot))
(define cave-goblin-berserker-m (mk-npct2 "cave goblin berserker" sp_cave_goblin  oc_warrior s_cgob_berserk    basic-traps  berserker-equip  nil 'generic-ai faction-monster  nil 'drop-generic berserker-loot))
(define gint-mage-m (mk-npct2 "gint mage" sp_gint oc_wizard s_gint_mage wizard-traps wizard-equip nil 'shaman-ai faction-monster nil 'drop-generic wizard-loot))
(define troll-geomancer-m (mk-npct2 "troll geomancer" sp_troll oc_wizard s_troll_geomancer no-traps geomancer-equip nil 'std-ai faction-monster nil 'drop-generic geomancer-loot))


;;define                        (mk-npct "                          sp_              oc_        s_                 nil          nil                    nil           'std-ai           ))

;;----------------------------------------------------------------------------
;; Type queries
;;----------------------------------------------------------------------------
(define (is-species? kchar species)
  (eqv? (kern-char-get-species kchar) species))

(define (is-occ? kchar occ)
  (eqv? (kern-char-get-occ kchar) occ))

(define (is-yellow-slime? kchar)
  (is-species? kchar sp_yellow_slime))

(define (is-green-slime? kchar)
  (is-species? kchar sp_green_slime))

(define (is-spider? kchar)
  (or (is-species? kchar sp_spider)
      (is-species? kchar sp_queen_spider)))

(define (is-troll? kchar)
  (is-species? kchar sp_troll))

(define (is-goblin? kchar)
  (or (is-species? kchar sp_cave_goblin)
      (is-species? kchar sp_forest_goblin)))

(define (is-skeleton? kchar)
  (is-species? kchar sp_skeleton))

(define (is-death-knight? kchar)
  (and (is-species? kchar sp_skeleton)
       (is-occ? kchar oc_warrior)))

(define (is-bandit? kchar)
  (is-occ? kchar oc_wrogue))

(define (is-halberdier? kchar)
  (is-guard-type? kchar 'halberdier))

(define (is-crossbowman? kchar)
  (is-guard-type? kchar 'crossbowman))

(define (is-gint? kchar)
  (is-species? kchar sp_gint))

(define (is-forest-goblin-shaman? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_wizard)))

(define (is-forest-goblin-hunter? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_warrior)))

;; fixme -- same as forest-goblin-hunter above
(define (is-forest-goblin-stalker? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_warrior)))

(define (is-skeletal-warrior? kchar)
  (let ((gob (kobj-gob-data kchar)))
    (and (not (null? gob))
         (eq? (car gob)
              'skeletal-warrior))))

(define (post-guard kguard x y)
  (npcg-set-post! (gob kguard) (list x y))
  kguard)

(define (mk-bull)
  (mk-npc 'bull 8))

(define (is-undead? kchar)
  (or (is-species? kchar sp_skeleton)
      (is-species? kchar sp_lich)))
      
(define (is-snake? kchar)
  (is-species? kchar sp_snake))

(define (is-rat? kchar)
  (or (is-species? kchar sp_rat)
      (is-species? kchar sp_ratling)))

(define (is-sludge-tentacle? kchar)
  (kbeing-is-npc-type? kchar 'sludge-tentacle))

(define (can-fly? kobj)
  (eqv? (kern-obj-get-movecost kobj pclass-canfly) norm))

(define (can-phase? kobj)
  (eqv? (kern-obj-get-mmode kobj)
        mmode-phase))

(define (is-abstract? kobj)
  (null? (kern-obj-get-name kobj)))