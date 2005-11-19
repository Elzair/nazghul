;;----------------------------------------------------------------------------
;; NPC type constructors
;;----------------------------------------------------------------------------

(load "yellow-slime.scm")
(load "troll.scm")
(load "spider.scm")

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
  ;;(println "mk-stock-char")
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
   0  ;;...........current experience points
   (max-mp species occupation default-level 0 0) ;;.current magic points
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

;; npcg -- generic NPC gob
(define (npcg-mk type) (list 'npcg type #f #f))
(define (npcg-type npcg) (cadr npcg))
(define (npcg-taunted? npcg) (caddr npcg))
(define (npcg-spawned? npcg) (cadddr npcg))
(define (npcg-is-type? npcg type) (equal? type (npcg-type npcg)))
(define (npcg-set-taunted! npcg val) (set-car! (cddr npcg) val))
(define (npcg-set-spawned! npcg val) (set-car! (cdddr npcg) val))

(define (is-npcg? gob) (eq? (car gob) 'npcg))

(define (kbeing-is-npc-type? kbeing type)
  (let ((npcg (gob kbeing)))
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-is-type? npcg type))))

(define (kbeing-was-spawned? kbeing)
  (let ((npcg (gob kbeing)))
    ;;(println "kbeing-was-spawned?" npcg)
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-spawned? npcg))))
  
;; eqp -- equipment package
(define (mk-eqp traps contents)
  (cons traps contents))
(define (eqp-traps eqp) (car eqp))
(define (eqp-contents eqp) (cdr eqp))
(define (eqp-generate eqp)
  (mk-chest
   (random-select (eqp-traps))
   (eval (eqp-contents eqp))))

;; npct -- NPC type
(define (mk-npct name spec occ spr traps equip ai)
  (list name spec occ spr traps equip ai))
 (define (npct-name npct) (car npct))
 (define (npct-spec npct) (cadr npct))
 (define (npct-occ npct) (caddr npct))
 (define (npct-spr npct) (cadddr npct))
 (define (npct-traps npct) (list-ref npct 4))
 (define (npct-eqp npct) (list-ref npct 5))
 (define (npct-ai npct) (list-ref npct 6))

;; mk-npc -- create a kernel character of the given type, faction and level
(define (mk-npc npct-tag faction lvl)
  (let ((npct (eval npct-tag)))
    ;;(println "mk-npc:" npct " " faction " " lvl)g
    (bind
     (set-level
      (kern-char-arm-self
       (mk-stock-char
        (npct-name npct)
        (npct-spec npct)
        (npct-occ npct)
        (npct-spr npct)
        faction
        (npct-ai npct)
        (mk-chest
         (random-select (npct-traps npct))
         (filter notnull?
                 (map (lambda (x)
                        (apply roll-to-add x))
                      (npct-eqp npct))))
        nil
        nil
        nil))
      lvl)
     (npcg-mk npct-tag))))

;; spawn-npc -- like mk-npc but mark the npc as spawned (this allows monster
;; managers to periodically clean up old spawned NPC's)
(define (spawn-npc npct-tag faction lvl)
  (let ((kchar (mk-npc npct-tag faction lvl)))
    (npcg-set-spawned! (gob kchar) #t)
    kchar))

;; common traps for different types of npcs
(define basic-traps  (list nil 'burn 'spike-trap))
(define wizard-traps (list nil 'poison-trap 'sleep-trap 'lightning-trap))

;; common equipment packages for different types of npcs
(define wizard-equip 
  (list (list 100 "1"     t_dagger)
        (list 100 "1d2-1" t_heal_potion)
        (list 100 "1d2+1" t_mana_potion)
        (list 100 "1d20"  t_gold_coins)
        (list 10  "1d3"   t_food)
        ))
(define archer-equip 
  (list (list 100 "1"     t_bow)
        (list 100 "10"    t_arrow)
        (list 100 "1"     t_dagger)
        (list 50  "1"     t_heal_potion)
        (list 100 "1d10"  t_gold_coins)
        (list 20  "1d3"   t_food)
        ))
(define stalker-equip 
  (list (list 100 "2"     t_dagger)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        (list 100 "1d2"   t_heal_potion)
        (list 100 "1d15"  t_gold_coins)
        (list 30  "1d3"   t_food)
        ))
(define slinger-equip 
  (list (list 100 "1"     t_sling)
        (list 100 "1d10"  t_gold_coins)
        (list 20  "1d3"   t_food)
        ))
(define berserker-equip 
  (list (list 100 "2"     t_axe)         
        (list 100 "1d2"   t_heal_potion)
        (list 100 "1d15"  t_gold_coins)
        (list 30  "1d3"   t_food)
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
        (list 100 "1d20"  t_gold_coins)
        ))
(define skeletal-spear-thrower-equip
  (list (list 100 "1d20"  t_spear)
        (list 100 "1"     t_iron_helm)
        (list 100 "1"     t_axe)
        (list 100 "1d20"  t_gold_coins)
        ))

;; npc types
(define forest-goblin-shaman
  (mk-npct "a forest goblin shaman" sp_forest_goblin oc_wizard s_orc wizard-traps wizard-equip 'shaman-ai))
(define forest-goblin-hunter
  (mk-npct "a forest goblin hunter" sp_forest_goblin oc_warrior s_orc basic-traps archer-equip 'generic-ai))
(define forest-goblin-stalker
  (mk-npct "a forest goblin stalker" sp_forest_goblin oc_warrior s_orc basic-traps stalker-equip 'generic-ai))
(define cave-goblin-slinger
  (mk-npct "a cave goblin slinger" sp_cave_goblin oc_warrior s_orc basic-traps slinger-equip 'generic-ai))
(define cave-goblin-berserker
  (mk-npct "a cave goblin berserker" sp_cave_goblin oc_warrior s_orc basic-traps berserker-equip 'generic-ai))
(define cave-goblin-priest
  (mk-npct "a cave goblin priest" sp_cave_goblin oc_wizard s_orc wizard-traps wizard-equip 'priest-ai))
(define ranger
  (mk-npct "a ranger" sp_human oc_warrior s_companion_ranger basic-traps ranger-equip 'generic-ai))
(define skeletal-spear-thrower
  (mk-npct "a skeletal spear-thrower" sp_skeleton oc_warrior s_skeleton basic-traps skeletal-spear-thrower-equip 'generic-ai))
(define skeletal-warrior 
  (mk-npct "a skeletal warrior" sp_skeleton oc_warrior s_skeleton basic-traps skeletal-warrior-equip 'generic-ai))

; (define (mk-skeletal-warrior)
;   (bind (kern-char-arm-self
;          (mk-stock-char
;           " a skeleton" ;;.....name
;           sp_skeleton ;;.......species
;           oc_warrior ;;.........occupation
;           s_skeleton ;;........sprite
;           faction-monster ;;...faction
;           nil ;;...............custom ai (optional)
          
;           ;;...................container (and contents)
;           (mk-chest
;            nil
;            (mk-contents (roll-to-add 25  "1"     t_2h_axe)
;                         (roll-to-add 25  "1"     t_halberd)
;                         (roll-to-add 50  "1"     t_sword)
;                         (roll-to-add 25  "1"     t_2H_sword)
;                         (roll-to-add 50  "1"     t_shield)
;                         (roll-to-add 25  "1"     t_bow)
;                         (roll-to-add 50  "1d20"  t_arrow)
;                         (roll-to-add 50  "1"     t_iron_helm)
;                         (roll-to-add 50  "1"     t_armor_leather)
;                         (roll-to-add 25  "1"     t_armor_leather)
;                         (roll-to-add 10  "1"     t_armor_chain)
;                         (roll-to-add 2   "1"     t_armor_plate)))
          
;           nil ;;...............readied arms (in addition to container contents)
;           nil ;;...............effects
;           nil ;;...............conversation
;           ))
;         (list 'skeletal-warrior)))

(define (is-skeletal-warrior? kchar)
  (let ((gob (kobj-gob-data kchar)))
    (and (not (null? gob))
         (eq? (car gob)
              'skeletal-warrior))))
        

;; Death knights can use Vampiric Touch at L3 and Disease at L6

(define (death-knight-ai kchar)
  (or (use-potion? kchar)
      (let ((vt (can-use-ability? vampiric-touch kchar))
            (dis (can-use-ability? disease-touch kchar)))
        (if (not (or vt dis))
            #f
            (let ((victims (get-hostiles-in-range kchar 1)))
              (if (null? victims)
                  #f
                  (if (wants-healing? kchar)
                      (use-ability vampiric-touch kchar (car victims))
                      (if (and dis
                               (>= (kern-dice-roll "1d20") 16))
                          (use-ability disease-touch kchar (car victims))
                          #f))))))))

(define (mk-death-knight)
  (kern-char-arm-self
   (mk-stock-char
    "a death knight" ;;..name
    sp_skeleton ;;.......species
    oc_warrior ;;.occupation
    s_knight ;;..........sprite
    faction-monster ;;...faction
    'death-knight-ai ;;..custom ai (optional)
    ;;...................container (and contents)
    (mk-chest
     nil
     (mk-contents (roll-to-add 100  "1"      t_2h_axe)
                  (roll-to-add 100 "1"       t_iron_helm)
                  (roll-to-add 100 "1"       t_armor_plate)
                  (roll-to-add 75  "1d30"    t_gold_coins)
                  (roll-to-add 1  "1d3"     t_mana_potion)
                  ))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
    nil ;;...............conversation
    )))

(define (mk-death-knight-at-level lvl-dice)
  (let ((dk (mk-death-knight))
        (lvl (kern-dice-roll lvl-dice)))
    (kern-char-set-level dk lvl)
    (kern-char-set-hp dk 
                      (max-hp (kern-char-get-species dk)
                              (kern-char-get-occ dk)
                              lvl 0 0))
    (kern-char-set-mana dk
                        (max-mp (kern-char-get-species dk)
                                (kern-char-get-occ dk)
                                lvl 0 0))))

;;----------------------------------------------------------------------------
;; Guards
;;----------------------------------------------------------------------------
(define (guard-mk type post)
  (list type post))
(define (guard-type guard) (car guard))
(define (guard-post guard) (cadr guard))
(define (guard-has-post? guard) (not (null? (guard-post guard))))
(define (guard-set-post! guard post) (set-car! (cdr guard) post))
(define (is-guard-type? kchar type)
  (let ((gob (kobj-gob-data kchar)))
    (and (pair? gob)
         (eq? (guard-type gob)
              type))))

(define (guard-ai kchar)

  (define (try-to-use-ability)
    ;;(display "try-to-use-ability")(newline)
    (if (can-use-ability? disarm kchar)
        (let ((victims (get-hostiles-in-range kchar 1)))
          (and (not (null? victims))
               (>= (kern-dice-roll "1d20") 16)
               (or (use-ability disarm kchar (car victims))
                   #t)))
        #f))

  (define (goto-post)
    ;;(display "goto-post")(newline)
    (let ((guard (kobj-gob-data kchar)))
      (if (guard-has-post? guard)
          (let ((post (cons (loc-place (kern-obj-get-location kchar))
                            (guard-post guard))))
            ;;(display "post:")(display post)(newline)
            (pathfind kchar post)))))

  (or (use-potion? kchar)
      (if (any-visible-hostiles? kchar)
          (try-to-use-ability)
          (goto-post))))

(define (post-guard kguard x y)
  (guard-set-post! (kobj-gob-data kguard)
                   (list x y))
  kguard)

(define (mk-halberdier)
  (bind
   (kern-char-arm-self
    (mk-stock-char
     "a guard" ;;.....name
     sp_human ;;.......species
     oc_warrior ;;.........occupation
     s_guard ;;........sprite
     faction-men ;;...faction
     'guard-ai ;;...............custom ai (optional)
     ;;...................container (and contents)
     (mk-chest
     nil
     (mk-contents (roll-to-add 100 "1"     t_halberd)
                  (roll-to-add 100 "1"     t_chain_coif)
                  (roll-to-add 100 "1"     t_armor_chain)
                  (roll-to-add 75  "1d2"   t_heal_potion)
                  (roll-to-add 75  "1d2"   t_mana_potion)
                  (roll-to-add 10  "1"     t_vas_mani_scroll)
                  (roll-to-add 10  "1"     t_in_an_scroll)
                  (roll-to-add 50  "1d5"   t_food)
                  ))
     nil ;;...............readied arms (in addition to container contents)
     nil ;;...............effects
     nil ;;...............conversation
     ))
   (guard-mk 'halberdier nil)))

(define (mk-crossbowman)
  (bind
   (kern-char-arm-self
    (mk-stock-char
     "a guard" ;;.....name
     sp_human ;;.......species
     oc_warrior ;;.........occupation
     s_guard ;;........sprite
     faction-men ;;...faction
     'guard-ai ;;...............custom ai (optional)
     ;;...................container (and contents)
     (mk-chest
     nil
     (mk-contents (roll-to-add 100 "1"     t_crossbow)
                  (roll-to-add 100 "1d100" t_bolt)
                  (roll-to-add 100 "1"     t_chain_coif)
                  (roll-to-add 100 "2"     t_dagger)
                  (roll-to-add 100 "1"     t_armor_chain)
                  (roll-to-add 75  "1d2"   t_heal_potion)
                  (roll-to-add 75  "1d2"   t_mana_potion)
                  (roll-to-add 10  "1"     t_vas_mani_scroll)
                  (roll-to-add 10  "1"     t_in_an_scroll)
                  (roll-to-add 50  "1d5"   t_food)
                  ))
     nil ;;...............readied arms (in addition to container contents)
     nil ;;...............effects
     nil ;;...............conversation
     ))
   (guard-mk 'crossbowman nil)))

;;----------------------------------------------------------------------------
;; Slimes
;;----------------------------------------------------------------------------

(define (mk-yellow-slime)
  (let ((slime
         (mk-stock-char
          " a yellow slime" ;;.name
          sp_yellow_slime ;;...species
          nil ;;...............occupation
          s_yellow_slime ;;....sprite
          faction-monster ;;...faction
          'yellow-slime-ai ;;..custom ai (optional)   
          nil ;;...............container (and contents)
          nil ;;...............readied arms (in addition to container contents)
          nil ;;...............effects
          nil ;;...............conversation
          )))
    (kern-obj-add-effect slime ef_poison_immunity nil)
    slime))

(define (mk-yellow-slime-verbose msg)
  (kern-log-msg msg)
  (mk-yellow-slime))

(define (mk-green-slime)
  (let ((slime 
         (mk-stock-char
          " a green slime" ;;...name
          sp_green_slime ;;.....species
          nil ;;................occupation
          s_slime ;;............sprite
          faction-monster ;;....faction
          nil ;;................custom ai (optional)   
          nil ;;................container (and contents)
          nil ;;................readied arms (in addition to container)
          nil ;;................effects
          nil ;;...............conversation
          )))
    (kern-obj-add-effect slime ef_slime_split nil)
    (kern-obj-add-effect slime ef_poison_immunity nil)
    slime))

(define (mk-green-slime-verbose msg)
  (kern-log-msg msg)
  (mk-green-slime))

(define (mk-wood-spider)
  (mk-stock-char
   " a wood spider" ;;..........name
   sp_spider ;;.................species
   nil ;;.......................occupation
   s_spider ;;..................sprite
   faction-wood-spider ;;.......faction
   'spider-ai ;;................custom ai (optional)   
   nil ;;.......................container (and contents)
   nil ;;.......................readied arms (in addition to container)
   nil ;;.......................effects
   nil ;;...............conversation
   )
  )

(define (mk-queen-spider)
  (mk-stock-char
   " a queen spider" ;;.........name
   sp_queen_spider ;;...........species
   nil ;;.......................occupation
   s_queen_spider ;;............sprite
   faction-wood-spider ;;.......faction
   'spider-ai ;;................custom ai (optional)   
   nil ;;.......................container (and contents)
   nil ;;.......................readied arms (in addition to container)
   nil ;;........................effects
   nil ;;...............conversation
   )
  )

(define (mk-bandit)
  (bind
   (kern-char-arm-self
    (mk-stock-char
     " a bandit" ;;......name
     sp_human ;;.........species
     oc_wrogue ;;........occupation
     s_brigand ;;........sprite
     faction-outlaw ;;...faction
     nil ; 'bandit-ai ;;.......custom ai (optional)
    
     ;;..................container (and contents, used to arm char)
     (mk-chest
      'spike-trap
      (mk-contents 
       (roll-to-add 50  "1"    t_heal_potion)
       (roll-to-add 75  "1"    t_dagger)
       (roll-to-add 50  "1"    t_sword)
       (roll-to-add 10  "1d2"  t_oil)
       (roll-to-add 50  "1"    t_mace)
       (roll-to-add 50  "1d20" t_bolt)
       (roll-to-add 25  "1"    t_crossbow)
       (roll-to-add 75  "1"    t_shield)
       (roll-to-add 90  "1"    t_armor_leather)
       (roll-to-add 90  "1"    t_leather_helm)
       (roll-to-add 100 "1d10" t_gold_coins)
       (roll-to-add 50  "1d3"  t_picklock)
       ))
     
     nil ;;...............readied arms (in addition to container contents)
     nil ;;...............effects
     nil ;;...............conversation
     ))
   (npcg-mk 'bandit)))

(define (mk-troll)
  (bind
   (mk-stock-char
    "a troll " ;;................name
    sp_troll ;;.................species
    oc_warrior ;;.................occupation
    s_troll ;;..................sprite
    faction-hill-troll ;;.......faction
    'troll-ai ;;................custom ai (optional)
    
    ;;.......container (and contents, used to arm char)
    (mk-chest nil 
              (mk-contents
               (roll-to-add 100 "1d3"    t_thrown_boulder)
               (roll-to-add 25  "1"      t_mace)
               (roll-to-add 100 "1d25-1" t_gold_coins)
               ))
    
    nil ;;......................readied arms (in addition to container)
    nil ;;......................effects
    nil ;;...............conversation
    )
   (npcg-mk 'troll)))

(define (mk-gint)
  (kern-char-arm-self
   (mk-stock-char
    "gint" ;;................name
    sp_gint ;;.................species
    oc_warrior ;;.................occupation
    s_ettin ;;..................sprite
    faction-gint ;;.......faction
    nil ;;................custom ai (optional)
    
    ;;.......container (and contents, used to arm char)
    (mk-chest nil 
              (mk-contents
               (roll-to-add 50  "1"      t_2h_axe)
               (roll-to-add 50  "1"      t_2h_sword)
               (roll-to-add 50  "1"      t_morning_star)
               (roll-to-add 50  "1"      t_halberd)
               (roll-to-add 100 "4d25"   t_gold_coins)
               (roll-to-add 100 "1d5"    t_food)
               ))
    
    nil ;;......................readied arms (in addition to container)
    nil ;;......................effects
    nil ;;...............conversation
    )))
  
;;----------------------------------------------------------------------------
;; Animals
;;----------------------------------------------------------------------------
(define (mk-bull)
  (mk-animal "bull" sp_bull s_bull))
  

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
