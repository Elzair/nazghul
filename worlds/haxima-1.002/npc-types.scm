;;----------------------------------------------------------------------------
;; NPC type constructors
;;
;; Dependencies:
;;  game.scm
;;  species.scm
;;  occ.scm
;;  containers.scm
;;  effects.scm
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

(define (max-hp sp occ lvl mod mult)
  (+ (kern-species-get-hp-mod sp)
     (if (null? occ) 0 (kern-occ-get-hp-mod occ))
     mod
     (* lvl
        (+ (kern-species-get-hp-mult sp)
           (if (null? occ) 0 (kern-occ-get-hp-mult occ))
           mult))))

(define (max-mp sp occ lvl mod mult)
  (+ (kern-species-get-mp-mod sp)
     (if (null? occ) 0 (kern-occ-get-mp-mod occ))
     mod
     (* lvl
        (+ (kern-species-get-mp-mult sp)
           (if (null? occ) 0 (kern-occ-get-mp-mult occ))
           mult))))
  

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
(define (mk-animal name species sprite faction)
  (mk-stock-char name species nil sprite faction nil nil nil nil))

(define (mk-readied-items . items)
  items)

(define (mk-bandit-gob)
  (list #f))

(define (mk-troll-gob)
  (list #f))

;;----------------------------------------------------------------------------
;; NPC Type Constructors
;;----------------------------------------------------------------------------

(define (mk-ranger)
  (kern-char-arm-self
   (mk-stock-char
    "a ranger" ;;........name
    sp_human ;;........species
    oc_archer ;;........occupation
    s_companion_ranger ;;............sprite
    faction-men ;;.....faction
    nil ;;..............custom ai (optional)
    
    ;;..................container (and contents, used to arm char)
    (mk-chest
     nil
     (mk-contents (roll-to-add 100 "1"     t_sword)
                  (roll-to-add 50  "1"     t_bow)
                  (roll-to-add 50  "1d20"  t_arrow)
                  (roll-to-add 100 "1"     t_leather_helm)
                  (roll-to-add 100 "1"     t_armor_leather)
                  (roll-to-add 100 "1d3-1" t_heal_potion)))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
    nil ;;...............conversation
    )))

(define (mk-goblin-raider)
  (kern-char-arm-self
   (mk-stock-char
    " an ork" ;;........name
    sp_goblin ;;........species
    oc_raider ;;........occupation
    s_orc ;;............sprite
    faction-orks ;;.....faction
    nil ;;..............custom ai (optional)
    
    ;;..................container (and contents, used to arm char)
    (mk-chest
     nil
     (mk-contents (roll-to-add 100 "1"     t_sword)
                  (roll-to-add 25  "1d3-1" t_spear)
                  (roll-to-add 75  "1"     t_shield)
                  (roll-to-add 50  "1"     t_leather_helm)
                  (roll-to-add 25  "1"     t_armor_leather)
                  (roll-to-add 100 "1d3-1" t_heal_potion)))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
    nil ;;...............conversation
    )))

(define (mk-goblin-hunter)
  (kern-char-arm-self
   (mk-stock-char
    " a goblin hunter" ;;........name
    sp_goblin ;;........species
    oc_archer ;;........occupation
    s_orc ;;............sprite
    faction-orks ;;.....faction
    nil ;;..............custom ai (optional)
    
    ;;..................container (and contents, used to arm char)
    (mk-chest
     nil
     (mk-contents (roll-to-add 95  "1"     t_dagger)
                  (roll-to-add 100 "1"     t_bow)
                  (roll-to-add 50  "1d20"  t_arrow)
                  (roll-to-add 25  "1"     t_leather_helm)
                  (roll-to-add 25  "1"     t_armor_leather)
                  (roll-to-add 75  "1d3-1" t_heal_potion)))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
    nil ;;...............conversation
    )))

(define (mk-skeletal-warrior)
  (kern-char-arm-self
   (mk-stock-char
    " a skeleton" ;;.....name
    sp_skeleton ;;.......species
    oc_raider ;;.........occupation
    s_skeleton ;;........sprite
    faction-monster ;;...faction
    nil ;;...............custom ai (optional)
    
    ;;...................container (and contents)
    (mk-chest
     nil
     (mk-contents (roll-to-add 25  "1"     t_2h_axe)
                  (roll-to-add 25  "1"     t_halberd)
                  (roll-to-add 50  "1"     t_sword)
                  (roll-to-add 25  "1"     t_2H_sword)
                  (roll-to-add 50  "1"     t_shield)
                  (roll-to-add 25  "1"     t_bow)
                  (roll-to-add 50  "1d20"  t_arrow)
                  (roll-to-add 50  "1"     t_iron_helm)
                  (roll-to-add 50  "1"     t_armor_leather)
                  (roll-to-add 25  "1"     t_armor_leather)
                  (roll-to-add 10  "1"     t_armor_chain)
                  (roll-to-add 2   "1"     t_armor_plate)))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
    nil ;;...............conversation
    )))

;; Death knights can use Vampiric Touch at L3 and Disease at L6
(define (use-potion? kchar)
  (or (and (wants-healing? kchar)
           (has-heal-potion? kchar)
           (drink-heal-potion kchar))
      (and (wants-mana? kchar)
           (has-mana-potion? kchar)
           (drink-mana-potion kchar))))

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
    oc_undead_warrior ;;.occupation
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

(define (mk-at-level ctor-tag lvl-dice . args)
  ;(display "mk-at-level args: ")(list args)(newline)
  (set-level (apply (eval ctor-tag) args) 
             (kern-dice-roll lvl-dice)))


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

(define (guard-ai kchar)
  (or (use-potion? kchar)
      (if (can-use-ability? disarm kchar)
          (let ((victims (get-hostiles-in-range kchar 1)))
            (and (not (null? victims))
                 (>= (kern-dice-roll "1d20") 16)
                 (or (use-ability disarm kchar (car victims))
                     #t)))
          #f)))

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
   'halberdier))

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
   'crossbowman))

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
     oc_bandit ;;........occupation
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
   (mk-bandit-gob)))

(define (mk-troll)
  (bind
   (mk-stock-char
    "a troll " ;;................name
    sp_troll ;;.................species
    oc_troll ;;.................occupation
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
   (mk-troll-gob)))


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
  (is-species? kchar sp_goblin))

(define (is-skeleton? kchar)
  (is-species? kchar sp_skeleton))

(define (is-death-knight? kchar)
  (and (is-species? kchar sp_skeleton)
       (is-occ? kchar oc_warrior)))

(define (is-bandit? kchar)
  (is-occ? kchar oc_bandit))

(define (is-halberdier? kchar)
  (eq? (kobj-gob-data kchar)
       'halberdier))

(define (is-crossbowman? kchar)
  (eq? (kobj-gob-data kchar)
       'crossbowman))
