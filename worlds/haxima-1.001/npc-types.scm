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
                       arms)
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
   nil ;;..........conversation (optional)
   nil ;;..........schedule (optional)
   ai ;;...........custom ai (optional)
   container ;;....container (and contents)
   arms ;;.........readied arms (in addition to the container contents)
   nil ;;..........hooks in effect
   ))

;; Curried version of mk-stock-char for characters without an occupation, ai,
;; container or armamenets
(define (mk-animal name species sprite faction)
  (mk-stock-char name species nil sprite faction nil nil nil))

(define (mk-readied-items . items)
  items)

(define (mk-bandit-gob)
  (list #f))

(define (mk-troll-gob)
  (list #f))

;;----------------------------------------------------------------------------
;; NPC Type Constructors
;;----------------------------------------------------------------------------

(define (mk-orc-raider)
  (kern-char-arm-self
   (mk-stock-char
    " an ork" ;;........name
    sp_goblin ;;........species
    oc_raider ;;........occupation
    s_orc ;;............sprite
    faction-orks ;;.....faction
    nil ;;..............custom ai (optional)
    
    ;;..................container (and contents, used to arm char)
    (mk-small-wooden-chest 
     nil
     (mk-contents (roll-to-add 75  "1"     t_dagger)
                  (roll-to-add 50  "1"     t_hatchet)
                  (roll-to-add 25  "1d3-1" t_spear)
                  (roll-to-add 100 "1d2-1" t_mushroom)
                  (roll-to-add 50  "1"     t_shield_wooden_buckler)
                  (roll-to-add 25  "1"     t_bow)
                  (roll-to-add 50  "1d20"  t_arrow)
                  (roll-to-add 50  "1"     t_leather_helm)
                  (roll-to-add 25  "1"     t_armor_leather)
                  (roll-to-add 100 "1d3-1" heal-potion)))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
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
    (mk-small-wooden-chest 
     nil
     (mk-contents (roll-to-add 50  "1"     t_war_axe)
                  (roll-to-add 25  "1"     t_halberd)
                  (roll-to-add 50  "1"     t_longsword)
                  (roll-to-add 25  "1"     t_2H_sword)
                  (roll-to-add 50  "1"     t_sm_shield)
                  (roll-to-add 25  "1"     t_bow)
                  (roll-to-add 50  "1d20"  t_arrow)
                  (roll-to-add 50  "1"     t_iron_helm)
                  (roll-to-add 10  "1"     t_ancient_helm)
                  (roll-to-add 1   "1"     t_silver_crown)
                  (roll-to-add 50  "1"     t_armor_leather)
                  (roll-to-add 25  "1"     t_armor_l_stud)
                  (roll-to-add 10  "1"     t_armor_chain)
                  (roll-to-add 2   "1"     t_armor_plate)))

    nil ;;...............readied arms (in addition to container contents)
    nil ;;...............effects
    )))

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
          )))
    (kern-obj-add-effect slime ef_poison_immunity nil)
    slime))

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
          )))
    (kern-obj-add-effect slime ef_slime_split nil)
    (kern-obj-add-effect slime ef_poison_immunity nil)
    slime))

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
     'bandit-ai ;;.......custom ai (optional)
    
     ;;..................container (and contents, used to arm char)
     (mk-small-wooden-chest 
      'spike-trap
      (mk-contents 
       (roll-to-add 50  "1"    heal-potion)
       (roll-to-add 75  "1"    t_dagger)
       (roll-to-add 50  "1"    short-sword)
       (roll-to-add 10  "1d2"  t_oil)
       (roll-to-add 50  "1"    t_mace)
       (roll-to-add 50  "1d20" t_bolt)
       (roll-to-add 25  "1"    t_xbow)
       (roll-to-add 75  "1"    t_sm_shield)
       (roll-to-add 90  "1"    t_armor_leather)
       (roll-to-add 90  "1"    t_leather_helm)
       (roll-to-add 100 "1d10" t_gold_coins)
       (roll-to-add 50  "1d3"  t_picklock)
       ))
     
     nil ;;...............readied arms (in addition to container contents)
     nil ;;...............effects
     ))
   (mk-bandit-gob)))

(define (mk-troll)
  (display "mk-troll")(newline)
  (bind
   (mk-stock-char
    "a troll " ;;................name
    sp_troll ;;.................species
    oc_troll ;;.................occupation
    s_troll ;;..................sprite
    faction-hill-troll ;;.......faction
    'troll-ai ;;................custom ai (optional)
    
    ;;..........................container (and contents, used to arm char)
    (mk-small-wooden-chest nil nil)
    
    nil ;;......................readied arms (in addition to container)
    nil ;;......................effects
    )
   (mk-troll-gob)))
