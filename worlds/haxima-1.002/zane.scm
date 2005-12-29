;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define zane-start-lvl 8)

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Enchanter's Tower Ground Floor"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_zane
               (list 0  0  enchtwr-zane-bed        "sleeping")
               (list 6  0  enchtwr-campsite        "idle")
               (list 8  0  enchtwr-dining-room-1   "eating")
               (list 9  0  enchtwr-campsite        "idle")
               (list 12 0  enchtwr-dining-room-1   "eating")
               (list 13 0  enchtwr-hall            "idle")
               (list 19 0  enchtwr-dining-room-1   "eating")
               (list 20 0  enchtwr-dining-room     "idle")
               (list 21 0  enchtwr-campsite        "idle")
               (list 22 0  enchtwr-zane-bed        "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (zane-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Zane is a ranger of the Fens. He camps at the Enchanter's Tower.
;;----------------------------------------------------------------------------
(define (zane-trade knpc kpc)
  (kern-conv-trade knpc kpc

                   ;; reagents
                   (list ginseng                 18)
                   (list garlic                  16)
                   (list blood_moss              32)
                   (list nightshade              48)

                   ;; potions
                   (list t_heal_potion            50)
                   (list t_cure_potion            44)
                   (list t_poison_immunity_potion 60)

                   )

  (say knpc "Watch your step out there."))

(define (zane-ench knpc kpc)
  (say knpc "Yeah, he's locked himself inside, see. "
       "He doesn't need people interrupting "
       "him all the time. If you're serious, you'll find a way in."))

(define (zane-fens knpc kpc)
  (say knpc "You're right in the middle of 'em, bub. They can be dangerous, "
       "so watch yourself."))
  
(define zane-conv
  (ifc ranger-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Ask somebody else.")))
       (method 'hail (lambda (knpc kpc) (say knpc "[He nods]")))
       (method 'bye (lambda (knpc kpc) (say knpc "Be seeing ya, buddy")))
       (method 'job (lambda (knpc kpc) 
                      (say knpc "I'm a Ranger. I patrol the Fens.")))
       (method 'name (lambda (knpc kpc) (say knpc "Zane.")))
       (method 'join (lambda (knpc kpc) 
                       (say knpc "Sorry, bub, I already got a job to do.")))
       (method 'ench zane-ench)
       (method 'fens zane-fens)
       (method 'dang
               (lambda (knpc kpc)
                 (say knpc "Poisonous and teeming with monsters. "
                      "You planning to spend much time in them?")
                 (if (kern-conv-get-yes-no? kpc)
                     (begin
                       (say knpc "You'll want the poison immunity spell. "
                            "You know it?")
                       (if (kern-conv-get-yes-no? kpc)
                           (say knpc "Good. I can sell you the fixin's.")
                           (say knpc "Mix Nightshade and Garlic and chant "
                                "Sanct Nox. I've got extra reagents I can "
                                "sell you.")
                           ))
                     (say knpc "Ok")
                     )))
       (method 'pois
               (lambda (knpc kpc)
                 (say knpc "A green potion or the An Nox spell will cure "
                      "poison.")))
       (method 'poti
               (lambda (knpc kpc)
                 (say knpc "I got some extra I can sell you.")))
       (method 'mons
               (lambda (knpc kpc)
                 (say knpc "Slimes, bandits and undead mostly.")))
       (method 'reag
               (lambda (knpc kpc)
                 (say knpc "I collect 'em where I can. "
                      "I can sell you my extras.")))
       (method 'buy zane-trade)
       (method 'sell zane-trade)
       (method 'trade zane-trade)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-zane-first-time tag)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     tag ;;..........tag
     "Zane" ;;.......name
     sp_human ;;.....species
     oc_ranger ;;.. .occupation
     s_companion_ranger ;;..sprite
     faction-men ;;..faction
     +1 ;;...........custom strength modifier
     0 ;;...........custom intelligence modifier
     +1 ;;...........custom dexterity modifier
     +1 ;;............custom base hp modifier
     +1 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     (max-hp sp_human oc_ranger zane-start-lvl 0 0) ;;..current hit points
     0  ;;...........current experience points
     (max-mp sp_human oc_ranger zane-start-lvl 0 0) ;;..current magic points
     zane-start-lvl  ;;..current level
     #f ;;...........dead?
     'zane-conv ;;...conversation (optional)
     sch_zane ;;.....schedule (optional)
     nil ;;..........custom ai (optional)

     ;;..............container (and contents)
     (mk-chest 
      'burn
      (mk-contents 
       (add-content 10  t_food)
       (add-content 100 t_arrow)
       (add-content 1   t_bow)
       (add-content 1   t_dagger)
       (add-content 1   t_sword)
       (add-content 1   t_leather_helm)
       (add-content 1   t_armor_leather)
       (add-content 5   t_torch)
       (add-content 5   t_cure_potion)
       (add-content 5   t_heal_potion)
       ))

     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (zane-mk)))
