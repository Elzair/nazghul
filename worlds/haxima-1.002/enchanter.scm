;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define enchanter-start-lvl 8)

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Enchanter's Tower Ground Floor"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_enchanter
               (list 0  0  enchtwr-bedroom-1       "sleeping")
               (list 8  0  enchtwr-dining-room-2   "eating")
               (list 9  0  enchtwr-workshop        "idle")
               (list 12 0  enchtwr-dining-room-2   "eating")
               (list 13 0  enchtwr-workshop        "idle")
               (list 19 0  enchtwr-dining-room-2   "eating")
               (list 20 0  enchtwr-garden          "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (enchanter-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Enchanter is, well, the Enchanter. He should give the player several special
;; quests. The first quest is to find a thief who has stolen something (he's
;; not specific about what), this is the CrOOAK quest which I was going to do
;; as a standalone episode.
;;----------------------------------------------------------------------------
(define (conv-trade knpc kpc)
  (kern-conv-trade knpc kpc

                   ;; reagents
                   (list ginseng                 4)
                   (list garlic                  4)
                   (list blood_moss              8)
                   (list nightshade              32)

                   ;; potions
                   (list t_heal_potion              30)
                   (list t_cure_potion       30)
                   (list t_poison_immunity_potion 60)

                   )
  (say knpc "Now don't play with magic you don't understand."))

(define (conv-hail knpc kpc)
  (say knpc "[You meet a mage with a grim visage] Yes?"))

(define (conv-name knpc kpc)
  (say knpc "I am the Enchanter. "
       "Do you have some business with me, outworlder?"))

(define (conv-job knpc kpc)
  (say knpc "Many! I am the Watcher of the North, the Shield against the "
       "Fens, the Master of this Tower, a Tutor to Wizard Adepts, a Merchant "
       "of Magical Accessories, Horticulturist, author of Historical "
       "Romances (under a pen-name) and I sit on the Council of "
       "the Wise."))

(define (conv-default knpc kpc)
  (say knpc "I cannot help you with that"))

(define (conv-bye knpc kpc)
  (say knpc "Keep to the Way"))

(define (conv-join knpc kpc)
  (say knpc "I don't think so. Will there be anything else?"))

(define (conv-outw knpc kpc)
  (say knpc "I know you are fresh from the Gate of the Shrine. "
       "I have dealt with your kind before."))

(define enchanter-conv
  (ifc nil
       (method 'default conv-default)
       (method 'hail conv-hail)
       (method 'bye conv-bye)
       (method 'job conv-job)
       (method 'join conv-join)

       (method 'acce conv-trade)
       (method 'buy conv-trade)
       (method 'sell conv-trade)
       (method 'trad conv-trade)
       (method 'magi conv-trade)
       (method 'merc conv-trade)

       (method 'auth conv-auth)
       (method 'accu conv-accu)
       (method 'adep conv-adep)
       (method 'farm conv-mush)
       (method 'fens conv-fens)
       (method 'hist conv-auth)
       (methof 'hort conv-hort)
       (method 'mast conv-mast)
       (method 'mush conv-mush)
       (method 'nort conv-nort)
       (method 'roma conv-auth)
       (method 'shar conv-shar)
       (method 'shri conv-shri)
       (method 'sund conv-sund)
       (method 'shie conv-fens)
       (method 'towe conv-mast)
       (method 'tuto conv-tuto)
       (method 'warl conv-warl)
       (method 'watc conv-nort)
       (method 'wiza conv-wiza)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-enchanter-first-time tag)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     tag ;;..........tag
     "Enchanter" ;;.......name
     sp_human ;;.....species
     oc_wizard ;;.. .occupation
     s_companion_wizard ;;..sprite
     faction-men ;;..faction
     0 ;;...........custom strength modifier
     5 ;;...........custom intelligence modifier
     0 ;;...........custom dexterity modifier
     10 ;;............custom base hp modifier
     2 ;;............custom hp multiplier (per-level)
     20 ;;............custom base mp modifier
     5 ;;............custom mp multiplier (per-level)
     (max-hp sp_human oc_wizard enchanter-start-lvl 0 0) ;;..current hit points
     0  ;;...........current experience points
     (max-mp sp_human oc_wizard enchanter-start-lvl 0 0) ;;..current magic points
     enchanter-start-lvl  ;;..current level
     #f ;;...........dead?
     'enchanter-conv ;;...conversation (optional)
     sch_enchanter ;;.....schedule (optional)
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
   (enchanter-mk)))
