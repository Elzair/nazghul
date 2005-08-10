;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define bill-start-lvl 3)

;;----------------------------------------------------------------------------
;; Schedule
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_bill
               (list 0  0  bole-bed-bill "sleeping")
               (list 6  0  bole-table-1  "idle")
               (list 7  0  bole-n-woods  "working")
               (list 12 0  bole-table-1  "eating")
               (list 13 0  bole-n-woods  "working")
               (list 18 0  bole-table-1  "eating")
               (list 19 0  bole-dining-hall "idle")
               (list 21 0  bole-bills-hut "idle")
               (list 22 0  bole-bed-bill "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (bill-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Bill is a ranger of the Fens. He camps at the Enchanter's Tower.
;;----------------------------------------------------------------------------
(define (bill-trade knpc kpc)
  (kern-conv-trade knpc kpc
                   (list t_torch 10)
                   (list t_arrow 2)
                   )
  (say knpc "If you need more I'll be around."))

(define (bill-goods knpc kpc)
  (say knpc "Want to buy some?")
  (if (kern-conv-get-yes-no? kpc)
      (bill-trade knpc kpc)
      (say knpc "Just ask if you ever do.")))

(define (bill-may knpc kpc)
  (say knpc "She runs the tavern in town an' is a good lady."))

(define (bill-lady knpc kpc)
  (say knpc "There's uh lady in town now. She's real pretty. But she has "
       "a big, mean boyfrien'."))

(define (bill-bole knpc kpc)
  (say knpc "Yeah, Ah like Bole. "
       "There's uh tavern, where Ah eat an' get shit-faced. "
       "An' muh house is there."))

(define (bill-wolves knpc kpc)
  (say knpc "I hafta wach out for them. "
       "They stays mostly inna south, away from town."))

(define (bill-scared knpc kpc)
  (say knpc "Ah wuz south and east, "
       "where's I hardly never go on account o' the wulves. "
       "There was a great big, old, dead oak. "
       "I sez to myself, that's a good-un! "
       "Well, no sooner had muh axe bit into the bark "
       "but it wakes up!"))

(define bill-conv
  (ifc nil
       (method 'default (lambda (knpc kpc) (say knpc "[He shrugs]")))
       (method 'hail (lambda (knpc kpc) (say knpc "Hi-dee.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Fare thee well.")))
       (method 'job (lambda (knpc kpc) 
                      (say knpc "I's a woodcutter.")))
       (method 'name (lambda (knpc kpc) (say knpc "Called Bill.")))
       (method 'join (lambda (knpc kpc) 
                       (say knpc "No, sirree! Yousa ass-kicker, "
                            "and I'sa woodcutter. Not a good match, no "
                            "offense.")))

       (method 'arro bill-goods)
       (method 'axe
               (lambda (knpc kpc)
                 (say knpc "It works better than muh knife on most trees.")))
       (method 'buy bill-trade)
       (method 'bole bill-bole)
       (method 'chop
               (lambda (knpc kpc)
                 (say knpc "Ah use muh axe.")))
       (method 'fore
               (lambda (knpc kpc)
                 (say knpc "Yeah, the forest is plumb full o' trees. "
                      "And wolves, too.")))
       (method 'haun
               (lambda (knpc kpc)
                 (say knpc "[He leans in close and whispers] "
                      "Thar's a haunted tree southeast in a nook o' the "
                      "hills. Scared the jinkins outta me! Har!")))
       (method 'jink bill-scared)
       (method 'ladi bill-lady)
       (method 'lady bill-lady)
       
       (method 'may bill-may)

       (method 'wood
               (lambda (knpc kpc)
                 (say knpc "I cuts down trees and chops 'em into firewood. "
                      "Sometimes Ah make torches or arrows, too. Um-hm.")))
       (method 'shit
               (lambda (knpc kpc)
                 (say knpc "Oops! Sorry. I's not supposed to say that. "
                      "May's alwiz tellin' me not tuh say words lak shit and "
                      "damn. Ladies don' lak that, you know.")))
       (method 'scar bill-scared)
       (method 'sell bill-trade)
       (method 'torc bill-goods)
       (method 'town bill-bole)
       (method 'trade bill-trade)
       (method 'tree
               (lambda (knpc kpc)
                 (say knpc "There be all kinds uh trees in this forest. "
                      "Why, t'other day I even came me across a haunted "
                      "tree!")))
       (method 'wake
               (lambda (knpc kpc)
                 (say knpc "Aye! That old dead oak came alive! "
                      "It bellered like a bull and two great eyes opened "
                      "und stared right at me! Uh jumped out of my britches!"
                      "I dropped muh axe and ran like nothing!")))
       (method 'wolv bill-wolves)
       (method 'wulv bill-wolves)
       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-bill)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_bill ;;......tag
     "Bill" ;;.......name
     sp_human ;;.....species
     nil ;;..........occupation
     s_townsman ;;...sprite
     faction-men ;;..faction
     0 ;;............custom strength modifier
     0 ;;............custom intelligence modifier
     0 ;;............custom dexterity modifier
     0 ;;............custom base hp modifier
     0 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     (max-hp sp_human nil bill-start-lvl 0 0) ;;..current hit points
     0  ;;...........current experience points
     (max-mp sp_human nil bill-start-lvl 0 0) ;;..current magic points
     bill-start-lvl  ;;..current level
     #f ;;...........dead?
     'bill-conv ;;...conversation (optional)
     sch_bill ;;.....schedule (optional)
     nil ;;..........custom ai (optional)

     ;;..............container (and contents)
     (mk-chest 
      nil ;;.........trap
      (mk-contents 
       (add-content 10  t_torch)
       (add-content 100 t_arrow)
       (add-content 1   t_2h_axe)
       ))
     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (bill-mk)))