;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_earl
               (list 0  0  trigrave-earls-bed        "sleeping")
               (list 5  0  trigrave-tavern-table-3a  "eating")
               (list 6  0  trigrave-earls-counter    "working")
               (list 12 0  trigrave-tavern-table-3a  "eating")
               (list 13 0  trigrave-earls-counter    "working")
               (list 18 0  trigrave-tavern-table-3a  "eating")
               (list 19 0  trigrave-tavern-hall      "idle")
               (list 20 0  trigrave-earls-room       "idle")
               (list 21 0  trigrave-earls-bed        "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (earl-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Earl is a merchant, and will trade with the player if he's at work. 
;; ...(removed some text which seems to have come from Jim.scm)...
;;----------------------------------------------------------------------------
(define earl-merch-msgs
  (list "Come by my shop when I'm open. It's the Dry Goods store in the southwest corner, open from 6:00AM to 6:00PM."
        "Here's what I have in stock right now."
        "Well, ok, let me see what you have to offer."
        "Step right up to the counter."
        "Tell 'em you got it at Earl's."
        "Just window-shopping? All right."
        "I'll put this in the back and clean it up later."
        "Come back later when you've got something else to offer, then."
        "Finished already? Suit yourself."
        "Let me know if there's anything else."
   ))

(define earl-catalog
  (list
   (list t_torch               5 "You don't want to run out of torches at the bottom of a dungeon.")
   (list t_sling              50 "The sling is just the thing for cheapskates who don't want to buy ammo.")
   (list t_staff              25 "A mage without a staff is like a dog without a bark.")
   
   (list t_heal_potion        22 "You want to keep plenty of these in your emergency kit.")
   (list t_cure_potion        22 "Headed north? You'll want some of these when travelling the Fens.")
   (list t_mana_potion        22 "This is just the thing when you're in a spot where you can't camp and your mages are exhausted.")
   
   (list t_arrow               1 "If you've got a bow then you can't have too many arrows.")
   (list t_bolt                1 "I've got the least expensive bolts anywhere around.")
   (list t_smoke_bomb          3 "Just throw these smoke bombs at enemy archers and they won't be able to see you.")
   
   (list t_shovel             50 "When you find that buried treasure you'll want a shovel to dig it up.")
   (list t_pick               50 "A pick is essential for busting up boulders that block your path.")
   
   (list t_sextant           500 "You can figure your location anywhere in the wilderness with one of these without wasting any reagents or scrolls.")
   (list t_chrono            300 "This little timepiece is invaluable when there aren't any clocks around.")
   (list t_grease             23 "Wrogues seem to like this stuff. I'm afraid to ask why.")
   ))

(define (earl-trade knpc kpc) (conv-trade knpc kpc "trade" earl-merch-msgs earl-catalog))

(define earl-conv
  (ifc trigrave-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "I forgot.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Welcome, stranger.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Oh, were we talking? Bye.")))
       (method 'job (lambda (knpc kpc) (say knpc "I keep the store. Need something?")
                            (if (kern-conv-get-yes-no? kpc)
                                (earl-trade knpc kpc)
                                (say knpc "Okay."))))
       (method 'name (lambda (knpc kpc) (say knpc "[He thinks for a minute] Earl! That's it!")))
       (method 'buy (lambda (knpc kpc) (conv-trade knpc kpc "buy"  earl-merch-msgs earl-catalog)))
       (method 'sell (lambda (knpc kpc) (conv-trade knpc kpc "sell"  earl-merch-msgs earl-catalog)))
       (method 'trad earl-trade)
       (method 'join (lambda (knpc kpc) (say knpc "You're too late! I forgot all my spells.")))

       (method 'batt
               (lambda (knpc kpc)
                 (say knpc "Yep. I fought with Lord Calvin against the "
                      "Goblin Horde!")))
       (method 'calv
               (lambda (knpc kpc)
                 (say knpc "Now there was a warlord! Calvin conquered "
                      "everything from the Gray Sea to the Northern Rim!")))
       (method 'hord
               (lambda (knpc kpc)
                 (say knpc "In those days the Goblins were united under one "
                      "chieftain, and threatened the whole Peninsula! By the "
                      "time Lord Calvin was done with them they were scattered "
                      "and hiding in the hills. They've never recovered!")))
       (method 'mage
               (lambda (knpc kpc)
                 (say knpc "I've forgotten all my magic. I even lost my wand! "
                      "I once knew spells that would slay whole armies.")))
       (method 'spel
               (lambda (knpc kpc) 
                 (say knpc "I was a battle mage once. Long ago.")))
       (method 'thie
               (lambda (knpc kpc)
                 (say knpc "I saw a stranger fleeing west! No, south! No... Oh, drat, I can't remember.")))
       ))
