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
;; Earl is a merchant, and will trade with the player if he's at work. He's a
;; tall, wiry blacksmith with a very dry wit. If the town has a leader it would
;; be him because the other townsfolk respect him and look to him in times of
;; crises. He isn't interested in being a celebrity, however, and doesn't
;; exercise any real ambition. He's not interested in adventures and considers
;; (privately) that adventurers are fools. But he's happy to trade with
;; them. He drinks hard, and probably had a very wild youth.
;;----------------------------------------------------------------------------
(define (earl-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by my shop when I'm open. "
           "It's the Dry Goods store in the southwest corner, "
           "open from 6:00AM to 6:00PM.")
      (begin
        (kern-conv-trade knpc kpc
                         (list t_torch              5)
                         (list t_heal_potion       22)
                         (list t_cure_potion       22)
                         (list t_mana_potion       22)
                         (list t_sling             50)
                         (list t_staff             90)
                         (list t_arrow             1)
                         (list t_shovel            50)
                         (list t_pick              50)
                         (list t_sextant           100)
                         (list t_chrono            75)
                         )
        (say knpc "Your life depends on your gear!"))))

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
       (method 'buy earl-trade)
       (method 'sell earl-trade)
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
