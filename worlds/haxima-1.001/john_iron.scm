;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_john
               (list 0  0  trigrave-johns-bed        "sleeping")
               (list 6  0  trigrave-tavern-table-1a  "eating")
               (list 7  0  trigrave-forge            "working")
               (list 12 0  trigrave-tavern-table-1a  "eating")
               (list 7  0  trigrave-forge            "working")
               (list 18 0  trigrave-tavern-hall      "idle")
               (list 12 0  trigrave-johns-bed        "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (john-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; John is a merchant, and will trade with the player if he's at work. He's a
;; tall, wiry blacksmith with a very dry wit. If the town has a leader it would
;; be him because the other townsfolk respect him and look to him in times of
;; crises. He isn't interested in being a celebrity, however, and doesn't
;; exercise any real ambition. He's not interested in adventures and considers
;; (privately) that adventurers are fools. But he's happy to trade with
;; them. He drinks hard, and probably had a very wild youth.
;;----------------------------------------------------------------------------
(define (john-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by my shop when I'm open. I'm in the northeast corner.")
      (begin
        (kern-conv-trade knpc kpc
                         (list t_dagger         10)
                         (list short-sword      30)
                         (list t_longsword      60)
                         (list t_2H_sword       100)
                         (list t_mace           40)
                         (list t_mace_and_chain 80)
                         (list t_iron_helm      40)
                         (list t_armor_chain    100)
                         (list t_armor_plate    200)
                         )
        (say knpc "Go do some damage."))))

(define john-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Beyond my ken.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Well met.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Fare well.")))
       (method 'job (lambda (knpc kpc) (say knpc "I'm the blacksmith of Trigrave. Need something?")
                            (if (kern-conv-get-yes-no? kpc)
                                (john-trade knpc kpc)
                                (say knpc "Look around all you like."))))
       (method 'name (lambda (knpc kpc) (say knpc "I'm John Iron.")))
       (method 'trad john-trade)
       (method 'join (lambda (knpc kpc) (say knpc "Sorry, I like it here.")))

       (method 'iron (lambda (knpc kpc) (say knpc "I always wanted to be a shepherd, "
                                             "but with a name like Iron my fate was sealed. "
                                             "Not like my buddy Yewer.")))
       (method 'yewe (lambda (knpc kpc) (say knpc "Get it? A shepherd named... well, anyway... "
                                             "Chanticleer thought it was funny.")))
       (method 'chan (lambda (knpc kpc) (say knpc "Chanticleer's a bard that plays at the Lusty Jugs. "
                                             "He's quite a storyteller, but a bit of a rogue.")))

       ))
