;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Trigrave"
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_jim
               (list 0  0  trigrave-jims-bed        "sleeping")
               (list 6  0  trigrave-tavern-table-1a  "eating")
               (list 7  0  trigrave-forge            "working")
               (list 12 0  trigrave-tavern-table-1a  "eating")
               (list 13 0  trigrave-forge            "working")
               (list 18 0  trigrave-tavern-hall      "idle")
               (list 22 0  trigrave-jims-bed         "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (jim-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;
;; Jim is a merchant, and will trade with the player if he's at work. He's a
;; tall, wiry blacksmith with a very dry wit. If the town has a leader it would
;; be him because the other townsfolk respect him and look to him in times of
;; crises. He isn't interested in being a celebrity, however, and doesn't
;; exercise any real ambition. He's not interested in adventures and considers
;; (privately) that adventurers are fools. But he's happy to trade with
;; them. He drinks hard, and probably had a very wild youth.
;;----------------------------------------------------------------------------
(define jim-merch-msgs
  (list "Come by my shop when I'm open. It's the Iron Works in the northeast corner. I'm open for business from 7:00AM til 6:00PM."
        "Let me know if you see something that interests you."
        "I'll buy back some items for salvage."
        "Let me know if you see something that interests you."
        "Strike hard, first and for the good, friend."
        "Maybe some other time."
        "I can melt down or re-use parts of this."
        "Fine."
        "Strike hard, first and for the good, friend."
        "Fine."
   ))

(define jim-catalog
  (list
   (list t_dagger          40 "This is a good weapon for the non-fighting classes.")
   (list t_sword           80 "It's not fancy, but note the perfect balance on that sword.")
   (list t_axe             70 "An axe is necessary if you camp in the wild, but it can be tricky to keep ahold of in a fight.")
   (list t_mace            75 "The mace is a simple but effective weapon against lightly armoured foes.")
   
   (list t_2H_axe         240 "The battle-axe is made for cleaving armour and shields.")
   (list t_2H_sword       350 "If you are strong enough to wield it, a two-handed sword is an excellent offensive weapon.")
   
   (list t_chain_coif     110 "The chain coif will protect your neck from decapitating strokes.")
   (list t_iron_helm      160 "With an iron helm you can survive a direct hit from a mace.")
   (list t_armor_chain    300 "Chain armor will turn aside most blades and arrows.")
   (list t_armor_plate    600 "Although heavy, plate armour will protect you from all but the mightiest blows or armour-piercing tips.")
   
   (list t_shield          45 "A shield is vital for close-in combat.")
   
   (list t_spiked_helm    150 "The spiked helm is favored by those with a more... direct style.")
   (list t_spiked_shield  150 "A spiked shield augments the footsoldier's basic thrust-and-push with extra damage.")
   ))

(define (jim-trade knpc kpc) (conv-trade knpc kpc "trade" jim-merch-msgs jim-catalog))

(define jim-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "I know not.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Well met.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Fare well.")))
       (method 'job 
               (lambda (knpc kpc) 
                 (say knpc "I'm the blacksmith of Trigrave. Need something?")
                            (if (kern-conv-get-yes-no? kpc)
                                (jim-trade knpc kpc)
                                (say knpc "Look around all you like."))))
       (method 'name (lambda (knpc kpc) (say knpc "Folks call me Jim.")))
       (method 'buy (lambda (knpc kpc) (conv-trade knpc kpc "buy" jim-merch-msgs jim-catalog)))
       (method 'sell (lambda (knpc kpc) (conv-trade knpc kpc "sell" jim-merch-msgs jim-catalog)))
       (method 'trad jim-trade)
       (method 'join (lambda (knpc kpc) 
                       (say knpc "Here I make my stand, come what may.")))


       (method 'chan (lambda (knpc kpc)
                       (say knpc "Chanticleer the bard frequents the Trigrave "
                            "tavern. He knows much of the region.")))
       (method 'char 
               (lambda (knpc kpc)
                 (say knpc "The charcoal burner who lives in the woods "
                      "keeps my forges hot.")))
       (method 'earl
               (lambda (knpc kpc)
                 (say knpc "Earl is the shopkeeper for the general store. "
                      "He claims he was once a battle-mage.")))
       (method 'gwen
               (lambda (knpc kpc)
                 (say knpc "Gwen is our innkeeper. She is a beautiful but "
                      "mysterious woman.")))
       (method 'iron (lambda (knpc kpc)
                       (say knpc "The hills are loaded with ore, yet so many "
                            "battles have been fought in this valley that one "
                            "need not visit them to find scrap.")))
       (method 'shie
               (lambda (knpc kpc)
                 (say knpc "[he gives you a cold look] I have cast away the "
                      "shield which bears the emblem of Glasdrin. I'll "
                      "thank you to speak of this no more.")))
       (method 'thief
               (lambda (knpc kpc)
                 (say knpc "I haven't seen anyone odd. But ask Gwen, she speaks to many travelers.")))
       (method 'trig 
               (lambda (knpc kpc) 
                 (say knpc "Trigrave is not much to speak of.")))
       (method 'wood 
               (lambda (knpc kpc)
                 (say knpc "The deep wood is home to beasts and bandits. "
                      "You'll need short arms and light armour there, "
                      "for the thicket is a hindrance to heavy arms "
                      "and ranged weapons.")))
       
       ))
