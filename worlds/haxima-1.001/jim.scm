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
(define (jim-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "Come by my shop when I'm open. "
           "It's the Iron Works in the northeast corner. "
           "I'm open for business from 7:00AM til 6:00PM.")
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
        (say knpc "Strike hard, first and for the good, friend."))))

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
       (method 'trad jim-trade)
       (method 'join (lambda (knpc kpc) 
                       (say knpc "Here I make my stand, come what may.")))


       (method 'bruc 
               (lambda (knpc kpc)
                 (say knpc "Lord Bruce of the west is a beast. He cares "
                      "for naught but his appetites; a great boar rooting "
                      "in the earth, despoiling all in his path.")))
       (method 'chan (lambda (knpc kpc)
                       (say knpc "Chanticleer the bard frequents the Trigrave "
                            "tavern. He knows much of the region.")))
       (method 'char 
               (lambda (knpc kpc)
                 (say knpc "The charcoal burner who lives in the woods "
                      "keeps my forges hot.")))
       (method 'clov 
               (lambda (knpc kpc)
                 (say knpc "Lord Clovis is a fiend who delights in cruelty. "
                      "Many have fled his domain to come here.")))
       (method 'cros 
               (lambda (knpc kpc) 
                 (say knpc "Roads converge on Trigrave from the north, "
                      "south east and west")))
       (method 'earl
               (lambda (knpc kpc)
                 (say knpc "Earl is the shopkeeper for the general store. "
                      "He claims he was once a battle-mage.")))
       (method 'east (lambda (knpc kpc)
                       (say knpc "The road east skirts the deep wood and "
                            "will carry you to the realm of the villian Lord "
                            "Clovis.")))
       (method 'ench 
               (lambda (knpc kpc)
                 (say knpc "The Enchanter to the north is among the Wise, "
                      "but he likes not guests. Speak to Chanticleer if "
                      "you would know more.")))
       (method 'fens (lambda (knpc kpc)
                       (say knpc "A vile bog which issues nightmare creatures "
                            "in a continual stream. Only the Enchanter keeps "
                            "them in check from his tower.")))
       (method 'froe 
               (lambda (knpc kpc)
                 (say knpc "Lord Froederick is the latest ruler of this "
                      "region in a long history of disputed rule. But we "
                      "are far to the north of his capital, and he is "
                      "old and weak, and forgetful of his realm.")))
       (method 'gwen
               (lambda (knpc kpc)
                 (say knpc "Gwen is our innkeeper. She is a beautiful but "
                      "mysterious woman. I understand she comes from a foreign "
                      "kingdom across the Grey Sea.")))
       (method 'iron (lambda (knpc kpc)
                       (say knpc "The hills are loaded with ore, yet so many "
                            "battles have been fought in this valley that one "
                            "need not visit them to find scrap.")))
       (method 'nort (lambda (knpc kpc)
                       (say knpc "The northern road will take you to the "
                            "shrine, and beyond that lie the Fens.")))
       (method 'shie
               (lambda (knpc kpc)
                 (say knpc "[he gives you a cold look] I have cast away the "
                      "shield which bears the emblem of Lord Bruce. I'll "
                      "thank you to speak of this no more.")))
       (method 'shri (lambda (knpc kpc)
                       (say knpc "Only the Wise remember it's purpose.")))
       (method 'sout 
               (lambda (knpc kpc)
                 (say knpc "The road south follows the river into the "
                      "heart of our Lord Froederick's realm. Eventually "
                      "it will touch the shores of the Gray Sea.")))
       (method 'trig 
               (lambda (knpc kpc) 
                 (say knpc "Trigrave is the crossroads of the "
                      "Three Corners region. This is a frontier town, "
                      "sourrounded by wild, supernatural savagery "
                      "on the one hand and brutish warlords on the other.")))
       (method 'west 
               (lambda (knpc kpc)
                 (say knpc "West the road leads through a country of "
                      "farms and groves, to the Orkish Hills. The pass "
                      "at Wivernscrosse has fallen at last to the forces "
                      "of Lord Bruce. War will descend again on the "
                      "Three Corners soon, and my forge will grow hungry "
                      "for charcoal and iron.")))
       (method 'wood 
               (lambda (knpc kpc)
                 (say knpc "The deep wood is home to beasts and bandits. "
                      "You'll need short arms and light armour there, "
                      "for the thicket is a hindrance to heavy arms "
                      "and ranged weapons.")))
       ))
