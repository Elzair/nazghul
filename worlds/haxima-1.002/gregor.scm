;;----------------------------------------------------------------------------
;; gregor.scm - read-only data for Gregor the Charcoal Burner
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Schedule
;; 
;; At the shrine gate (moongate-clearing.scm)
;; His home is Gregor's Hut (gregors-hut.scm).
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_gregor
               (list 0  0  gh-gregors-bed   "sleeping")
               (list 6  0  gh-graveyard     "idle")
               (list 7  0  mgc-roadbend     "idle")
               (list 13 0  gh-table-2       "eating")
               (list 14 0  gh-pasture       "working")
               (list 17 0  gh-table-2       "eating")
               (list 18 0  gh-living-room   "idle")
               (list 20 0  gh-gregors-bed   "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
(define (gregor-mk) (list (mk-quest)))
(define (gregor-quest gob) (car gob))


;;----------------------------------------------------------------------------
;; Conv
;; 
;; Gregor is an elderly charcoal burner, living near the Shrine Gate.
;; He tends the grounds of the shrine, and takes care of his grandaughter Ilya.
;; 
;; Gregor is the first NPC which the player is likely to encounter,
;; and has a variety of helpful responses for the starting character
;; and the first-time player.
;;----------------------------------------------------------------------------

(define (gregor-hail knpc kpc)
  (if (in-inventory? kpc t_letter_from_enchanter)
      (say knpc "I see you got your stuff, and that letter from the ^c+rEnchanter^c-, eh?"
	   "Don't forget to ^c+rready^c- your weapons before leaving. "
           "It's ^c+rdangerous^c- out there!")
      (say knpc "[You meet a grizzled old peasant]"
           " Welcome, Wanderer. I've been watching for you."
           " There's some things that belong to you, over in yonder cave."
           " Go in where the ^c+rchest^c- is, ^c+ropen^c- it, and ^c+rget^c- the things inside."
	   " It's all for you.")))

;; Some prompting with initial commands:
;; 
;; Hmmm...perhaps it would be desirable to have game-UI promts
;; spoken out-of-character, so that the NPCs don't break the game fiction...
(define (gregor-open knpc kpc)
  (say knpc "Use the 'o' key to ^c+gOPEN^c- chests."))

(define (gregor-get knpc kpc)
  (say knpc "Use the 'g' key to ^c+gGET^c- stuff that's on the ground."))

(define (gregor-read knpc kpc)
  (say knpc "Use the 'r' key to ^c+gREADY^c- weapons or armor. "
       "You can't use a weapon in combat until it's ready."))

(define (gregor-camp knpc kpc)
  (say knpc "Use the 'k' key to ^c+gKAMP^c-, err...camp, in the wilderness and heal up."))


(define (gregor-dang knpc kpc)
  (say knpc "Very dangerous! If you need healing, a town inn is the safest place. "
       "You can ^c+rcamp^c- in the wilderness, but it's dangerous when you're alone and have no one to keep watch. "
       "Of course, there are spells and potions for healing, too."))

(define (gregor-dead knpc kpc)
  (say knpc "Aye, it's a shame. My daughter and her husband both - "
       "killed by trolls."))

(define (gregor-charcoal knpc kpc)
  (say knpc "I take charcoal into town and sell it, "
       "and some folks come by my place to buy it."))

(define (gregor-hut knpc kpc)
  (say knpc "My hut's in the forest to the South and East. "
       "Just myself and my granddaughter living there now."))

(define (gregor-ench knpc kpc)
  (say knpc "The ^c+rEnchanter^c-? "
       "He's one of the ^c+rWise^c-, and is a kind and noble soul. "
       "It is said that he forsaw the coming of a ^c+rWanderer^c-, such as yourself. "
       "He wishes to speak with you, and has asked me to direct you to him. "
       "Do you want directions? ")
  (cond ((yes? kpc)
	 (say knpc "He dwells in a tower in the middle of a terrible swamp.\n"
	      "\n"
	      "To get there, \n"
	      "  take the road ^c+gSouth^c- and ^c+gWest^c- \n"
	      "  to the keep that guards the pass.\n"
	      "\n"
	      "You shall meet rangers there, and soldiers tasked with guarding that keep.\n"
	      "\n"
	      "Through the pass, \n"
	      "  head ^c+gWest^c- and ^c+gNorth^c- \n"
	      "  to ^c+rTrigrave^c-.\n"
	      "\n"
	      "The journey from there, I'm afraid may be difficult -- fear the swamps!"))
	(else 
	 (say knpc "As you wish. If you should want directions later, ask me of the ^c+rEnchanter^c-.")
	 ))
)

(define (gregor-cave knpc kpc)
  (say knpc "There, that little trail that leads off the main path to the South and West. "
       "Follow it on in. ^c+rOpen^c- the chest. ^c+rGet^c- the stuff. "
       "Come back and we'll talk again, if you have more questions."))

(define (gregor-ches knpc kpc)
  (say knpc "Yes. ^c+rOpen^c- it! ^c+rGet^c- the stuff inside!"))

(define (gregor-stuf knpc kpc)
  (say knpc "The common folk made offerings of such things for you, knowing one day a Wanderer would come again."))

(define (gregor-leav knpc kpc)
  (say knpc "When you're ready to leave just follow the trail south and step off the map."))

(define (gregor-band knpc kpc)
  (let ((quest (gregor-quest (kobj-gob-data knpc))))
    (cond ((quest-accepted? quest)
           (say knpc "Have you found the bandits?")
           (cond ((yes? kpc)
                  (say knpc "The old gods be praised!")
                  (quest-done! quest #t)
                  )
                 (else 
                  (say knpc "Go to ^c+gGreen Tower^c- and ask around about the ^c+rbandits^c-.")
                  )))
          (else
           (say knpc "Bandits are in the woods. "
                "They robbed me in my own hut. "
                "I tried to fight them, "
                "and now I walk with a limp and a cane. "
                "The rangers would not help me. "
                "I have a granddaughter living with me now, "
                "and I am afraid of what they will do the next time they come... "
                "If it's not too much trouble, will you help me?")
           (cond ((yes? kpc)
                  (say knpc "Thank you. "
                       "When you get your equipment, go to ^c+gGreen Tower^c-. "
                       "Ask there about ^c+rbandits^c-. "
                       "Someone may know where to find them.")
                  (quest-accepted! quest #t)
                  )
                 (else
                  (say knpc "[He turns away sadly]")
                  (kern-conv-end)
                  ))))))

(define (gregor-bye knpc kpc)
  (let ((quest (gregor-quest (kobj-gob-data knpc))))
    (cond ((quest-accepted? quest)
           (say knpc "Farewell, and be careful."))
          (else
           (say knpc "Wait! Before you go, I have a favor to ask you.")
           (prompt-for-key)
           (gregor-band knpc kpc)
           ))))

(define gregor-conv
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Can't help you there.")))
       (method 'hail gregor-hail)
       (method 'heal (lambda (knpc kpc) (say knpc "[cough] Well enough, my granddaughter helps take care of me.")))
       (method 'bye gregor-bye)
       (method 'job (lambda (knpc kpc) (say knpc "I'm a charcoal burner. I also care for this ^c+rshrine^c-.")))
       (method 'join (lambda (knpc kpc) (say knpc "Nope. Already got a job.")))
       (method 'name (lambda (knpc kpc) (say knpc "Gregor's my name.")))

       (method 'open gregor-open)
       (method 'get  gregor-get)

       (method 'cave gregor-cave)
       (method 'ches gregor-ches)

       (method 'stuf gregor-stuf)
       (method 'equi gregor-stuf)  ;; A synonym
       (method 'gear gregor-stuf)  ;; A synonym

       (method 'read gregor-read)
       (method 'dang gregor-dang)

       (method 'camp gregor-camp)
       (method 'kamp gregor-camp) ;; A synonym

       (method 'band gregor-band)
       (method 'leav gregor-leav)

;; SAM -- This response seems to be shadowed by the gregor-band declaration above?
       (method 'band (lambda (knpc kpc) (say knpc "A band of rogues been raiding the shrine "
                                             "when I'm not around. They haven't attacked me, "
                                             "so they're probably just vagabonds, "
                                             "afraid of an old man's cudgel.")))

       (method 'char gregor-charcoal)
       (method 'burn gregor-charcoal)  ;; A synonym

       (method 'daug (lambda (knpc kpc) (say knpc "Aye, she was a near-witch like her mother. "
                                             "Had the knack, but not enough to be among the ^c+rWise^c-.")))
       (method 'dead gregor-dead)
       (method 'ench gregor-ench)
       (method 'folk (lambda (knpc kpc) (say knpc "There's homesteads scattered about in "
                                             "the woods and the foothills.")))
       (method 'fore (lambda (knpc kpc) (say knpc "Stay out of the deep woods. "
                                               "Bandits, spiders and worse live there.")))
       (method 'gate (lambda (knpc kpc) (say knpc "No one can predict when it will open, "
                                             "or if anything will come through if it does. "
                                             "I've heard of other gates in other parts of the land, "
                                             "and stories tell of others long forgotten now.")))
       (method 'gran (lambda (knpc kpc) (say knpc "I've a granddaughter name of Ilya.")))
       (method 'help (lambda (knpc kpc) (say knpc "There's always folks who need help. "
                                             "These are hard times in a hard land.")))
       (method 'hill (lambda (knpc kpc) (say knpc "Trolls are always a threat in the foothills, "
                                             "but more so of late.")))
       (method 'husb (lambda (knpc kpc) (say knpc "My son-in-law was a simple farmer. "
                                             "Why the trolls attacked I don't know. "
                                             "Maybe they were driven out of the hills "
                                             "by something else.")))
       (method 'hut gregor-hut)
       (method 'ilya (lambda (knpc kpc) (say knpc "Yep. She lives at my place now "
                                             "that her parents are dead.")))

;; SAM: I dont' see any reference to this lake anywhere, commenting this one out for now...
;;       (method 'lake (lambda (knpc kpc) (say knpc "Exit this shrine and ye'll find yourself in a "
;;                                             "hidden valley. Head south and you'll see the Gray Lake "
;;                                             "to the west.")))

       (method 'offe (lambda (knpc kpc) (say knpc "There in the ^c+rcave^c- you'll find a ^c+rchest^c-. "
                                             "Take what's inside. ^c+rWanderers^c- enter this world with little, "
                                             "and in the past some have done great good, "
                                             "so folks leave stuff in good will for the next one.")))
       (method 'pare gregor-dead)
       (method 'plac gregor-hut)

       (method 'shar (lambda (knpc kpc) (say knpc "The Shard?  That's what we call this land, ^c+rWanderer^c-.")))

       (method 'shri (lambda (knpc kpc) (say knpc "This shrine is for those who come through the gate. "
                                             "^c+rWanderers^c- like yourself. "
                                             "Folks leave simple ^c+rofferings^c- here to help you on "
                                             "your journey.")))

       (method 'spid (lambda (knpc kpc) (say knpc "Some of the spiders in the deep parts  "
					     "of the woods are monstrous --  "
					     " as big as oxen! "
					     "Children of ^c+rAngriss^c-, we call those.")))
       (method 'angr (lambda (knpc kpc) (say knpc "Just a legend. "
					     "Mother of all wood spiders. "
                                             "Scares the kids, keeps them out o' the woods "
                                             "where they can get lost.")))

       (method 'town (lambda (knpc kpc) (say knpc "^c+gTrigrave^c- is the closest town. "
					     "Follow the road South and you can't miss it.")))

       (method 'trol (lambda (knpc kpc) (say knpc "Trolls eat folks. "
					     "Even crack the bones and suck the marrow. "
					     "Nothing left to bury.")))

       (method 'wand (lambda (knpc kpc) (say knpc "We call those who come through the gate Wanderers. "
                                             "No one knows where they come from or where they go. "
                                             "You are the first to come through in a long, long time.")))

       (method 'wise (lambda (knpc kpc) (say knpc "The Wise are both strong and -mostly- good. "
                                             "They help the land, as they can, "
					     "and keep the ^c+rAccursed^c- at bay.")))
       (method 'accu (lambda (knpc kpc) 
                       (say knpc "The Accursed? Rumour has it they trade their souls for power. "
                            "If not for the ^c+rWise^c- they would overrun the ^c+rShard^c-.")))     

       (method 'witc (lambda (knpc kpc) (say knpc "Don't know of any witches in these parts any more.")))
       ))

;;----------------------------------------------------------------------------
;; Ctor
(define (mk-gregor)
  (bind 
   (kern-mk-char 'ch_gregor ; tag
                 "Gregor"              ; name
                 sp_human            ; species
                 nil                 ; occ
                 s_old_townsman          ; sprite
                 faction-men         ; starting alignment
                 0 10 5              ; str/int/dex
                 0 0                 ; hp mod/mult
                 0 0                 ; mp mod/mult
                 max-health -1 max-health 0 2  ; hp/xp/mp/AP_per_turn/lvl
                 #f                  ; dead
                 'gregor-conv        ; conv
                 sch_gregor          ; sched
                 'townsman-ai                 ; special ai
                 nil                 ; container
					   (list t_axe
					         t_armor_leather
					         )              ; readied
                 )
   (gregor-mk)
   ))
