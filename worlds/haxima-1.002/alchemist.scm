;;----------------------------------------------------------------------------
;; The Alchemist is numbered among the Wise, but he's not a very nice person.
;; He's very clever, very greedy and likes to trick people. He is also very
;; knowledgeable. He knows there is a rune buried in trigrave, and that the
;; enchanter knows what the runes are for. He also knows that one of then
;; entrances to the MAN's hideout is in the northwest.
;;
;; The Alchemist would be very happy to obtain the blood of a hydra, dragon and
;; lich. He will also teach the player how to make potions for gold?
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Schedule
;; 
;; In Oparine
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_alch
               (list 0   0  alkemist-shop "idle")
               (list 2   0  alkemist-bed  "sleeping")
               (list 8   0  bilge-water-seat-9   "eating")
               (list 9   0  alkemist-shop "working")
               (list 12  0  bilge-water-seat-9 "eating")
               (list 13  0  alkemist-shop "working")
               (list 17  0  bilge-water-seat-9 "eating")
               (list 18  0  bilge-water-hall "idle")
               (list 19  0  sea-witch-shop   "idle")
               (list 20  0  alkemist-shop "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (alch-mk)
	(list
        (mk-quest) ;; dragon
        #f ;; lich
        #f ;; hydra
        ))


(define (alchq-dragon gob) (car gob))
(define (alchq-lich? gob) (cadr gob))
(define (alchq-hydra? gob) (caddr gob))
(define (alchq-lich! gob val) (set-car! (cdr gob) val))
(define (alchq-hydra! gob val) (set-car! (cddr gob) val))

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

(define alch-catalog
  (list
   (list t_heal_potion             18 "When you run out of Mani or mana in the pitch of battle, these will save your life!")
   (list t_cure_potion             18 "An Nox may be cheaper, but nothing works better than my cure potions!")
   (list t_mana_potion             18 "No other potions rejuvenate your mana like mine do!")
   
   (list t_poison_immunity_potion  18 "Prevention is better than a cure! With my immunity potions you need never fear poison again!")
   (list t_invisibility_potion    100 "One quaff of this and your enemies will never find you!")
   (list t_str_potion             999 "The strength of a troll will be yours with this potion!" )  ;; limited stock would be nice...
   (list t_dex_potion             999 "Your arrows will fly straight as truth when you drink this potion!")  ;; limited stock would be nice...
   (list t_int_potion             999 "The wise man seeks more wisdom! With this potion, it can be yours for a very reasonable price!")  ;; limited stock would be nice...
   (list t_info_potion            150 "The sage said to 'Know thyself'. This potion will help!")
   
   (list t_oil                      6 "Hurl fire at your foes! Protect your flanks or cover your retreat with flaming fields of death!")
   (list t_slime_vial              25 "More fun than a barrel of monkeys! Confound your foes with a multiplying army of slimes!")
   ))

(define alch-merch-msgs
  (list "I'm afraid my shop is closed now. Come by between 9:00AM to 5:00PM."
        "I'm sure I have something you'll like! [He rubs his hands briskly]"
        "I sometimes buy used goods... at a discount of course."
        "Yes, let's get down to business!"
        "You'll be back for more when you see for yourself how good my potions are!"
        "I hope you don't regret passing up these fine potions."
        "I can probably find some use for these."
        "I doubt you'll find a better offer anywhere else."
        "A pleasure doing business with you!"
        "Perhaps next time."
        ))

;; Basics...
(define (alch-hail knpc kpc)
  (say knpc "[You meet a short, fat old man with a long nose] "
       "Hello and welcome, Traveler!"))

(define (alch-default knpc kpc)
  (say knpc "I'm afraid I can't help you with that."))

(define (alch-name knpc kpc)
  (say knpc "I'm known as the Alchemist.")
  (quest-data-update 'questentry-alchemist 'found 1)
  (quest-data-complete 'questentry-alchemist)
  )

(define (alch-join knpc kpc)
  (say knpc "Too busy! And far too old for adventures."))

(define (alch-job knpc kpc)
  (say knpc "I make potions, dabble in mysteries, that sort of thing. "
       "If you want to buy something just say so!"))

(define (alch-bye knpc kpc)
  (say knpc "Farewell! Come back again soon!"))

;; Trade...
(define (alch-trade knpc kpc) (conv-trade knpc kpc "trade" alch-merch-msgs alch-catalog))
(define (alch-buy knpc kpc) (conv-trade knpc kpc "buy" alch-merch-msgs alch-catalog))
(define (alch-sell knpc kpc) (conv-trade knpc kpc "sell" alch-merch-msgs alch-catalog))

;; Rune...
;; offered: shown k rune
;; accepted: sent to find p rune
;; done: known to have found p rune
(define (alch-dragon-reward knpc kpc)
  (say knpc "Oh, yes, the rune...")
	(prompt-for-key)
  (say knpc
	   "The paladins have built several fortifications in the "
	   "deeps of Kurpolis. One of the runes was buried in the "
	   "foundations of the deepest fort.")
	(prompt-for-key)
  (say knpc
	   "A pick and shovel may be enough to get it out again, "
	   "but it might be difficult with a dozen paladins breathing "
	   "down your neck.")
	   (quest-data-assign-once 'questentry-rune-p)
	   (quest-rune-p-update)
	   )
	   
(define (alch-dragon-done knpc kpc)
  (say knpc "I am afraid I dont know the locations of the other "
					"runes. Try asking the rest of the Wise."))
		
(define (alch-dragon-quest knpc kpc qstat)
	(if (kern-conv-get-yes-no? kpc)
		(cond
			((quest-done? qstat)
				(alch-dragon-done knpc kpc)
				)
			((in-inventory? kpc t_rune_p)
				(quest-done! qstat #t)
				(say knpc "I see you have collected the Rune of Power.")
				(alch-dragon-done knpc kpc)
				)
			((quest-accepted? qstat)
				(alch-dragon-reward knpc kpc qstat)
				)
			((in-inventory? kpc t_dragons_blood 1)
			  (begin
				(say knpc "I know where ones is buried, "
					 "and I'll tell you in exchange for that vial of "
					 "dragon's blood you're carrying. Deal?")
				(if (kern-conv-get-yes-no? kpc)
					(begin
						  (quest-accepted! qstat #t)
						  (kern-obj-remove-from-inventory kpc 
														  t_dragons_blood 
														  1)
						  (kern-obj-add-to-inventory knpc
													 t_dragons_blood
													 1)
						  (say knpc "[He eyes the vial hungrily] "
							   "Yes! It's just what I need!")
							(quest-data-update 'questentry-dragon 'done 1)
							(quest-data-complete 'questentry-dragon)
							(quest-data-assign-once 'questentry-dragon)
						  (alch-dragon-reward knpc kpc))
					(begin
						(say knpc "Well, I suppose if you dig up the "
						"whole Shard you'll someday find it without "
						"my help. Good luck!")
						(quest-data-assign-once 'questentry-dragon)
					))
				))
			(#t
				(say knpc "Then perhaps we can exchange favors. "
				   "I happen to know where one of these runes "
				   "is buried. I'll tell you its location if you "
				   "bring me a vial of dragon's blood. ")
				(quest-data-assign-once 'questentry-dragon)))
		(say knpc "Well, if you are, I happen to know where one is "
		   "hidden.")))

(define (alch-more knpc kpc)
	(let ((qstat (alchq-dragon (gob knpc))))
		(say knpc "Abe knows more about the runes themselves. Are you "
			"interested in finding the others?")
		(quest-data-update-with 'questentry-runeinfo 'abe 1 (quest-notify nil))
		(alch-dragon-quest knpc kpc qstat)
	))
		   
(define (alch-rune knpc kpc)
	(if (not (null? (quest-data-getvalue 'questentry-dragon 'rerune)))
		(alch-more knpc kpc)
		(begin
			(say knpc "[He gets a canny look] Runes, eh? I've seen a few in my time. "
			   "Have you one to look at?")
			(if (kern-conv-get-yes-no? kpc)
				(if (in-inventory? kpc t_rune_k 1)
					(begin
					  (say knpc "Yes, I see. This once belonged to the Enchanter, "
						   "I believe. I hope you didn't steal it! "
						   "I have seen several more like it, "
						   "but the person you really should speak to "
						   "is Abe.")
							(quest-data-update 'questentry-dragon 'rerune 1)
						   (quest-data-update-with 'questentry-runeinfo 'abe 1 (quest-notify nil))
					  )
					(say knpc "I don't see it. Perhaps you dropped it?"))
				(say knpc "I might be able to help if you could show me.")))
		))

(define (alch-abe knpc kpc)
  (say knpc "An old acquaintance of mine. "
       "Last I heard he was studying the ruins at Green Tower."))

(define (alch-drag knpc kpc)
  (say knpc "I've never done it personally, but if one wants to obtain some "
       "dragon's blood my understanding is that one must kill a dragon! "
       "I hear they're common as cows in the regions of the Fire Sea."
			)
	(quest-data-update-with 'questentry-dragon 'sea 1 (quest-notify nil)))


;; The Wise...
(define (alch-necr knpc kpc)
  (say knpc "The Necromancer is an old acquaintance of mine. "
       "Since the razing of Absalot he's retired to the underworld. "
       "'Tis a pity, but we've lost touch."))

(define (alch-ench knpc kpc)
  (say knpc "The Enchanter is a great and knowledgable Wizard. "
       "I'm afraid we don't always see eye-to-eye. "
       "Lately he's been pre-occupied with the Accursed."
       ))

(define (alch-man-reward knpc kpc)
	(say knpc "In the mountains of "
	   "the northeast, along their southern face, "
	   "there is a secret passage. It is near coordinates "
	   "[92 10]."))

(define (alch-man knpc kpc)
  (let ((qstat (gob knpc)))
	  (say knpc "I've never met the MAN. "
		   "Being the most accomplished of Wrogues, "
		   "he probably has a fortune stashed in his hideout. "
		   "If I were the adventurous type I might go seek it out myself. "
		   "What about you?")
	  (if (kern-conv-get-yes-no? kpc)
		(cond
			((alchq-hydra? qstat)
				(say knpc "I've heard, from a reliable source, of an entrance "
					"to the MAN's secret hideout.")
				(alch-man-reward knpc kpc))
			((in-inventory? kpc t_hydras_blood 1)
				(begin
					(say knpc "I've heard, from a reliable source, of an entrance "
						"to the MAN's secret hideout. I'll tell you in exchange "
						"for that vial of hydra's blood in your pack. Yes?")
					(if (kern-conv-get-yes-no? kpc)
						(begin
							(alchq-hydra! qstat #t)
							(kern-obj-remove-from-inventory kpc 
								t_hydras_blood 
								1)
							(kern-obj-add-to-inventory knpc
								t_hydras_blood
								1)
							(say knpc "[He fairly drools over the noxious stuff] "
								"Oh, lovely... lovely!")
							(say knpc "Ahem.")
							(alch-man-reward knpc kpc))
						(say knpc "'Tis a pity. You have no use for the "
							"vial, and I am too old to go treasure-hunting."))))
			(#t 
				(say knpc "Well, I do hear many things, many of which are only "
					"rumour. But a reliable source has told me of where to find "
					"an entrance to the MAN's hideout. If you bring me a vial of "
					"hydra's blood I'll disclose it to you.")))
	   (say knpc "For fie, Wanderer! "
			"I thought you were the adventurous type."))))

(define (alch-hydr knpc kpc)
  (say knpc "The hydra is a most difficult foe. "
       "I understand that striking them only increases their strength! "
       "But if you do succeed in killing one their blood is quite useful "
       "to the arcane arts."))

(define (alch-warr knpc kpc)
  (say knpc "On a few occasions I have met the Warritrix. "
       "Her ferocity is legendary, "
       "but I found her to be very calm and gracious in her demeanor. "
       "I understand she refused to take part in the destruction of "
       "Absalot."))

(define (alch-engi knpc kpc)
  (say knpc "I've never met the Engineer, "
       "I understand he is quite the recluse."))

(define (alch-alch knpc kpc)
  (say knpc "Yes, that's me. I am the Alchemist.")
  (quest-data-update 'questentry-alchemist 'found 1)
  (quest-data-complete 'questentry-alchemist)
  )


;; Absalot...
(define (alch-absa-reward knpc kpc)
	(say knpc "There was a fortress overlooking a river of fire. "
		 "Pity it wasn't manned, it might have turned Glasdrin's invasion. "
		 "It will make a perilous crossing if monsters have taken it over.")
	(prompt-for-key)
	(say knpc 
		 "There is, however, an older route that bypasses the fortress. "
		 "Probe the east wall of the first cavern, "
		 "you will find a hidden passage.")
	(prompt-for-key)
	(say knpc
		 "You will still need to cross the river of fire. "
		 "There is a statue upon the river. Speak the password 'ONUS' to pass unharmed. "
		 "Write that password down!")
	(prompt-for-key)
	(say knpc
		 "The passage rejoins the main route near the stairway which leads up to "
		 "the lost city. You won't escape all the hazards of the journey, "
		 "but it should make your life somewhat easier."))

(define (alch-absa knpc kpc)
  (let ((qstat (gob knpc)))
    (say knpc "The passage to Absalot was always dangerous even when it was maintained. "
         "You wouldn't happen to be thinking of going there?")
    (if (kern-conv-get-yes-no? kpc)
        (cond
         ((alchq-lich? qstat)
          (alch-absa-reward knpc kpc))
         ((in-inventory? kpc t_lichs_blood 1)
          (say knpc "In exchange for that vial of lich's blood I'd be "
               "happy to tell you of a back door. What do you say?")
          (if (kern-conv-get-yes-no? kpc)
              (begin
                (alchq-lich! qstat #t)
                (kern-obj-remove-from-inventory kpc 
                                                t_lichs_blood 
                                                1)
                (kern-obj-add-to-inventory knpc
                                           t_lichs_blood
                                           1)
                (say knpc "[He grins and winks] Just the stuff I need!")
                (alch-absa-reward knpc kpc))
              (say knpc "I see. No doubt you have IMPORTANT plans for "
                   "that lich's blood. I can always get some from "
                   "another adventurer.")))
         (else
          (say knpc "Bring me a vial of lich's blood and I'll tell you "
               "a secret way.")))
        (say knpc "It's just a ruin now anyways. Everything was destroyed "
             "when it was sacked."))))

(define (alch-sack knpc kpc)
  (say knpc "Oh yes, didn't you know? Absalot was sacked by the armies "
       "of Glasdrin, Green Tower and Oparine. Destroyed for its wickedness, "
       "they say. [He chuckles without humour]"))

(define (alch-esca knpc kpc)
  (say knpc "Er... did I say that? I can't imagine why. Anyone who escaped "
       "from Absalot would have the death sentence on their head."))

(define (alch-wick knpc kpc)
  (say knpc "Yes, Absalot was so wicked that every man, woman and child "
       "who dwelt there had to be put to the sword. Lucky for us to have "
       "paladins willing to carry out this righteous work! "
       "[You detect a hint of irony in his raised eyebrows and innocent "
       "expression]"))

(define (alch-lich knpc kpc)
  (say knpc "A lich is an undead wizard. This foul thing corrupts all it "
       "touches and commands armies of the dead. Its blood has many uses "
       "in necromancy, which is not my specialty."))

;; The Accursed...
(define (alch-accu knpc kpc)
  (say knpc "The so-called Accursed are a secret society blamed for "
       "many heinous deeds, but who can say how much of that is rumour?"))

;; Townsfolk
(define (alch-lia knpc kpc)
  (say knpc "A bewitching creature! "
       "If I could, I would break her curse. In fact, I would do it for free. "
       "Call me an old fool!"))

(define alch-conv
  (ifc basic-conv
       (method 'default alch-default)
       (method 'hail alch-hail)
       (method 'bye alch-bye) 
       (method 'job alch-job)
       (method 'name alch-name)
       (method 'join alch-join)

       (method 'trad alch-trade)
       (method 'buy  alch-buy)
       (method 'sell alch-sell)
       (method 'poti alch-buy)

       (method 'rune alch-rune)
       (method 'more alch-more)
       (method 'drag alch-drag)

       (method 'necr alch-necr)
       (method 'ench alch-ench)
       (method 'man  alch-man)
       (method 'hydr alch-hydr)
       (method 'warr alch-warr)
       (method 'engi alch-engi)
       (method 'alch alch-alch)

       (method 'absa alch-absa)
       (method 'sack alch-sack)
       (method 'esca alch-esca)
       (method 'wick alch-wick)
       (method 'lich alch-lich)

       (method 'accu alch-accu)

       (method 'lia alch-lia)
       (method 'abe alch-abe)

       ))

;;----------------------------------------------------------------------------
;; First-time constructor
;;----------------------------------------------------------------------------
(define (mk-alchemist)
  (bind 
   (kern-char-arm-self
    (kern-mk-char 
     'ch_alchemist ;;.....tag
     "Alchemist" ;;.......name
     sp_human ;;.....species
     oc_wright ;;...occupation
     s_companion_tinker ;;......sprite
     faction-men ;;..faction
     0 ;;............custom strength modifier
     4 ;;............custom intelligence modifier
     1 ;;............custom dexterity modifier
     0 ;;............custom base hp modifier
     0 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     max-health ;;..current hit points
     -1  ;;...........current experience points
     max-health ;;..current magic points
     0
     8  ;;..current level
     #f ;;...........dead?
     'alch-conv ;;...conversation (optional)
     sch_alch ;;.....schedule (optional)
     'townsman-ai ;;..........custom ai (optional)
     nil ;;..........container (and contents)
     (list t_dagger
				t_armor_leather
				)  ;;......... readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (alch-mk)))
