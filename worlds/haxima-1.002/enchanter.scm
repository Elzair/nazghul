;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------
(define enchanter-start-lvl 8)

;;----------------------------------------------------------------------------
;; Schedule
;;
;; The schedule below is for the place "Enchanter's Tower Ground Floor"
;; 
;; (The fact that he never visits the second floor of his own tower
;; should serve as an incentive for us to eventually get multi-place
;; schedules working  :-)
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_enchanter
               (list 0  0  enchtwr-ench-bed        "sleeping")
               (list 8  0  enchtwr-dining-room-2   "eating")
               (list 9  0  enchtwr-hall            "idle")
               (list 12 0  enchtwr-dining-room-2   "eating")
               (list 13 0  enchtwr-hall            "idle")
               (list 19 0  enchtwr-dining-room-2   "eating")
               (list 20 0  enchtwr-bedroom-1       "idle")
               )

;;----------------------------------------------------------------------------
;; Gob
;;
;; Quest flags, etc, go here.
;;----------------------------------------------------------------------------
(define (enchanter-mk)
  (list #f 
        (mk-quest) ;; get stolen rune
        (mk-quest) ;; learn the purpose of the runes
        (mk-quest) ;; get all runes
        (mk-quest) ;; open demon gate
        ))
(define (ench-met? gob) (car gob))
(define (ench-first-quest gob) (cadr gob))
(define (ench-second-quest gob) (caddr gob))
(define (ench-quest gob n) (list-ref gob n))
(define (ench-met! gob val) (set-car! gob val))

;;----------------------------------------------------------------------------
;; Conv
;;
;; The Enchanter is a powerful Mage, and one of the Wise.
;; He lives in the Enchanter's Tower.
;; He plays an important role in multiple stages of the main quest.
;;----------------------------------------------------------------------------
(define (ench-hail knpc kpc)
  (let ((ench (gob knpc)))

    ;; Fourth Quest -- open the demon gate
    (define (check-fourth-quest)
      (say knpc "Have you found the location of the Demon Gate?")
      (if (yes? kpc)
          (begin
            (say knpc "And have you found the locks which the Rune-keys will open?")
            (if (yes? kpc)
                (say knpc "So you are just fooling around, wasting time. I see.")
                (say knpc "I would expect them to appear as altars in a shrine. "
                     "The shrine may be hidden, perhaps revealing itself with a "
                     "password. Find and search the Accursed, "
                     "they must have had a clue.")))
          (say knpc "Search well the library at Absalot.")))

    ;; Third Quest -- find all the Runes
    (define (finish-third-quest)
      (say knpc "You have found all the Runes! "
           "Well, it couldn't have been all that hard. ")
      (prompt-for-key)
      (say knpc "I have one last task for you. "
           "You must find the Demon Gate and re-open it. "
           "Do you know where to find it?")
      (if (yes? kpc)
          (say knpc
               "I don't know what you will face, "
               "so prepare yourself well, "
               "and take anyone foolish enough to join you. "
               )
          (say knpc "The Accursed must have some idea, "
               "and the library of Absalot may yet hold some clue. "
               "Search well.")
          )
      (quest-done! (ench-quest ench 3) #t)
      (kern-char-add-experience kpc 100)
      (quest-accepted! (ench-quest ench 4) #t)      
      )

    (define (check-third-quest)
	(define (missing-only-s-rune?)
		(all-in-inventory? kpc
                         (filter (lambda (ktype)
                                   (not (eqv? ktype t_rune_s)))
                                 rune-types)))
	(cond			 
		((missing-only-s-rune?)
			(say knpc "You appear to have found most of the Runes now.")
		(prompt-for-key)
		(say knpc "Hmmm.")
		(prompt-for-key)
		(say knpc "I hesitated to raise this before, but it may be within your abilities after all.")
		(prompt-for-key)
		(say knpc "We know that the Accursed also seek the runes. We should consider ourselves fortunate if only one has fallen into their clutches.")
		(prompt-for-key)
		(say knpc "You will need to seek it in the very lair of our enemies: the ruins of Absalot.")
		(quest-data-assign-once 'questentry-rune-s)
		)
		
		((has-all-runes? kpc)
          (finish-third-quest)
          (say knpc "Return when you have found all the Runes. "
               "Consult with the other Wise, they may have clues about where to find the Runes.")
	       ))
	       )

    ;; Second Quest -- find out what the Runes are for
    (define (second-quest-spurned)
      (say knpc "It is the duty of all good men to stand up to evil. I "
           "don't have time for sluggards or cynics. Now give me my rune, "
           "take your reward and get out!")
      (kern-obj-remove-from-inventory kpc t_rune_k 1)
      (kern-obj-add-to-inventory knpc t_rune_k 1)
      (quest-accepted! (ench-second-quest ench) #f)
      (kern-conv-end))

    (define (start-second-quest)
      (quest-accepted! (ench-second-quest ench) #t)
      (say knpc "Good! First, keep my rune, and guard it well. ")
      (quest-data-update 'questentry-rune-k 'entrusted-with-rune 1)
      (prompt-for-key)
      (say knpc "Second, find the other Wise and ask them of the RUNE. "
           "You might start with the Alchemist near Oparine. "
           "Although obscenely greedy, "
           "he has devoted his life to the acquisition of secrets.")
      (quest-data-assign-once 'questentry-runeinfo)
      (quest-wise-subinit 'questentry-alchemist)
      (quest-wise-init)
      (kern-conv-end)
      )

    (define (offer-second-quest-again)
      (say knpc "You're back. Perhaps you've had an attack of conscience. "
           "It happens to the worst of us. "
           "Now, are you ready to help me thwart the Accursed?")
      (if (kern-conv-get-yes-no? kpc)
          (begin
            (kern-obj-remove-from-inventory knpc t_rune_k 1)
            (kern-obj-add-to-inventory kpc t_rune_k 1)
            (start-second-quest))
          (begin
            (say knpc "Like a pig to the trough, "
                 "a fool returns to his own folly. "
                 "Go back to filling your belly!")
            (kern-conv-end)))
      )
    
    (define (finish-second-quest)
      (say knpc "[He looks very grave] "
           "So my Rune is one of eight keys to the Demon Gate. "
           "Very well, you must find the rest. "
           "The Accursed have a head start on us. "
           "No doubt they already have some of the Runes. "
           "When you have found all the Runes return to me.")
      (quest-done! (ench-second-quest ench) #t)
      (quest-data-update 'questentry-runeinfo 'abe 1)
      (quest-data-update 'questentry-runeinfo 'keys 1)
      (quest-data-update 'questentry-runeinfo 'gate 1)
      (quest-data-update 'questentry-rune-k 'entrusted-with-rune 1)
      (quest-data-update-with 'questentry-runeinfo 'done 1 (grant-party-xp-fn 30))
      (quest-data-complete 'questentry-runeinfo)
      ;; temporary setup- will require information gathering first when done
      (prompt-for-key)
      (quest-data-assign-once 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-k 'questentry-allrunes)
      (quest-data-assign-once 'questentry-rune-k)
      (quest-data-convert-subquest 'questentry-rune-c 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-d 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-l 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-p 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-s 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-w 'questentry-allrunes)
      (quest-data-convert-subquest 'questentry-rune-f 'questentry-allrunes)
      (quest-accepted! (ench-quest ench 3) #t)
      )

    (define (check-second-quest)
      (say knpc "Have you learned what the Rune is for?")
      (if (yes? kpc)
          (begin
            (say knpc "Well, what?")
            (let ((reply (kern-conv-get-reply kpc)))
              (if (or (equal? reply 'demo) (equal? reply 'gate) (equal? reply 'key))
                  (finish-second-quest)
                  (begin
                    (say knpc "I don't think so. Have you asked all of the Wise about the ^c+mrune^c-?")
                    (if (yes? kpc)
                        (say knpc "Surely one of them must have given you some clue!")
                        (say knpc "Seek them all."))))))
          (say knpc "Ask all the Wise about the ^c+mrune^c-.")))

    ;; First Quest -- find the stolen Rune
    (define (finish-first-quest)
      (say knpc "Ah, I see you've found my Rune at last!")
      (kern-obj-add-gold kpc 200)
		(quest-data-update-with 'questentry-thiefrune 'done 1 (grant-party-xp-fn 20))
		(quest-data-complete 'questentry-thiefrune)
      (say knpc "Perhaps your are not completely useless. "
           "Did you encounter any... resistance?")      
      (kern-conv-get-yes-no? kpc)
      (say knpc "The Accursed were behind this theft. "
           "We must find out what the Rune is for. "
           "Will you help?")
      (quest-offered! (ench-second-quest ench) #t)
      (if (kern-conv-get-yes-no? kpc)
          (start-second-quest)
          (second-quest-spurned)))

    (define (check-first-quest)
      (if (in-inventory? kpc t_rune_k)
          (finish-first-quest)
          (say knpc "Hmph. I see you still haven't found my item yet!"
               " [He mutters something about Wanderers and Rogues]")
            ))
    
    ;; Main
    (if (ench-met? ench)
        (if (quest-done? (ench-quest ench 4))
            (say knpc "Welcome, friend of the Wise")
            (if (quest-accepted? (ench-quest ench 4))
                (check-fourth-quest)
                (if (quest-accepted? (ench-quest ench 3))
                    (check-third-quest)
                    (if (quest-offered? (ench-second-quest ench))
                        (if (quest-accepted? (ench-second-quest ench))
                            (check-second-quest)
                            (offer-second-quest-again))
                        (if (quest-accepted? (ench-first-quest ench))
                            (check-first-quest)
                            (say knpc "Yes, what is it this time?"))))))
        (begin
          (quest-data-update-with 'questentry-calltoarms 'talked 1 (quest-notify (grant-xp-fn 10)))
          (kern-log-msg "This ageless mage looks unsurprised to see you.")
          (say knpc "I was wondering when you would get here. "
               "It took you long enough!")
          (ench-met! ench #t)))))

(define (ench-name knpc kpc)
  (say knpc "I am known as the Enchanter."))

(define (ench-job knpc kpc)
  (say knpc "I help as I can in the struggle against evil."))

(define (ench-default knpc kpc)
  (say knpc "I cannot help you with that"))

(define (ench-bye knpc kpc)
  (say knpc "Beware the Accursed!"))

(define (ench-join knpc kpc)
  (say knpc "No, I belong here. Seek the Warritrix if you desire a powerful "
       "companion."))


(define (ench-warr knpc kpc)
  (say knpc "The Warritrix is Wise and fierce, "
       "and like yourself prone to Wandering. "
       "In fact, at the moment I don't know where she is. "
       "Try Glasdrin.")
       (quest-wise-subinit 'questentry-warritrix)
       (quest-data-update 'questentry-warritrix 'general-loc 1)
       )

(define (ench-wand knpc kpc)
  (say knpc "Yes, I've met your type before. Unpredictable. "
       "And as to whether you are good or evil, that depends upon you."))

(define (ench-offer-first-quest knpc kpc)
  (say knpc "I do not quibble over definitions of good and evil. "
       "They are easily recognized when encountered. "
       "Do you intend to do good while you are here?")
  (if (kern-conv-get-yes-no? kpc)
      ;; yes - player intends to do good
      (begin
        (say knpc "Then you will find me to be your ally. "
             "But beware! Many who claim to be good are not, "
             "or fail when put to the test. Are you ready to be tested?")
        (if (kern-conv-get-yes-no? kpc)
            ;; yes - player is ready to be tested
            (begin
              (say knpc "Very well. An item was recently stolen from me. "
                   "I need someone to find the thief, "
                   "recover the item and return it to me. Are you willing?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes -- player is willing
                  (begin
                    (say knpc "Good! Rangers have tracked the thief to "
                         "Trigrave. Go there and inquire about a ^c+mthief^c-.")
							(quest-data-assign-once 'questentry-thiefrune)
              		(quest-data-complete 'questentry-calltoarms)
                  	;; if you dont read the letter, you might not get the quest till now!
              		(quest-data-assign-once 'questentry-calltoarms)
                  	(quest-data-update-with 'questentry-calltoarms 'done 1 (grant-xp-fn 10))
                    (quest-accepted! (ench-first-quest (gob knpc)) #t)
                    )
                  ;; no -- player is not willing
                  (say knpc "Perhaps I misjudged you.")))
            ;; no -- player is not ready
            (say knpc "It is not enough to speak of doing good, one cannot "
                 "BE good without DOING good.")))
      ;; no -- player does not intend to do good
      (say knpc "We shall see. Evil men can do good without meaning to, "
           "and men who would be callous find they can't ignore their "
           "conscience.")))

(define (ench-good knpc kpc)
  (if (quest-accepted? (ench-first-quest (gob knpc)))
      (say knpc "The wicked flee when no one pursues, but the righteous are "
           "bold as dragons.")
      (ench-offer-first-quest knpc kpc)))

(define (ench-gate knpc kpc)
  (say knpc "There are many gates in the land which connect to "
       "one another and appear with the moons. "
       "But the Shrine Gate is the only one I know of for certain that connects with "
       "other worlds."))

(define (ench-wise knpc kpc)
  (say knpc "The Wise are the most powerful Warriors, Wizards, Wrights and "
       "Wrogues in the land. Although they function to protect the Shard, "
       "they are not all good."))

(define (ench-accu knpc kpc)
  (say knpc "The Accursed are an evil secret society, responsible for many "
       "crimes and atrocities. With the destruction of Absalot I thought "
       "they were finished. I was wrong. I fear now their number and "
       "strength is greater than ever before."))

(define (ench-moon knpc kpc)
  (say knpc "Ask Kalcifax the Traveler of moongates. "
       "She is quite the expert."))

(define (ench-shri knpc kpc)
  (say knpc "The Shrine Gate opens unpredictably, and only for a short time. "
       "Those who enter never return, "
       "and those who emerge are strangers and Wanderers like yourself."))

(define (ench-rune knpc kpc)
  (say knpc "The rune was passed to me by my master long ago. "
       "He did not know what it was for, and for all my research I never "
       "found its purpose, either. I decided it was an unimportant old relic. "
       "Why else would it not be mentioned in any of the arcane tomes or "
       "histories?"))

(define (ench-wiza knpc kpc)
  (say knpc "The Warrior, Wright and Wrogue all derive some power from their "
       "knowledge of the physical world. But a Wizard's power comes from his "
       "knowledge of the magical world."))

(define (ench-know knpc kpc)
  (say knpc "Just as a blind man cannot perceive colors, those without the "
       "Inner Eye cannot perceive the forces of magic. But those who have "
       "opened the Eye perceive causes and effects invisible to others."))

(define (ench-wrog knpc kpc)
  (say knpc "The Wisest of Wrogues is The MAN, who comes and goes as if on "
       "the wind. If the MAN has a home, it is well-hidden. Ask around, "
       "perhaps your inquiries will prompt a meeting.")
       (quest-wise-subinit 'questentry-the-man)
       (quest-data-update 'questentry-the-man 'common 1)
       )

(define (ench-wrig knpc kpc)
  (say knpc "The Wisest Wright prefers to work in isolation. You may find him "
       "if you are persistent, but not in any city. "
       "Seek the mage Kalcifax, I think she knows the Engineer well.")
       (quest-wise-subinit 'questentry-engineer)
       (quest-data-update 'questentry-engineer 'kalcifax 1)
       )

(define (ench-necr knpc kpc)
  (say knpc "The most depraved and wicked of all the Wise, "
       "my nemesis the Necromancer abides somewhere in the underworld. "
       "He is powerful, deceitful and corrupt beyond redemption.")
       (quest-wise-subinit 'questentry-necromancer)
       (quest-data-update 'questentry-necromancer 'general-loc 1)
      )

(define (ench-alch knpc kpc)
  (say knpc "The Alchemist keeps a lab near Oparine. "
       "He is greedy and very cunning, so be wary of him.")
       (quest-wise-subinit 'questentry-alchemist)
       (quest-data-update 'questentry-alchemist 'oparine 1)
       )

(define (ench-thie knpc kpc)
	;;in case quest generated once in progress
	(quest-data-assign-once 'questentry-thiefrune)
  (if (quest-done? (ench-first-quest (gob knpc)))
      (say knpc "Although a nuisance, he was only a middleman. "
           "I hope you did not treat him too harshly.")
      (say knpc "The thief who stole my item must be very clever. The rangers "
           "lost his trail in Trigrave. Inquire among everyone there if they "
           "have seen the ^c+mthief^c-.")))

(define (ench-kalc knpc kpc)
  (say knpc "Kalcifax? She's rather hard to keep track of I'm afraid."))

(define (ench-demo knpc kpc)
  (say knpc "The Demon Gate is a legendary gate that wizards of old used to cross into other worlds. "
       "Then, for reasons that vary in the telling, it was lost or sealed or forgotten or destroyed. "
       "I always thought it a fiction."))

(define (ench-ench knpc kpc)
  (say knpc "Yes?"))

(define enchanter-conv
  (ifc basic-conv
       (method 'default ench-default)
       (method 'hail ench-hail)
       (method 'name ench-name)
       (method 'bye ench-bye)
       (method 'job ench-job)
       (method 'join ench-join)
       
       (method 'ench ench-ench)
       (method 'accu ench-accu)
       (method 'alch ench-alch)
       (method 'evil ench-good)
       (method 'gate ench-gate)
       (method 'good ench-good)
       (method 'know ench-know)
       (method 'moon ench-moon)
       (method 'necr ench-necr)
       (method 'rune ench-rune)
       (method 'shri ench-shri)
       (method 'thie ench-thie)
       (method 'wand ench-wand)
       (method 'warr ench-warr)
       (method 'wise ench-wise)
       (method 'wiza ench-wiza)
       (method 'wrog ench-wrog)
       (method 'wrig ench-wrig)
       (method 'emgi ench-wrig)
       (method 'kalc ench-kalc)
       (method 'demo ench-demo)
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
     s_old_mage ;;..sprite
     ;;(mk-composite-sprite (list s_hum_body s_hum_belt s_hum_beard))
     faction-men ;;..faction
     0 ;;...........custom strength modifier
     7 ;;...........custom intelligence modifier
     0 ;;...........custom dexterity modifier
     10 ;;............custom base hp modifier
     2 ;;............custom hp multiplier (per-level)
     20 ;;............custom base mp modifier
     5 ;;............custom mp multiplier (per-level)
     max-health ;;..current hit points
     -1  ;;...........current experience points
     max-health ;;..current magic points
     0
     enchanter-start-lvl  ;;..current level
     #f ;;...........dead?
     'enchanter-conv ;;...conversation (optional)
     sch_enchanter ;;.....schedule (optional)
     'townsman-ai ;;..........custom ai (optional)
     ;;..............container (and contents)
     (mk-inventory
      (list
       (list 10  t_food)
       (list 100 t_arrow)
       (list 1   t_bow)
       (list 1   t_dagger)
       (list 1   t_sword)
       (list 1   t_leather_helm)
       (list 1   t_armor_leather)
       (list 5   t_torch)
       (list 5   t_cure_potion)
       (list 5   t_heal_potion)
       ))
     nil ;;.........readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (enchanter-mk)))
