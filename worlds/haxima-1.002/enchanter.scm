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
(define (mk-quest) (list #f #f #f))
(define (quest-offered? qst) (car qst))
(define (quest-accepted? qst) (cadr qst))
(define (quest-done? qst) (caddr qst))
(define (quest-offered! qst val) (set-car! qst val))
(define (quest-accepted! qst val) (set-car! (cdr qst) val))
(define (quest-done! qst val) (set-car! (cddr qst) val))

(define (enchanter-mk)
  (list #f 
        (mk-quest)
        (mk-quest)))
(define (ench-met? gob) (car gob))
(define (ench-first-quest gob) (cadr gob))
(define (ench-second-quest gob) (caddr gob))
(define (ench-met! gob val) (set-car! gob val))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Enchanter is, well, the Enchanter. He should give the player several special
;; quests. The first quest is to find a thief who has stolen something (he's
;; not specific about what), this is the CrOOAK quest which I was going to do
;; as a standalone episode.
;;----------------------------------------------------------------------------
(define (conv-hail knpc kpc)
  (let ((ench (kobj-gob-data knpc)))

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
      (say knpc "Good for you! First, keep my rune. "
           "If you encounter any of the other Wise ask them of it. "
           "Try the Alchemist near Oparine. "
           "Although obscenely greedy, "
           "he knows things even I do not.")
      (prompt-for-key)
      (say knpc "Second, seek the other runes. "
           "I know not their full number nor their resting places. "
           "One, I know, hangs about the neck of the Warritrix. "
           "Seek her in Glasdrin.")
      (prompt-for-key)
      (say knpc "Third, find out what you can of the Accursed. "
           "They grow bold. I sense a new leadership is guiding them. "
           "And beware! For they surely know of you by know.")
      (prompt-for-key)
      (say knpc "Well, get to it! Come back when you have the other Runes.")
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
            (kern-conv-end))))


    (define (finish-first-quest)
      (say knpc "Ah, I see you found my Rune!")
      (kern-obj-add-gold kpc 200)
      (kern-char-add-experience kpc 100)
      (say knpc "Perhaps your are not completely useless. "
           "Did you encounter any... resistance?")      
      (kern-conv-get-yes-no? kpc)
      (say knpc "The Accursed were behind this theft, why I do not know. But "
           "if they are seeking this rune, they are probably seeking the "
           "others as well. And their intentions, whatever they are, will "
           "not be good. We must stop them. Will you help?")
      (quest-offered! (ench-second-quest ench) #t)
      (if (kern-conv-get-yes-no? kpc)
          (start-second-quest)
          (second-quest-spurned)))

    (define (check-second-quest)
      ;; FIXME: need more better stuff here...
      (say knpc "I see the Accursed haven't killed you yet. "
           "I guess that's something."))

    (define (check-first-quest)
      (if (in-inventory? kpc t_rune_k)
          (finish-first-quest)
          (say knpc "Hmph. I see you still haven't found my item yet. "
               "[He mutters something about Wanderers and thieves]")))
      
    (if (ench-met? ench)
        (if (quest-done? (ench-second-quest ench))
            (say knpc "Welcome, friend of the Wise")
            (if (quest-offered? (ench-second-quest ench))
                (if (quest-accepted? (ench-second-quest ench))
                    (check-second-quest)
                    (offer-second-quest-again))
                (if (quest-offered? (ench-first-quest ench))
                    (check-first-quest)
                    (say knpc "Yes, what is it this time?"))))
        (say knpc "[This ageless mage looks unsurprised to see you] "
             "I was wondering when you would get here, Wanderer. "
             "It took you long enough!")
        (ench-met! #t))))

(define (conv-name knpc kpc)
  (say knpc "I am known as the Enchanter."))

(define (conv-job knpc kpc)
  (say knpc "I help as I can in the struggle against evil."))

(define (conv-default knpc kpc)
  (say knpc "I cannot help you with that"))

(define (conv-bye knpc kpc)
  (say knpc "Beware the Accursed!"))

(define (conv-join knpc kpc)
  (say knpc "No, I belong here. Seek the Warritrix if you desire a powerful "
       "companion."))


(define (conv-warr knpc kpc)
  (say knpc "The Warritrix is Wise and fierce, "
       "and like yourself prone to Wandering. "
       "In fact, at the moment I don't know where she is. "
       "Try Glasdrin."))

(define (conv-wand knpc kpc)
  (say knpc "Yes, I've met your type before. Unpredictable. "
       "And as to whether you are good or evil, that depends upon you."))

(define (conv-offer-first-quest knpc kpc)
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
                         "Trigrave. Go there and inquire if anyone has seen "
                         "the thief.")
                    (quest-accepted! (ench-first-quest (kobj-gob-data knpc)
                                                           #t)))
                  ;; no -- player is not willing
                  (say knpc "Perhaps I misjudged you.")))
            ;; no -- player is not ready
            (say knpc "It is not enough to speak of doing good, one cannot "
                 "BE good without DOING good.")))
      ;; no -- player does not intend to do good
      (say knpc "We shall see. Evil men can do good without meaning to, "
           "and men who would be callous find they can't ignore their "
           "conscience.")))

(define (conv-good knpc kpc)
  (if (quest-accepted? (ench-first-quest (kobj-gob-data knpc)))
      (say knpc "The wicked flee when no one pursues, but the righteous are "
           "bold as dragons.")
      (conv-offer-first-quest knpc kpc)))

(define (conv-gate knpc kpc)
  (say knpc "There are many gates in the land which connect to "
       "one another and appear with the moons. "
       "But the Shrine Gate is the only one I know of that connects with "
       "other worlds."))

(define (conv-wise knpc kpc)
  (say knpc "The Wise are the most powerful Warriors, Wizards, Wrights and "
       "Wrogues in the land. Although they function to protect the Shard, "
       "they are not all good."))

(define (conv-accu knpc kpc)
  (say knpc "The Accursed are an evil secret society, responsible for many "
       "crimes and atrocities. With the destruction of Absalot I thought "
       "they were finished. I was wrong. I fear now their number and "
       "strength is greater than ever before."))

(define (conv-moon knpc kpc)
  (say knpc "Ask Kalcifax the Traveler of moongates. "
       "He is quite the expert."))

(define (conv-shri knpc kpc)
  (say knpc "The Shrine Gate opens unpredictably, and only for a short time. "
       "Those who enter never return, "
       "and those who emerge are strangers and Wanderers like yourself."))

(define (conv-rune knpc kpc)
  (say knpc "The rune was passed to me by my master long ago. "
       "He did not know what it was for, and for all my research I never "
       "found its purpose, either. I decided it was an unimportant old relic. "
       "Why else would it not be mentioned in any of the arcane tomes or "
       "histories?"))

(define (conv-wiza knpc kpc)
  (say knpc "The Warrior, Wright and Wrogue all derive some power from their "
       "knowledge of the physical world. But a Wizard's power comes from his "
       "knowledge of the magical world."))

(define (conv-know knpc kpc)
  (say knpc "Just as a blind man cannot perceive colors, those without the "
       "Inner Eye cannot perceive the forces of magic. But those who have "
       "opened the Eye perceive causes and effects invisible to others."))

(define (conv-wrog knpc kpc)
  (say knpc "The Wisest of Wrogues is The MAN, who comes and goes as if on "
       "the wind. If the MAN has a home, it is well-hidden. Ask around, "
       "perhaps your enquiries will prompt a meeting."))

(define (conv-wrig knpc kpc)
  (say knpc "The Wisest Wright prefers to work in isolation. You may find him "
       "if your are persistent, but not in any city."))

(define (conv-necr knpc kpc)
  (say knpc "The most depraved and wicked of all the Wise, "
       "my nemesis the Necromancer abides somewhere in the underworld. "
       "He is powerful, deceitful and corrupt beyond redemption."))

(define (conv-alch knpc kpc)
  (say knpc "The Alchemist keeps a lab near Oparine. "
       "He is greedy and very cunning, so be wary of him."))

(define (conv-thie knpc kpc)
  (if (quest-done? (ench-first-quest (kobj-gob-data knpc)))
      (say knpc "Although a nuisance, he was only a middleman. "
           "I hope you did not treat him too harshly.")
      (say knpc "The thief who stole my item must be very clever. The rangers "
           "lost his trail in Trigrave. Inquire among everyone there if they "
           "have seen the THIEF."))

(define enchanter-conv
  (ifc nil
       (method 'default conv-default)
       (method 'hail conv-hail)
       (method 'name conv-name)
       (method 'bye conv-bye)
       (method 'job conv-job)
       (method 'join conv-join)
       
       (method 'accu conv-accu)
       (method 'alch conv-alch)
       (method 'evil conv-good)
       (method 'gate conv-gate)
       (method 'good conv-good)
       (method 'know conv-know)
       (method 'moon conv-moon)
       (method 'necr conv-necr)
       (method 'rune conv-rune)
       (method 'shri conv-shri)
       (method 'thie conv-thie)
       (method 'wand conv-wand)
       (method 'warr conv-warr)
       (method 'wise conv-wise)
       (method 'wiza conv-wiza)
       (method 'wrog conv-wrog)
       (method 'wrig conv-wrig)

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
