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
(define (ench-hail knpc kpc)
  (let ((ench (gob knpc)))

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

    (define (need-to-find-accursed)
      (say knpc "Search Absalot well. They were there once, I am certain, and must have left a clue."))
                
    (define (has-found-accursed)
      (say knpc "Well done. Do you know why they want the Runes?")
      (if (yes? kpc)
          (begin
            (say knpc "Why?")
            (let ((why (kern-conv-get-reply kpc)))
              (if (or (eq? why 'demo)
                      (eq? why 'gate))
                  (begin
                    (say knpc "They want to re-open the Demon Gate. I see. This cannot be allowed. "
                         "The Runes will never be safe while the Accursed exist. ")
                    (prompt-for-key)
                    (say knpc "I have considered the matter, and see only one option. "
                         "You must destroy the Demon Gate itself. "
                         "Assemble whatever help you can find and seek the Gate. "
                         "I know not what you will find there, but I sense it is your destiny."))
                  (say knpc "That does not ring true. Keep seeking."))))
          (say knpc "You must find out. Search their quarters. Pretend to befriend them if you must.")))
            
    (define (need-to-find-runes)
      (say knpc "Have you found any more Runes?")
      (yes? kpc)
      (say knpc "Ask the other Wise. When you have collected them all return to me."))
      

    (define (has-runes)
      (say knpc "Have you found the Accursed yet?")
      (if (yes? kpc)
          (has-found-accursed)
          (need-to-find-accursed)))

    (define (check-second-quest)
      (if (or (has-all-runes? kpc)
              (missing-only-s-rune? kpc))
          (has-runes)
          (need-to-find-runes)))

    (define (check-first-quest)
      (if (in-inventory? kpc t_rune_k)
          (finish-first-quest)
          (say knpc "Hmph. I see you still haven't found my item yet!"
               " [He mutters something about Wanderers and thieves]")
            ))
      
    (if (ench-met? ench)
        (if (quest-done? (ench-second-quest ench))
            (say knpc "Welcome, friend of the Wise")
            (if (quest-offered? (ench-second-quest ench))
                (if (quest-accepted? (ench-second-quest ench))
                    (check-second-quest)
                    (offer-second-quest-again))
                (if (quest-accepted? (ench-first-quest ench))
                    (check-first-quest)
                    (say knpc "Yes, what is it this time?"))))
        (begin
          (kern-print "[This ageless mage looks unsurprised "
                      "to see you]")
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
       "Try Glasdrin."))

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
                         "Trigrave. Go there and inquire if anyone has seen "
                         "the thief.")
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
       "But the Shrine Gate is the only one I know of that connects with "
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
       "perhaps your enquiries will prompt a meeting."))

(define (ench-wrig knpc kpc)
  (say knpc "The Wisest Wright prefers to work in isolation. You may find him "
       "if you are persistent, but not in any city. "
       "Seek the mage Kalcifax, I think she knows the Engineer well."))

(define (ench-necr knpc kpc)
  (say knpc "The most depraved and wicked of all the Wise, "
       "my nemesis the Necromancer abides somewhere in the underworld. "
       "He is powerful, deceitful and corrupt beyond redemption."))

(define (ench-alch knpc kpc)
  (say knpc "The Alchemist keeps a lab near Oparine. "
       "He is greedy and very cunning, so be wary of him."))

(define (ench-thie knpc kpc)
  (if (quest-done? (ench-first-quest (gob knpc)))
      (say knpc "Although a nuisance, he was only a middleman. "
           "I hope you did not treat him too harshly.")
      (say knpc "The thief who stole my item must be very clever. The rangers "
           "lost his trail in Trigrave. Inquire among everyone there if they "
           "have seen the THIEF.")))

(define (ench-kalc knpc kpc)
  (say knpc "Kalcifax? She's rather hard to keep track of I'm afraid."))

(define enchanter-conv
  (ifc basic-conv
       (method 'default ench-default)
       (method 'hail ench-hail)
       (method 'name ench-name)
       (method 'bye ench-bye)
       (method 'job ench-job)
       (method 'join ench-join)
       
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
       (method 'kalc ench-kalc)

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
