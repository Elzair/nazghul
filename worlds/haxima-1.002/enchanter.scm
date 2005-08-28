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
  (list #f #f #f))

(define (ench-met? gob) (car gob))
(define (ench-quest? gob) (cadr gob))
(define (ench-quest-done? gob) (caddr gob))

(define (ench-met! gob val) (set-car! gob val))
(define (ench-quest! gob val) (set-car! (cdr gob) val))
(define (ench-quest-done! gob val) (set-car! (cddr gob) val))

;;----------------------------------------------------------------------------
;; Conv
;;
;; Enchanter is, well, the Enchanter. He should give the player several special
;; quests. The first quest is to find a thief who has stolen something (he's
;; not specific about what), this is the CrOOAK quest which I was going to do
;; as a standalone episode.
;;----------------------------------------------------------------------------
(define (conv-update-quest knpc kpc)
  (if (kern-obj-has? kpc t_wise_wizard_rune)
      (begin
        (say knpc "[He sags with relief] You have recovered my Rune! I "
             "cannot begin to thank you enough. If that had fallen into the "
             "wrong hands... it is unthinkable. I owe you a very great debt.")
        (kern-obj-remove-from-inventory kpc t_wise_wizard_rune 1)
        (kern-obj-add-to-inventory kpc t_gold 500))
      (say knpc "[He looks worried] No luck finding my stolen item yet, I "
           "see. Keep searching. It is very important.")))

(define (conv-hail knpc kpc)
  (let ((ench (kobj-gob-data knpc)))
    (if (ench-met? ench)
        (if (ench-quest? ench)
            (if (ench-quest-done? ench)
                (say knpc "Welcome back, friend of the Wise.")
                (conv-update-quest knpc kpc))
            (say knpc "Welcome back, Wanderer."))
        (begin
          (say knpc "[This ageless mage looks a bit startled to see you] "
               "A Wanderer! I haven't met one of your kind in ages... "
               "this is most unexpected... [he recovers his composure] "
               "Forgive me! And welcome, Wanderer.")
          (ench-met! #t)))))

(define (conv-name knpc kpc)
  (say knpc "I am known as the Enchanter."))

(define (conv-job knpc kpc)
  (say knpc "I help as I can in the struggle against evil."))

(define (conv-default knpc kpc)
  (say knpc "I cannot help you with that"))

(define (conv-bye knpc kpc)
  (say knpc "Keep to the Way"))

(define (conv-join knpc kpc)
  (say knpc "No, I belong here. Seek the Warritrix if you desire a powerful "
       "companion."))


(define (conv-warr knpc kpc)
  (say knpc "The Warritrix is Wise and fierce, "
       "and like yourself prone to Wandering. "
       "In fact, at the moment I don't know where she is."))

(define (conv-wand knpc kpc)
  (say knpc "I know that you come from the Gate of the Shrine, "
       "and that you are from another dimension. "
       "But as to whether you are good or evil, that depends upon you."))

(define (conv-offer-quest knpc kpc)
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
              (say knpc "Very well. An item of extreme importance was recently "
                   "stolen from me. I need someone to find the thief, "
                   "recover the item and return it to me. Are you willing?")
              (if (kern-conv-get-yes-no? kpc)
                  ;; yes -- player is willing
                  (begin
                    (say knpc "Good! Rangers have tracked the thief south and "
                         "east to Trigrave. Go there and inquire if anyone has "
                         "seen the thief.")
                    (ench-quest! (kobj-gob-data knpc) #t))
                  ;; no -- player is not willing
                  (say knpc "Perhaps I misjudged you.")))
            ;; no -- player is not ready
            (say knpc "It is not enough to speak of doing good, one cannot "
                 "BE good without DOING good.")))
      ;; no -- player does not intend to do good
      (say knpc "We shall see. Evil men can do good without meaning to, "
           "and men who would be callous find they can't ignore their conscience. "
           "I sense that you are good, whether you know it or not.")))

(define (conv-good knpc kpc)
  (if (ench-quest? (kobj-gob-data knpc))
      (say knpc "The wicked flee when no one pursues, but the righteous are "
           "bold as dragons.")
      (conv-offer-quest knpc kpc)))

(define (conv-gate knpc kpc)
  (say knpc "There are many gates in the land which connect to "
       "one another and appear with the moons. "
       "But there are two gates that connect to other worlds: "
       "the Shrine Gate and the Demon Gate. "
       ))

(define (conv-migh knpc kpc)
  (say knpc "The Mighty are the most powerful Warriors, Wizards, Wrights and "
       "Wrogues in the land. There has always been two of each, one Wise and "
       "one Accursed."))

(define (conv-wise knpc kpc)
  (say knpc "The Wise seek the good of all."))

(define (conv-accu knpc kpc)
  (say knpc "The Accursed seek only to gratify themselves. They are powerful "
       "and dangerous enemies of the Wise."))

(define (conv-moon knpc kpc)
  (say knpc "Ask Kalcifax the Traveler of moongates. "
       "He is quite the expert."))

(define (conv-shri knpc kpc)
  (say knpc "The Shrine Gate opens unpredictably, and only for a short time. "
       "Those who enter never return, "
       "and those who emerge are strangers and Wanderers like yourself."))

(define (conv-demo knpc kpc)
  (say knpc "The Demon Gate opens on other worlds. "
       "The mighty wizards of old would visit them, "
       "and demons from the other worlds would visit ours. "
       "But the gate has been sealed."))

(define (conv-seal knpc kpc)
  (say knpc "One of the worlds through the Demon Gate began to conquer the others, "
       "using the gate to move its armies. "
       "The Shard would have suffered the same fate, "
       "but the Mighty of old shut the gate shut from our side and sealed it with eight locks. "
       "Now, none may pass."))

(define (conv-lock knpc kpc)
  (say knpc "Each lock on the Demon Gate was sealed with a Rune, "
       "each Rune was given to one and only one of the Mighty. "
       "They vowed to never reassamble the Runes in one place, "
       "so that the Gate could never be re-opened."))

(define (conv-rune knpc kpc)
  (say knpc "The Runes were passed down through generations. "
       "I know where some of them are, but not all."))

(define (conv-wiza knpc kpc)
  (say knpc "The Warrior, Wright and Wrogue all derive some power from their "
       "knowledge of the physical world. But a Wizard's power comes from his "
       "knowledge of the magical world."))

(define (conv-know knpc kpc)
  (say knpc "Just as a blind man cannot perceive colors, those without the "
       "Inner Eye cannot perceive the forces of magic. But those who have "
       "opened the Eye perceive causes and effects invisible to others."))

(define (conv-wrog knpc kpc)
  (say knpc "The Wisest of Wrogues is The MAN, who is known to sometimes "
       "frequent Glasdrin. I suggest you ask around there."))

(define (conv-wrig knpc kpc)
  (say knpc "The Wisest Wright prefers to work in isolation. You may find him "
       "if your are persistent, but not in any city."))

(define (conv-necr knpc kpc)
  (say knpc "The most evil of all the Accursed, my nemesis the Necromancer "
       "abides in the ruins of Absalot. He is powerful, deceitful and corrupt "
       "beyond redemption."))

(define (conv-alch knpc kpc)
  (say knpc "The Alchemist keeps a lab in the underworld. He is the least "
       "troublesome of the Accursed, but he is greedy and unafraid to sacrifice "
       "others to serve his own ends."))

(define (conv-assa knpc kpc)
  (say knpc "The Assassin is the Accursed Warrior. I don't know who or where he "
       "is. He strikes unexpectedly and disappears. A most bothersome fellow."))

(define (conv-rat knpc kpc)
  (say knpc "The Rat is the Accursed Wrogue. Although not very dangerous, "
       "he is a cunning, slippery and most inconvenient pest. Needless to say "
       "I don't know where he is."))

(define (conv-thie knpc kpc)
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
       (method 'assa conv-assa)
       (method 'demo conv-demo)
       (method 'evil conv-good)
       (method 'gate conv-gate)
       (method 'good conv-good)
       (method 'know conv-know)
       (method 'lock conv-lock)
       (method 'migh conv-migh)
       (method 'moon conv-moon)
       (method 'necr conv-necr)
       (method 'rat conv-rat)
       (method 'rune conv-rune)
       (method 'seal conv-seal)
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
