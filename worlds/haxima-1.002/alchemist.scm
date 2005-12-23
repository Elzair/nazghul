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
(define (alch-mk) nil)

;;----------------------------------------------------------------------------
;; Conv
;;----------------------------------------------------------------------------

;; Basics...
(define (alch-hail knpc kpc)
  (say knpc "[You meet a short, fat old man with a long noise] "
       "Hello and welcome, Traveler!"))

(define (alch-default knpc kpc)
  (say knpc "I'm afraid I can't help you with that."))

(define (alch-name knpc kpc)
  (say knpc "I'm known as the Alchemist."))

(define (alch-join knpc kpc)
  (say knpc "Too busy! And far too old for adventures."))

(define (alch-job knpc kpc)
  (say knpc "I make potions, dabble in mysteries, that sort of thing. "
       "If you want to buy something just say so!"))

(define (alch-bye knpc kpc)
  (say knpc "Farewell! Come back again soon!"))

;; Trade...
(define (alch-trade knpc kpc)
  (if (not (string=? "working" (kern-obj-get-activity knpc)))
      (say knpc "My shop is open from 9:00AM to 5:00PM, "
           "come by then.")
      (begin
        (say knpc "I'm sure I have something you'll like! "
             "[He rubs his hands briskly]")
        (kern-conv-trade knpc kpc
                         (list t_heal_potion       50)
                         (list t_cure_potion       50)
                         (list t_mana_potion       50)
                         (list t_poison_immunity_potion 80)
                         (list t_inv_potion        200)
                         )
        (say knpc "Always carry plenty of potions!"))))

;; Rune...
(define (alch-rune knpc kpc)
  (say knpc "[He gets a canny look] Runes, eh? I've seen a few in my time. "
       "Have you one to look at?")
  (if (kern-conv-get-yes-no? kpc)
        (if (in-inventory? kpc t_rune_k 1)
            (begin
              (say knpc "Yes, I see. This once belonged to the Enchanter, "
                   "I believe. I hope you didn't steal it! "
                   "I have seen several more like it, "
                   "but the person you really should speak to "
                   "is Abe. Are you interested in finding the "
                   "other runes?")
              (if (kern-conv-get-yes-no? kpc)
                  (if (in-inventory? kpc t_dragons_blood 1)
                      (begin
                        (say knpc "I know where ones is buried, "
                             "and I'll tell you in exchange for that vial of "
                             "dragon's blood you're carrying. Deal?")
                        (if (kern-conv-get-yes-no? kpc)
                            (begin
                              (kern-obj-remove-from-inventory kpc 
                                                              t_dragons_blood 
                                                              1)
                              (kern-obj-add-to-inventory knpc
                                                         t_dragons_blood
                                                         1)
                              (say knpc "[He eyes the vial hungrily] "
                                   "Yes! It's just what I need! Oh, yes, "
                                   "the rune... search in Trigrave, in the "
                                   "southeast corner of town."))
                            (say knpc "Well, I suppose if you dig up the "
                                 "whole Shard you'll someday find it without "
                                 "my help. Good luck!")))
                      (say knpc "Then perhaps we can exchange favors. "
                           "I happen to know where one of these runes "
                           "is buried. I'll tell you its location if you "
                           "bring me a vial of dragon's blood. "))
                  (say knpc "Well, if you are, I happen to know where one is "
                       "hidden.")))
            (say knpc "I don't see it. Perhaps you dropped it?"))
        (say knpc "I might be able to help if you could show me.")))

(define (alch-abe knpc kpc)
  (say knpc "An old acquaintance of mine. "
       "Last I heard he was studying the ruins at Green Tower."))

(define (alch-drag knpc kpc)
  (say knpc "I've never done it personally, but if one wants to obtain some "
       "dragon's blood my understanding is that one must kill a dragon! "
       "I hear they're common as cows in the regions of the Fire Sea."
       ))


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

(define (alch-man knpc kpc)
  (say knpc "I've never met the MAN. "
       "Being the most accomplished of Wrogues, "
       "he probably has a fortune stashed in his hideout. "
       "If I were the adventurous type I might go seek it out myself. "
       "What about you?")
  (if (kern-conv-get-yes-no? kpc)
      (if (in-inventory? kpc t_hydras_blood 1)
          (begin
            (say knpc "I've heard, from a reliable source, of an entrance "
                 "to the MAN's secret hideout. I'll tell you in exchange "
                 "for that vial of hydra's blood in your pack. Yes?")
            (if (kern-conv-get-yes-no? kpc)
                (begin
                  (kern-obj-remove-from-inventory kpc 
                                                  t_hydras_blood 
                                                  1)
                  (kern-obj-add-to-inventory knpc
                                             t_hydras_blood
                                             1)
                  (say knpc "[He fairly drools over the noxious stuff] "
                       "Oh, lovely... lovely! Ahem. In the mountains of "
                       "the northwest, along their southern face, "
                       "there is a secret passage. It is near coordinates "
                       "[92 10]."))
                (say knpc "'Tis a pity. You have no use for the "
                     "vial, and I am too old to go treasure-hunting.")))
          (say knpc "Well, I do hear many things, many of which are only "
               "rumour. But a reliable source has told me of where to find "
               "an entrance to the MAN's hideout. If you bring me a vial of "
               "hydra's blood I'll disclose it to you."))
      (say knpc "For fie, Wanderer! "
           "I thought you were the adventurous type.")))

(define (alch-hydr knpc kpc)
  (say knpc "The hydra is a most difficult foe. "
       "I understand conventional weapons are useless, "
       "because striking them only increases their number! "
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
  (say knpc "Yes, that's me. I am the Alchemist."))


;; Absalot...
(define (alch-absa knpc kpc)
  (say knpc "The pass to Absalot is sealed. But I know another way in. "
       "You wouldn't happen to be thinking of going there?")
  (if (kern-conv-get-yes-no? kpc)
      (begin
        (if (in-inventory? kpc t_lichs_blood 1)
            (begin
              (say knpc "In exchange for that vial of lich's blood I'd be "
                   "happy to tell you of the back door. What do you say?")
              (if (kern-conv-get-yes-no? kpc)
                  (begin
                    (kern-obj-remove-from-inventory kpc 
                                                    t_lichs_blood 
                                                    1)
                    (kern-obj-add-to-inventory knpc
                                               t_lichs_blood
                                               1)
                    (say knpc "[He grins and winks] Just the stuff I need! "
                         "Now, there is a way into Absalot through the "
                         "underworld. You must go to the very bottom "
                         "of Kurpolis by the Fire Sea. ")
                    (prompt-for-key)
                    (say knpc 
                         "You will find a great bridge over a river of fire. "
                         "Across the bridge is a door. To pass through "
                         "the door you must know the password: ONUS. "
                         "Write that down!")
                    (prompt-for-key)
                    (say knpc
                         "Beyond the doorway is a stairway which leads up to "
                         "the lost city. The way is not easy! "
                         "It was hard when I escaped long ago, "
                         "it will be worse now."))
                  (say knpc "I see. No doubt you have important plans for "
                       "that lich's blood. I can always get some from "
                       "another adventurer.")))
            (say knpc "Bring me a vial of lich's blood and I'll tell you "
                 "a secret way.")))
      (say knpc "It's just a ruin now anyways. Everything was destroyed "
           "when it was sacked.")))

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
       "touches and commands armies of the dead. It's blood has many uses "
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
       (method 'buy  alch-trade)
       (method 'sell alch-trade)
       (method 'poti alch-trade)

       (method 'rune alch-rune)
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
     0 ;;............custom intelligence modifier
     0 ;;............custom dexterity modifier
     0 ;;............custom base hp modifier
     0 ;;............custom hp multiplier (per-level)
     0 ;;............custom base mp modifier
     0 ;;............custom mp multiplier (per-level)
     (max-hp sp_human oc_wright 8 0 0) ;;..current hit points
     0  ;;...........current experience points
     (max-mp sp_human oc_wright 8 0 0) ;;..current magic points
     8  ;;..current level
     #f ;;...........dead?
     'alch-conv ;;...conversation (optional)
     sch_alch ;;.....schedule (optional)
     nil ;;..........custom ai (optional)
     nil ;;..........container (and contents)
     nil ;;......... readied arms (in addition to the container contents)
     nil ;;..........hooks in effect
     ))
   (alch-mk)))
