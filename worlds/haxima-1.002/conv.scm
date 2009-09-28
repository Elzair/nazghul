;;----------------------------------------------------------------------------
;; Generic conversation
;;----------------------------------------------------------------------------

;; fundamentals
(define (generic-hail knpc kpc)
  (say knpc "Well met"))

(define (generic-unknown knpc kpc)
  (say knpc "I can't help you with that"))

(define (generic-bye knpc kpc)
  (say knpc "Farewell")
  (kern-conv-end))

(define (generic-join knpc kpc)
  (say knpc "I cannot join you."))

(define (generic-leav knpc kpc)
  (cond ((is-player-party-member? knpc)
         (cond ((is-only-living-party-member? knpc)
                (say knpc "Maybe I should resurrect the Wanderer first... "
                     "or sell his body parts to a thaumaturge, at least."))
               (else
                (say knpc "Do you want me to leave your party now?")
                (cond ((yes? kpc)
                       (cond ((kern-char-leave-player knpc)
                              (say knpc "If you change your mind I'll be here waiting.")
                              (kern-conv-end)
                              )
                             (else 
                              (say knpc "I can't leave right now!"))))
                      (else
                       (say knpc "You made me nervous there for a minute."))))))
         (else
          (say knpc "I'm not a member of your party!"))))

;; wise
(define (basic-ench knpc kpc)
  (say knpc "The Enchanter is the Wise Wizard. "
       "He lives in a tower by the Fens, do you need directions?")
  (quest-wise-subinit 'questentry-enchanter)
  (quest-data-update 'questentry-enchanter 'general-loc 1)
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the ladder down. You'll come out in Eastpass. "
                    "The knights can help you from there."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the road west to Trigrave and ask around there."))
              ((equal? kplace p_trigrave)
              	(quest-data-update 'questentry-calltoarms 'directions 1)
               (say knpc "Take the road north to The Fen."))
              (else 
               (say knpc "The Fens are somewhere in the northwest."))
        ))))

;; towns
(define (basic-trig knpc kpc)
  (say knpc "Trigrave is a small town in the west, "
       "settled where two rivers meet."))

(define (basic-gree knpc kpc)
  (say knpc "Green Tower, home of the Rangers, lies deep in the Great Forest. "
       "Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the road east into the forest. "
                    "Eventually it turns into a trail, follow it as best you can."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the ladder down to Westpass and ask the Rangers there."))
              ((equal? kplace p_trigrave)
               (say knpc "Take the road east to the mountains and go through Eastpass. "
                    "After that, you'll have to ask around."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              ((equal? kplace p_oparine)
               (say knpc "Take the road north to Trigrave and ask there."))
              ((equal? kplace p_moongate_clearing)
               (say knpc "Follow the road south to the junction, then travel east. "
                    "When the road bends north keep going east into the woods."))
              (else 
               (say knpc "It's in the middle of the Great Forest."))
              ))))

(define (basic-bole knpc kpc)
  (say knpc "The hamlet of Bole sits in a canyon in the mountains north of "
       "the Great Wood. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "It's northeast of here. Follow the mountains."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the ladder down to Westpass and ask the Rangers there."))
              ((equal? kplace p_trigrave)
               (say knpc "Take the road east to the mountains and go through Eastpass. "
                    "After that, you'll have to ask around."))
              ((equal? kplace p_green_tower)
               (say knpc "Go north through the forest until you hit the mountains, "
                    "then follow them east a short while."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              (else 
               (say knpc "I think it's by the mountains north of the Great Wood."))
              ))))
              
(define (basic-absa knpc kpc)
  (say knpc "Absalot, a great and wicked city, was destroyed for its sins."))

(define (basic-opar knpc kpc)
  (say knpc "The city of Oparine can be found in the southwest by a "
       "deep harbor. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the ladder down to Eastpass and follow the road west."))
              ((equal? kplace p_eastpass)
               (say knpc "Follow the road west."))
              ((equal? kplace p_trigrave)
               (say knpc "Follow the road west and south all the way to the sea."))
              ((equal? kplace p_green_tower)
               (say knpc "Follow the trail south and west to Westpass and ask the rangers when you get there."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              ((equal? kplace p_glasdrin)
               (say knpc "Take the road south."))
              ((equal? kplace p_oparine)
               (say knpc "Well, here you are already!"))
              (else 
               (say knpc "It's on the southern coast somewhere."))
              ))))

(define (basic-east knpc kpc)
  (say knpc "Eastpass guards the eastern pass into the River Plain. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Take the ladder down, you'll come out in Eastpass."))
              ((equal? kplace p_eastpass)
               (say knpc "You're here already."))
              ((equal? kplace p_trigrave)
               (say knpc "Follow the road east and you'll run right into it."))
              ((equal? kplace p_green_tower)
               (say knpc "Travel west through the woods, then follow the road west to Westpass and ask there."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              ((equal? kplace p_glasdrin)
               (say knpc "Take the road south as far as you can and ask there."))
              ((equal? kplace p_oparine)
               (say knpc "Take the road north to Trigrave and ask there."))
              (else 
               (say knpc "It's by the mountains west of the Great Forest."))
              ))))

(define (basic-west knpc kpc)
  (say knpc "Westpass guards the western pass into the Great Forest. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "This is it."))
              ((equal? kplace p_eastpass)
               (say knpc "Take the ladder down and you'll come out in it."))
              ((equal? kplace p_trigrave)
               (say knpc "Follow the road east and ask in Eastpass."))
              ((equal? kplace p_green_tower)
               (say knpc "Travel west through the woods, then follow the road west."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go south to Trigrave and ask there."))
              ((equal? kplace p_glasdrin)
               (say knpc "Take the road south as far as you can."))
              ((equal? kplace p_oparine)
               (say knpc "Take the road north to Trigrave and ask there."))
              (else 
               (say knpc "Follow the road east from Trigrave."))
              ))))

(define (basic-glas knpc kpc)
  (say knpc "Glasdrin is the fortified city of the Paladins. Do you need directions?")
  (if (yes? kpc)
      (let ((kplace (get-place knpc)))
        (cond ((equal? kplace p_westpass)
               (say knpc "Follow the road east and north."))
              ((equal? kplace p_eastpass)
               (say knpc "Go east to Westpass and ask there."))
              ((equal? kplace p_trigrave)
               (say knpc "Go east to Eastpass and ask there."))
              ((equal? kplace p_green_tower)
               (say knpc "Go west through the woods until you hit the road, then follow it north."))
              ((equal? kplace p_enchanters_tower)
               (say knpc "Go east through the pass and follow the river."))
              ((equal? kplace p_oparine)
               (say knpc "Follow the road north to Trigave and ask there, or take a ship and follow the coastline all the way north."))
              (else 
               (say knpc "It's up near the northeast coast on an island."))
              ))))

(define (basic-fens knpc kpc)
  (say knpc "The Fens are a swampy area in the northwest."))

(define (basic-kurp knpc kpc)
  (say knpc "Kurpolis is an ancient underground ruin. "
       "The entrance is somewhere in the northern mountains."))

(define (basic-lost knpc kpc)
  (say knpc "The Lost Halls? I've only heard them mentioned in bard's songs. "
       "I didn't know they really existed."))

;; establishments
(define (basic-whit knpc kpc)
  (say knpc "The White Stag is in Green Tower."))

;; quests
(define (basic-thie knpc kpc)
  (say knpc "No, I don't know anything about a thief."))

(define (basic-rune knpc kpc)
  (say knpc "I don't know much about runes. Try asking one of the Wise."))

(define (basic-wise knpc kpc)
	(say knpc "The Wise have great influence over affairs in the Shard. Do you want to know their names?")
	(if (yes? kpc)
		(begin
			(say knpc "There's the Enchanter, the Necromancer, the Alchemist, the MAN, the Engineer and the Warritrix.")
			(map quest-wise-subinit
				(list 'questentry-enchanter 'questentry-warritrix  'questentry-alchemist
						'questentry-the-man 'questentry-engineer  'questentry-necromancer)
			)
		)
	))

(define (basic-shar knpc kpc)
  (say knpc "The Shard is what we call our world.")
  (quest-data-update 'questentry-whereami 'shard 1)
  )

(define (basic-peni knpc kpc)
  (say knpc "The Peninsula is our little corner of the Shard."))

(define (basic-warr knpc kpc)
  (say knpc "The Warritrix is the Wise Warrior. If you're looking for her try Glasdrin.")
  (quest-wise-subinit 'questentry-warritrix)
  (quest-data-update 'questentry-warritrix 'general-loc 1)
  )

(define (basic-engi knpc kpc)
  (say knpc "I've heard the Engineer is the greatest Wright in the land, "
       "but I don't know much about him.")
       (quest-wise-subinit 'questentry-engineer)
       (quest-data-update 'questentry-engineer 'common 1)
       )

(define (basic-man knpc kpc)
  (say knpc "The MAN is a master wrogue. Nobody knows where his hideout is. "
       "It's rumoured that he travels in disguise.")
       (quest-wise-subinit 'questentry-the-man)
       (quest-data-update 'questentry-the-man 'common 1)
       )

(define (basic-alch knpc kpc)
  (say knpc "The Alchemist is a Wise Wright who specializes in potions. "
       "You'll find his shop in Oparine.")
       (quest-wise-subinit 'questentry-alchemist)
       (quest-data-update 'questentry-alchemist 'general-loc 1)
       )

(define (basic-necr knpc kpc)
  (say knpc "The Necromancer is a Wise Wizard who specializes in death magic. "
       "I've heard he lives in a hidden cave.")
       (quest-wise-subinit 'questentry-necromancer)
       (quest-data-update 'questentry-necromancer 'general-loc 1)
       )

(define (basic-drag knpc kpc)
  (say knpc "Stories say a mighty dragon is terrorizing shipping on the "
       "east coast."))

(define (basic-fire knpc kpc)
  (say knpc "The Fire Sea? That's a volcano on an island off the east coast."))

(define basic-conv
  (ifc '()
       ;; fundamentals
       (method 'hail generic-hail)
       (method 'default generic-unknown)
       (method 'bye generic-bye)
       (method 'join generic-join)
       (method 'leav generic-leav)
       
       ;; wise
       (method 'ench basic-ench)
       (method 'wise basic-wise)
       (method 'warr basic-warr)
       (method 'man basic-man)
       (method 'engi basic-engi)
       (method 'alch basic-alch)
       (method 'necr basic-necr)

       ;; towns & regions
       (method 'absa basic-absa)
       (method 'bole basic-bole)
       (method 'gree basic-gree)
       (method 'trig basic-trig)
       (method 'lost basic-lost)
       (method 'opar basic-opar)
       (method 'fens basic-fens)
       (method 'shar basic-shar)
       (method 'peni basic-peni)
       (method 'kurp basic-kurp)
       (method 'glas basic-glas)
       (method 'fire basic-fire)

       ;; establishments
       (method 'whit basic-whit)

       ;; quests
       (method 'thie basic-thie)
       (method 'rune basic-rune)

       ;; monsters
       (method 'drag basic-drag)

       ))

;; Helper(s)
(define (say knpc . msg) (kern-conv-say knpc msg))
(define (yes? kpc) (kern-conv-get-yes-no? kpc))
(define (no? kpc) (not (kern-conv-get-yes-no? kpc)))
(define (reply? kpc) (kern-conv-get-reply kpc))
(define (ask? knpc kpc . msg)
  (kern-conv-say knpc msg)
  (kern-conv-get-yes-no? kpc))
(define (prompt-for-key)
  (kern-log-msg "<Hit any key to continue>")
  (kern-ui-waitkey))
(define (meet msg)
  (kern-log-msg msg))
(define (get-gold-donation knpc kpc)
  (let ((give (kern-conv-get-amount kpc))
        (have (kern-player-get-gold)))
    (cond ((> give have)
           (say knpc "You don't have that much!")
           0)
          (else
           (kern-player-set-gold (- have give))
           give))))
(define (get-food-donation knpc kpc)
  (let ((give (kern-conv-get-amount kpc))
        (have (kern-player-get-food)))
    (cond ((> give have)
           (say knpc "You don't have that much!")
           0)
          (else
           (kern-player-set-food (- have give))
           give))))
(define (working? knpc)
  (string=? "working" (kern-obj-get-activity knpc)))

;; Not really an aside in the theatrical sense, this routine causes a party
;; member to interject something into the conversation. kpc is the character
;; being conversed with, mem-tag is either nil or the party member who should
;; do the interjection. If mem-tag is nil then a party member (other than the
;; speaker) will be chosen at random. msg is the text of the comment. If kpc is
;; the only member of the party then the aside will not do anything.
(define (aside kpc kchar-tag . msg)
  ;;(println msg)
  (if (null? kchar-tag)
      (let ((members (filter (lambda (kchar)
                               (not (eqv? kchar kpc)))
                             (kern-party-get-members (kern-get-player)))
                     ))
        (if (not (null? members))
            (let ((kchar (random-select members)))
              (say kchar msg)
              #t)
            #f)
        )
      (if (in-player-party? kchar-tag)
          (begin
            (kern-conv-say (eval kchar-tag) msg)
            #t)
          #f)
      ))
         
;;----------------------------------------------------------------------------
;; Quests
;;----------------------------------------------------------------------------
(define (mk-quest) (list #f #f #f))
(define (quest-offered? qst) (car qst))
(define (quest-accepted? qst) (cadr qst))
(define (quest-done? qst) (caddr qst))
(define (quest-offered! qst val) (set-car! qst val))
(define (quest-accepted! qst val) (set-car! (cdr qst) val))
(define (quest-done! qst val) (set-car! (cddr qst) val))


;;----------------------------------------------------------------------------
;; Ranger Conversation
;;----------------------------------------------------------------------------
(define (ranger-ranger knpc kpc)
  (say knpc "Rangers guard the borders between wilderness and "
       "civilization. We patrol the frontier and give aid where we can to the "
       "Wise."))

(define (ranger-wise knpc kpc)
  (say knpc "Rangers have an informal alliance with the Wise. They give us "
       "aid and hospitality. In return we give them news. Sometimes we serve "
       "them as messengers and scouts."))

(define (ranger-join knpc kpc)
  (cond ((has? kpc t_ranger_orders 1)
         (say knpc "Let's see those orders... ok. Looks like we're partners "
              "for a while.")
         (take kpc t_ranger_orders 1)
         (join-player knpc)
         ;; NOTE: the following only permits one ranger at a time to join the
         ;; player!
         (kern-tag 'ch_ranger_merc knpc)
         (give kpc t_arrow 20)
         (kern-conv-end)
         )
        (else
         (say knpc "Sorry, I've got to get back to my patrol."))))

(define (ranger-band knpc kpc)
  (say knpc "When men get in trouble with the law, they flee to the woods. "
       "There are always bandits in the forest."))

(define ranger-conv
  (ifc basic-conv
       (method 'join ranger-join)
       (method 'rang ranger-ranger)
       (method 'wise ranger-wise)
       (method 'band ranger-band)
       ))


;; Knight conversation -- used by Lord Froederick's troops
(define knight-conv basic-conv)

;; Glasdrin
(define (glasdrin-warr knpc kpc)
  (if (player-found-warritrix?)
      (say knpc "We all mourn her loss.")
      (say knpc "The Warritrix is the most cunning warrior of the age. I'm not sure where she is right now, ask the Steward or Commander Jeffries.")
  	)
  	(quest-data-update 'questentry-warritrix 'general-loc 1)
  )

(define (glasdrin-stew knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "Her name of the Stewardess is a curse among us now. The new Steward is Valus, a former commander.")
      (say knpc "The Steward is the keeper of the city and realms of Glasdrin. You can usually find her in the Citadel.")))

(define (glasdrin-jeff knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "At best Jeffries failed in his duties as commander to protect those under his command. "
           "At worst, he was an accomplice in the betrayal of the Warritrix. "
           "Our new commander is Janice.")
      (say knpc "Jeffries is the commander of the Glasdrin militia. He's usually at work in the Citadel.")
      ))

(define (glasdrin-kurp knpc kpc)
         (say knpc "Take the bridge north across the river then follow the "
              "mountains east and north into a canyon."))
(define (glasdrin-cita knpc kpc)
  (say knpc "The Citadel is the inner keep in the north part of the castle."))
(define (glasdrin-ghol knpc kpc)
  (say knpc "I seem to recall a man named Gholet was arrested for theft. You might check the Citadel's dungeon.")
   (quest-data-update 'questentry-ghertie 'gholet-dungeon 1)
   )
(define (glasdrin-kurp knpc kpc)
  (say knpc "The dungeon Kurpolis is where most of our troops are now. Follow the mountains west, you'll find the entrance in a canyon."))

(define (glasdrin-glas knpc kpc)
  (say knpc "Glasdrin is the city of the Paladins."))

(define (glasdrin-pala knpc kpc)
  (say knpc "The Paladins of Glasdrin are the greatest military force in the realm."))

(define glasdrin-conv
  (ifc basic-conv
       (method 'warr glasdrin-warr)
       (method 'stew glasdrin-stew)
       (method 'jeff glasdrin-jeff)
       (method 'kurp glasdrin-kurp)
       (method 'cita glasdrin-cita)
       (method 'ghol glasdrin-ghol)
       (method 'kurp glasdrin-kurp)
       (method 'glas glasdrin-glas)
       (method 'pala glasdrin-pala)
       (method 'jani 
               (lambda (knpc kpc) 
                 (if (player-stewardess-trial-done?)
                      (say knpc "The military council has elected Janice to replace Jeffries as commander of the militia.")
                      (say knpc "Jeffries is an able leader, but everyone knows that his assistant Janice has the brains."))))
       (method 'valu
               (lambda (knpc kpc)
                 (if (player-stewardess-trial-done?)
                     (say knpc "Valus was exonerated by the Stewardess's journal. "
                          "It is a shame we ever believed such lies about him. "
                          "We have elected him to be the new Steward.")
                     (say knpc "Valus has been imprisoned for shameful indecency. "
                          "It's a pity, really. He was a well-respected general."))))
       ))

;; Kurpolis
(define kurpolis-conv
  (ifc basic-conv
       ))

;; Green Tower
(define (gt-gobl knpc kpc)
  (say knpc "Since the goblin wars there's been an uneasy truce. Sometimes they trade here in town, but if you meet them in the forest be careful."))
(define (gt-towe knpc kpc)
  (say knpc "The tower that gives this town its name is now the Ranger headquarters."))
(define (gt-ruin knpc kpc)
  (say knpc "The old ruins are in the southwest corner of town."))
(define (gt-band knpc kpc)
  (say knpc "Ask Deric about bandits. "
       "He's the one who should be dealing with them."))


(define green-tower-conv
  (ifc basic-conv
       (method 'gree
               (lambda (knpc kpc)
                 (say knpc "Yes, this town gets its name from the old tower in its center.")))
       (method 'gobl gt-gobl)
       (method 'towe gt-towe)
       (method 'ruin gt-ruin)
       (method 'band gt-band)
       ))

;; Trigrave
(define trigrave-conv
  (ifc basic-conv
       (method 'thie 
               (lambda (knpc kpc) 
                 (say knpc "I don't know anything about a thief. Ask Gwen, maybe a traveler told her something.")))                       
       ))

;;----------------------------------------------------------------------------
;; Merchant

;; Indices into the merchant message list
(define merch-closed           0)
(define merch-buy              1)
(define merch-sell             2)
(define merch-trade            3)
(define merch-sold-something   4)
(define merch-sold-nothing     5)
(define merch-bought-something 6)
(define merch-bought-nothing   7)
(define merch-traded-something 8)
(define merch-traded-nothing   9)

(define (conv-trade knpc kpc menu msgs catalog)
  (println "conv-trade: " (kern-obj-get-activity knpc))
  ;;(println "conv-trade: " menu msgs catalog)
  (if (and (not (string=? "working" (kern-obj-get-activity knpc)))
           (not (null? (list-ref msgs merch-closed))))
      (say knpc (list-ref msgs merch-closed) 
           " I'm " (kern-obj-get-activity knpc) " right now.")
      (cond ((string=? menu "buy")
             (say knpc (list-ref msgs merch-buy))
             (if (kern-conv-trade knpc kpc "buy" catalog)
                 (say knpc (list-ref msgs merch-sold-something))
                 (say knpc (list-ref msgs merch-sold-nothing))))
            ((string=? menu "sell")
             (say knpc (list-ref msgs merch-sell))
             (if (kern-conv-trade knpc kpc "sell" catalog)
                 (say knpc (list-ref msgs merch-bought-something))
                 (say knpc (list-ref msgs merch-bought-nothing))))
            (else
             (say knpc (list-ref msgs merch-trade))
             (if (kern-conv-trade knpc kpc "trade" catalog)
                 (say knpc (list-ref msgs merch-traded-something))
                 (say knpc (list-ref msgs merch-traded-nothing))))
            )))

